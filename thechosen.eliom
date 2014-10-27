{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Lwt
}}

{client{
  open D
}}

open Batteries

module TheChosen_app =
  Eliom_registration.App (
    struct
      let application_name = "thechosen"
    end)

(*
 * Ocsipersist db, storing [event mininame -> descr, registered users]
 *)
let db = Ocsipersist.open_table Config.db_name

let fresh_name () =
  let next_char c =
    if (Char.code c >= Char.code 'a' && Char.code c < Char.code 'z') ||
       (Char.code c >= Char.code 'A' && Char.code c < Char.code 'Z') ||
       (Char.code c >= Char.code '0' && Char.code c < Char.code '9') then
      Some (Char.chr (Char.code c + 1))
    else if c = 'z' then
      Some 'A'
    else if c = 'Z' then
      Some '0'
    else
      None
  in
  let next name =
    let rec incr = function
      | [] -> ['a']
      | c::cs ->
        match next_char c with
        | None -> 'a' :: (incr cs)
        | Some c' -> c' :: cs
    in
    name |> String.to_list |> List.rev |> incr |> List.rev |> String.of_list
  in
  Lwt.catch
    (fun () -> Ocsipersist.find db "#next" >|= fst)
    (fun _ -> Lwt.return "aa") >>= fun next_name ->
  let next_next = next next_name in
  Ocsipersist.add db "#next" (next_next, Set.empty) >>= fun () ->
  Lwt.return next_name

let get_event event =
  Lwt.catch
    (fun () -> Ocsipersist.find db event >>= fun x -> Lwt.return (Some x))
    (fun _ -> Lwt.return None)

let pick_n (name, n, excluded) =
  get_event name >|= fun e ->
  match e with
  | None -> []
  | Some (_, set) ->
    Set.diff set (Set.of_list excluded)
    |> Set.enum
    |> Random.multi_choice n
    |> List.of_enum

{shared{
  type pick_n_rpc = (string * int * string list) deriving(Json)
}}

(*
 * Eliom ref, scope: user's browser, storing its participation to events.
 *)

type event_participation =
  | Admin
  | Submitted of string
  | Nil

let participation_ref = Eliom_reference.eref
                          ~scope:Eliom_common.default_session_scope
                          ~persistent:Config.cookies_db_name
                          Map.empty

let participation event =
  Eliom_reference.get participation_ref >|= fun m ->
  (try begin match Map.find event m with
       | `Admin -> Admin
       | `Submitted s -> Submitted s end
       with Not_found -> Nil)

let set_participation event p =
  Eliom_reference.modify participation_ref (fun m ->
    match p with
    | Admin -> Map.add event `Admin m
    | Submitted s -> Map.add event (`Submitted s) m
    | Nil -> Map.remove event m)

(*
 * Counters signals hashtable, for server->client communications
 *)
let counters = Hashtbl.create 37

let counter name =
  try Hashtbl.find counters name with
    Not_found ->
    let e, send_e = React.E.create () in
    let e = Eliom_react.Down.of_react e in
    Hashtbl.add counters name (e, send_e);
    e, send_e

(*
 * Some client code
 *)

{client{
  let names_s, send_names = React.S.create []
  let pick_n_rpc = %(server_function Json.t<pick_n_rpc> pick_n)

  let pick_n name field _ =
    Lwt.async (fun () ->
      let n = field##value |> Js.parseInt in
      pick_n_rpc (name, n, []) >|= fun chosen ->
      send_names chosen
    )

  let pick_another name _ =
    Lwt.async (fun () ->
      let names = React.S.value names_s in
      pick_n_rpc (name, 1, names) >|= fun another ->
      send_names (names @ another)
    )

  let build_table names =
    let open D in
    if names = [] then pcdata "" else
      table
        (List.mapi (fun i name ->
             tr [td [pcdata (string_of_int (i+1))];
                 td [pcdata name]])
            names)
}}

(*
 * Services
 *)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let create_event_service =
  Eliom_service.App.post_service ~fallback:main_service
    ~post_params:Eliom_parameter.(string "event_desc") ()

let event_service =
  Eliom_service.App.service ~path:[]
    ~get_params:Eliom_parameter.(suffix (string "mininame")) ()

let delete_event_service =
  Eliom_service.App.post_service ~fallback:event_service
    ~post_params:Eliom_parameter.unit ()

let submit_service =
  Eliom_service.App.post_service ~fallback:event_service
    ~post_params:Eliom_parameter.(string "submit") ()

(*
 * Pages
 *)

module P = Pages.Make (struct
  let main_service = main_service
  let create_event_service = create_event_service
  let submit_service = submit_service
  let delete_event_service = delete_event_service
end)

open P

let () =
  Random.self_init ();
  let open Eliom_registration in

  TheChosen_app.register
    ~service:main_service
    (fun () () -> Lwt.return main);

  Redirection.register
    ~service:create_event_service
    (fun () desc ->
      fresh_name () >>= fun mininame ->
      Ocsipersist.add db mininame (desc, Set.empty) >>= fun () ->
      set_participation mininame Admin >>= fun () ->
      Lwt.return (Eliom_service.preapply event_service mininame));
 
  TheChosen_app.register
    ~service:event_service
    (fun mininame () ->
      get_event mininame >>= fun e ->
      match e with
      | None -> Lwt.return event_not_found
      | Some (desc, set) ->
        begin
          participation mininame >|= fun p ->
          match p with
          | Admin ->
            let c, _ = counter mininame in
            let n = Set.cardinal set in

            let open D in
            let pick_nb_input = int_input ~input_type:`Number
                ~a:[a_input_min 1.]
                ~value:1
                ()
            in

            admin desc mininame
              (C.node {{ R.pcdata React.S.(%c |> hold %n |> map string_of_int) }})

              [div ~a:[a_id "chosen"] [
                  Raw.form [
                    div ~a:[a_class ["pick"]] [
                      pick_nb_input;
                      button ~button_type:`Button
                        ~a:[a_onclick {{ pick_n %mininame (To_dom.of_input %pick_nb_input) }}]
                        [pcdata "Pick"]
                    ];
                    br ();
                
                    C.node {{ R.node (React.S.map build_table names_s) }};
                    button ~button_type:`Button
                      ~a:[a_onclick {{ pick_another %mininame }}]
                      [pcdata "Pick another one"];
                  ];
                ];
              ]

          | Submitted s -> invite_done desc s
          | Nil -> invite desc mininame
        end
    );

  Any.register
    ~service:submit_service
    (fun mininame submit ->
      participation mininame >>= fun p ->
      match p with
      | Nil ->
         Lwt.catch
           (fun () ->
              Ocsipersist.find db mininame >>= fun (desc, set) ->
              let set' = Set.add submit set in
              Ocsipersist.add db mininame (desc, set') >>= fun () ->
              set_participation mininame (Submitted submit) >>= fun () ->
              let _, send = counter mininame in
              send (Set.cardinal set') |> Lwt.return)
           (fun _ -> Lwt.return ()) >>= fun () ->
         Redirection.send (Eliom_service.preapply event_service mininame)
      | Admin ->
         TheChosen_app.send admin_cannot_submit
      | Submitted s ->
         TheChosen_app.send (already_submitted s));

  Redirection.register
    ~service:delete_event_service
    (fun mininame () ->
       Ocsipersist.remove db mininame >>= fun () ->
       set_participation mininame Nil >>= fun () ->
       Lwt.return main_service)
