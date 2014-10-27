open Eliom_content.Html5.F

module type S = sig
  val main_service :
    (unit, unit, [< Eliom_service.service_method > `Get ],
     [< Eliom_service.attached > `Attached ],
     [< Eliom_service.service_kind > `Service ], [ `WithoutSuffix ],
     unit, unit, [< Eliom_service.registrable > `Registrable ],
     [> Eliom_service.appl_service ]) Eliom_service.service

  val create_event_service :
    (unit, string, [< Eliom_service.service_method > `Post ],
     [< Eliom_service.attached > `Attached ],
     [< `AttachedCoservice | `Service > `Service ], [ `WithoutSuffix ],
     unit, [ `One of string ] Eliom_parameter.param_name,
     [< Eliom_service.registrable > `Registrable ],
     [> Eliom_service.appl_service ]) Eliom_service.service

  val submit_service :
    (string, string, [< Eliom_service.service_method > `Post ],
     [< Eliom_service.attached > `Attached ],
     [< `AttachedCoservice | `Service > `Service ], [ `WithSuffix ],
     [ `One of string ] Eliom_parameter.param_name,
     [ `One of string ] Eliom_parameter.param_name,
     [< Eliom_service.registrable > `Registrable ],
     [> Eliom_service.appl_service ]) Eliom_service.service      

  val delete_event_service :
    (string, unit, [< Eliom_service.service_method > `Post ],
     [< Eliom_service.attached > `Attached ],
     [< `AttachedCoservice | `Service > `Service ], [ `WithSuffix ],
     [ `One of string ] Eliom_parameter.param_name, unit,
     [< Eliom_service.registrable > `Registrable ],
     [> Eliom_service.appl_service ]) Eliom_service.service

  (* Uffff… *)
end

module Make (M: S) = struct
  open M

  let uri s = uri_of_string (fun () -> s)

  let mkhead t =
    (head (title (pcdata t)) [
        meta ~a:[a_charset "utf-8"] ();
        meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scape=1"] ();
        link ~rel:[`Stylesheet] ~href:(uri "reset.css") ();
        link ~rel:[`Stylesheet] ~href:(uri "design.css") ();
        link ~rel:[`Icon] ~href:(uri "images/favicon.png") ();
      ])

  let mkbody content =
    body ([
        header [
          h1 [a ~service:main_service ~a:[a_title "Homepage"] [pcdata "The Chosen"] ()]
        ];
      ] @ content)

  let footer_contents =
    div ~a:[a_id "footerbottom"] [
      hr ();
      pcdata "Copyleft 2014 - An open-source project by ";
      Raw.a ~a:[a_href (uri "https://github.com/Tremege/"); a_title "Github"]
        [pcdata "Trémège Inc"];
      pcdata "."
    ]
    
  let main =
    html
      (mkhead "The Chosen")
      (mkbody [
          section ~a:[a_id "homepage"] [
            div ~a:[a_class ["wrap"]] [
              div ~a:[a_class ["about"]] [
                pcdata "Create events, invite people, randomly pick participants."
              ];
              post_form ~service:create_event_service (fun desc ->
                  [fieldset [
                      input ~name:desc ~input_type:`Text ~a:[a_id "addevent"; a_placeholder "Name of your event"] ();
                      br ();
                      input ~input_type:`Submit ~value:"Create" ();
                    ]]
                ) ();
            ]
          ];
          footer [
            div ~a:[a_id "homefooter"] [
              article [
                h2 [pcdata "Easy to create"];
                p [pcdata "It's easy to create an event, no need to register. A 2s only step."];
              ];
              article [
                h2 [pcdata "Easy to participate"];
                p [pcdata "It's easy to participate to an event: just type in your pseudo."];
              ];
              article [
                h2 [pcdata "Easy to manage"];
                p [pcdata "Monitor in real-time the participants, pick randomly some of those in one click."];
              ];
            ];
            footer_contents
          ]
        ])

  let generic_page t content =
    html
      (mkhead (t ^ " | The Chosen"))
      (mkbody [
          section [
            div ~a:[a_class ["wrap"]] content;
          ];
          footer [
            footer_contents
          ]
        ])

  let invite desc mininame =
    generic_page desc [
      h2 [pcdata ("Participate to: " ^ desc)];
      post_form ~service:submit_service (fun submit ->
          [fieldset [
              input ~name:submit ~input_type:`Text ~a:[a_id "addevent"; a_placeholder "Name, pseudo, email…"] ();
              br ();
              input ~input_type:`Submit ~value:"Participate" ();
          ]]) mininame
    ]

  let invite_done desc submitted =
    generic_page desc [
      h2 [pcdata ("Participate to: " ^ desc)];
      p [pcdata "You registered for this event as: "; b [pcdata submitted]; pcdata "."]
    ]

  let event_not_found =
    generic_page "Error" [p [pcdata "Error: this event doesn't exist."]]

  let already_submitted submit =
    generic_page "Error" [
      p [pcdata "Error: you already registered for this event as: "; b [pcdata submit]; pcdata "."]
    ]
      
  let admin_cannot_submit =
    generic_page "Error" [
      p [pcdata "Error: you cannot register for an event you created."]
    ]

  let admin desc mininame counter_node app_content =
    let invite = Config.hostname ^ "/" ^ mininame in
    let twitter =
      "http://twitter.com/intent/tweet?" ^
      (Uri.encoded_of_query [("text", [desc]); ("url", [invite])])
    in
    let facebook =
      "https://www.facebook.com/sharer/sharer.php?" ^
      (Uri.encoded_of_query [("u", [invite])])
    in

    generic_page desc ([
        h2 [pcdata ("Manage: " ^ desc)];
      
        div ~a:[a_id "share"] [
          input ~input_type:`Text ~value:invite
            ~a:[a_readonly `ReadOnly] ();
          ul [
            li [
              Raw.a ~a:[a_href (uri twitter);
                        a_target "_blank";
                        a_title "Share on Twitter"] [
                img ~src:(uri "images/twitter.svg") ~alt:"Twitter" ();
              ];
            ];
            li [
              Raw.a ~a:[a_href (uri facebook);
                        a_target "_blank";
                        a_title "Share on Facebook"] [
                img ~src:(uri "images/facebook.svg") ~alt:"Facebook" ();
              ];
            ];
          ];
        ];
        hr ();
        
        p [pcdata "You have ";
           span ~a:[a_id "participants"] [counter_node];
           pcdata " participants"];
        hr ();
      ] @ app_content @ [
        hr ();
               
        div ~a:[a_id "delete"] [
          post_form ~service:delete_event_service
            (fun () ->
               [fieldset [
                   input ~input_type:`Submit ~value:"Delete this event" ()
               ]]) mininame
        ]
      ])
end

