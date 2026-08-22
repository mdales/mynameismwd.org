open Htmlit
open Webplats

let months = [| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sept" ; "Oct" ; "Nov"; "Dec" |]

let ptime_to_str (t : Ptime.t) : string =
  let ((year, month, day), _) = Ptime.to_date_time t in
  Printf.sprintf "%d %s %d" day months.(month - 1) year

let render_index site =
	let header = Render.render_head ~site () in
	let topbar = Renderer.render_header (Section.uri (Site.toplevel site)) (Section.title (Site.toplevel site)) in
	let footer = Renderer.render_footer () in

	let sectionlist = List.map (fun sec ->
		El.div [
			El.a ~at:[At.href (Uri.to_string (Section.uri sec))] [
				El.div ~at:[At.class' ("homebutton colour-" ^ (Section.title sec))] [
					El.h3 [ El.txt (Section.title sec)];
					El.p [
						El.txt (
							(Int.to_string (List.length (Section.pages sec))) ^ " " ^
							(Section.title sec) ^ ", last updated " ^ (ptime_to_str (Page.date (List.hd (Section.pages sec))))
						)
					]
				]
			]
		]
	) (List.filter (fun s -> not (Section.synthetic s)) (Site.sections site)) in

	let body = El.body [
		El.div ~at:[At.class' "almostall"] [
			topbar;
			El.div ~at:[At.id "container"] [
				El.div ~at:[At.class' "content"] [
					El.section ~at:[At.role "main"] [
						El.div ~at:[At.class' "article"] [
							El.h2 [El.txt "My things here"];
							El.div ~at:[At.class' "index"] sectionlist;
							El.h2 [El.txt "My other sites"];
							El.div ~at:[At.class' "index"] [
								El.div [
									El.a ~at:[At.href "https://mwdales-guitars.uk/"] [
										El.div ~at:[At.class' "homebutton colour-EF"] [
											El.h3 [El.txt "M. W. Dales Guitars"];
											El.p [El.txt "Guitar building, designing, 3D-printing, laser-cutting"]
										]
									]
								];
								El.div [
									El.a ~at:[At.href "https://digitalflapjack.com/"] [
										El.div ~at:[At.class' "homebutton colour-DF"] [
											El.h3 [El.txt "Digital Flapjack"];
											El.p [El.txt "Programming mostly, with a bit of design and planning too"]
										]
									]
								]
							]
						]
					]
				]
			];
			footer
		]
	] in

	El.html [header; body]
