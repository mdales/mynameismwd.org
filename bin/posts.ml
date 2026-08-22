open Htmlit
open Webplats

let months = [| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sep" ; "Oct" ; "Nov"; "Dec" |]

let ptime_to_str (t : Ptime.t) : string =
  let ((year, month, day), _) = Ptime.to_date_time t in
  Printf.sprintf "%d %s %d" day months.(month - 1) year

let render_section site sec =
	let header = Render.render_head ~site ~sec () in
	let topbar = Renderer.render_header (Section.uri sec) (Section.title sec) in
	let footer = Renderer.render_footer () in

	let pagelist = List.map (fun page ->
		El.div
		~at:[At.class' "blogcontents__item"]
		[
			El.ul
			~at:[At.class' "leaders"]
			[
				El.li [
					El.span [
						El.a
						~at:[ At.href (Uri.to_string (Section.uri ~page sec))] [
							El.txt (Page.title page)
						]
					];
					El.span [
						El.txt (ptime_to_str (Page.date page))
					]
				]
			];
			El.div
			~at:[At.class' "blogcontents__item__inner"] [
				El.div [
					El.p [
						El.txt (match (Page.synopsis page) with None -> "" | Some p -> p)
					]
				]
			]
		]
	) (Section.pages sec)
	in

	let body =
		El.body
			[
				El.div
				~at:[ At.class' "almostall" ]
				[
					topbar;
					El.div
					~at:[ At.class' "container" ]
					[
						El.section
						~at:[At.role "main"]
						[
							El.div
							~at:[ At.class' "blogcontents" ] pagelist
						]
					];
					footer
				]
			]
	in

  El.html [ header; body ]