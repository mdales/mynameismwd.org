open Htmlit
open Webplats

let render_section site sec =
	let header = Render.render_head ~site ~sec () in
	let topbar = Renderer.render_header (Section.uri sec) (Section.title sec) in
	let footer = Renderer.render_footer () in

	let pagelist = List.map (fun page ->
		let imageinfo = match (Page.titleimage page) with
		| Some i -> (
			let desc = match i.description with
			| Some txt -> [At.alt txt]
			| None -> []
			in
			[
				At.src (Uri.to_string (Section.uri ~page ~resource:{|thumbnail.jpg|} sec));
				At.v "srcset" ((Uri.to_string (Section.uri ~page ~resource:{|thumbnail@2x.jpg|} sec)) ^ " 2x, " ^ (Uri.to_string (Section.uri ~page ~resource:{|thumbnail.jpg|} sec)) ^ " x1");
			] @ desc
		)
		| None -> []
		in

		El.div ~at:[At.class' ("tagcell colour-" ^ (Page.original_section_title page))] [
			El.div ~at:[At.class' "tagcelllabel"] [
				El.span [El.txt (Page.title page)]
			];
			El.div ~at:[At.class' "tagcellinner"] [
				El.a ~at:[At.href (Uri.to_string (Section.uri ~page sec))] [
					El.div ~at:[At.class' "tagcellimage"] [
						El.figure [
							El.img ~at:([
								At.v "loading" "lazy";
							] @ imageinfo) ()
						]
					]
				]
			]
		]
	) (Section.pages sec) in

	let body = El.body [
		El.div ~at:[At.class' "almostall"] [
			topbar;
			El.div ~at:[At.id "container"] [
				El.div ~at:[At.class' "content"] [
					El.section ~at:[At.role "main"] [
						El.div ~at:[At.class' "tagcontents"] [
							El.h2 [El.txt "Snapshots"];
							El.div ~at:[At.class' "tagcellholder"] pagelist;
						]
					]
				]
			];
			footer;
		]
	] in

	El.html [header; body]


let is_image_retina dims =
	match dims with
	| None -> true
	| Some (width, height) -> (
		(width > (720 * 2)) && (height > (1200 * 2))
	)

let render_body page =
	let imagelist = List.map (fun (i : Frontmatter.image) ->
		let name, ext = Fpath.split_ext (Fpath.v i.filename) in
		let desc = match i.description with
		| Some txt -> [At.alt txt]
		| None -> []
		in
		let retina_info = match (is_image_retina i.dimensions) with
		| true -> (
			let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
			[At.v "srcset" (retina_filename ^ " 2x, " ^ i.filename ^ " 1x")]
		)
		| false -> []
		in

		El.div ~at:[At.class' "snapshotitem single"] [
			El.figure ~at:[At.class' "single"] [
				El.img ~at:([
					At.src i.filename
				] @ retina_info @ desc) ()
			];
			El.div ~at:[At.class' "holder holder-top-left"] [];
			El.div ~at:[At.class' "holder holder-top-right"] [];
			El.div ~at:[At.class' "holder holder-bottom-left"] [];
			El.div ~at:[At.class' "holder holder-bottom-right"] [];
		]

	) (Page.images page)
	in
	let videolist = List.map (fun (filename : string) ->
		El.div ~at:[At.class' "video"] [
			El.video ~at:[At.v "controls" ""] [
				El.source ~at:[
					At.src filename;
					At.type' "video/mp4"
				] ()
			]
		]
	) (Page.videos page) in
	let itemlist = imagelist @ videolist in
	[
		El.unsafe_raw (Render.render_body page);
		El.div ~at:[At.class' "snapshotlist"] itemlist
	]

let render_page site sec previous_page page next_page =
	let header = Render.render_head ~site ~sec () in
	let topbar = Renderer.render_header (Section.uri sec) (Section.title sec) in
	let footer = Renderer.render_footer () in

	let body = El.body [
		El.div ~at:[At.class' "almostall"] [
			topbar;
			El.div ~at:[At.id "container"] [
				El.div ~at:[At.class' "content"] [
					El.section ~at:[At.role "main"] [
						El.div ~at:[At.class' "article"] [
							El.article ([
								El.div ~at:[At.class' "headerflex"] [
									El.div ~at:[At.class' "headerflextitle"] [
										El.h3 [El.txt (Page.title page)]
									];
									El.div ~at:[At.class' "headerflexmeta"] [
										El.p [
											El.txt (Renderer.ptime_to_str (Page.date page))
										]
									]
								]
							] @ (render_body page) @ (
								[Renderer.navigation_links sec previous_page next_page]
							))
						]
					]
				]
			];
			footer
		]
	] in

	El.html [header; body]
