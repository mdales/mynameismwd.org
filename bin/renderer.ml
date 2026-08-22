open Htmlit
open Webplats

let render_header uri title =
	El.div
	~at:[At.class' "header stripes"] [
		El.header ~at:[At.role "banner"] [

			El.a (*~at:[At.ref="home"]*) [
				El.h1 [
					El.a
					~at:[At.href "/"] [
						El.txt "my name is mwd"
					]
				];
				El.h2 [
					El.txt "the ";
					El.a ~at:[At.href (Uri.to_string uri)] [
						El.txt title
					];
					El.txt " of Michael Winston Dales"
				]
			]
		]
	]


let months = [| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sept" ; "Oct" ; "Nov"; "Dec" |]

let ptime_to_str (t : Ptime.t) : string =
	let ((year, month, day), _) = Ptime.to_date_time t in
	Printf.sprintf "%s %d, %d" months.(month - 1) day year

let render_footer () =
	El.div
	~at:[At.id "foot"; At.class' "stripes"] [
		El.nav [
			El.div
			~at:[At.id "endlinks"] [
				El.div [
					El.ul ~at:[At.class' "rsslinks"] [
						El.li [
							El.a ~at:[At.href "/"] [El.txt "All"];
							El.txt " (";
							El.a ~at:[
								At.href "/index.xml";
								At.type' "application/rss+xml";
								At.v "target" "_blank";
							] [ El.txt "RSS"];
							El.txt ")"
						];
						El.li [
							El.a ~at:[At.href "/posts/"] [El.txt "Posts"];
							El.txt " (";
							El.a ~at:[
								At.href "/posts/index.xml";
								At.type' "application/rss+xml";
								At.v "target" "_blank";
							] [ El.txt "RSS"];
							El.txt ")"
						];
						El.li [
							El.a ~at:[At.href "/sounds/"] [El.txt "Sounds"];
							El.txt " (";
							El.a ~at:[
								At.href "/sounds/index.xml";
								At.type' "application/rss+xml";
								At.v "target" "_blank";
							] [ El.txt "RSS"];
							El.txt ")"
						];
					]
				];
				El.div [
					El.ul ~at:[At.class' "rsslinks"] [
						El.li [
							El.a ~at:[At.href "/photos/"] [El.txt "Photos"];
							El.txt " (";
							El.a ~at:[
								At.href "/photos/index.xml";
								At.type' "application/rss+xml";
								At.v "target" "_blank";
							] [ El.txt "RSS"];
							El.txt ")"
						];
						El.li [
							El.a ~at:[At.href "/snapshots/"] [El.txt "Snapshots"];
							El.txt " (";
							El.a ~at:[
								At.href "/snapshots/index.xml";
								At.type' "application/rss+xml";
								At.v "target" "_blank";
							] [ El.txt "RSS"];
							El.txt ")"
						];
						El.li [
							El.a ~at:[At.href "/zines/"] [El.txt "Zines"];
							El.txt " (";
							El.a ~at:[
								At.href "/zines/index.xml";
								At.type' "application/rss+xml";
								At.v "target" "_blank";
							] [ El.txt "RSS"];
							El.txt ")"
						];
					]
				];
				El.div [
					El.ul ~at:[At.class' "rsslinks"] [
						El.li [
							El.a ~at:[At.href "https://mwdales-guitars.uk"] [El.txt "Guitar making"];
						];
						El.li [
							El.a ~at:[At.href "https://digitalflapjack.com"] [El.txt "Computering"];
						];
					]
				];
				El.div [
					El.ul ~at:[At.class' "rsslinks"] [
						El.li [
							El.a ~at:[At.href "https://toot.mynameismwd.org/@michael"] [El.txt "Social"];
						];
						El.li [
							El.a ~at:[At.href "/about/"] [El.txt "About"];
						];
						El.li [
							El.a ~at:[At.href "/search/"] [El.txt "Search"];
						];
					]
				]
			]
		]
	]


	let render_section site sec =
		let header = Render.render_head ~site ~sec () in
		let topbar = render_header (Section.uri sec) (Section.title sec) in
		let footer = render_footer () in

		let pagelist = List.map (fun page ->
			El.li [
				El.a ~at:[At.href (Uri.to_string (Section.uri ~page sec))] [
					El.txt (Page.title page)
				]
			]
		) (Section.pages sec) in

		let body = El.body [
			El.div ~at:[At.class' "allmostall"] [
				topbar;
				El.ul pagelist;
				footer;
			]
		] in

		El.html [header; body]


(*
let render_error site _error _debug_info suggested_response =
	let status = Dream.status suggested_response in
	let code = Dream.status_to_int status
	and reason = Dream.status_to_string status in

	Dream.set_header suggested_response "Content-Type" Dream.text_html;
	Dream.set_body suggested_response begin
		<html>
		<%s! (Render.render_head ~site ()) %>
		<body>
			<div class="almostall">
				<%s! render_header (Section.uri (Site.toplevel site)) (Section.title (Site.toplevel site)) %>
				<div id="container">
					<div class="content">
						<section role="main">
							<div class="article">
								<article>
									<h2><%i code %> <%s reason %></h2>
								</article>
							</div>
						</section>
					</div>
				</div>
				<%s! render_footer () %>
			</div>
		</body>
		</html>
	end;
	Lwt.return suggested_response *)

let navigation_links sec previous_page next_page =
	let previous_page =
		match previous_page with
		| Some page ->
				[
					El.li [
						El.strong [El.txt "Next"];
						El.txt ": ";
						El.a ~at:[At.href (Uri.to_string (Section.uri ~page sec))] [
							El.txt (Page.title page)
						]
					]
				]
		| None -> []
	and next_page =
		match next_page with
		| Some page ->
				[
					El.li [
						El.strong [El.txt "Previous"];
						El.txt ": ";
						El.a ~at:[At.href (Uri.to_string (Section.uri ~page sec))] [
							El.txt (Page.title page)
						]
					]
				]
		| None -> []
	in
	El.div
		~at:[ At.class' "postscript" ] [ El.ul
		(List.concat_map Fun.id [ previous_page; next_page ])
	]

let render_page site sec previous_page page next_page =
	let header = Render.render_head ~site ~sec () in
	let topbar = render_header (Section.uri sec) (Section.title sec) in
	let footer = render_footer () in
	let navlinks = navigation_links sec previous_page next_page in

	let body = El.body [
		El.div ~at:[At.class' "allmostall"] [
			topbar;
			El.div ~at:[At.id "container"] [
				El.div ~at:[At.class' "content"] [
					El.section ~at:[At.role "main"] [
						El.div ~at:[At.class' "article"] [
							El.article [
								El.div ~at:[At.class' "headerflex"] [
									El.div ~at:[At.class' "headerflextitle"] [
										El.h3 [ El.txt (Page.title page)]
									];
									El.div ~at:[At.class' "headerflexmeta"] [
										El.p [ El.txt (ptime_to_str (Page.date page))]
									]
								];
								El.unsafe_raw (Render.render_body page);
								navlinks;
							]
						]
					]
				]
			];
			footer;
		]
	]
	in

	El.html [header; body]


let render_taxonomy site taxonomy =
	let header = Render.render_head ~site () in
	let topbar = render_header (Taxonomy.uri taxonomy) (Taxonomy.title taxonomy) in
	let footer = render_footer () in

	let pagelist = List.map (fun sec ->
		El.li [
			El.a ~at:[At.href (Uri.to_string (Section.uri sec))] [
				El.txt (Section.title sec)
			];
			El.txt " - ";
			El.txt (Int.to_string (List.length (Section.pages sec)));
			El.txt " items";
		]
	) (Taxonomy.sections taxonomy)
	in

	let body = El.body [
		El.div ~at:[At.class' "allmostall"] [
			topbar;
			El.ul pagelist;
			footer;
		]
	] in

	El.html [header; body]

