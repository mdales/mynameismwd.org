open Htmlit
open Webplats

let months = [| "January" ; "Febuary" ; "March" ; "April" ; "May" ; "June" ; "July" ; "August" ; "September" ; "October" ; "November"; "December" |]


let ptime_to_str (t : Ptime.t) : string =
  let ((year, month, day), _) = Ptime.to_date_time t in
  Printf.sprintf "%d %s %d"day  months.(month - 1) year


let fit_dimensions max_width max_height width height =
  let fwidth = float_of_int width
  and fheight = float_of_int height in
  let wratio = (float_of_int max_width) /. fwidth
  and hratio = (float_of_int max_height) /. fheight in
  match (wratio >= 1.0) && (hratio >= 1.0) with
  | true -> (width, height)
  | false -> (
	let ratio = min wratio hratio in
	let newwidth = int_of_float (ratio *. fwidth)
	and newheight = int_of_float (ratio *. fheight) in
	(newwidth, newheight)
  )

let location_info page =
  let city = match (Page.get_key_as_string page "City") with
  | None -> ""
  | Some s -> Printf.sprintf "%s, " s
  in
  let country = match (Page.get_key_as_string page "Country") with Some s -> s | None -> "" in
  match country with
  | "" | "United States" | "United States of America" -> Printf.sprintf "%s%s" city (match (Page.get_key_as_string page "State") with Some x -> x | None -> "")
  | _ -> Printf.sprintf "%s%s" city country


let camera_info page =
  match (Page.get_key_as_string page "Make") with
  | None -> ""
  | Some make -> (
	let model = match (Page.get_key_as_string page "Model") with
	  | None -> ""
	  | Some "ILCE-7RM2" -> "A7RII"
	  | Some model -> model
	in
	let camera = Printf.sprintf "%s %s" make model in
	let lens = match (Page.get_key_as_string page "LensInfo") with
	  | None -> ""
	  | Some info -> (
		let lensmake = match (Page.get_key_as_string page "LensMake") with None -> "" | Some m -> m in
		Printf.sprintf " with a %s %s lens" lensmake info
	  )
	in
	Printf.sprintf "%s%s" camera lens
  )

let render_header title =
	El.div ~at:[At.class' "miniheader"] [
		El.div ~at:[At.class' "miniheadertitle"] [
			El.header ~at:[At.role "banner"] [
				El.a (* ~at:[At.ref "home"] *) [
					El.h1 [
						El.txt "my name is mwd: ";
						El.txt title;
					]
				]
			]
		];
		El.div ~at:[At.class' "miniheadernav"] [
			El.ul [
				El.li [
					El.a ~at:[At.href "/photos/"] [El.txt "Latest"]
				];
				El.li [
					El.a ~at:[At.href "/albums/"] [El.txt "Albums"]
				];
			]
		]
	]


let render_section site sec =
	let header = Render.render_head ~site ~sec () in
	let topbar = render_header (Section.title sec) in

	let pagelist = List.map (fun page ->
		let i = Option.get (Page.titleimage page) in
		let dims, card_adjust = match (i.dimensions) with
		| Some (width, height) -> (
			let width, height = fit_dimensions 640 350 width height in
			let adjusted_width = width + 40 in
			[
				At.width width;
				At.height height;
			], [At.style (Printf.sprintf "width: %dpx" adjusted_width)])
		| None -> [], []
		in
		let desc = match (i.description) with
		| Some txt -> [At.alt txt]
		| None -> []
		in
		let photo_date = match (Page.get_key_as_date page "taken") with
		| Some d -> d
		| None -> Page.date page
		in

		El.article [
			El.div ~at:[At.class' "galleryitem gallerylandscape"] [
				El.div ~at:[At.class' "galleryimage"] [
					El.a ~at:[At.href (Uri.to_string (Section.uri ~page sec))] [
						El.img ~at:([
							At.v "loading" "lazy";
							At.src (Uri.to_string (Section.uri ~page ~resource:{|thumbnail.jpg|} sec));
							At.v "srcset" ((Uri.to_string (Section.uri ~page ~resource:{|thumbnail@2x.jpg|} sec)) ^ " 2x, " ^ (Uri.to_string (Section.uri ~page ~resource:{|thumbnail.jpg|} sec)) ^ " 1x");
							At.title (Page.title page);
						] @ dims @ desc) ()
					]
				];
				El.div ~at:([At.class' "gallerycard gallerycard-landscape"] @ card_adjust) [
					El.a ~at:[At.href (Uri.to_string (Section.uri ~page sec)); At.class' "title"] [
						El.txt (Page.title page)
					];
					El.br ();
					El.br ();
					El.div ~at:[At.class' "gallerycardinner"] [
						El.div [
							El.txt (location_info page);
							El.br ();
							El.txt (ptime_to_str photo_date);
							El.br ();
						]
					]
				]
			]
		]
	) (Section.pages sec) in

	let body = El.body [
		El.div ~at:[At.class' "allmostall"] [
			topbar;
			El.div ~at:[At.id "container"] [
				El.div ~at:[At.class' "gallery"] pagelist
			]
		]
	] in

	El.html [header; body]


	let render_body page =
		let i = Option.get (Page.titleimage page) in
		let width, height = Option.get i.dimensions in
		let name, ext = Fpath.split_ext (Fpath.v i.filename) in
		let retina_filename = Printf.sprintf "scrn_%s@2x%s" (Fpath.to_string name) ext in
		let width, height = fit_dimensions 1008 800 width height in
		let layout = match width > height with true -> "landscape" | false -> "portrait" in
		let adjusted_width = width + 40 in
		let scrn_filename = Printf.sprintf "scrn_%s" i.filename in
		let desc = match i.description with
		| Some txt -> [At.alt txt]
		| None -> []
		in
		let card_adjust = match width > height with
		| true -> [At.style (Printf.sprintf "width: %dpx" adjusted_width)]
		| false -> []
		in

		El.div ~at:[At.class' "gallery singlegallery"] [
			El.div ~at:[At.class' ("galleryitem gallery" ^ layout)] [
				El.div ~at:[At.class' "galleryimage"] [
					El.img ~at:([
						At.v "loading" "lazy";
						At.src scrn_filename;
						At.v "srcset" (retina_filename ^ " 2x, " ^ scrn_filename ^ " 1x");
						At.title (Page.title page);
						At.width width;
						At.height height;
					] @ desc) ()
				];
				El.div ~at:([At.class' ("gallerycard gallerycard-" ^ layout)] @ card_adjust) [
					El.div ~at:[At.class' "gallerycardinner"] [
						El.div ~at:[At.class' "gallerycardcontent"] [
							El.unsafe_raw (Render.render_body page)
						]
					]
				]
			]
		]


	let navigation_links sec previous_page next_page =
		let previous_page =
			match previous_page with
			| Some page ->
					[
						El.a
							~at:[ At.href (Uri.to_string (Section.uri ~page sec)) ]
							[ El.unsafe_raw "&#10094; "; El.txt (Page.title page) ];
					]
			| None -> []
		and next_page =
			match next_page with
			| Some page ->
					[
						El.a
							~at:[ At.href (Uri.to_string (Section.uri ~page sec)) ]
							[ El.txt (Page.title page); El.unsafe_raw " &#10095;" ];
					]
			| None -> []
		in
		El.div
			~at:[ At.class' "headerflex" ]
			(List.concat_map Fun.id [ previous_page; next_page ])


	let render_page site sec previous_page page next_page =
		let header = Render.render_head ~site ~sec () in
		let topbar = render_header (Section.title sec) in

		let i = Option.get (Page.titleimage page) in
		let width, height = Option.get i.dimensions in
		let name, ext = Fpath.split_ext (Fpath.v i.filename) in
		let retina_filename = Printf.sprintf "scrn_%s@2x%s" (Fpath.to_string name) ext in
		let width, height = fit_dimensions 1008 800 width height in
		let layout = match width > height with true -> "landscape" | false -> "portrait" in
		let adjusted_width = width + 40 in
		let scrn_filename = Printf.sprintf "scrn_%s" i.filename in
		let desc = match i.description with
		| Some txt -> [At.alt txt]
		| None -> []
		in
		let card_adjust = match width > height with
		| true -> [At.style (Printf.sprintf "width: %dpx" adjusted_width)]
		| false -> []
		in
		let photo_date = match (Page.get_key_as_date page "taken") with
		| Some d -> d
		| None -> Page.date page
		in
		let caption = match (Page.get_key_as_string page "Caption") with
		| Some caption -> [El.txt (caption ^ " film"); El.br()]
		| None -> []
		in
		let albums = match (Page.get_key_as_string_list page "albums") with
		| [] -> []
		| alist -> (
			[
				El.br ();
				El.txt "Appears in:";
				El.br ();
			] @ (
				List.concat_map (fun album ->
					[
						El.a ~at:[At.href ("/albums/" ^ (String.lowercase_ascii album |> String.map (fun c -> match c with ' ' -> '-' | x -> x)))] [
							El.txt album
						];
						El.br ();
					]
				) alist
			)
		)
		in
		let navitems = navigation_links sec previous_page next_page in

		let body = El.body [
			El.div ~at:[At.class' "almostall"] [
				topbar;
				El.div ~at:[At.id "container"] [
					El.div ~at:[At.class' "article galoveride"] [
						El.article [
							El.div ~at:[At.class' "gallery singlegallery"] [
								El.div ~at:[At.class' ("galleryitem gallery" ^ layout)] [
									El.div ~at:[At.class' "galleryimage"] [
										El.img ~at:([
											At.v "loading" "lazy";
											At.src (Uri.to_string (Section.uri ~page ~resource:scrn_filename sec));
											At.v "srcset" (
												(Uri.to_string (Section.uri sec ~resource:retina_filename ~page)) ^ " 2x, " ^
												(Uri.to_string (Section.uri ~page ~resource:scrn_filename sec)) ^ " 1x"
											);
											At.title (Page.title page);
											At.width width;
											At.height height;
										] @ desc) ()
									];
									El.div ~at:([At.class' ("gallerycard gallerycard-" ^ layout)] @ card_adjust) [
										El.a ~at:[At.href (Uri.to_string (Section.uri ~page sec))] [
											El.txt (Page.title page)
										];
										El.br ();
										El.br ();
										El.div ~at:[At.class' "gallerycardinner"] [
											El.div ~at:[At.class' "gallerycardcontent"] [
												El.unsafe_raw (Render.render_body page)
											]
										];
										El.div ([
											El.txt (location_info page);
											El.br ();
											El.txt (ptime_to_str photo_date);
											El.br();
											El.txt (camera_info page);
											El.br();
										]
										@ caption @ [
											El.a ~at:[At.href "https://creativecommons.org/licenses/by-nc/4.0/"] [ El.txt "License CC BY-NC"];
											El.txt " - ";
											El.a ~at:[At.href i.filename; At.download ""] [
												El.txt "download"
											];
											El.br();
										] @ albums);
										El.div ~at:[At.class' "photo"] [navitems];
									]
								]
							]
						]
					]
				]
			]
		] in

		El.html [header;body]



	let rec take n l =
		match n with
		| 0 -> []
		| _ -> (
			match l with
			| [] -> []
			| hd :: tl -> hd :: (take (n - 1) tl)
		)

	let render_taxonomy site taxonomy =
		let header = Render.render_head ~site () in
		let topbar = render_header (Taxonomy.title taxonomy) in

		let pagelist = List.map (fun sec ->
			let imagelist = (Section.pages sec) |> take 3 |> List.map (fun page ->
				let i = Option.get (Page.titleimage page) in
				let width, height = Option.get i.dimensions in
				let layout = match width > height with true -> "Y" | false -> "X" in
				let rot = Random.int_in_range ~min:(-5) ~max:5 in
				let shift = Random.int_in_range ~min:(-7) ~max:7 in
				let name, ext = Fpath.split_ext (Fpath.v i.filename) in
				let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
				let desc = match i.description with
				| Some txt -> [At.alt txt]
				| None -> []
				in
				El.div ~at:[
					At.class' "galleryimage";
					At.style (Printf.sprintf "transform: rotate(%ddeg) translate%s(%dpx);" rot layout shift);
				] [
					El.img ~at:([
						At.v "loading" "lazy";
						At.src ((Page.original_section_url page) ^ (Page.url_name page) ^ "/album_" ^ i.filename);
						At.v "srcset" (((Page.original_section_url page) ^ (Page.url_name page) ^ "/album_" ^ retina_filename) ^ " 2x, " ^ ((Page.original_section_url page) ^ (Page.url_name page) ^ "/album_" ^ i.filename) ^ " 1x");
					] @ desc) ()
				]
			) in

			El.div ~at:[At.class' "galleryitem gallerylandscape album"] [
				El.a ~at:[At.href (Uri.to_string (Section.uri sec))] [
					El.div ~at:[At.class' "albumstack"] imagelist;
				];
				El.div ~at:[At.class' "gallerycard albumcard"] [
					El.p [
						El.a ~at:[At.href (Uri.to_string (Section.uri sec))] [
							El.txt (Section.title sec)
						];
						El.br ();
						El.txt ((Int.to_string (List.length (Section.pages sec))) ^ " photos")
					]
				]
			]
		) (Taxonomy.sections taxonomy) in

		let body = El.body [
			El.div ~at:[At.class' "almostall"] [
				topbar;
				El.div ~at:[At.id "container"] [
					El.div ~at:[At.class' "gallery"] pagelist;
				]
			]
		] in

		El.html [header; body]
