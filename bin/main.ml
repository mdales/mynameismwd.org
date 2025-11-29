open Webplats

let thumbnail_loader page thumbnail_size request =
  let pathp = Image.render_thumbnail_lwt page thumbnail_size in
  Lwt.bind pathp (fun path ->
    Router.static_loader "/" (Fpath.to_string path) request
  )

let snapshot_image_loader page image bounds request =
  let pathp = Image.render_image_lwt page image Fit bounds in
  Lwt.bind pathp (fun path ->
    Router.static_loader "/" (Fpath.to_string path) request
  )

let general_thumbnail_loader ~retina page =
  match Page.original_section_title page with
  | "photos" ->
      let i = Option.get (Page.titleimage page) in
      snapshot_image_loader page i.filename
        (if retina then (1280, 700) else (640, 350))
  | _ -> thumbnail_loader page (if retina then 600 else 300)

let section_render sec =
  match Section.title sec with
  | "posts" -> Posts.render_section
  | "photos" -> Photos.render_section
  | _ -> Snapshots.render_section

let taxonomy_section_renderer taxonomy _sec =
  match Taxonomy.title taxonomy with
  | "albums" -> Photos.render_section
  | _ -> Snapshots.render_section

let taxonomy_renderer taxonomy =
  match Taxonomy.title taxonomy with
  | "albums" -> Photos.render_taxonomy
  | _ -> Renderer.render_taxonomy

let page_renderer page =
  match Page.original_section_title page with
  | "photos" -> Photos.render_page
  | "sounds" | "snapshots" -> Snapshots.render_page
  | _ -> Renderer.render_page

let page_body_renderer page =
  match Page.original_section_title page with
  | "snapshots" -> Snapshots.render_body
  | "photos" -> Photos.render_body
  | _ -> Render.render_body

let () =
  let website_dir =
    match Array.to_list Sys.argv with
    | [ _; path ] -> Fpath.v path
    | _ -> failwith "Expected one arg, your website dir"
  in

  let site = Site.of_directory website_dir in

  let overrides =
    [
      Dream.get "/" (fun _ -> Index.render_index site |> Dream.html);
    ]
  in

  let routes = Router.of_site
    ~thumbnail_loader:general_thumbnail_loader
    ~image_loader:snapshot_image_loader
    ~taxonomy_renderer
    ~taxonomy_section_renderer
    ~section_renderer:section_render
    ~page_renderer
    ~page_body_renderer
    site
  in

  let routes = overrides @ routes in

  Dream.log "Adding %d routes"
    (List.length routes);
  Dream.run ~error_handler:(Dream.error_template (Renderer.render_error site))
  @@ Dream.logger
  @@ Dream.router routes
