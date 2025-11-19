open Webplats

let render_section site sec =
  <html>
  <%s! (Render.render_head ~site ~sec ()) %>
  <body>
    <div class="almostall">
      <%s! Renderer.render_header (Section.uri sec) (Section.title sec) %>

      <div id="container">
        <div class="content">
          <section role="main">
            <div class="tagcontents">
              <h2>Snapshots</h2>
              <div class="tagcellholder">

% (Section.pages sec) |> List.iter begin fun (page) ->
                <div class="tagcell colour-<%s Page.original_section_title page %>">
                  <div class="tagcelllabel">
                    <span><%s Page.title page %></span>
                  </div>
                  <div class="tagcellinner">
                    <a href="<%s Uri.to_string (Section.uri ~page sec) %>">
                      <div class="tagcellimg">
                        <figure>
                          <img
                            loading="lazy"
% (match (Page.titleimage page) with Some i ->
                            src="<%s Uri.to_string (Section.uri ~page ~resource:{|thumbnail.jpg|} sec) %>"
                            srcset="<%s Uri.to_string (Section.uri ~page ~resource:{|thumbnail@2x.jpg|} sec) %> 2x,
                            <%s Uri.to_string (Section.uri ~page ~resource:{|thumbnail.jpg|} sec) %> 1x"
% (match (i.description) with Some desc ->
                            alt="<%s desc %>"
% | None -> ());

% | None -> ());
                          />
                        </figure>
                      </div>
                    </a>
                  </div>
                </div>
% end;

              </div>
            </div>
          </section>
        </div>
      </div>
      <%s! Renderer.render_footer () %>
    </div>
  </body>
  </html>

let is_image_retina dims =
  match dims with
  | None -> true
  | Some (width, height) -> (
    (width > (720 * 2)) && (height > (1200 * 2))
  )

let render_body page =
  <%s! Render.render_body page %>
  <div class="snapshotlist">
% List.iter (fun (i : Frontmatter.image) ->
% let name, ext = Fpath.split_ext (Fpath.v i.filename) in
% let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
     <div class="snapshotitem single">
       <figure class="single">
         <img
            src="<%s i.filename %>"
% (match (is_image_retina i.dimensions) with true ->
            srcset="<%s retina_filename %> 2x,
              <%s i.filename %> 1x"
% | false -> ());
% (match (i.description) with Some desc ->
            alt="<%s desc %>"
% | None -> ());
         />
       </figure>
       <div class="holder holder-top-left"></div>
       <div class="holder holder-top-right"></div>
       <div class="holder holder-bottom-left"></div>
       <div class="holder holder-bottom-right"></div>
     </div>
% ) (Page.images page);
% List.iter (fun (filename :string) ->
      <div class="video">
        <video controls>
          <source src="<%s filename %>" type="video/mp4"/>
          Your browser does not support the video element.
        </video>
      </div>
% ) (Page.videos page);
  </div>

let render_page site sec previous_page page next_page =
  <html>
  <%s! (Render.render_head ~site ~sec ~page ()) %>
  <body>
    <div class="almostall">
      <%s! Renderer.render_header (Section.uri sec) (Section.title sec) %>
      <div id="container">
        <div class="content">
          <section role="main">
            <div class="article">
              <article>
                <div class="headerflex">
                  <div class="headerflextitle">
                    <h3><%s Page.title page %></h3>
                  </div>
                  <div class="headerflexmeta">
                    <p><%s Renderer.ptime_to_str (Page.date page) %></p>
                  </div>
                </div>
                <%s! render_body page %>

                <div class="postscript">
                  <ul>
% (match previous_page with Some page ->
                    <li><strong>Next</strong>: <a href="<%s Uri.to_string (Section.uri sec ~page) %>/"><%s Page.title page %></a></li>
% | None -> ());
% (match next_page with Some page ->
                    <li><strong>Previous</strong>: <a href="<%s Uri.to_string (Section.uri ~page sec) %>/"><%s Page.title page %></a></li>
% | None -> ());
% (match (Page.tags page) with [] -> () | tags ->
% let count = (List.length tags) - 1 in
                    <li><strong>Tags</strong>:
% (List.iteri (fun i tag ->
% let term_for_url = String.map (fun c -> match c with ' ' -> '-' | x -> x) tag in
% let seperator = if (i < count) then "," else "" in
                    <a href="/tags/<%s term_for_url %>/"><%s tag %></a><%s seperator %>
% ) tags));
                  </ul>
                </div>

              </article>
            </div>
          </section>
        </div>
      </div>
      <%s! Renderer.render_footer () %>
    </div>
  </body>
  </html>
