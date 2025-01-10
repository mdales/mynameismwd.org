(lang dune 3.16)

(name mynameismwd)

(generate_opam_files true)

(source
 (github mdales/mynameismwd))

(authors "Michael Dales <michael@digitalflapjack.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>")

(license EUPL)

(documentation https://url/to/documentation)

(package
 (name mynameismwd)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune webplats)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
