;;;; clim-pkg-doc.asd

(asdf:defsystem #:clim-pkg-doc
  :version "0.4"
  :description "clim-package-documentation"
  :author "<schatzer.johann@gmail>"
  :license "BSD Simplified"
  :depends-on (clim-widgets
                alexandria
                asdf
                onlisp  ;for testing  <-- remove
                anaphora
                stdutils
                manifest repl-utilities)
  :serial t
  :components ((:file "package")
;(:file "utils") ;remove
;;;(:file "new.13.4.17")
;;;(:file "new-pd")
;;;;;;;
;(:file "pkg-tree")
;(:file "n-clim-pkg-doc")
               (:file "clim-pkg-doc")))

