;;;; clim-pkg-doc.asd

(asdf:defsystem #:clim-pkg-doc
  :version "0.2"
  :description "clim-package-documentation"
  :author "<schatzer.johann@gmail>"
  :license "BSD Simplified"
  :depends-on (clim-widgets nsort perlre
               ;named-readtables 
               manifest repl-utilities alexandria)
  :serial t
  :components ((:file "package")
               (:file "clim-pkg-doc")))

