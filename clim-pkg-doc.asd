;;;; clim-pkg-doc.asd

(asdf:defsystem #:clim-pkg-doc
  :description "clim-package-documentation"
  :author "<schatzer.johann@gmail>"
  :license "BSD Simplified"
  :depends-on (clim-widgets nsort perlre
               named-readtables manifest alexandria repl-utilities)
  :serial t
  :components ((:file "package")
               (:file "clim-pkg-doc")))

