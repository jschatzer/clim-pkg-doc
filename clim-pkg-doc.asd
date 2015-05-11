;;;; clim-pkg-doc.asd

(asdf:defsystem #:clim-pkg-doc
  :description "Describe clim-pkg-doc here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (clim-widgets nsort perlre
               named-readtables manifest alexandria)
  :serial t
  :components ((:file "package")
               (:file "clim-pkg-doc")))

