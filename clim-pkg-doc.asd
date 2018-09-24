;;;; clim-pkg-doc.asd

(asdf:defsystem #:clim-pkg-doc
  :version "0.4"
  :description "clim-package-documentation"
  :author "<schatzer.johann@gmail>"
  :license "BSD Simplified"
  :depends-on (clim-widgets  ; laden perlre
                alexandria
                asdf
;                onlisp  ;for testing  <-- remove
;hans-helper ;for testing  <-- remove
                prove
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


               (:file "clim-pkg-doc")
               ;--------------------
               ;; scroll problem, alexandrio
               ;(:file "clim-pkg-doc.10.9.2018")    ;; alexandria geht, scroll auch nicht ???   <------
               ;-------------------

;                 "clim-pkg-doc.9.10.16_2_geht"   ;geht nicht mehr
;                "clim-pkg-doc12.5.17"  ;geht nicht mehr

;               (:file "test")
               ))

