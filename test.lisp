;test.lisp
(defpackage test (:use cl prove clim-pkg-doc))
(in-package test)

(plan 1)

(is (clim-pkg-doc::readme :onlis) "No System Info?")

(finalize)



(plan 1)
; test unprintable objects - how to ??

(is (find-package :mcclim) NIL)

;#<PACKAGE "CLIM">
;(is-print (find-package :clim) #"#<PACKAGE "CLIM">"#)


;#<ASDF/SYSTEM:SYSTEM "mcclim">
;(ignore-errors (asdf:find-system :mcclim))

;#<ASDF/SYSTEM:SYSTEM "clim">
;(ignore-errors (asdf:find-system :clim))

(finalize)



