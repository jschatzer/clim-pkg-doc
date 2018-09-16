;test.lisp

;(defpackage test (:use cl prove clim-pkg-doc))
(in-package test)

(plan 2)

; find a system with no doc <---
;(is (clim-pkg-doc::readme :onlis) "No System Info?")  ; macht jetzt error: Component "onlis" not found

;Raised an error Component "common-lisp" not found
;(is (clim-pkg-doc::readme :common-lisp) "No System Info?")

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


(plan 2)
;gehen nicht, warum? use package geht nur mit export symbols <---
;(is (car (last (spec-op))) 'UNWIND-PROTECT)
;(is (caadr (clim-constants)) :COLOR-NAMES)

(is (car (last (clim-pkg-doc::spec-op))) 'UNWIND-PROTECT)
(is (caadr (clim-pkg-doc::clim-constants)) :COLOR-NAMES)

(finalize)

