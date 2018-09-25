;test.lisp

(defpackage test 
  (:use cl prove clim-pkg-doc))
(in-package test)

(plan 7)
; test unprintable objects - how to ??
(ok (asdf:find-system :cl-fad)) ; #<ASDF/SYSTEM:SYSTEM "cl-fad">
(ok (asdf:find-system 'cl-fad))  ; #<ASDF/SYSTEM:SYSTEM "cl-fad">
(ok (asdf:find-system "cl-fad")) ; #<ASDF/SYSTEM:SYSTEM "cl-fad">
;(is-error (asdf:find-system "CL-FAD") 'ASDF/FIND-SYSTEM:MISSING-COMPONENT)  ; error
;(is-error (asdf:find-system "CL-FAD") "Component \"CL-FAD\" not found")  ; error
(is (package-name 'cl-fad) "CL-FAD")
(is (package-name :cl-fad) "CL-FAD")
(is (package-name "CL-FAD") "CL-FAD")
;(is-error (package-name "cl-fad") 'SB-KERNEL:SIMPLE-PACKAGE-ERROR) ; error
(is (clim-pkg-doc::readme-text "COMMON-LISP") "No System Info?")
(finalize)


(plan 1)
(is (find-package "MCCLIM") NIL)
;#<PACKAGE "CLIM">
;(is-print (find-package :clim) #"#<PACKAGE "CLIM">"#)

;#<ASDF/SYSTEM:SYSTEM "mcclim">
;(ignore-errors (asdf:find-system :mcclim))

;#<ASDF/SYSTEM:SYSTEM "clim">
;(ignore-errors (asdf:find-system :clim))

; undef functions?
;(is (car (last (clim-pkg-doc::spec-op))) 'UNWIND-PROTECT)
;(is (caadr (clim-pkg-doc::clim-constants)) :COLOR-NAMES)
(finalize)


(plan 9)
(is (package-name :alexandria) "ALEXANDRIA.0.DEV")
(ok (clim-pkg-doc::readme-file "ALEXANDRIA"))
;; regex match
(ok (#~m'out of the door.\n$' (clim-pkg-doc::readme-text "ALEXANDRIA"))) ; geht <-- cool
(like (clim-pkg-doc::readme-text "ALEXANDRIA") "out of the door.\\n$")
;; standard output test
(is-print (princ "Hi, there") "Hi, there")       
;(ok (asdf:load-system "ALEXANDRIA"))  ;t
;(ok (asdf:load-system (clim-pkg-doc::pkg2sys "ALEXANDRIA.0.DEV"))) ;t
(is-error (error "Something wrong") 'simple-error)
; vergleiche repl output
(is-error (asdf:load-system :alexandria.0.dev) 'ASDF/FIND-SYSTEM:MISSING-COMPONENT) 
(ok (find-package "ALEXANDRIA")) ; ;#<PACKAGE "ALEXANDRIA.0.DEV">
(ok (find-package "ALEXANDRIA.0.DEV")) ; ;#<PACKAGE "ALEXANDRIA.0.DEV"> 
; (ok (clim-pkg-doc::pkg-tree :alexandria))
; (ok (clim-pkg-doc::pkg-tree :alexandria.0.dev))
; (ok (asdf:find-system :alexandria)) ; #<ASDF/SYSTEM:SYSTEM "alexandria">
; (is-error (asdf:find-system :alexandria.0.dev) 'ASDF/FIND-SYSTEM:MISSING-COMPONENT)  ; Component "alexandria.0.dev" not found
; (ok (asdf:find-system (clim-pkg-doc::pkg2sys :alexandria.0.dev)))
(finalize)


(plan 2)
;CLIM COLOR
(is (manifest::clim-color-p 'clim:+cyan+) "+CYAN+")
(is (car (member (#~s'+''g (symbol-name 'cyan)) (mapcar #'fourth clim-internals::*xpm-x11-colors*) :test 'equalp)) "cyan")
(finalize)


(plan 4)
; cl/clim CONSTANTS ;There is no class named COMMON-LISP:NIL
(is (length (remove-if-not 'constantp (clim-pkg-doc::pkg-symbols :cl))) 62)
(ok (constantp NIL))
(ok (constantp 'NIL))
(is (remove-if-not 'constantp (clim-pkg-doc::pkg-symbols :clim)) '(NIL T) :test 'equalp)
(finalize)


(plan 5)
;onlisp README
(ok (clim-pkg-doc::readme-file "ONLISP"))
(ok (clim-pkg-doc::readme-file "ONLISP"))
(like (clim-pkg-doc::readme-text "ONLISP") "^A set of utilities")
;(ok (not (eql #p"a" #p"a"))) ; (eql #p"a" #p"a") ->nil  
(ok (eql #p"a" #p"a"))   ; (eql #p"a" #p"a") ->nil    ;prove bug ??
(ok (equal #p"a" #p"a")) ;t
(finalize)
