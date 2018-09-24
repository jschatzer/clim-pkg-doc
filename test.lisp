;test.lisp

;(defpackage test (:use cl prove clim-pkg-doc))
(in-package test)

#|
ev ql:quickload
(asdf:find-system :cl-fad) ; #<ASDF/SYSTEM:SYSTEM "cl-fad">
(asdf:find-system 'cl-fad)  ; #<ASDF/SYSTEM:SYSTEM "cl-fad">
(asdf:find-system "cl-fad") ; #<ASDF/SYSTEM:SYSTEM "cl-fad">
(asdf:find-system "CL-FAD")  ; error

(package-name 'cl-fad) ; "CL-FAD"
(package-name :cl-fad) ; "CL-FAD"
(package-name "CL-FAD") ; "CL-FAD"
(package-name "cl-fad") ; error
|#






(plan 1)

; find a system with no doc <---
(is (clim-pkg-doc::readme-text :s-xml) "No System Info?") ; 18.9.18

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

(plan 17)
;ALEXANDRIA problem
(is (package-name :alexandria) "ALEXANDRIA.0.DEV")
(is (clim-pkg-doc::readme-file :alexandria) #P"/home/hans/.quicklisp/dists/quicklisp/software/alexandria-20170830-git/README" :test 'equal)
(is (clim-pkg-doc::readme-file :alexandria) #P"/home/hans/.quicklisp/dists/quicklisp/software/alexandria-20170830-git/README")


;(is (#~m'out of the door.\n$' (clim-pkg-doc::readme-text :alexandria)))  ;; error

;; regex match, geht
(ok (#~m'out of the door.\n$' (clim-pkg-doc::readme-text :alexandria)))    ; geht <-- cool
(like (clim-pkg-doc::readme-text :alexandria) "out of the door.\\n$")
(like "Hatsune 39" "\\d")

;; standard output test, geht
(is-print (princ "Hi, there") "Hi, there")       

(ok (asdf:load-system :alexandria))  ;t
(ok (asdf:load-system (clim-pkg-doc::pkg2sys :alexandria.0.dev))) ;t

;error, geht
;(is-error (error (asdf:load-system :alexandria.0.dev)) 'simple-error)   ; Component :ALEXANDRIA.0.DEV not found     error
(is-error (error "Something wrong") 'simple-error)
;;geht, vergleiche repl output
(is-error (asdf:load-system :alexandria.0.dev) 'ASDF/FIND-SYSTEM:MISSING-COMPONENT) 

(ok (find-package :alexandria)) ; ;#<PACKAGE "ALEXANDRIA.0.DEV">
(ok (find-package :alexandria.0.dev)) ; ;#<PACKAGE "ALEXANDRIA.0.DEV"> 

(ok (clim-pkg-doc::pkg-tree :alexandria))
(ok (clim-pkg-doc::pkg-tree :alexandria.0.dev))

(ok (asdf:find-system :alexandria)) ; #<ASDF/SYSTEM:SYSTEM "alexandria">
(is-error (asdf:find-system :alexandria.0.dev) 'ASDF/FIND-SYSTEM:MISSING-COMPONENT)  ; Component "alexandria.0.dev" not found
(ok (asdf:find-system (clim-pkg-doc::pkg2sys :alexandria.0.dev)))

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


(plan 3)
;onlisp README
(is (clim-pkg-doc::readme-file :onlisp) #P"/home/hans/src/lisp/onlisp/README")
(is (clim-pkg-doc::readme-file :onlisp) #P"/home/hans/src/lisp/onlisp/README" :test 'equal)
(ok (equal (clim-pkg-doc::readme-file :onlisp) #P"/home/hans/src/lisp/onlisp/README"))
(like (clim-pkg-doc::readme-text :onlisp) "^A set of utilities")

;(ok (not (eql #p"a" #p"a"))) ; (eql #p"a" #p"a") ->nil  
(ok (eql #p"a" #p"a"))   ; (eql #p"a" #p"a") ->nil    ;prove bug ??
(ok (equal #p"a" #p"a")) ;t
(finalize)



