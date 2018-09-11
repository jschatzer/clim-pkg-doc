;test.lisp
(defpackage test (:use cl prove clim-pkg-doc))
(in-package test)

(plan 1)

(is (clim-pkg-doc::readme :onlis) "No System Info?")

(finalize)

