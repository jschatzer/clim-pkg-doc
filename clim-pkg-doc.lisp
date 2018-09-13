;;;; clim-pkg-doc.lisp

#|--------------------------------------------------------------------
some ideas, concepts and code from Peter Seibel's manifest package
;;;-------------------------------------------------------------------
; this is form the manifest LICENSE file:

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------|# 

(in-package #:clim-pkg-doc)

;todo: 
;1) index.html with html2text, for now only strip-html

#|
;einteilung, order
System description
GUI
GUI commands
Symboltree  pkg system
...
|#

;--------------------------------------------------------
; 1) helper
;--------------------------------------------------------
(defparameter *help* "Help:
Click the root node to see a package's description or readme-file
-----------------------------------------------------------------
APROPOS: 
1) on the first prompt type a string
2) on the second promt press enter or type the name of a package
-----------------------------------------------------------------
CONFIGURE-POSSIBILITIES:
1) adapt local-libs
2) (setf clim-pkg-doc::*st* :a)  ;to change the symbol-type  :e external(default) :p resent :a available
-----------------------------------------------------------------
")

;1) cl+ssl.asd hat 2x defpackage vor defsystem -- loop
;2) alexandria.0.dev    pathname systemfile

;mit match (a b ...)
(defun asdf-description (sys)
  (let ((x (asdf/system:find-system sys)))
    (list (asdf/system:system-description x) (asdf/system:system-long-description x))))

(defun strip-html (s) (#~s'<.*?>''gs s))

#|
;;;;;;;;;;;;;;;;;;
;README describes the system, not the package. 
; mcclim ok, clim not ok
;(clim-pkg-doc::readme-file :mcclim)

;system/package with different names:
;mcclicm clim     -- (h:ql :mcclim)
;-----------------------------------

"README           ; cl-fad
"README.txt"      ; hans-helper
"README.md"       ; mcclim
"README.markdown"
"README.org"
"doc/README"      ; submarine hat index.html und doc/README txt <--
"doc/index.html"  ; submarine 
"docs/index.html" ; chunga
|#

(defmacro readme-file (pkg)
  "Find doc-file for SYSTEM"
  `(or 
     ,@(loop for x in '("README" "README.txt" "README.md" "README.markdown" "README.org" "doc/README" "doc/index.html" "docs/index.html")
             collect `(probe-file (asdf:system-relative-pathname ,pkg ,x)))))

; so geht clim/mcclim 
; no readme errors, scheint zu gehen
(defun readme (syst)
  "Get text from the SYSTEM's docfile,
  if doc is html strip the tags"
  (let ((pkg (case syst
               (:clim :mcclim) ;belassen
               (t syst))))
    (or 
      (ignore-errors
        (pre:match (file-namestring (readme-file pkg))
                   (#~m'html' (strip-html (alexandria:read-file-into-string (readme-file pkg))))
                   (#~m'.*' (alexandria:read-file-into-string (readme-file pkg)))))    ; match  t / otherwise ?? <---
      "No System Info?")))
      

(defun pkg-description (s pkg)
  (let 
    ((nr (length (pkg-symbols pkg)))
     (a1 (car (asdf-description pkg)))
     (a2 (cadr (asdf-description pkg)))
;     (a3 (manifest::readme-text pkg)))
     (a3 (readme pkg)))

    ;;; test
    ;(format s "~a~2%" (readme-file pkg))
    ;(format s "~a~2%" (describe (find-package pkg) s))

    (format s "Nickname: ~{~a~}~%" (package-nicknames pkg))

  (with-drawing-options (s :ink +red+) (format s "~a " nr)) (format s "external-symbols")
  (with-drawing-options (s :ink +red+ :text-face :bold) (format s 
"
-------------------------
 ASDF Description
-------------------------"))
  (with-drawing-options (s :text-face :bold) (format s "~&SHORT: ")) (format s "~a" a1)
  (with-drawing-options (s :text-face :bold) (format s "~2&LONG: ")) (format s "~a" a2)
  (with-drawing-options (s :text-face :bold :ink +red+) (format s 
"~&
-------------------------
 README
-------------------------"))
  (format s "~&~a" a3)))

;--------------------------------
;--------------------------------------------------------
;; CREATE A PACKAGE- OR SYSTEM-TREE    --- SYMBOL-TREE
;-------------------------------------------------


;----------------------------------
; HIERARCHY BY SYMBOLNAME
;----------------------------------
; (parts 'a-b-c) -> ("a-" "b-" "c")
(defun parts (x) (#~d'(?<=-)' x))

;---------------------------
; (key 'a-b-c)  ; "A-"
; (key 'a-b-c 1) ;"A-B-"
(defun key (s &optional (i 0))
 (#~s' ''g (stdutils:list-to-delimited-string (reverse (key% s i))))) 

;(key% 'a-b-c 1) ; (B- A-)
(defun key% (s &optional (i 0))
  "key ~ header"
  (cond ((zerop i) (list (nth i (parts s))))
        (t (cons 
             (nth i (parts s))
             (key% s (1- i))))))

;  ;geht nicht ??, warum nicht?
;  #;(defun key (s &optional (i 0))
;    ;(h:lststg (subseq (parts s) 0 (1+ i))))
;    (stdutils:list-to-delimited-string (subseq (parts s) 0 (1+ i))))



(defun r-add-header (l ind) ; recursive-add-header list index
  (cons (key (car l) ind) (pack (reverse l) (1+ ind))))

;e.g. clim macro with- geht richtig
(defun pack (l &optional (i 0) v)
  (cond ((null l) (if (= 1 (length v)) v (list (r-add-header v i))))
        ((null v) (pack (cdr l) i (list (car l))))
        ((equal (key (car v) i) (key (car l) i)) (pack (cdr l) i (push (car l) v)))
        (t (cons (if (= 1 (length v))
                   (car v)
                   (r-add-header v i))
                 (pack (cdr l) i (list (car l)))))))

#|
;------
;geht richtig!!!, 30.4.17
(defun pack (l &optional (i 0) v)
  (cond ((null l) (if (= 1 (length v)) v (list (cons (key (car v) i) (pack (reverse v) (1+ i))))))
        ((null v) (pack (cdr l) i (list (car l))))
        ((equal (key (car v) i) (key (car l) i)) (pack (cdr l) i (push (car l) v)))
        (t (cons (if (= 1 (length v))
                   (car v)
                   (cons (key (car v) i) (pack (reverse v) (1+ i))))
                 (pack (cdr l) i (list (car l)))))))
;------
|#



;stört clim macro with-     <-----!! 
(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    ((and (consp (car l)) (notany #'consp (car l))) (cons (car l) (remove-empty-bags (cdr l))))
    ((and (= 2 (length l)) (atom (car l)) (consp (cadr l))) (remove-empty-bags (cadr l)))
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

;so geht clim macro with-  nicht richtig
(defun hierarchy-by-symbolname (l)
  (remove-empty-bags (pack l)))

#|
;damit geht clim macro with-  richtig
(defun hierarchy-by-symbolname (l)
  (pack l))
|#


;-------------------------------
;das brauchts damit CL ladet
;--------------------------------------------------------
; -2) edit some package, for now common-lisp, clim
;--------------------------------------------------------
;;; if package is CLIM: 1) remove all constants, 2) then add them again as a finished, i.e. sorted, group with a sorted color-subgroup  
(defun clim-constant-p (s) (if (or (constantp s) (#~m'^\+.+\+$' (symbol-name s))) s))
(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (cw:key (car x)) (mapcar #'list x)))) (cw:pack l)))

(defun clim-constants ()
  "divide clim-constants into colors and other constants" ; color-names are mixed case strings, without + or -, e.g. "antique white"
  (let* ((clim-color-constants (mapcar (lambda (x) (find-symbol (string-upcase (#~s'(.*)'+\1+' (#~s' '-'g x))) :clim)) (mapcar #'fourth clim-internals::*xpm-x11-colors*))) ; see also cl-colors pkg
         (clim-non-color-constants (remove-if-not #'clim-constant-p (set-difference (pkg-symbols :clim) clim-color-constants)))
         (o (sort clim-non-color-constants #'string<))     ;other constants
         (c (sort clim-color-constants #'nsort:nstring<))) ;color-name constants
    (cons "constant:" (cons (cons :color-names (mktree c)) (mktree o)))))  ; 14.12.16 ist einheitlicher <----
;    (cons "constant:" (cons (pack (list (cons :color-names c))) (pack o)))))  ; 14.12.16 ist einheitlicher <----

;;; if package is COMMON LISP remove special operatores and add them as a separate group.
(defun spec-op () (cons "special-operator:" (sort (remove-if-not #'special-operator-p (pkg-symbols :common-lisp)) #'string<)))

;--------------------------------
;|#

(defun pkg-symbols (pkg) (loop for s being the external-symbols of pkg collect s))

#|
(defun sorted-symbols-in-a-category (pkg what)
  "return a sorted list of all symbols in a category"
  (sort (loop for sym in 
              (case pkg
                (:clim (remove-if #'clim-constant-p (pkg-symbols :clim)))
                (:common-lisp (remove-if #'special-operator-p (pkg-symbols :cl)))
                (t (pkg-symbols pkg))) 
              when (manifest::is sym what) collect sym) #'nsort:nstring<))
|#

(defun sorted-symbols-in-a-category (pkg what)
  "return a sorted list of all symbols in a category"
  (sort (loop for sym in 
              (case pkg
                ;(:clim (remove-if #'clim-constant-p (pkg-symbols :clim)))
                ;(:common-lisp (remove-if #'special-operator-p (pkg-symbols :cl)))
                (t (pkg-symbols pkg))) 
              when (manifest::is sym what) collect sym) #'nsort:nstring<))

(defun symbol-groups (pkg)
  "group symbols into manifest::*categories*"
  (loop for what in manifest::*categories*
        for category = (sorted-symbols-in-a-category pkg what)
        when category collect (cons (#~s'$':' (princ-to-string what)) category)))

(defun hierarchical-category (l) ;package category
  (hierarchy-by-symbolname
    (cw:sym2stg l)))

(defun symbol-groups (pkg)
  "group symbols into manifest::*categories*"
  (loop for what in manifest::*categories*
        for category = (sorted-symbols-in-a-category pkg what)
        when category collect (cons (#~s'$':' (string-downcase (princ-to-string what))) (hierarchical-category category))))

#|
;orig
(defun sym-gr (p)
  (case p 
    (:clim (substitute (mapcar 'cw::sym2stg (clim-constants)) '("constant:" NIL) (mapcar 'pack (mapcar 'cw::sym2stg (symbol-groups p))) :test 'equal))
    (:common-lisp (cons (pack (mapcar 'cw::sym2stg (spec-op))) (mapcar 'pack (mapcar 'cw::sym2stg (symbol-groups p)))))
    (t (mapcar 'pack (mapcar 'cw::sym2stg (symbol-groups p))))))


(defun sym-gr (p)
  (case p 
    (:clim (substitute (mapcar 'cw::sym2stg (clim-constants)) '("constant:" NIL) (mapcar (alexandria:compose 'remove-empty-bags 'pack) (mapcar 'cw::sym2stg (symbol-groups p))) :test 'equal))
    (:common-lisp (cons (remove-empty-bags (pack (mapcar 'cw::sym2stg (spec-op)))) (mapcar (alexandria:compose 'remove-empty-bags 'pack) (mapcar 'cw::sym2stg (symbol-groups p)))))
    (t (mapcar (alexandria:compose 'remove-empty-bags 'pack) (mapcar 'cw::sym2stg (symbol-groups p))))))
|#

;---------------------------
(defun insert-what (l)
   (mapcar 'insert-what%% l))

;(insert-what% 'macro- 'abc-def-g)
(defun insert-what% (w s) ;what symbol 
  (stdutils:list-to-delimited-string 
    (cons w (parts s)) ""))

(defun insert-what%% (l)
  (let ((w (#~s'$'-' (car l))))
    (cons w
      (labels ((rec (y)
                 (cond ((null y) nil)
                       ((atom y) (insert-what% w y))
                       (t (cons (rec (car y)) (rec (cdr y)))))))
        (rec (cdr l))))))

;scheint gut zu gehen außer cl und clim
(defun pkg-tree (p) (cons p (insert-what (symbol-groups p))))

;--------------------------------------------------------
; 4) GUI helper
;--------------------------------------------------------
; -1) color lambda list
(defun color-lambda (s l)
  (mapc (lambda (x)
          (if (#~m'^&' x)
            (with-drawing-options (s :ink +red+ :text-face :bold) (format s "~(~a~)" x))
            (format s "~(~a~)" x)))
        (ppcre:split "(&[^ )]+)" (princ-to-string l) :with-registers-p t)))

;--------------------------------------------------------
; 7) gui
;--------------------------------------------------------
;create node- and leaf-classes, and corresponding methods
(cw:inf-meth :nc node-pkg
             :nn (let ((n (cw:sup cw:n))) (if (#~m':-$' n) (#~s'-$'' n) (#~s'.+:-'' n)))
             :ln (#~s'.+:-'' (cw:sup cw:n)))

(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  (:panes 
   (tree-pane :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info-pane :application :display-function 'disp-info :incremental-redisplay t :end-of-page-action :allow))
  (:layouts (double (horizontally () tree-pane (make-pane 'clim-extensions:box-adjuster-gadget) info-pane))))

(add-menu-item-to-command-table 'pkg-doc "textsize" :command 'txt-size) ;not working <---

; system and pkg are the same, 13.9.18 <-----
(defun disp-info (f p) 
  (let* ((pkg (alexandria:make-keyword (cw:item-name (cw:group *application-frame*))))
         (sys (case pkg ; vorerst so
                ;(:ALEXANDRIA.0.DEV :alexandria)   ; pkg :ALEXANDRIA.0.DEV --  system :alexandria -- pkg-nicknames :alexandria
                ;(:clim :mcclim)
                ;(:jpeg :cl-jpeg)
                (t pkg)))


         #|
         (file-ql (ignore-errors (ql-dist:find-asdf-system-file sys)))   ; brauchts nicht mehr
         (file.asd (asdf:system-relative-pathname sys sys :type "asd"))
         (file-asdf (ignore-errors (ASDF/SYSTEM:system-source-directory sys)))
         |#

         (inf-ap-fr (info *application-frame*))
         (sym (find-symbol (string-upcase inf-ap-fr) pkg)))
#|-test-
    (format p "~&---info app-frame: ~s" inf-ap-fr)
    (format p "~&---package: ~s" pkg)
    (format p "~&---system: ~s" sys)
    (format p "~&---symbol: ~s" sym)
    (format p "~&---path-q: ~s" file-ql)
    (format p "~&---path-a: ~s" file.asd)
    (format p "~&---path-a: ~s~2%" file-asdf)



    ;(print (cw:group *application-frame*) p)
|#
    (flet ((doc-stg (f)
             (with-drawing-options (p :text-face :bold) (format p "~2%Documentation String:~%"))
             (princ (or (manifest::docs-for sym f) "no-doc-string") p)))
      (dolist (what manifest::*categories*)
        (when (manifest::is sym what) 
          (cond 
;((string= inf-ap-fr "help") (princ (info *application-frame*) p))
((#~m'^Help' inf-ap-fr) (with-drawing-options (p :ink +blue+) (format p (info *application-frame*))))


            ((string= inf-ap-fr pkg) (pkg-description p sys))
            ((member what '(:function :macro :generic-function :slot-accessor)) 
             (with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~2%Argument List:~%" pkg sym))  ; pkg to upper-case
             (color-lambda p (repl-utilities:arglist sym))
             (unless (null sym) (doc-stg what)))
            ((member what '(:variable :class :constant :condition)) 
;(with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~%~a" pkg sym what))   ; <----- 13.3.17  ev überlegen
;(format p "~a" what)
             (unless (null sym) (doc-stg what)))
            (t "there could be other documantation??")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; system idem pkg 
; but mcclim = sys, clim = pkg
(defun disp-info (f p) 
  (let* ((pkg (alexandria:make-keyword (cw:item-name (cw:group *application-frame*))))
         (inf-ap-fr (info *application-frame*))
         (sym (find-symbol (string-upcase inf-ap-fr) pkg)))
    (flet ((doc-stg (f)
                    (with-drawing-options (p :text-face :bold) (format p "~2%Documentation String:~%"))
                    (princ (or (manifest::docs-for sym f) "no-doc-string") p)))
      (dolist (what manifest::*categories*)
        (when (manifest::is sym what) 
          (cond 
            ((#~m'^Help' inf-ap-fr) (with-drawing-options (p :ink +blue+) (format p (info *application-frame*))))
            ((string= inf-ap-fr pkg) (pkg-description p pkg))
            ((member what '(:function :macro :generic-function :slot-accessor)) 
             (with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~2%Argument List:~%" pkg sym))  ; pkg to upper-case
             (color-lambda p (repl-utilities:arglist sym))
             (unless (null sym) (doc-stg what)))
            ((member what '(:variable :class :constant :condition)) 
             (unless (null sym) (doc-stg what)))
            (t "there could be other documantation??")))))))

(defun tview (tree key)
  (cw-utils::t2h-r tree)
  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'string 'pkg-doc :right 800))

;;; GUI COMMANDS --------------------------------------

;==============================================================
; create hierarchical menu to choose a package or a system
;==============================================================
; 1) sorted lists of lower-case strings 
;---------------------------------------
;                     .. / systemname-.... /
;                        / cffi_0.19.0 /     und diverse andere
; #<QL-DIST:SYSTEM zsort / zsort-20120520-git / quicklisp 2015-06-08>) 
(defun ql-system-name (ql-system) (#~s'(-|_)[^-_]+?(-git|-darcs|-svn|-http|-hg)?$'' (second (#~d' / ' (princ-to-string ql-system)))))      ; what is hg ??

#|
;orig
(defun current-packages () 
  (cons "common-lisp" (sort (remove "common-lisp" (mapcar (alexandria:compose 'string-downcase 'package-name) (list-all-packages)) :test 'string=) 'string<)))

;geht
(defun current-packages () 
  (remove-if-not (lambda (x) (ignore-errors (asdf:find-system x))) (mapcar (alexandria:compose 'string-downcase 'package-name) (list-all-packages))))

;sorted, geht
(defun current-packages () 
  (sort (remove-if-not (lambda (x) (ignore-errors (asdf:find-system x))) (mapcar (alexandria:compose 'string-downcase 'package-name) (list-all-packages))) 'string<))
|#

; 13.9.18
;common lisp ladet nicht, alexandria ist nicht dabei, sonst ~ok
(defun current-packages () 
  (cons "common-lisp" (sort (remove-if-not (lambda (x) (ignore-errors (asdf:find-system x))) (mapcar (alexandria:compose 'string-downcase 'package-name) (list-all-packages))) 'string<)))

#|
(defun current-packages () 
  (cons "COMMON-LISP" (sort (remove-if-not (lambda (x) (ignore-errors (asdf:find-system x))) (mapcar (alexandria:compose 'string-downcase 'package-name) (list-all-packages))) 'string<)))
|#


#+quicklisp
(defun quicklisp-systems () 
  (sort (remove-duplicates (mapcar 'ql-system-name (ql:system-list)) :test 'string=) 'string<))

#+quicklisp
(defun local-systems ()
  (if (probe-file #P"~/src/lisp/") (push #P"~/src/lisp/" ql:*local-project-directories*)) ; <--- comment out or adapt to your system -----
  (sort (ql:list-local-systems) 'string<))

; 2) create hierarchical menu to choose a package or a system. Hierarchy by symbol-name: com. cl- asdf/ ...
;----------------------------------------------------------------------------------------------------------
(defun create-menu (l)
  "turn a list into a sorted numbered list"
  (create-menu% (hierarchy-by-symbolname l)))

(defun create-menu% (l &aux (n 0))
  "insert :items and :value into a tree to create a clim-menu"
    (mapcar (lambda (x)
              (if (atom x)
                (list (lol:mkstr (incf n) #\space x) :value x)
                (cons (lol:mkstr (incf n) #\space (car x)) (cons :items (list (create-menu% (cdr x)))))))
            l))

(defun create-menu% (l &aux (n 0))
  "insert :items and :value into a tree to create a clim-menu"
    (mapcar (lambda (x)
              (if (atom x)
                (list (lol:mkstr (incf n) #\space x) :value x) ; orig
 ;               (prog1 (cons (lol:mkstr (incf n) #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (incf n (+ n (length x))))))
;                (prog1 (cons (lol:mkstr (incf n) #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (incf n))))
;                (prog1 (cons (lol:mkstr (incf n) #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (setf n (+ n (length x)))))) ; geht fast
;                (prog1 (cons (lol:mkstr  #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (setf n (+ n (length x)))))) ;  ; use (incf n) lenght to indent <---  fast richtig
                (prog1 (cons (lol:mkstr  #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (setf n (1- (+ n (length x))))))) ;
            l))


#|
;test
#;(defun create-menu% (l &optional (n 0))
  "insert :items and :value into a tree to create a clim-menu"
    (mapcar (lambda (x)
              (if (atom x)
                (list (lol:mkstr (incf n) #\space x) :value x)
                (cons (lol:mkstr (incf n) #\space (car x)) (cons :items (list (create-menu% (cdr x) 0))))))
            l))
|#

(defun print-numbered-pkg (item strm)
  (if (#~m'[-./]$' (car item))
    (with-drawing-options (strm :ink +red+ :text-face :bold) (princ (string-downcase (car item)) strm))
    (princ (string-downcase (car item)) strm)))

; 3) select a package or system, 
;    quickload the selected system if necessary, 
;    create a hierarchical tree with the symbols of the selected package or the corresponding package of a system,
;    populate the hash-table with parents, 
;    make instances of parent-nodes
(defmacro select-pkg (system-category)
  `(let ((pkg (string-upcase (menu-choose (create-menu ,system-category) :printer 'print-numbered-pkg :n-columns 5))))  ; 3 bisher
     #+quicklisp(load-package pkg)))

(define-pkg-doc-command (packages :menu t) ()
  (select-pkg (current-packages)))

#+quicklisp
(define-pkg-doc-command (quicklisp :menu t) ()
  (select-pkg (quicklisp-systems)))

#+quicklisp
(define-pkg-doc-command (local-projects :menu "LocalLibs") ()
  (select-pkg (local-systems)))

(defun packages-by-system (sys)
  "sorted"
  (let ((before (list-all-packages)))
    (ignore-errors (ql:quickload sys))     ;<----
    (sort (mapcar 'package-name (set-difference (list-all-packages) before)) 'string<)))

(defun create-tview (pkg)
  (cw-utils::t2h-r (pkg-tree pkg))
  (with-application-frame (f) 
    (setf (cw:group f) (make-instance 'node-pkg :sup pkg :disp-inf t)) 
    (redisplay-frame-panes f :force-p t)))

(defun load-package (pkg)
  ;(format nil "~a" pkg)   ; <--- remove, for testing only
  (anaphora:acond 
    ((find-package pkg) (create-tview pkg))
    ((packages-by-system pkg) (if (find-package pkg) (create-tview pkg) anaphora:it))
    (t "hello-test")))

; damit ladet quicklisp nicht, present packages gehen gut
;mcclim ladet nicht mit quicklisp
;The name "MCCLIM" does not designate any package
;(defun load-package (pkg) (create-tview pkg))   <-----------------

#|
(defun load-package (pkg) 
  (let ((pkg (case pkg 
               ("mcclim" "clim"))))
  (create-tview pkg)))
|#


;---------------------------------------------------------------
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

(define-pkg-doc-command (cl-apropos :menu t) () ; common-lisp apropos
  (setf (info *application-frame*) (apropos (accept 'string) (accept 'string :default nil) 'external-only)))

#+quicklisp
(define-pkg-doc-command (ql-apropos :menu t) () ; quicklisp apropos
  (setf (info *application-frame*) (ql:system-apropos (accept 'string))))

(define-pkg-doc-command (help :menu t) ()
  (setf (info *application-frame*) (princ *help* *standard-input*)))

(define-pkg-doc-command (help :menu t) ()
  (setf (info *application-frame*) *help*))

(define-pkg-doc-command (help :menu t) ()
  (with-drawing-options (t :ink +blue+) (princ *help* *standard-output*)))

(define-pkg-doc-command (clear :menu t) ()
  (window-clear *standard-input*))

(define-pkg-doc-command (features :menu t) ()
  (setf (info *application-frame*) 
  (with-drawing-options (t :ink +red+) (format t "~{~&  ~a~}" (sort *features* 'string<)))))

(define-pkg-doc-command (modules :menu t) ()
  (setf (info *application-frame*) (format t "~{~&  ~a~}" (sort *modules* 'string<))))

;--------------------------------------------------------
; 8) main
;--------------------------------------------------------
(defun pkg-doc (&optional (pkg :clim)) 
 (tview  (pkg-tree pkg) (package-name pkg)))

(defun pd () (clim-sys:make-process #'pkg-doc))
