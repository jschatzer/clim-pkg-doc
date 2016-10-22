;;;; clim-pkg-doc.lisp

#|--------------------------------------------------------------------
some ideas, concepts and code from Peter Seibel's manifest package
;;;-------------------------------------------------------------------
; this is form the manifest LIENSE file:

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

;--------------------------------------------------------
; 1) helper
;--------------------------------------------------------
(defparameter *help* "
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

(defvar *st* :e) ;symbol-type :e external :a available :p present

(defun pkg-symbols (pkg &optional (stype :e))
  "a available, p present, e external" ; returns a list of upper-case symbols
  (case stype
    (:e (loop for s being the external-symbols of pkg collect s))
    (:p (loop for s being the present-symbols  of pkg collect s))
    (:a (loop for s being the symbols          of pkg collect s))))

; ql-dist::name - mail xach 8.6.2015 [quicklisp-projects] clim-pkg-doc (#927) - that package is not available
; #<QL-DIST:SYSTEM zsort / zsort-20120520-git / quicklisp 2015-06-08>) 
(defun ql-system-name (ql-system) (#~m'(?<= )\S+' (princ-to-string ql-system)))

;--------------------------------------------------------
; 2) sytem-categories, all 3 return upper-case-keywords
;--------------------------------------------------------
(defun current-systems () 
  (cons "common-lisp" (sort (remove "common-lisp" (mapcar (alexandria:compose 'string-downcase 'package-name) (list-all-packages)) :test 'string=) 'string<)))

#+quicklisp
(defun quicklisp-systems () 
  (remove-if (lambda (x) (#~m'[-.]test[s]*$' x)) (mapcar 'ql-system-name (ql:system-list)))) ; sorted "downcase", remove ca 500 system-test, 3016 -> 2453 
  ;(remove-if (lambda (x) (#~m'[-.]test[s]*$' x)) (mapcar 'ql-dist::name (ql:system-list)))) ; sorted "downcase", remove ca 500 system-test, 3016 -> 2453 

#+quicklisp
(defun local-systems ()
  (if (probe-file #P"~/src/lisp/") (push #P"~/src/lisp/" ql:*local-project-directories*)) ; <--- comment out or adapt to your system -----
  (sort (ql:list-local-systems) 'string<))

;--------------------------------------------------------
; 3) create a grouped tree from the symbols of a package, with editing of some packages, e.g. common-lisp, clim
;--------------------------------------------------------
(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (cw:key (car x)) (mapcar #'list x)))) (cw:pack l))) ; ev recursive for all -

;;; if package is CLIM: 1) remove all constants, 2) then add them again as a finished, i.e. sorted, group with a sorted color-subgroup  
(defun clim-constant-p (s) (if (or (constantp s) (#~m'^\+.+\+$' (symbol-name s))) s))

(defun clim-constants ()
  "divide clim-constants into colors and other constants" ; color-names are mixed case strings, without + or -, e.g. "antique white"
  (let* ((clim-color-constants (mapcar (lambda (x) (find-symbol (string-upcase (#~s'(.*)'+\1+' (#~s' '-'g x))) :clim)) (mapcar #'fourth clim-internals::*xpm-x11-colors*))) ; see also cl-colors pkg
         (clim-non-color-constants (remove-if-not #'clim-constant-p (set-difference (pkg-symbols :clim *st*) clim-color-constants)))
         (o (sort clim-non-color-constants #'string<))     ;other constants
         (c (sort clim-color-constants #'nsort:nstring<))) ;color-name constants
    (cons :constant (cons (cons :color-names (mktree c)) (mktree o)))))

;;; if package is COMMON-LISP: 1) remove all special-operators, 2) then add them again as a finished, i.e. sorted, group
(defun spec-op () (cons :special-operator (mktree (sort (remove-if-not #'special-operator-p (pkg-symbols :common-lisp *st*)) #'string<))))

;--------------------------------------------------------
; 4) group symbols into manifest::*categories*
;    idea/concept/code from "manifest" quicklisp package
;--------------------------------------------------------
(defun sorted-symbols-in-a-category (pkg what)
  "return a sorted list of all symbols in a category"
  (sort (loop for sym in 
              (case pkg
                (:clim (remove-if #'clim-constant-p (pkg-symbols :clim *st*)))
                (:common-lisp (remove-if #'special-operator-p (pkg-symbols :cl *st*)))
                (t (pkg-symbols pkg *st*))) 
              when (manifest::is sym what) collect sym) #'nsort:nstring<))

(defun symbol-groups (pkg)
  "group symbols into manifest::*categories*"
  (loop for what in manifest::*categories*
        for category = (sorted-symbols-in-a-category pkg what)
        ;when category collect (cons what category)))
        when category collect (cons (#~s'$':' (princ-to-string what)) category)))
; a symbol-name  function: etc is seldom found in a package, function not so seldom, so clicking a category node does not show any "info"

(defun pkg-tree (p)
  "Group stringified pkg-symbols into a tree, and
  if pkg is clim, show colors in a separate group,
  if pkg is common-lisp, show special forms in a separate group."
  (cw:sym2stg
    (list (cons p
                (case p 
                  (:clim (reverse (cons (clim-constants) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (cdr (reverse (symbol-groups p)))))))
                  (:common-lisp (cons (spec-op) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (symbol-groups p))))
                  (t (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (symbol-groups p))))))))

;--------------------------------------------------------
; 5) color lambda list
;--------------------------------------------------------
(defun color-lambda (s l)
  (mapc (lambda (x)
          (if (#~m'^&' x)
            (with-drawing-options (s :ink +red+ :text-face :bold) (format s "~(~a~)" x))
            (format s "~(~a~)" x)))
        (ppcre:split "(&[^ )]+)" (princ-to-string l) :with-registers-p t)))

;--------------------------------------------------------
; 6) create a package or system menu-list     ---> ev submenus (com. cl- ...)
;--------------------------------------------------------
(defmethod cw:key ((s symbol)) (#~s'([-./]).*'\1'(symbol-name s)))   ; cl- com. asdf/
(defmethod cw:key ((s string)) (#~s'([-./]).*'\1' s))   ; cl- com. asdf/
;(#~m'[^./-]+[./-]' s) ;; ginge auch

(defun create-menu (lst)
  "turn a list into a sorted numbered list"
  (let ((n 0))
    (mapcar 
      (lambda (x) 
        (if (null (cdr x)) 
          (list (lol:mkstr (incf n) #\space (car x)) :value (car x))
          (let ((m 0))
            (list (lol:mkstr (incf n) #\space (cw:key (car x)))
                  :items (mapcar
                           (lambda (y) 
                             (list (lol:mkstr (incf m) #\space y) :value y))
                           x)))))
      (cw:pack lst))))

(defun print-numbered-pkg (item strm)
  (if (#~m'[-./]$' (car item))
    (with-drawing-options (strm :ink +red+ :text-face :bold) (princ (string-downcase (car item)) strm))
    (princ (string-downcase (car item)) strm)))

;--------------------------------------------------------
; 7) gui
;--------------------------------------------------------
;create node- and leaf-classes, and corresponding methods
(cw:inf-meth :nc node-pkg)

(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  (:panes 
   (tree-pane :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info-pane :application :display-function 'disp-info :incremental-redisplay t :end-of-page-action :allow))
  (:layouts (double (horizontally () tree-pane (make-pane 'clim-extensions:box-adjuster-gadget) info-pane))))

(add-menu-item-to-command-table 'pkg-doc "textsize" :command 'txt-size) ;not working <---

(defun disp-info (f p) 
  (let* ((pkg (cw:item-name (cw:group *application-frame*)))
         (inf-ap-fr (info *application-frame*))
         (sym (find-symbol (string-upcase inf-ap-fr) (string-upcase pkg))))
#|-test-
    (format p "info app-frame: ~s" inf-ap-fr)
    (format p "~&---pkg: ~s" pkg)
    (format p "~&---sym: ~s~2%" sym)
|#;------
    (flet ((doc-stg (f)
             (with-drawing-options (p :text-face :bold) (format p "~2%Documentation String:~%"))
             (princ (or (manifest::docs-for sym f) "no-doc-string") p)))
      (dolist (what manifest::*categories*)
        (when (manifest::is sym what) 
          (cond 

            ;((string= inf-ap-fr pkg) (format p "inf: ~a -- pkg: ~a" inf-ap-fr pkg))
            ;((string= inf-ap-fr pkg) (princ (manifest::readme-text pkg) p))
            ;((string= inf-ap-fr pkg) (format p "~a" (manifest::readme-text pkg)))
            ((string= inf-ap-fr pkg) (format p "description asd-file~%info-app-frame: ~a -- pkg: ~a~2%----~2%README~2%~a" inf-ap-fr pkg (manifest::readme-text pkg)))

            ((member what '(:function :macro :generic-function :slot-accessor)) 
             (with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~2%Argument List:~%" pkg sym))  ; pkg to upper-case
             (color-lambda p (repl-utilities:arglist sym))
             (unless (null sym) (doc-stg what)))
            ((member what '(:variable :class :constant :condition)) 
             (unless (null sym) (doc-stg what)))
            (t "there could be other documantation??")))))))


(defun tview (tree key)
  (cw:t2h tree)
  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'string 'pkg-doc :right 800))

;;; commands --------------------------------------
(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

; menu-commands 
(defmacro select-pkg (system-category)
  `(let ((sys (menu-choose (create-menu ,system-category) :printer 'print-numbered-pkg :n-columns 3)))
     (unless (find-package (string-upcase sys)) (ql:quickload sys))
     (cw:t2h (pkg-tree (string-upcase sys)))
     (with-application-frame (f) 
       (setf (cw:group f) (make-instance 'node-pkg :sup sys :disp-inf t)) (redisplay-frame-panes f :force-p t))))

(define-pkg-doc-command (packages :menu t) ()
  (select-pkg (current-systems)))

#+quicklisp
(define-pkg-doc-command (quicklisp :menu t) ()
  (select-pkg (quicklisp-systems)))

#+quicklisp
(define-pkg-doc-command (local-projects :menu "LocalLibs") ()
  (select-pkg (local-systems)))

(define-pkg-doc-command (com-apropos :menu t) ()
  (setf (info *application-frame*) (apropos (accept 'string) (accept 'string :default nil) 'external-only)))

#+quicklisp
(define-pkg-doc-command (com-ql-apropos :menu t) ()
  (setf (info *application-frame*) (ql:system-apropos (accept 'string))))

(define-pkg-doc-command (help :menu t) ()
  (setf (info *application-frame*) (princ *help* *standard-input*)))

(define-pkg-doc-command (clear :menu t) ()
  (window-clear *standard-input*))

; meaning/utility not yet clear
(define-pkg-doc-command (symboltypes :menu t) ()
  (setf *st* (menu-choose '((external . :e) (present . :p) (available . :a))))
  (redisplay-frame-panes *application-frame*))

;--------------------------------------------------------
; 8) main
;--------------------------------------------------------
#|
;geht doch nicht richtig??
;geht prinzipiell, falls singel-process muß allerdings ein pkg wegen &optional angegeben werden; ev etwas anderes erdenken
(defun pkg-doc% (pkg) (tview  (stgtree pkg) (string-downcase (package-name pkg))))

(defun pkg-doc (&optional (pkg :clim) &key single-process) 
  (if single-process
    (clim-sys:make-process (lambda () (pkg-doc% pkg)))
    (pkg-doc% pkg)))
|#

;;; 
(defun pkg-doc (&optional (pkg :clim)) 
 (tview  (pkg-tree pkg) (string-downcase (package-name pkg))))


; ;---------------
; (defun pkg-doc% (&optional (pkg :clim)) (tview  (stgtree pkg) (string-downcase (package-name pkg))))
; 
; (defun pkg-doc (&optional (pkg :clim) &key single-process) 
;   (if single-process
;     (clim-sys:make-process (lambda () (pkg-doc% pkg)))
;     (pkg-doc% pkg)))
; 

; with opt oder keyword
;(bordeaux-threads:make-thread 'clim-pkg-doc:pkg-doc)
;:vs 
(defun pd () (clim-sys:make-process #'clim-pkg-doc:pkg-doc))


