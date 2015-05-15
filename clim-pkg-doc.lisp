;;;; clim-pkg-doc.lisp

#|
;;;----------------------------------------------------------------
some ideas, concepts and code from Peter Seibel's manifet package
;;;----------------------------------------------------------------
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
;;;----------------------------------------------------------------
|# 

(in-package #:clim-pkg-doc)
(named-readtables:in-readtable lol:lol-syntax)

; 1) app-logic and helper --------------
(defvar *help* "
APROPOS: 
1) on the first prompt type a string
2) on the second promt press enter or type the name of a package
")

(defparameter pkg-list (cons :common-lisp (sort (remove :common-lisp (mapcar (lambda (x) (intern (package-name x) :keyword)) (manifest::public-packages))) #'string<)))
(defparameter ql-packages (qlpi::ql-libs-notests))

;;; if package is clim 
; color-names are mixed case strings, without + or -, e.g. "antique white"
(defun clim-color-constants () (mapcar (lambda (x) (alexandria:format-symbol t "+~:@(~a~)+" (#~s' '-'g x))) (mapcar #'fourth clim-internals::*xpm-x11-colors*)))
(defparameter clim-without-colors (set-difference (loop for s being the external-symbols of :clim collect s) (clim-color-constants)))
(defun clim-constant-p (s) (if (or (constantp s) (#~m'^\+.+\+$' (symbol-name s))) s))
(defun clim-non-color-constants () (remove-if-not #'clim-constant-p clim-without-colors))
(defparameter clim-without-constants (set-difference clim-without-colors (clim-non-color-constants)))
(defun clim-constants ()
  "divide clim-constants into colors and other constants"
  (let ((o (sort (clim-non-color-constants) #'string<))     ;other constants
        (c (sort (clim-color-constants) #'nsort:nstring<))) ;color-name constants
    (cons :constant (cons (cons 'color-names (mktree c)) (mktree o)))))

;;; if package is common lisp 
(defun spec-op () (cons 'special-operator (mktree (sort (remove-if-not #'special-operator-p (loop for s being the external-symbols of :cl collect s)) #'string<))))
(defparameter cl-without-spec-op (remove-if #'special-operator-p (loop for s being the external-symbols of :cl collect s)))

; form http://www.ic.unicamp.br/~meidanis/courses/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
(defun key (s) (subseq (symbol-name s) 0 (position #\- (symbol-name s))))
(defun pega (l)
  (cond ((null l) nil)
        ((null (cdr l)) l)
        ((equal (key (car l)) (key (cadr l))) (cons (car l) (pega (cdr l))))
        (t (list (car l)))))
(defun tira (l)
  (cond ((null l) nil)
        ((null (cdr l)) nil)
        ((equal (key (car l)) (key (cadr l))) (tira (cdr l)))
        (t (cdr l))))
(defun pack (l) (if (null l) nil (cons (pega l) (pack (tira l)))))

;do it recursive, all -
(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (key (car x)) (mapcar #'list x)))) (pack l)))

#|
(defun mktree (l) (mapcar (lambda (x) 
                            (cond 
                              ((null (cdr x)) x)
                              ((= 2 (length (ppcre:split "-" (car x)))) (cons (key (car x)) (mapcar #'list x)))
                              (t (cons (second (ppcre:split "-" (car x))) (mapcar #'list x)))))
                          (pack l)))
|#

(defvar *symbol-type* :e)

(defun pkg-symbols (pkg &optional (stype :e))
  "a available, p present, e external"
  (case stype
    (:e (loop for s being the external-symbols of pkg collect s))
    (:p (loop for s being the present-symbols of pkg collect s))
    (:a (loop for s being the symbols of pkg collect s))))

; idea/concept/code from "manifest" quicklisp package
(defun names (pkg what)
  (cond ((eql pkg :clim) (sort (loop for sym in clim-without-constants when (manifest::is sym what) collect sym) #'nsort:nstring<))
        ((eql pkg :cl) (sort (loop for sym in cl-without-spec-op when (manifest::is sym what) collect sym) #'nsort:nstring<))
        (t (sort (loop for sym in (pkg-symbols pkg *symbol-type*) when (manifest::is sym what) collect sym) #'nsort:nstring<))))

(defun symbols (pkg)
  (loop for what in manifest::*categories*
        for names = (names pkg what)
        when names collect (cons what names)))

(defun symbol-tree (p)
  "If pkg is clim, show colors in a separate group.
  If pkg is common-lisp, show special forms in a separate group."
  (cond ((eql p :clim) (reverse (cons (clim-constants) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (cdr (reverse (symbols p)))))))
        ((eql p (or :common-lisp :cl)) (cons (spec-op) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (symbols p))))
        (t (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (symbols p)))))

; 2) gui ------------------------------- nodes should not be sensible <-----
(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  (:panes 
   (tree :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

; use repl-utilities:doc    <------- with lambda list  <--
; or  repl-utilietes:arglist
(defun disp-info (f p) 
  (let ((sym (info *application-frame*)))
    (dolist (what manifest::*categories*)
      (when (manifest::is sym what) (princ (manifest::docs-for sym what) p)))))

(defun tview (tree key)
  (cw:t2h tree)
  (cw:tree-view (make-instance 'cw::node :sup key :disp-inf t) 'symbol 'pkg-doc :right 800))

;;; main --------------------------------------
(defun pkg-doc (&optional (pkg :clim)) (tview (list (cons pkg (symbol-tree pkg))) pkg))
;--------------------------------------

;;; commands --------------------------------------
(define-pkg-doc-command show-info ((item 'symbol :gesture :select))   
  (setf (info *application-frame*) item))

;;; menu-commands --------------------------------------
#|
;creates a separtate window for every new package
(define-pkg-doc-command (packages :menu t) ()   ; loaded packages  -- ev mouse over info
  (let ((pkg (menu-choose pkg-list)))
    (tview (list (cons pkg (symbol-tree pkg))) pkg)))
|#
(define-pkg-doc-command (packages :menu t) ()   ; loaded packages  -- ev mouse over info
  (let ((pkg (menu-choose pkg-list)))
    (cw:t2h (list (cons pkg (symbol-tree pkg))))
    (with-application-frame (f) (setf (cw::group f) (make-instance 'cw::node :sup pkg :disp-inf t)) (redisplay-frame-panes f))))

(define-pkg-doc-command (quicklisp :menu t) ()  ; available quicklisp packages
  (let ((pkg (alexandria:format-symbol t "~:@(~a~)" (menu-choose ql-packages :n-columns 4))))
    (ql:quickload pkg)
    (cons :common-lisp (sort (cons pkg (cdr pkg-list)) #'string<))))

;ev include nicknames a: with repl-utilities:package-apropos ?
(define-pkg-doc-command (com-apropos :menu t) ()
  (setf (info *application-frame*) (apropos (accept 'string) (accept 'symbol :default nil))))

(define-pkg-doc-command (help :menu t) ()
  (setf (info *application-frame*) (princ *help* *standard-input*)))

(define-pkg-doc-command (clear :menu t) ()
  (window-clear *standard-input*))

(define-pkg-doc-command (readme :menu t) ()  ;pkg Readme file
  (setf (info *application-frame*) (princ (manifest::readme-text (cw::node-name (cw::group *application-frame*))) *standard-input*)))

; not yet clear
(define-pkg-doc-command (symboltypes :menu t) ()
  (setf *symbol-type* (menu-choose '((external . :e) (present . :p) (available . :a))))
  (redisplay-frame-panes *application-frame*))
