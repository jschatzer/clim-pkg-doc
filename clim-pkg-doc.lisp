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

;--------------------------------------------------------
; 1) app-logic and helper
;--------------------------------------------------------
(defvar *symbol-type* :e) ; :e external :a available :p present

(defparameter *help* "
Click the root node to see a package's description or readme-file
-----------------------------------------------------------------
APROPOS: 
1) on the first prompt type a string
2) on the second promt press enter or type the name of a package
-----------------------------------------------------------------
")

(defparameter current-packages (cons "COMMON-LISP" (sort (remove "COMMON-LISP" (mapcar 'package-name (list-all-packages)) :test 'equal) 'string<))) ; "UPCASE"
(defparameter ql-systems (remove-if (lambda (x) (#~m'[-.]test[s]*$' x)) (mapcar 'ql-dist::name (ql:system-list)))) ; sorted "downcase", remove ca 500 system-test, 3016 to 2453 
(defun pkg-symbols (pkg &optional (stype :e))  ; CL-FAD:DIRECTORY-EXISTS-P
  "a available, p present, e external"
  (case stype
    (:e (loop for s being the external-symbols of pkg collect s))
    (:p (loop for s being the present-symbols  of pkg collect s))
    (:a (loop for s being the symbols          of pkg collect s))))

;do it recursive, all -
(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (cw:key (car x)) (mapcar #'list x)))) (cw:pack l)))
#|
(defun mktree (l) (mapcar (lambda (x) 
                            (cond 
                              ((null (cdr x)) x)
                              ((= 2 (length (ppcre:split "-" (car x)))) (cons (key (car x)) (mapcar #'list x)))
                              (t (cons (second (ppcre:split "-" (car x))) (mapcar #'list x)))))
                          (pack l)))
|#

;--------------------------------------------------------
;;; edit package-tree in some packages, e.g. common-lisp, clim
;--------------------------------------------------------
;;; if package is CLIM    --- ev use (remove-if-not 'constantp (clim-pkg-doc::pkg-symbols :cl-colors :e))
; color-names are mixed case strings, without + or -, e.g. "antique white"
(defun clim-color-constants () (mapcar (lambda (x) (intern (string-upcase (#~s'(.*)'+\1+' (#~s' '-'g x))))) (mapcar #'fourth clim-internals::*xpm-x11-colors*))) ; see cl-colors pkg <--
(defparameter clim-without-colors (set-difference (pkg-symbols :clim) (clim-color-constants)))
(defun clim-constant-p (s) (if (or (constantp s) (#~m'^\+.+\+$' (symbol-name s))) s))
(defun clim-non-color-constants () (remove-if-not #'clim-constant-p clim-without-colors))
(defparameter clim-without-constants (set-difference clim-without-colors (clim-non-color-constants)))
(defun clim-constants ()
  "divide clim-constants into colors and other constants"
  (let ((o (sort (clim-non-color-constants) #'string<))     ;other constants
        (c (sort (clim-color-constants) #'nsort:nstring<))) ;color-name constants
    (cons :constant (cons (cons 'color-names (mktree c)) (mktree o)))))

;;; if package is COMMON-LISP 
(defun spec-op () (cons 'special-operator (mktree (sort (remove-if-not #'special-operator-p (pkg-symbols :cl)) #'string<))))
(defparameter cl-without-spec-op (remove-if #'special-operator-p (pkg-symbols :cl)))

;--------------------------------------------------------
;color lambda list
;--------------------------------------------------------
(defun &symbol-p (s) (if (char= #\& (elt (symbol-name s) 0)) t))

(defun draw (s x)
  (if (&symbol-p x)
    (with-drawing-options (s :ink +red+ :text-face :bold) (format s "~(~a ~)" x))
    (with-drawing-options (s :ink +black+) (format s "~(~a ~)" x))))

; corrige last space
(defun color-lambda (s lst)
  (princ "(" s)
  (mapc (lambda (x)
          (cond ((and (atom x) (not (symbolp x))) (format s "~(~a ~)" x)) ;;; there my be strings,  numbers as vaules , create errors with draw
                ((atom x) (draw s x))
                (t (color-lambda s x))))
        lst)
  (princ ")" s))

;--------------------------------------------------------
;menu-list helper
;--------------------------------------------------------
(defmethod cw:key ((s string)) (#~s'[-./].*'' s))   ; cl- com. asdf/

(defun create-menu (lst)
  "turn a list into a sorted numbered list"
  (let ((n 0))
    (mapcar 
      (lambda (x) 
        (if (null (cdr x)) 
          (list (lol:mkstr (incf n) #\space (car x)) :value (car x))
          (let ((m 0))
            (list (lol:mkstr (incf n) #\space (cw:key (car x)) #\%) ; mark submenus with %
                  :items (mapcar
                           (lambda (y) 
                             (list (lol:mkstr (incf m) #\space y) :value y))
                           x)))))
      (cw:pack lst))))

(defun print-numbered-pkg (item strm)
  (princ (if (string= "COMMON-LISP" (getf (cdr item) :value))
           (string-upcase (car item))
           (string-downcase (car item)))
         strm))

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

;--------------------------------------------------------
; 2) gui
;--------------------------------------------------------
(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  (:panes 
   (tree-pane :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info-pane :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree-pane (make-pane 'clim-extensions:box-adjuster-gadget) info-pane))))

(defun disp-info (f p) 
  (let ((sym (info *application-frame*)))
    (if (equalp sym (cw::node-name (cw::group *application-frame*))) (princ (manifest::readme-text sym)))   ; u/o short pkg doc <--------
    (dolist (what manifest::*categories*)
      (when (manifest::is sym what) 
        (if (member what '(:function :macro :generic-function :slot-accessor)) 
          (progn 
            (with-drawing-options (p :text-face :bold) (princ "Argument List:" p)) (terpri  p)
            (color-lambda p (repl-utilities:arglist sym)))
          "")   ;;; <----------------------- ev etwas für andere kategorien anzeigen
        (terpri  p) (terpri  p) (terpri  p)
        (with-drawing-options (p :text-face :bold) (princ "Documentation String:" p)) (terpri  p)
        (princ (manifest::docs-for sym what) p)))))

(defun tview (tree key)
  (cw:t2h tree)
  (cw:tree-view (make-instance 'cw::node :sup key :disp-inf t) 'symbol 'pkg-doc :right 800))

;;; commands --------------------------------------
(define-pkg-doc-command show-info ((item 'symbol :gesture :select))   
  (setf (info *application-frame*) item))

; menu-commands 
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose (create-menu current-packages) :printer 'print-numbered-pkg :n-columns 3)))
    (cw:t2h (list (cons pkg (symbol-tree pkg))))
    (with-application-frame (f) (setf (cw::group f) (make-instance 'cw::node :sup pkg :disp-inf t)) (redisplay-frame-panes f))))

(define-pkg-doc-command (quicklisp :menu t) ()
  (let ((syst (alexandria:format-symbol t "~:@(~a~)" (menu-choose (create-menu ql-systems) :printer 'print-numbered-pkg :n-columns 4))))
    (ql:quickload syst)
    (let ((pkg (if (find-package (string-upcase syst)) syst)))
    (cons "COMMON-LISP" (sort (cons pkg (cdr (copy-list current-packages))) #'string<))
    (cw:t2h (list (cons pkg (symbol-tree pkg))))
    (with-application-frame (f) (setf (cw::group f) (make-instance 'cw::node :sup pkg :disp-inf t)) (redisplay-frame-panes f)))))

(define-pkg-doc-command (com-apropos :menu t) ()
  (setf (info *application-frame*) (apropos (accept 'string) (accept 'symbol :default nil))))

(define-pkg-doc-command (help :menu t) ()
  (setf (info *application-frame*) (princ *help* *standard-input*)))

(define-pkg-doc-command (clear :menu t) ()
  (window-clear *standard-input*))

; meaning/utility not yet clear
(define-pkg-doc-command (symboltypes :menu t) ()
  (setf *symbol-type* (menu-choose '((external . :e) (present . :p) (available . :a))))
  (redisplay-frame-panes *application-frame*))

;--------------------------------------------------------
; 3) main
;--------------------------------------------------------
(defun pkg-doc (&optional (pkg :clim)) (tview (list (cons pkg (symbol-tree pkg))) pkg))


