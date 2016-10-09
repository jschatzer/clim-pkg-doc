;;;; clim-pkg-doc.lisp
; ev use docparser, 15.6.15 <----
;COM.INFORMATIMAGO.TOOLS.QUICKLISP    <------
;COM.INFORMATIMAGO.LISPDOC

#|
;;;----------------------------------------------------------------
some ideas, concepts and code from Peter Seibel's manifest package
;;;----------------------------------------------------------------
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
;;;----------------------------------------------------------------
|# 

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

;#|
; ql-dist::name - see mail xach 8.6.2015 [quicklisp-projects] clim-pkg-doc (#927) -  that package is not available <--- ?
; #<QL-DIST:SYSTEM zsort / zsort-20120520-git / quicklisp 2015-06-08>) 
(defun ql-system-name (ql-system) (#~s'\S+ (.\S+) .+'\1' (princ-to-string ql-system)))
;|#

;--------------------------------------------------------
; 2) sytem-categories, all 3 return upper-case-keywords
;--------------------------------------------------------
(defun current-packages () 
  (mapcar (lambda (x) (intern x :keyword)) 
          (cons "COMMON-LISP" (sort (remove "COMMON-LISP" (mapcar 'package-name (list-all-packages)) :test 'equal) 'string<))))

;#|
#+quicklisp
(defun ql-systems () 
  (mapcar (lambda (x) (intern (string-upcase x) :keyword)) 
          (remove-if (lambda (x) (#~m'[-.]test[s]*$' x)) (mapcar 'ql-system-name (ql:system-list))))) ; sorted "downcase", remove ca 500 system-test, 3016 -> 2453 

#+quicklisp
(defun local-libs ()
  (if (probe-file #P"~/src/lisp/") (push #P"~/src/lisp/" ql:*local-project-directories*)) ; <--- comment out or adapt to your system -----
  (sort (mapcar (lambda (x) (intern (string-upcase x) :keyword)) (ql:list-local-systems)) 'string<))
;|#

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
        when category collect (cons what category)))

(defun symbol-tree (p)
  "If pkg is clim, show colors in a separate group.
  If pkg is common-lisp, show special forms in a separate group."
  (case p (:clim (reverse (cons (clim-constants) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (cdr (reverse (symbol-groups p)))))))
          (:common-lisp (cons (spec-op) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (symbol-groups p))))
          (t (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (symbol-groups p)))))

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
(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  ;(:menu-bar cw:tree)  ; damit geht textsize, sonstige menu fehlt
  ;(:menu-bar pkg-doc)   ; textsize geht nicht       <----- DEFAULT
  ;(:menu-bar (pkg-doc :inherit-from (cw:tree))) ;; error <--
  ;(:menu-bar xyz)
  ;(:menu-bar (list cw:tree pkg-doc))
  (:panes 
   (tree-pane :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info-pane :application :display-function 'disp-info :incremental-redisplay t :end-of-page-action :allow))
  (:layouts (double (horizontally () tree-pane (make-pane 'clim-extensions:box-adjuster-gadget) info-pane))))

;(define-command-table xyz :inherit-from '(cw:tree pkg-doc) :inherit-menu t)
;(define-command-table xyz :inherit-from (list cw:tree pkg-doc) :inherit-menu t)

;(add-menu-item-to-command-table 'pkg-doc "textsize" :command 'cw::txt-size)  ; zeit txtsize in gray , geht noch nicht
(add-menu-item-to-command-table 'pkg-doc "textsize" :command 'txt-size)


(defun disp-info (f p) 
  (let ((sym (info *application-frame*))
        (pkg (cw::node-name (cw::group *application-frame*))))
    (dolist (what manifest::*categories*)
      (when (manifest::is sym what) 
        (flet ((doc-stg ()
                 (with-drawing-options (p :text-face :bold) 
                   (format p "~2%Documentation String:~%"))
                 (princ (or (manifest::docs-for sym what) "") p)))
          (cond ((equalp sym pkg) (princ (manifest::readme-text sym) p))   ; u/o short pkg doc <--------   ev statt "" descrip in asd file verwenden
                ((member what '(:function :macro :generic-function :slot-accessor)) 
                 (progn 
                   (with-drawing-options (p :text-face :bold) 
                     (format p "~a:~a~2%Argument List:~%" pkg sym))
                   (color-lambda p (repl-utilities:arglist sym))
                   (doc-stg)))
                ((member what '(:variable :class :constant :condition)) (doc-stg))  ; cond noch zu testen
                (t "")))))))

;(cw:inf-meth cw:node-pd)
(cw:inf-meth node-pd)

;(defclass node-pd (node) ())
;(defclass leaf-pd (leaf) ())

(defmethod cw:node-name ((n node-pd)) (cw:sup n))
(defmethod cw:children ((n node-pd)) (gethash (cw:sup n) cw:*nodes*)) ;children are symobols 
(defmethod cw:c-nodep ((n symbol)) (gethash n cw:*nodes*)) ; the child is a node if it has children <----
(defmethod cw:childnode-is-youngestsibling ((n symbol) ch) (and (cw:c-nodep n) (eql n (car (reverse ch)))))

(defun tview (tree key)
  (cw:t2h tree)
  (cw:tree-view (make-instance 'node-pd :sup key :disp-inf t) 'symbol 'pkg-doc :right 800))

;;; commands --------------------------------------
(define-pkg-doc-command show-info ((item 'symbol :gesture :select))   
  (setf (info *application-frame*) item))

; menu-commands 
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose (create-menu (current-packages)) :printer 'print-numbered-pkg :n-columns 3)))
    (cw:t2h (list (cons pkg (symbol-tree pkg))))
    (with-application-frame (f) (setf (cw:group f) (make-instance 'node-pd :sup pkg :disp-inf t)) (redisplay-frame-panes f :force-p t))))

#+quicklisp
(define-pkg-doc-command (quicklisp :menu t) ()
  (let ((sys (menu-choose (create-menu (ql-systems)) :printer 'print-numbered-pkg :n-columns 3)))
    (ql:quickload sys)
    (if (find-package sys) (cw:t2h (list (cons sys (symbol-tree sys)))))
    (with-application-frame (f) (setf (cw:group f) (make-instance 'node-pd :sup sys :disp-inf t)) (redisplay-frame-panes f :force-p t))))

#+quicklisp
(define-pkg-doc-command (local-projects :menu "LocalLibs") ()
  (let ((sys (menu-choose (create-menu (local-libs)) :printer 'print-numbered-pkg :n-columns 3)))
    (ql:quickload sys)
    (if (find-package sys) (cw:t2h (list (cons sys (symbol-tree sys)))))
    (with-application-frame (f) (setf (cw:group f) (make-instance 'node-pd :sup sys :disp-inf t)) (redisplay-frame-panes f :force-p t))))

(define-pkg-doc-command (com-apropos :menu t) ()
  ;(setf (info *application-frame*) (apropos (accept 'string) (accept 'symbol :default nil))))
  (setf (info *application-frame*) (apropos (accept 'string) (accept 'symbol :default nil) 'external-only)))

#+quicklisp
(define-pkg-doc-command (com-ql-apropos :menu t) ()
  (setf (info *application-frame*) (ql:system-apropos (accept 'string))))  ; geht <----

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
(defun pkg-doc (&optional (pkg :clim)) (tview (list (cons pkg (symbol-tree pkg))) pkg))

; (defun pd () ; pdt
; ev mit opt oder keyword
;(bordeaux-threads:make-thread 'clim-pkg-doc:pkg-doc)
;:vs 
(defun pd () "load clim-pkg-doc" (clim-sys:make-process #'clim-pkg-doc:pkg-doc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;
; #|
; (defun disp-info (f p) 
;   (let ((sym (info *application-frame*)))
;     (if (equalp sym (cw::node-name (cw::group *application-frame*))) (princ (manifest::readme-text sym) p))   ; u/o short pkg doc <--------
;     (dolist (what manifest::*categories*)
;       (when (manifest::is sym what) 
;         (if (member what '(:function :macro :generic-function :slot-accessor)) 
;           (progn 
;             (with-drawing-options (p :text-face :bold) (princ "Argument List:" p)) (terpri  p)
;             (color-lambda p (repl-utilities:arglist sym)))
;           "")   ;;; <----------------------- ev do something for other categories?
;         (terpri  p) (terpri  p) (terpri  p)
;         (with-drawing-options (p :text-face :bold) (princ "Documentation String:" p)) (terpri  p)
;         (princ (manifest::docs-for sym what) p)))))
; 
; 
; (defun disp-info (f p) 
;   (let ((sym (info *application-frame*)))
;     (if (equalp sym (cw::node-name (cw::group *application-frame*))) (princ (manifest::readme-text sym) p))   ; u/o short pkg doc <--------
;     (dolist (what manifest::*categories*)
;       (when (manifest::is sym what) 
; 
; (flet ((doc-stg ()
; (progn 
;            (with-drawing-options (p :text-face :bold) 
;   (princ sym p) (terpri  p) (terpri  p)
; 
; ;        (terpri  p) (terpri  p) (terpri  p)
; ;        (with-drawing-options (p :text-face :bold) 
;                               (princ "Documentation String:" p)) (terpri  p)
;         (princ (manifest::docs-for sym what) p))))
;  
; 
;         (if (member what '(:function :macro :generic-function :slot-accessor)) 
; 
;           (progn 
;             (with-drawing-options (p :text-face :bold) 
;                                   (princ sym p) (terpri  p) (terpri  p)
;                                   (princ "Argument List:" p)) (terpri  p)
;             (color-lambda p (repl-utilities:arglist sym))
;            (doc-stg)
;             
;             )
; ;          "")   ;;; <----------------------- ev do something for other categories?
; (doc-stg)
;   ))))))
; 
; 
; (defun disp-info (f p) 
;   (let ((sym (info *application-frame*)))
;     (if (equalp sym (cw::node-name (cw::group *application-frame*))) (princ (manifest::readme-text sym) p))   ; u/o short pkg doc <--------
;     (dolist (what manifest::*categories*)
;       (when (manifest::is sym what) 
;         (flet ((doc-stg ()
;                  (with-drawing-options (p :text-face :bold) 
;                    (format p "~2%Documentation String:~%"))
;                  (princ (manifest::docs-for sym what) p)))
;           (if (member what '(:function :macro :generic-function :slot-accessor)) 
;             (progn 
;               (with-drawing-options (p :text-face :bold) 
; ;                (format p "~a~2%Argument List:~%" sym))
;                 (format p "~a:~a~2%Argument List:~%" (cw::node-name (cw::group *application-frame*)) sym))
;               (color-lambda p (repl-utilities:arglist sym))
;               (doc-stg))
;             (doc-stg)))))))
; 
; 
; (defun disp-info (f p) 
;   (let ((sym (info *application-frame*))
;         (pkg (cw::node-name (cw::group *application-frame*))))
;     (if (equalp sym pkg) (princ (manifest::readme-text sym) p) "")   ; u/o short pkg doc <--------   ev statt "" descrip in asd file verwenden
;     (dolist (what manifest::*categories*)
;       (when (manifest::is sym what) 
;         (flet ((doc-stg ()
;                  (with-drawing-options (p :text-face :bold) 
;                    (format p "~2%Documentation String:~%"))
;                  (princ (manifest::docs-for sym what) p)))
; 
;           #|
;           (if (member what '(:function :macro :generic-function :slot-accessor)) 
;             (progn 
;               (with-drawing-options (p :text-face :bold) 
;                 (format p "~a:~a~2%Argument List:~%" pkg sym))
;               (color-lambda p (repl-utilities:arglist sym))
;               (doc-stg))
;             (doc-stg)))))))
;           |#
; 
; (cond ((member what '(:function :macro :generic-function :slot-accessor)) 
;        (progn 
;          (with-drawing-options (p :text-face :bold) 
;            (format p "~a:~a~2%Argument List:~%" pkg sym))
;          (color-lambda p (repl-utilities:arglist sym))
;          (doc-stg)))
;       ((member what '(:variable :class :constant :condition)) (doc-stg))  ; cond noch zu testen
;       (t "")))))))
; 
; (defun disp-info (f p) 
;   (let ((sym (info *application-frame*))
;         (pkg (cw::node-name (cw::group *application-frame*))))
;     (if (equalp sym pkg) (princ (manifest::readme-text sym) p) "")   ; u/o short pkg doc <--------   ev statt "" descrip in asd file verwenden
;     (dolist (what manifest::*categories*)
;       (when (manifest::is sym what) 
;         (flet ((doc-stg ()
;                  (with-drawing-options (p :text-face :bold) 
;                    (format p "~2%Documentation String:~%"))
;                  (princ (or (manifest::docs-for sym what) "") p)))
;           (cond ((member what '(:function :macro :generic-function :slot-accessor)) 
;                  (progn 
;                    (with-drawing-options (p :text-face :bold) 
;                      (format p "~a:~a~2%Argument List:~%" pkg sym))
;                    (color-lambda p (repl-utilities:arglist sym))
;                    (doc-stg)))
;                 ((member what '(:variable :class :constant :condition)) (doc-stg))  ; cond noch zu testen
;                 (t "")))))))
; 
; 
; ;geht ~gut
; (defun disp-info (f p) 
;   (let ((sym (info *application-frame*))
;         (pkg (cw::node-name (cw::group *application-frame*))))
; ;    (if (equalp sym pkg) (princ (manifest::readme-text sym) p) "")   ; u/o short pkg doc <--------   ev statt "" descrip in asd file verwenden
;     (dolist (what manifest::*categories*)
;       (when (manifest::is sym what) 
;         (flet ((doc-stg ()
;                  (with-drawing-options (p :text-face :bold) 
;                    (format p "~2%Documentation String:~%"))
;                  (princ (or (manifest::docs-for sym what) "") p)))
;           (cond 
; 
;                      ((equalp sym pkg) (princ (manifest::readme-text sym) p))   ; u/o short pkg doc <--------   ev statt "" descrip in asd file verwenden
;                  ((member what '(:function :macro :generic-function :slot-accessor)) 
;                  (progn 
;                    (with-drawing-options (p :text-face :bold) 
;                      (format p "~a:~a~2%Argument List:~%" pkg sym))
;                    (color-lambda p (repl-utilities:arglist sym))
;                    (doc-stg)))
;                 ((member what '(:variable :class :constant :condition)) (doc-stg))  ; cond noch zu testen
; 
; 
; 
;                 (t "")))))))
; |#
; 
; 
; 
; 
; ;-------------
; ;orig
; #;(defun disp-info (f p) 
;   (let ((sym (info *application-frame*)))
;     (if (equalp sym (cw-treeview::node-name (cw-treeview::group *application-frame*))) (princ (manifest::readme-text sym) p))   ; u/o short pkg doc <--------
;     (dolist (what manifest::*categories*)
;       (when (manifest::is sym what) 
;         (if (member what '(:function :macro :generic-function :slot-accessor)) 
;           (progn 
;             (with-drawing-options (p :text-face :bold) (princ "Argument List:" p)) (terpri  p)
;             (color-lambda p (repl-utilities:arglist sym)))
;           "")   ;;; <----------------------- ev do something for other categories?
;         (terpri  p) (terpri  p) (terpri  p)
;         (with-drawing-options (p :text-face :bold) (princ "Documentation String:" p)) (terpri  p)
;         (princ (manifest::docs-for sym what) p)))))
; 
; 
; 
; 
; 
; #|
; ;==================================================================
; (defun tview (tree key)
;   (cw:t2h tree)
;   (cw:tree-view (make-instance 'cw::node :sup key :disp-inf t) 'symbol 'pkg-doc :right 800))
; 
; ;;; commands --------------------------------------
; (define-pkg-doc-command show-info ((item 'symbol :gesture :select))   
;   (setf (info *application-frame*) item))
; 
; ; menu-commands 
; (define-pkg-doc-command (packages :menu t) ()
;   (let ((pkg (menu-choose (create-menu (current-packages)) :printer 'print-numbered-pkg :n-columns 3)))
;     (cw:t2h (list (cons pkg (symbol-tree pkg))))
;     ;(with-application-frame (f) (setf (cw::group f) (make-instance 'cw:node :sup pkg :disp-inf t)) (redisplay-frame-panes f))))
;     (with-application-frame (f) (setf (cw::group f) (make-instance 'cw:node :sup pkg :disp-inf t)) (redisplay-frame-panes f :force-p t))))
; 
; ;#|
; #+quicklisp
; (define-pkg-doc-command (quicklisp :menu t) ()
;   (let ((sys (menu-choose (create-menu (ql-systems)) :printer 'print-numbered-pkg :n-columns 3)))
;     (ql:quickload sys)
;     (if (find-package sys) (cw:t2h (list (cons sys (symbol-tree sys)))))
;     (with-application-frame (f) (setf (cw::group f) (make-instance 'cw:node :sup sys :disp-inf t)) (redisplay-frame-panes f :force-p t))))
; 
; #+quicklisp
; (define-pkg-doc-command (local-projects :menu "LocalLibs") ()
;   (let ((sys (menu-choose (create-menu (local-libs)) :printer 'print-numbered-pkg :n-columns 3)))
;     (ql:quickload sys)
;     (if (find-package sys) (cw:t2h (list (cons sys (symbol-tree sys)))))
;     (with-application-frame (f) (setf (cw::group f) (make-instance 'cw:node :sup sys :disp-inf t)) (redisplay-frame-panes f :force-p t))))
; ;|#
; ;==================================================================
; |#
; 
; 
; 
; ;==================================================================
; #|
; (cw:inf-meth cw::node-pd) ;pkg-doc
; (defmethod cw::children ((n cw::node-pd)) (gethash (cw::sup n) cw::*nodes*))
; (defmethod cw::c-nodep ((n string)) (gethash n cw::*nodes*))
; (defmethod cw::childnode-is-youngestsibling ((n string) cw::ch) (and (cw::c-nodep n) (string= n (car (reverse cw::ch)))))
; 
; 
; (defun tview (tree key)
;   (cw:t2h tree)
;   (cw:tree-view (make-instance 'cw::node-pd :sup key :disp-inf t) 'symbol 'pkg-doc :right 800))
; 
; ;;; commands --------------------------------------
; (define-pkg-doc-command show-info ((item 'symbol :gesture :select))   
;   (setf (info *application-frame*) item))
; 
; ; menu-commands 
; (define-pkg-doc-command (packages :menu t) ()
;   (let ((pkg (menu-choose (create-menu (current-packages)) :printer 'print-numbered-pkg :n-columns 3)))
;     (cw:t2h (list (cons pkg (symbol-tree pkg))))
;     ;(with-application-frame (f) (setf (cw::group f) (make-instance 'node-pd :sup pkg :disp-inf t)) (redisplay-frame-panes f))))
;     (with-application-frame (f) (setf (cw::group f) (make-instance 'cw::node-pd :sup pkg :disp-inf t)) (redisplay-frame-panes f :force-p t))))
; 
; ;#|
; #+quicklisp
; (define-pkg-doc-command (quicklisp :menu t) ()
;   (let ((sys (menu-choose (create-menu (ql-systems)) :printer 'print-numbered-pkg :n-columns 3)))
;     (ql:quickload sys)
;     (if (find-package sys) (cw:t2h (list (cons sys (symbol-tree sys)))))
;     (with-application-frame (f) (setf (cw::group f) (make-instance 'cw::node-pd :sup sys :disp-inf t)) (redisplay-frame-panes f :force-p t))))
; 
; #+quicklisp
; (define-pkg-doc-command (local-projects :menu "LocalLibs") ()
;   (let ((sys (menu-choose (create-menu (local-libs)) :printer 'print-numbered-pkg :n-columns 3)))
;     (ql:quickload sys)
;     (if (find-package sys) (cw:t2h (list (cons sys (symbol-tree sys)))))
;     (with-application-frame (f) (setf (cw::group f) (make-instance 'cw::node-pd :sup sys :disp-inf t)) (redisplay-frame-panes f :force-p t))))
; ;|#
; |#
; ;==================================================================
; 
; #|
; ;(defmethod cw-treeview::node-name ((n cw-treeview::node-pd)) (package-name (cw-treeview::sup n)))
; ;(defmethod node-name ((n cw-treeview::node-pd)) (string-downcase (cw-treeview::sup n)))
; ;(defmethod node-name ((n cw-treeview::node-pd)) (string-downcase (o:stg (car (cw-treeview::sup n)))))
; 
; 
; ;(defmethod cw-treeview::children ((n cw-treeview::node-pd)) (gethash (cw-treeview::sup n) cw-treeview::*nodes*))
; ;(defmethod cw-treeview::children ((n cw-treeview::node-pd)) (gethash (node-name (cw-treeview::sup n)) cw-treeview::*nodes*))
; ;(defmethod cw-treeview::children ((n cw-treeview::node-pd)) (gethash n cw-treeview::*nodes*))
; 
; ;das geht noch am besten, aber falsch
; (defmethod cw-treeview::children ((n cw-treeview::node-pd)) (symbol-tree (cw-treeview::sup n)))    ; <----
; ;(defmethod cw-treeview::children ((n cw-treeview::node-pd)) (cdr (symbol-tree (cw-treeview::sup n))))    ; <----    anschauen
; 
; ;The name :FUNCTION does not designate any package
; (defmethod cw-treeview::children ((n cw-treeview::node-pd)) (mapcar 'car (symbol-tree (cw-treeview::sup n))))  ; das geht schon fast <---
; 
; 
; 
; 
; (defmethod cw-treeview::c-nodep ((n string)) (gethash n cw-treeview::*nodes*))
; (defmethod cw-treeview::c-nodep ((n symbol)) (gethash n cw-treeview::*nodes*))
; 
; (defmethod cw-treeview::childnode-is-youngestsibling ((n string) ch) (and (cw-treeview::c-nodep n) (string= n (car (reverse ch)))))
; (defmethod cw-treeview::childnode-is-youngestsibling ((n symbol) ch) (and (cw-treeview::c-nodep n) (equal n (car (reverse ch)))))
; |#
; 
; ;#|
; ;---------------------------
