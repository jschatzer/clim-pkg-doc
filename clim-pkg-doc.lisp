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

;1) cl+ssl.asd hat 2x defpackage vor defsystem -- loop
;2) alexandria.0.dev    pathname systemfile
#|
(defun asdf-description (f)
  "return list of short and long asdf-description"
  (with-open-file (i f)
    (loop 
      (let ((asd (read i nil nil)))
      (if (#~m'defsystem'i (lol:mkstr (car asd)))
        (return (list (getf (cddr asd) :description) (getf (cddr asd) :long-description))))))))
|#

(defun asdf-description (sys)
  (list
    (asdf/system:system-description (asdf/system:find-system sys))
    (asdf/system:system-long-description (asdf/system:find-system sys))))

;--------------------------------
;(defun color-info (s nr a1 a2 a3) 
(defun color-info (s pkg) 
  (let 
    ((nr (length (pkg-symbols pkg)))
     (a1 (car (asdf-description pkg)))
     (a2 (cadr (asdf-description pkg)))
     (a3 (manifest::readme-text pkg)))



  (with-drawing-options (s :ink +red+) (format s "~a " nr)) (format s "external-symbols") ; nr ext symbols
  (with-drawing-options (s :ink +red+ :text-face :bold) (format s 
"
-------------------------
 ASDF Description
-------------------------"))
  (with-drawing-options (s :text-face :bold) (format s "~&SHORT: ")) (format s "~a" a1)
  (with-drawing-options (s :text-face :bold) (format s "~2&LONG: ")) (format s "~a" a2)
  (with-drawing-options (s :ink +red+ :text-face :bold) (format s 
"~&
-------------------------
 README
-------------------------"))
  (format s "~&~a" a3))
  )

;--------------------------------
; #<QL-DIST:SYSTEM zsort / zsort-20120520-git / quicklisp 2015-06-08>) 
;(defun ql-system-name (ql-system) (#~m'(?<= )\S+' (princ-to-string ql-system)))

;--------------------------------------------------------
; 2) sytem-categories
;--------------------------------------------------------
(defun current-systems () 
  (cons "common-lisp" (sort (remove "common-lisp" (mapcar (alexandria:compose 'string-downcase 'package-name) (list-all-packages)) :test 'string=) 'string<)))

#+quicklisp
(defun quicklisp-systems () 
  ;(remove-if (lambda (x) (#~m'[-.]test[s]*$' x)) (mapcar 'ql-system-name (ql:system-list)))) ;remove system-test
  (remove-if (lambda (x) (#~m'[-.]test[s]*$' x)) (mapcar 'ql-dist::name (ql:system-list))))

#+quicklisp
(defun local-systems ()
  (if (probe-file #P"~/src/lisp/") (push #P"~/src/lisp/" ql:*local-project-directories*)) ; <--- comment out or adapt to your system -----
  (sort (ql:list-local-systems) 'string<))

;----------------------------------
; -3) create hierarchy by symbolname
;----------------------------------
;(parts 'a-b-c) -> ("a-" "b-" "c")
(defun parts (x)
  (pre:ifmatch (#~m'^(/+)$' x)
    (list $1)
    (#~m'[^-./]+(-|\.|/)?'g x)))

;---------------------------
;(key 'a-b-c)  ; "A-"
;(key 'a-b-c 1) ;"A-B-"
(defun key (s &optional (i 0))
 (#~s' ''g (stdutils:list-to-delimited-string (reverse (key% s i))))) 

;(key% 'a-b-c 1) ; (B- A-)
(defun key% (s &optional (i 0))
  "key ~ header"
  (cond ((zerop i) (list (nth i (parts s))))
        (t (cons 
             (nth i (parts s))
             (key% s (1- i))))))

(defun r-add-header (l ind) ; recursive-add-header list index
  (cons (key (car l) ind) (pack (reverse l) (1+ ind))))

(defun pack (l &optional (i 0) v)
  (cond ((null l) (if (= 1 (length v)) v (list (r-add-header v i))))
        ((null v) (pack (cdr l) i (list (car l))))
        ((equal (key (car v) i) (key (car l) i)) (pack (cdr l) i (push (car l) v)))
        (t (cons (if (= 1 (length v))
                   (car v)
                   (r-add-header v i))
                 (pack (cdr l) i (list (car l)))))))

(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    ((and (consp (car l)) (notany #'consp (car l))) (cons (car l) (remove-empty-bags (cdr l))))
    ((and (= 2 (length l)) (atom (car l)) (consp (cadr l))) (remove-empty-bags (cadr l)))
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

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
(defun pkg-symbols (pkg) (loop for s being the external-symbols of pkg collect s))

(defun sorted-symbols-in-a-category (pkg what)
  "return a sorted list of all symbols in a category"
  (sort (loop for sym in 
              (case pkg
                (:clim (remove-if #'clim-constant-p (pkg-symbols :clim)))
                (:common-lisp (remove-if #'special-operator-p (pkg-symbols :cl)))
                (t (pkg-symbols pkg))) 
              when (manifest::is sym what) collect sym) #'nsort:nstring<))

(defun symbol-groups (pkg)
  "group symbols into manifest::*categories*"
  (loop for what in manifest::*categories*
        for category = (sorted-symbols-in-a-category pkg what)
        when category collect (cons (#~s'$':' (princ-to-string what)) category)))

(defun sym-gr (p)
  (case p 
    (:clim (substitute (mapcar 'cw::sym2stg (clim-constants)) '("constant:" NIL) (mapcar 'pack (mapcar 'cw::sym2stg (symbol-groups p))) :test 'equal))
    (:common-lisp (cons (pack (mapcar 'cw::sym2stg (spec-op))) (mapcar 'pack (mapcar 'cw::sym2stg (symbol-groups p)))))
    (t (mapcar 'pack (mapcar 'cw::sym2stg (symbol-groups p))))))

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

;---------------------------
(defun pkg-tree (p) (remove-empty-bags (cons (symbol-name p) (insert-what (sym-gr p)))))

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
; -2) create a package or system menu-list -- with submenus: com. cl- asdf/ ...
;--------------------------------------------------------
(defun create-menu (l)
  "turn a list into a sorted numbered list"
  (create-menu% (remove-empty-bags (pack l))))

(defun create-menu% (l &aux (n 0))
  "insert :items and :value into a tree to create a clim-menu"
    (mapcar (lambda (x)
              (if (atom x)
                (list (lol:mkstr (incf n) #\space x) :value x)
                (cons (lol:mkstr (incf n) #\space (car x)) (cons :items (list (create-menu% (cdr x)))))))
            l))

;---------------------------
(defun print-numbered-pkg (item strm)
  (if (#~m'[-./]$' (car item))
    (with-drawing-options (strm :ink +red+ :text-face :bold) (princ (string-downcase (car item)) strm))
    (princ (string-downcase (car item)) strm)))

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

;-------------------------------------------------------------------
;   (defun disp-info (f p) 
;     (let* ((pkg (cw:item-name (cw:group *application-frame*)))
;            (inf-ap-fr (info *application-frame*))
;            (sym (find-symbol (string-upcase inf-ap-fr) (string-upcase pkg))))
;   ;#|-test-
;       (format p "~&---info app-frame: ~s" inf-ap-fr)
;       (format p "~&---pkg: ~s" pkg)
;       (format p "~&---sym: ~s" sym)
;       ;insert path <---
;       (format p "~&---path: ~s~2%" (ql-dist:find-asdf-system-file (alexandria:make-keyword pkg)))
;   ;|#
;       (flet ((doc-stg (f)
;                (with-drawing-options (p :text-face :bold) (format p "~2%Documentation String:~%"))
;                (princ (or (manifest::docs-for sym f) "no-doc-string") p)))
;         (dolist (what manifest::*categories*)
;           (when (manifest::is sym what) 
;             (cond 
;               ((string= inf-ap-fr pkg) 
;                (color-info p
;                  (length (pkg-symbols pkg))
;                  #+quicklisp(or (car (asdf-description (ql-dist:find-asdf-system-file (alexandria:make-keyword pkg)))))
;                  #+quicklisp(or (cadr (asdf-description (ql-dist:find-asdf-system-file (alexandria:make-keyword pkg)))))
;                  (manifest::readme-text (alexandria:make-keyword pkg))))
;               ((member what '(:function :macro :generic-function :slot-accessor)) 
;                (with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~2%Argument List:~%" pkg sym))  ; pkg to upper-case
;                (color-lambda p (repl-utilities:arglist sym))
;                (unless (null sym) (doc-stg what)))
;               ((member what '(:variable :class :constant :condition)) 
;   ;(with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~%~a" pkg sym what))   ; <----- 13.3.17  ev überlegen
;   ;(format p "~a" what)
;                (unless (null sym) (doc-stg what)))
;               (t "there could be other documantation??")))))))

;======================================================================
;-to edit ----------------------------------------------------------
; pkg vs system ??
(defun disp-info (f p) 
  (let* ((pkg (alexandria:make-keyword (cw:item-name (cw:group *application-frame*))))
         (file (ql-dist:find-asdf-system-file pkg))   ; brauchts nicht mehr
         (sys (case pkg
                (:ALEXANDRIA.0.DEV :alexandria)
                (t pkg)))

         (inf-ap-fr (info *application-frame*))
         (sym (find-symbol (string-upcase inf-ap-fr) pkg)))
;#|-test-
    (format p "~&---info app-frame: ~s" inf-ap-fr)
    (format p "~&---package: ~s" pkg)
    (format p "~&---system: ~s" sys)
    (format p "~&---symbol: ~s" sym)
    (format p "~&---path: ~s~2%" file)
    ;(print (cw:group *application-frame*) p)
;|#
    (flet ((doc-stg (f)
             (with-drawing-options (p :text-face :bold) (format p "~2%Documentation String:~%"))
             (princ (or (manifest::docs-for sym f) "no-doc-string") p)))
      (dolist (what manifest::*categories*)
        (when (manifest::is sym what) 
          (cond 
;            ((string= inf-ap-fr pkg) 
            ((string= inf-ap-fr pkg) (color-info p sys))


;(or (string= "x" :x) (string= "x" "x"))
;            ((or (string= inf-ap-fr pkg) (string= inf-ap-fr (car (package-nicknames pkg))))

;            ((#~m/inf-ap-fr/ (lol:mkstr pkg))
;             (color-info p sys)
#|
               (length (pkg-symbols pkg))
               (car (asdf-description pkg))
               (cadr (asdf-description pkg))
              (manifest::readme-text pkg)))
|#         
            ((member what '(:function :macro :generic-function :slot-accessor)) 
             (with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~2%Argument List:~%" pkg sym))  ; pkg to upper-case
             (color-lambda p (repl-utilities:arglist sym))
             (unless (null sym) (doc-stg what)))
            ((member what '(:variable :class :constant :condition)) 
;(with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~%~a" pkg sym what))   ; <----- 13.3.17  ev überlegen
;(format p "~a" what)
             (unless (null sym) (doc-stg what)))
            (t "there could be other documantation??")))))))

;;;;;;;;;;;;;;;;;;;;;
(defun disp-info (f p) 
  (let* ((pkg (alexandria:make-keyword (cw:item-name (cw:group *application-frame*))))
         (sys (case pkg ; vorerst so
                (:ALEXANDRIA.0.DEV :alexandria)   ; pkg :ALEXANDRIA.0.DEV --  system :alexandria -- pkg-nicknames :alexandria
                (:jpeg :cl-jpeg)
                (t pkg)))


         (file-ql (ignore-errors (ql-dist:find-asdf-system-file sys)))   ; brauchts nicht mehr
         (file.asd (asdf:system-relative-pathname sys sys :type "asd"))
         (file-asdf (ignore-errors (ASDF/SYSTEM:system-source-directory sys)))

         (inf-ap-fr (info *application-frame*))
         (sym (find-symbol (string-upcase inf-ap-fr) pkg)))
;#|-test-
    (format p "~&---info app-frame: ~s" inf-ap-fr)
    (format p "~&---package: ~s" pkg)
    (format p "~&---system: ~s" sys)
    (format p "~&---symbol: ~s" sym)
    (format p "~&---path-q: ~s" file-ql)
    (format p "~&---path-a: ~s" file.asd)
    (format p "~&---path-a: ~s~2%" file-asdf)



    ;(print (cw:group *application-frame*) p)
;|#
    (flet ((doc-stg (f)
             (with-drawing-options (p :text-face :bold) (format p "~2%Documentation String:~%"))
             (princ (or (manifest::docs-for sym f) "no-doc-string") p)))
      (dolist (what manifest::*categories*)
        (when (manifest::is sym what) 
          (cond 
            ((string= inf-ap-fr pkg) (color-info p sys))
            ((member what '(:function :macro :generic-function :slot-accessor)) 
             (with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~2%Argument List:~%" pkg sym))  ; pkg to upper-case
             (color-lambda p (repl-utilities:arglist sym))
             (unless (null sym) (doc-stg what)))
            ((member what '(:variable :class :constant :condition)) 
;(with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~%~a" pkg sym what))   ; <----- 13.3.17  ev überlegen
;(format p "~a" what)
             (unless (null sym) (doc-stg what)))
            (t "there could be other documantation??")))))))


(defun tview (tree key)
  (cw-utils::t2h-r tree)
  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'string 'pkg-doc :right 800))

;;; commands --------------------------------------
(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

(defmacro select-pkg (system-category)
  `(let ((sys (menu-choose (create-menu ,system-category) :printer 'print-numbered-pkg :n-columns 3)))
     #+quicklisp(unless (find-package (string-upcase sys)) (ql:quickload sys))
     (clrhash cw:nodes)
     (cw-utils::t2h-r (pkg-tree (alexandria:make-keyword (string-upcase sys))))
     (with-application-frame (f) 
       (setf (cw:group f) (make-instance 'node-pkg :sup (string-upcase sys) :disp-inf t)) (redisplay-frame-panes f :force-p t))))

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

(define-pkg-doc-command (features :menu t) ()
   ;(setf (info *application-frame*) (print (o:lststg (o:group (sort *features* 'string<) 10)))))
   (setf (info *application-frame*) (format t "~{~&  ~a~}" (sort *features* 'string<))))

;--------------------------------------------------------
; 8) main
;--------------------------------------------------------
(defun pkg-doc (&optional (pkg :clim)) 
 (tview  (pkg-tree pkg) (package-name pkg)))

(defun pd () (clim-sys:make-process #'pkg-doc))
