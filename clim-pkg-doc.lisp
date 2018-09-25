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
; TODO: 
;--------------------------------------------------------
; with- problem
; nil problem hierachical-category, cw:sym2stg
; http://bauhh.dyndns.org:8000/clim-spec/index.html  - ev link ??

;--------------------------------------------------------
; 0) CONFIGURE
;--------------------------------------------------------
;1) adapt local-libs - optionally add a directory to quicklisp/local-projects??, ev append a list of dirs, ev config.lisp
(defvar my-project-dir #P"~/src/lisp/") ; my-libs ??, export function?

;--------------------------------------------------------
; 1) SYSTEM DESCRIPTION
;--------------------------------------------------------
(defun pkg-description (s pkg)
  "system description"
  (let ((nr (length (pkg-symbols pkg)))
        (a1 (car (asdf-description pkg)))
        (a2 (cadr (asdf-description pkg)))
        (a3 (readme-text pkg)))
    (format s "Nickname: ~{~a~}~%" (package-nicknames pkg))
    (with-drawing-options (s :ink +red+) (format s "~a " nr)) (format s "external-symbols~%")
    (with-drawing-options (s :ink +red+ :text-face :bold) (format s 
"-------------------------
 ASDF Description
-------------------------"))
  (with-drawing-options (s :text-face :bold) (format s "~&SHORT: ")) (format s "~a" a1)
  (with-drawing-options (s :text-face :bold) (format s "~2&LONG: ")) (format s "~a~%" a2)
  (with-drawing-options (s :text-face :bold :ink +red+) (format s 
"-------------------------
 README
-------------------------"))
  (format s "~&~a" a3)))

;mit match (a b ...)
(defun asdf-description (sys)
  (let ((x (asdf/system:find-system (pkg2sys sys))))
    (list (asdf/system:system-description x) (asdf/system:system-long-description x))))

#| 
;EXAMPLES OF OTHER DOC FILES:   - 1) show with pdf-viewer, 2) display pdf in clim, 3) pdf2txt ??
sequence-iterators-20130813-darcs/doc/sequence-iterators.html
iterate-20180228-git/doc/tex/iterate-manual.pdf
|#
(defmacro readme-file (sys)
  "Look for a system's documentation file"
  `(or ,@(loop for x in '("README" "README.txt" "README.md" "README.markdown" "README.org" 
                          "doc/README" "doc/index.html" "docs/index.html")
               collect `(probe-file (asdf:system-relative-pathname (pkg2sys ,sys) ,x)))))

#|
;to test
(defun readme-file (sys)
  (loop for x in '("README" "README.txt" "README.md" "README.markdown" "README.org" 
                   "doc/README" "doc/index.html" "docs/index.html")
        while x do (ignore-errors (probe-file (asdf:system-relative-pathname (pkg2sys sys) x)))))
|#

(defun readme-text (p)
  "Get text from the system's docfile. If doc is html strip the tags"
  (let ((sys (pkg2sys p)))
    (or (ignore-errors
          (pre:match (file-namestring (readme-file sys))
            (#~m'html$' (strip-html (alexandria:read-file-into-string (readme-file sys))))
            (t (alexandria:read-file-into-string (readme-file sys)))))
        "No System Info?")))

(let ((sys-pkg '(("mcclim" . "CLIM")
                 ("alexandria" . "ALEXANDRIA.0.DEV")
                 ("cl-jpeg" . "JPEG"))))
  (defun pkg2sys (p) (or (car (rassoc p sys-pkg :test 'equal)) (string-downcase p)))
  (defun sys2pkg (p) (or (cdr (assoc p sys-pkg :test 'equal)) (string-upcase p))))

(defun strip-html (s) (#~s'<.*?>''gs s))

;--------------------------------------------------------
; 2) SYMBOL-TREE
;--------------------------------------------------------
; ev post-edit pkg-tree with css-selectors??
(defun pkg-tree (p) (cons (package-name p) (insert-what (symbol-groups p))))

;;; Hierarchy by symbolname ;;;

; (parts 'a-b-c) -> ("a-" "b-" "c")
(defun parts (x) (#~d'(?<=-)' x))

; (key 'a-b-c) -> "A-" ; (key 'a-b-c 1) -> "A-B-"
(defun key (s &optional (i 0))
 (#~s' ''g (stdutils:list-to-delimited-string (reverse (key% s i))))) 

;(key% 'a-b-c 1) -> (B- A-)
(defun key% (s i)
  "key ~ header"
  (cond ((zerop i) (list (nth i (parts s))))
        (t (cons (nth i (parts s)) (key% s (1- i))))))

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
;geht richtig!!!, 30.4.17
(defun pack (l &optional (i 0) v)
  (cond ((null l) (if (= 1 (length v)) v (list (cons (key (car v) i) (pack (reverse v) (1+ i))))))
        ((null v) (pack (cdr l) i (list (car l))))
        ((equal (key (car v) i) (key (car l) i)) (pack (cdr l) i (push (car l) v)))
        (t (cons (if (= 1 (length v))
                   (car v)
                   (cons (key (car v) i) (pack (reverse v) (1+ i))))
                 (pack (cdr l) i (list (car l)))))))
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

;--------------------------------------------
(defun pkg-symbols (pkg) (loop for s being the external-symbols of pkg collect s))

(defun sorted-symbols-in-a-category (pkg what)
  "return a sorted list of all symbols in a category"
  (sort (loop for sym in (pkg-symbols pkg) 
              when (manifest::is sym what) collect sym) #'nsort:nstring<))

#|
(defun hierarchical-category (l) ;package category
  (hierarchy-by-symbolname
    (cw:sym2stg l)))
|#

;;; simple hack: (cw:sym2stg '(a b nil t)) ; ("a" "b" NIL "t") 
; diese NIL stört constants in clim und cl, so fehlt nil in beiden, geleg zu richten
(defun hierarchical-category (l) ;package category
  (remove nil
  (hierarchy-by-symbolname
    (cw:sym2stg l))))

;------------------------------------------
(in-package manifest)
;------------------------------------------
(manifest::define-category :SPECIAL-OPERATOR (symbol what)
  (:is (special-operator-p symbol)))

(manifest::define-category :CLIM-COLOR (symbol what)
  (:is (clim-color-p symbol)))

#|
;; clim colors +cyan+ are in clim variables, remove them <----

; manifest definitions ev edit ??
(define-category :variable (symbol what)
  (:is (and (variable-p symbol) (not (is symbol :constant))))
  (:docs   (documentation symbol 'variable)))

(define-category :constant (symbol what)
  (:is (and (variable-p symbol) (constantp symbol)))
  (:docs (documentation symbol 'variable)))

(defun variable-p (name)
    (ignore-errors (boundp name)))
|#
(define-category :constant (symbol what)
  (:is (constantp symbol))
  (:docs (documentation symbol 'variable)))

(defun clim-color-p (x)
  (and (member (#~s'\+''g (symbol-name x)) clim-internals::*xpm-x11-colors* :test 'equalp :key 'fourth)
       (#~m'^\+.+\+$' (symbol-name x))))
;------------------------------------------
(in-package clim-pkg-doc)
;------------------------------------------

(defun symbol-groups (pkg)
  "group symbols into manifest::*categories*"
  (loop for what in (case pkg 
                      (:common-lisp  (cons :SPECIAL-OPERATOR manifest::*categories*))
                      (:clim (append manifest::*categories* '(:CLIM-COLOR)))
                      (t manifest::*categories*))
        for category = (sorted-symbols-in-a-category pkg what)
        when category collect (cons (#~s'$':' (string-downcase (princ-to-string what))) 
                                    (hierarchical-category category))))

(defun insert-what (l)
   (mapcar 'insert-what%% l))

;;;(insert-what% 'macro- 'abc-def-g)    <-- dzt error:
;(clim-pkg-doc::insert-what% 'macro- "abc-def-g") -> "MACRO-abc-def-g"
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

;--------------------------------------------------------
; 3) GUI
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

(defun disp-info (f p) 
  (declare (ignore f))
  (let* ((pkg (cw:item-name (cw:group *application-frame*)))
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
             (with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~2%Argument List:~%" pkg sym))
             (color-lambda p (repl-utilities:arglist sym))
             (unless (null sym) (doc-stg what)))
            ((member what '(:variable :class :constant :condition)) 
             (unless (null sym) (doc-stg what)))
            (t "there could be other documantation??")))))))

(defun color-lambda (s l)
  "color lambda list"
  (mapc (lambda (x)
          (if (#~m'^&' x)
            (with-drawing-options (s :ink +red+ :text-face :bold) (format s "~(~a~)" x))
            (format s "~(~a~)" x)))
        (ppcre:split "(&[^ )]+)" (princ-to-string l) :with-registers-p t)))

#|
(defun color-lambda (s l)
  "color lambda list"
  (mapc (lambda (x)
          (if (#~m'^&' x)
            (with-drawing-options (s :ink +red+ :text-face :bold) (format s "~(~a~)" x))
            (format s "~(~a~)" x)))
        ;(ppcre:split "(&[^ )]+)" (princ-to-string l) :with-registers-p t)))
        (#~d'(&[^ )]+)'r (princ-to-string l)))   ; macht end of line error  <-----
|#

(defun tview (tree key)
  (cw-utils::t2h-r tree)
  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'string 'pkg-doc :right 800))

;--------------------------------------------------------
; 4) GUI COMMANDS
;--------------------------------------------------------
(define-pkg-doc-command (packages :menu t) ()
  (select-pkg (current-packages)))

#+quicklisp
(define-pkg-doc-command (quicklisp :menu t) ()
  (select-pkg (quicklisp-systems)))

#+quicklisp
(define-pkg-doc-command (local-projects :menu "LocalLibs") ()
  (select-pkg (local-systems)))

; style warning; The variable PKG is defined but never used
(defun select-pkg (system-category)
  (let ((pkg (string-upcase (menu-choose (create-menu system-category) 
                                         :printer 'print-numbered-pkg :n-columns 5))))
     #+quicklisp(load-package pkg)))

; style warning: The variable SYS is defined but never used
(defun load-package (p) 
  (let ((pkg (sys2pkg p))
        (sys (pkg2sys p)))
    (and (or (find-package pkg) #+quicklisp(ql:quickload sys)) 
         (create-tview  pkg))))

(defun create-tview (pkg)
  (cw-utils::t2h-r (pkg-tree pkg))
  (with-application-frame (f) 
    (setf (cw:group f) (make-instance 'node-pkg :sup (package-name pkg) :disp-inf t)) 
    (redisplay-frame-panes f :force-p t)))

;==============================================================
; create hierarchical menu to choose a package or a system
;==============================================================
; 1) sorted lists of strings 
;---------------------------------------
;                     .. / systemname-.... /
;                        / cffi_0.19.0 /     und diverse andere
; #<QL-DIST:SYSTEM zsort / zsort-20120520-git / quicklisp 2015-06-08>) 
(defun ql-system-name (sys) 
  (#~s'(-|_)[^-_]+?(-git|-darcs|-svn|-http|-hg)?$'' 
   (second (#~d' / ' (princ-to-string sys))))) ; ev ql:system-name

; ("ABC" ...)
(defun current-packages ()  
  (cons "common-lisp" 
        (sort 
          (remove-if-not 
            (lambda (x) (ignore-errors (asdf:find-system (pkg2sys x)))) 
            (mapcar 'package-name (list-all-packages)))
          'string<)))

; ("abc" ...)
#+quicklisp
(defun quicklisp-systems () 
  (sort (remove-duplicates (mapcar 'ql-system-name (ql:system-list)) :test 'string=) 
        'string<))

; ("abc" ...)
#+quicklisp
(defun local-systems ()
  (if (probe-file my-project-dir) (push my-project-dir ql:*local-project-directories*))
  (sort (ql:list-local-systems) 'string<))

; 2) create hierarchical menu to choose a package or a system. 
;    Hierarchy by symbol-name: com. cl- asdf/ ...
;----------------------------------------------------------------------------------------
(defun create-menu (l)
  "turn a list into a sorted numbered list"
  (create-menu% (hierarchy-by-symbolname l)))

(defun create-menu% (l &aux (n 0))
  "insert :items and :value into a tree to create a clim-menu"
    (mapcar (lambda (x)
              (if (atom x)
                (list (lol:mkstr (incf n) #\space x) :value x)
                (prog1 (cons (lol:mkstr  #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (setf n (1- (+ n (length x))))))) ; geht ~gut
;                (prog1 (cons (lol:mkstr (text-style-width n)??  #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (setf n (1- (+ n (length x))))))) 
            l))

(defun print-numbered-pkg (item strm)
  (if (#~m'[-./]$' (car item))
    (with-drawing-options (strm :ink +red+ :text-face :bold) 
      (princ (string-downcase (car item)) strm))
    (princ (string-downcase (car item)) strm)))

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

(define-pkg-doc-command (cl-apropos :menu t) () ; common-lisp apropos
  (setf (info *application-frame*) 
        (apropos (accept 'string) (accept 'string :default nil) 'external-only)))

#+quicklisp
(define-pkg-doc-command (ql-apropos :menu t) () ; quicklisp apropos
  (setf (info *application-frame*) (ql:system-apropos (accept 'string))))

(define-pkg-doc-command (help :menu t) ()
  (with-drawing-options (t :ink +blue+) (princ *help* *standard-output*)))

(define-pkg-doc-command (clear :menu t) ()
  (window-clear *standard-input*))

(define-pkg-doc-command (features :menu t) ()
  (setf (info *application-frame*) 
  (with-drawing-options (t :ink +red+) (format t "~{~&  ~a~}" (sort *features* 'string<)))))

(define-pkg-doc-command (modules :menu t) ()
  (setf (info *application-frame*) (format t "~{~&  ~a~}" (sort *modules* 'string<))))

(defvar *help* "Help:
Click the root node to see a package's description or readme-file
-----------------------------------------------------------------
APROPOS: 
1) on the first prompt type a string
2) on the second promt press enter or type the name of a package
-----------------------------------------------------------------
CONFIGURE-POSSIBILITIES:
1) adapt local-libs
;;2) (setf clim-pkg-doc::*st* :a)  ;to change the symbol-type  :e external(default) :p resent :a available ???
;;3) all symbols alfabetically ??
-----------------------------------------------------------------
")

;--------------------------------------------------------
; 5) MAIN
;--------------------------------------------------------
(defun pkg-doc (&optional (pkg "CLIM")) 
 (tview  (pkg-tree pkg) pkg))

(defun pd () (clim-sys:make-process #'pkg-doc))
