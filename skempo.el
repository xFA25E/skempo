;;; skempo.el --- Enhancements for skeleton/tempo + abbrev -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: abbrev, convenience
;; Version: 0.2.2
;; URL: https://github.com/xFA25E/skempo
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an attempt to fuse skeleton, tempo and abbrev.  Emacs has excellent
;; support for templates, but they are not 100% feature full.  Tempo has a good
;; "jumping after expansion" and text completion mechanisms.  Skeleton has a
;; good support for iterative templates and abbrev.  This package tries to
;; balance each-other weaknesses by giving a tempo-jumping and completion
;; capability for skeleton and abbrev support for tempo.

;; This package tries to preserve default behavior everywhere.  However, it
;; provides a bunch of custom options to enhance bulit-in functions.  See,
;; skempo customization group.

;; Every symbol has an exhaustive documentation string.

;; To define templates, use: skempo-define-tempo, skempo-define-skeleton and
;; skempo-define-function.

;; To use this package: enable skempo-mode in a buffer.  To expand tags, call
;; skempo-complete-tag-or-call-on-region.  To jump on points of interest, use:
;; skempo-forward-mark, skempo-backward-mark.

;; This package also provides skeleton-like conditional and iterative elements
;; to tempo.  Add skempo-tempo-user-elements to tempo-user-elements.  See
;; skempo-tempo-user-elements documentation to learn about these extensions.

;;; Code:

;;;; REQUIRES

(require 'cl-lib)
(require 'derived)
(require 'mode-local)
(require 'pcase)
(require 'rx)
(require 'skeleton)
(require 'subr-x)
(require 'tempo)

;;;; KEYMAP

(defvar skempo-mode-map (make-sparse-keymap)
  "Keymap for command `skempo-mode'.")

;;;; CUSTOMIZATION

(defgroup skempo nil
  "Skeleton+Tempo+Abbrev."
  :group 'abbrev
  :group 'tempo)

(defcustom skempo-mode-lighter " Skempo"
  "Lighter for command `skempo-mode'."
  :type '(string :tag "Lighter")
  :risky t
  :group 'skempo)

(defcustom skempo-completing-read nil
  "Override default `tempo-display-completions'.
By default it uses a completion buffer to show completions.  This
option overrides this function to use `completing-read' to select
partial skempo tag or complete tag on region.

If you wish to set this variable from ELisp code, you have to
remove `skempo--complete-template' advice from
`tempo-display-completions' on nil and add it as on :override
advice on non-nil."
  :type '(boolean :tag "Override?")
  :set (lambda (variable value)
         (if value
             (advice-add 'tempo-display-completions :override #'skempo--complete-template)
           (advice-remove 'tempo-display-completions #'skempo--complete-template))
         (set-default variable value))
  :group 'skempo)

(defcustom skempo-delete-duplicate-marks nil
  "Override default `tempo-insert-mark'.
Marks are used to jump on points of interest in a template.  By
default `tempo-insert-mark' does not remove duplicate marks.
Duplicate marks might appear when the buffer shrinks and some of
the marks start pointing to the same location.  This option tries
to fix this by checking for duplicate marks every time the
function is called.  Emacs might get slower with a lot of
marks.

If you want to set this option from ELisp, you have to remove
`skempo--insert-mark' advice from `tempo-insert-mark' on nil and
add it as on :override advice on non-nil."
  :type '(boolean :tag "Override?")
  :set (lambda (variable value)
         (if value
             (advice-add 'tempo-insert-mark :override #'skempo--insert-mark)
           (advice-remove 'tempo-insert-mark #'skempo--insert-mark))
         (set-default variable value))
  :group 'skempo)

(defcustom skempo-update-identical-tags nil
  "Override default `tempo-add-tag'.
By default this function does not update tag functions.  If you
want to set a new function to an existing tag, it will not work.
This option overrides this behavior by always updating tags.

If you want to set this option from ELisp, you have to remove
`skempo--add-tag' advice from `tempo-add-tag' on nil and add it
as on :override advice on non-nil."
  :type '(boolean :tag "Override?")
  :set (lambda (variable value)
         (if value
             (advice-add 'tempo-add-tag :override #'skempo--add-tag)
           (advice-remove 'tempo-add-tag #'skempo--add-tag))
         (set-default variable value))
  :group 'skempo)

(make-obsolete-variable
 'skempo-update-identical-tags
 "It is not required anymore, because bug was fixed."
 "Emacs 28")

(defcustom skempo-skeleton-marks-support nil
  "Add `tempo-marks' support for skeleton.
This option enables jumping on skeleton points of interest with
`skempo-forward-mark' and `skempo-backward-mark'.  It reuses
`tempo-marks' functionality.  By default, skeleton does nothing
with its points of interest.

If you want to set this option from ELisp, you have to remove
`skempo--add-skeleton-markers' from `skeleton-end-hook' on nil
and add it on non-nil."
  :type '(boolean :tag "Skeleton marks?")
  :set (lambda (variable value)
         (if value
             (add-hook 'skeleton-end-hook #'skempo--add-skeleton-markers)
           (remove-hook 'skeleton-end-hook #'skempo--add-skeleton-markers))
         (set-default variable value))
  :group 'skempo)

(defcustom skempo-enable-tempo-elements nil
  "Enable extra tempo elements.
These elements add conditionals and looping support for tempo
like those in skeleton, making skeleton pretty much obsolete.

If you want to set this option from ELisp, you have to remove
`skempo-tempo-user-elements' from `tempo-user-elements' on nil
and add it on non-nil."
  :type '(boolean :tag "Enable tempo elements?")
  :set (lambda (variable value)
         (if value
             (add-hook 'tempo-user-elements #'skempo-tempo-user-elements)
           (remove-hook 'tempo-user-elements #'skempo-tempo-user-elements))
         (set-default variable value))
  :group 'skempo)

;;;; FUNCTIONS

(defalias 'skempo-forward-mark 'tempo-forward-mark)
(defalias 'skempo-backward-mark 'tempo-backward-mark)

(defun skempo--tags-variable (mode)
  "Return a tempo tags variable's symbol for MODE."
  (when mode
    (intern (replace-regexp-in-string
             (rx "-mode" eos) "-skempo-tags"
             (symbol-name mode)))))

(defun skempo--remove-tag-list (tag-list)
  "Remove TAG-LIST from `tempo-local-tags'."
  (setf (alist-get tag-list tempo-local-tags nil t) nil))

(defun skempo--complete-template (string tag-list)
  "An :override advice function for `tempo-display-completions'.
Show completion for STRING in a TAG-LIST.  After selection
expand template.

Rewritten because the original function uses an old way of
displaying completions in a separate buffer, which is not
clickable anyway.  Now it uses new (compared to the originial
tempo package) and shiny `completing-read' interface."
  (let* ((tags (mapcar #'car tag-list))
         (tag (completing-read "Skempo: " tags nil t string)))
    (delete-char (- (length string)))
    (tempo-insert-template (cdr (assoc tag tag-list)) nil)))

(defun skempo--insert-mark (marker)
  "Insert a MARKER to `tempo-marks' while keeping it sorted.
Remove duplicate marks from `tempo-marks'.  Set to nil removed
markers.  This function is used as an :override advice to
`tempo-insert-mark', because the original function does not
remove duplicate elements.  Duplicate markers appear when the
buffer gets smaller, markers start pointing to the same location.
We don't want that, because a lot of useless markers can slow
down Emacs."
  (if (not tempo-marks)
      (setq tempo-marks (list marker))
    (let ((markers tempo-marks))
      (cond
       ((< marker (car markers))
        (setq tempo-marks (cons marker tempo-marks)))
       (t
        (while (and (cdr markers) (<= (cadr markers) marker))
          (if (/= (car markers) (cadr markers))
              (setq markers (cdr markers))
            (when (markerp (cadr markers)) (set-marker (cadr markers) nil))
            (setcdr markers (cddr markers))))

        (if (= marker (car markers))
            (when (markerp marker) (set-marker marker nil))
          (setcdr markers (cons marker (cdr markers))))))

      (while (cdr markers)
        (if (/= (car markers) (cadr markers))
            (setq markers (cdr markers))
          (when (markerp (cadr markers)) (set-marker (cadr markers) nil))
          (setcdr markers (cddr markers)))))))

(defun skempo--add-skeleton-markers ()
  "Add `skeleton-position' positions to `tempo-marks'."
  (dolist (position skeleton-positions)
    (tempo-insert-mark (set-marker (make-marker) position))))

(defun skempo--add-tag (tag template &optional tag-list)
  "Add a TEMPLATE TAG to TAG-LIST or to `tempo-tags'.
It is an :override function for `tempo-add-tag'.  The original
function does not update identical tags."
  (interactive "sTag: \nCTemplate: ")
  (let ((tag-list (or tag-list 'tempo-tags)))
    (if-let ((value (assoc tag (symbol-value tag-list))))
        (setcdr value template)
      (set tag-list (cons (cons tag template) (symbol-value tag-list))))
    (tempo-invalidate-collection)))

(defun skempo--list-derived-modes (mode)
  "List all derived modes of MODE + MODE itself."
  (let ((modes nil))
    (while mode
      (when-let ((alias (symbol-function mode)))
        (when (symbolp alias)
          (setq mode alias)))
      (push mode modes)
      (setq mode (get mode 'derived-mode-parent))  )
    (nreverse modes)))

;;;; COMMANDS

;;;###autoload
(defun skempo-complete-tag-or-call-on-region ()
  "Replacement for `tempo-complete-tag'.
Complete skempo tag or provide completion for a template around
region.  If the region is active, then provide a completion for a
skempo template.  Otherwise, try to complete with
`tempo-comelete-tag'.  If it fails to find a suitable tag,
provide template completion."
  (interactive)
  (if (use-region-p)
      (skempo--complete-template "" (tempo-build-collection))
    (tempo-complete-tag)))

;;;###autoload
(define-minor-mode skempo-mode
  "Minor mode for skempo-templates.
It helps initializing templates for a certain mode and provides a convinient
macro for template definition.  Also, it provides some tempo enhancements for
completion."
  :init-value nil
  :lighter skempo-mode-lighter
  :keymap skempo-mode-map
  (let* ((major-modes (skempo--list-derived-modes major-mode))
         (tag-vars (mapcar #'skempo--tags-variable major-modes))
         (bound-tag-vars (cl-delete-if-not #'boundp tag-vars)))
    (if skempo-mode
        (mapc #'tempo-use-tag-list bound-tag-vars)
      (mapc #'skempo--remove-tag-list bound-tag-vars))))

;;;; MACROS

(defun skempo--define-tempo (function-symbol body &optional docstring)
  "Define a tempo template with BODY.
This will generate a function with FUNCTION-SYMBOL and
DOCSTRING.

The main purpose of this function is to have a better controlled
alternative to `tempo-define-template'."
  (let ((template-symbol (gensym (symbol-name function-symbol))))
    (set template-symbol body)
    (defalias function-symbol
      (lambda (&optional arg)
        (interactive "*P")
        (tempo-insert-template template-symbol (xor tempo-insert-region arg)))
      docstring)))

(defun skempo--define-skeleton (function-symbol body &optional docstring)
  "Define a skeleton template with BODY.
This will generate a function with FUNCTION-SYMBOL and
DOCSTRING.

The main purpose of this function is to have a better controlled
alternative to `define-skeleton', especially because it is a
function instead of a macro."
  (defalias function-symbol
    (lambda (&optional str arg)
      (interactive "*P\nP")
      (skeleton-proxy-new body str arg))
    docstring))

(defun skempo--define-function (function-symbol function &optional docstring)
  "This will generate an alias to FUNCTION with FUNCTION-SYMBOL.
DOCSTRING is used as a docstring to FUNCTION-SYMBOL."
  (defalias function-symbol function docstring))

(defun skempo--mode-name (mode)
  "Get MODE name without a -mode suffix."
  (string-trim-right (symbol-name mode) (rx "-mode" eos)))

(defun skempo--function-name (name modes)
  "Generate a name for a skempo template function.
NAME and MODES are used to generate unique, but consistent
names."
  (concat "skempo-template-"
          (mapconcat (lambda (mode) (concat (skempo--mode-name mode) "-"))
                     (sort modes #'string<) "")
          name))

(defun skempo--mode-abbrev-table (mode)
  "Get abbrev table for MODE or `global-abbrev-table' if nil."
  (if mode
      (derived-mode-abbrev-table-name mode)
    'global-abbrev-table))

(defun skempo--abbrev-table (mode)
  "Get skempo abbrev table for MODE."
  (intern (concat "skempo-" (symbol-name (skempo--mode-abbrev-table mode)))))

(defun skempo--abbrev-table-names (table)
  "Return abbrev TABLE names."
  (let ((names nil))
    (mapatoms (lambda (abbrev)
                (when (symbol-value abbrev)
                  (push (symbol-name abbrev) names)))
              (symbol-value table))
    names))

(defun skempo--modes (mode)
  "Normalize MODE argument."
  (cond ((consp mode) mode)
        ((null mode) nil)
        ((symbolp mode) (list mode))))

;;;###autoload
(defun skempo-define (define-function name modes tag abbrev docstring body)
  "Define a skempo template.

DEFINE-FUNCTION is a function that takes a function symbol, BODY
and DOCSTRING as its arguments.  It must define a new function
with that symbol and that docstring.

NAME is a string used in generating a function symbol, TAG and
ABBREV.

MODES is a list of modes for which TAG and ABBREV will be
created.  If it's nil, TAG and ABBREV will be generated
globally.

TAG/ABBREV is a boolean, which indicates whether a tag/abbrev
must be created for this template.

DOCSTRING is a string (or nil) which will be supplied to
DEFINE-FUNCTION.

BODY is an arbitrary argument passed to DEFINE-FUNCTION."
  (let* ((function-symbol (intern (skempo--function-name name modes)))
         (modes (or modes '(nil))))
    (funcall define-function function-symbol body docstring)
    (put function-symbol 'no-self-insert t)

    (when tag
      (let ((tag-symbol (gensym (symbol-name function-symbol))))
        (if (eq #'skempo--define-tempo define-function)
            (set tag-symbol body)
          (set tag-symbol `((ignore (,function-symbol)))))
        (dolist (mode modes)
          (let ((var (skempo--tags-variable mode)))
            (unless (boundp var)
              (set var nil))
            (tempo-add-tag name tag-symbol var)))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (and (or (equal '(nil) modes) (apply #'derived-mode-p modes))
                       skempo-mode)
              (skempo-mode -1)
              (skempo-mode 1))))))

    (when abbrev
      (dolist (mode modes)
        (let ((mode-table (skempo--mode-abbrev-table mode))
              (table (skempo--abbrev-table mode)))
          (define-abbrev-table mode-table nil)
          (define-abbrev-table table nil :case-fixed t :skempo t)
          (define-abbrev (symbol-value table) name "" function-symbol
            :case-fixed t :system t :skempo t)

          (let* ((names (skempo--abbrev-table-names table))
                 (regexp (concat (regexp-opt names "\\_<\\(") " *")))
            (abbrev-table-put (symbol-value table) :regexp regexp))

          (let ((parents (abbrev-table-get (symbol-value mode-table) :parents)))
            (cl-pushnew (symbol-value table) parents :test #'eq)
            (abbrev-table-put (symbol-value mode-table) :parents parents)))))

    function-symbol))

;;;###autoload
(cl-defmacro skempo-define-tempo (name (&key mode tag abbrev docstring) &rest body)
  "Define a tempo template.
This macro defines a new tempo template or updates the old one.
NAME is a symbol.  ARGS is a list of the form ([KEY VALUE]...)
where each KEY can be one of :tag, :abbrev, :docstring or :mode.

If KEY is :tag, VALUE should be a boolean.  If VALUE is non-nil,
then a tempo tag with NAME will be created for this template.

If KEY is :abbrev, VALUE should be a boolean.  If VALUE is
non-nil, then a NAME abbrev will be created for this template.

If KEY is :docstring, VALUE should be a string.  It will be a
docstring of the generated function.

If KEY is :mode, VALUE should be a list of modes or single mode.
If this option is provided, than a tempo tag and an abbrev will
be created for these modes, otherwise they will be global (if
:tag and :abbrev options were provided, of course).

BODY is a sequence of tempo elements that will be passed as a
list directly to `tempo-define-template's second argument.

Example:
\(skempo-define-tempo defvar (:mode `emacs-lisp-mode' :tag t :abbrev t
                             :docstring \"defvar template\")
  \"(defvar \" (string-trim-right (buffer-name) (rx \".el\" eos)) \"-\" p n>
  r> \")\")"
  `(skempo-define #'skempo--define-tempo ,(symbol-name name)
                  ',(skempo--modes mode) ,tag ,abbrev ,docstring ',body))

;;;###autoload
(cl-defmacro skempo-define-skeleton (name (&key mode tag abbrev docstring) &rest body)
  "Define skeleton template.
See `skempo-define-tempo' for explanation of NAME, MODE, TAG,
ABBREV and DOCSTRING.

BODY is a sequence of skeleton elements that will be passed
directly to `define-skeleton'.

Example:
\(skempo-define-skeleton defun (:mode (emacs-lisp-mode `lisp-interaction-mode')
                               :tag t :abbrev t
                               :docstring \"defun template\")
  \"(defun \" str \" (\" @ - \")\" \n
  @ _ \")\" \n)"
  `(skempo-define #'skempo--define-skeleton ,(symbol-name name)
                  ',(skempo--modes mode) ,tag ,abbrev ,docstring ',body))

;;;###autoload
(cl-defmacro skempo-define-function (name (&key mode tag abbrev docstring) function)
  "Define FUNCTION template.
See `skempo-define-tempo' for explanation of NAME, MODE, TAG,
ABBREV and DOCSTRING.

The main purpose of this macro, is to create tempo tags and
abbrevs for existing skeleton templates, such as `sh-case'.

Example:
\(skempo-define-function shcase (:tag t :abbrev t :mode `sh-mode') `sh-case')"
  `(skempo-define #'skempo--define-function ,(symbol-name name)
                  ',(skempo--modes mode) ,tag ,abbrev ,docstring ',function))

;;;###autoload
(progn
  (put 'skempo-define-tempo 'lisp-indent-function 2)
  (put 'skempo-define-skeleton 'lisp-indent-function 2)
  (put 'skempo-define-function 'lisp-indent-function 2))

;;;; TEMPO ELEMENTS

(defvar skempo-tempo-else-key (kbd "C-M-g")
  "Key used to execute else branch in tempo conditional.")

(defun skempo-tempo--prompt (prompt)
  "Make prompt for tempo conditional.
PROMPT is preceded with `skempo-tempo-else-key'."
  (concat "(" (key-description skempo-tempo-else-key) " to quit) " prompt))

(defun skempo-tempo-user-elements (element)
  "Support for conditional and looping tempo elements.
The following forms are supported for ELEMENT:

\(:if (PROMPT VAR) THEN ELSE)

\(:when (PROMPT VAR) BODY...)

\(:while (PROMPT VAR) BODY...)

PROMPT is a string used to read value for VAR.  VAR is a tempo
variable symbol.  Its value can be read with s, as usual.  BODY,
THEN and ELSE are tempo elements.  To abort the execution of
these elements, user must press `skempo-tempo-else-key'.

The main purpose of this extension is to mimic skeleton
conditionals and iterative templats.  Skeleton becomes pretty
much obsolete with this extension."
  (pcase element
    (`(:if (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) ,then ,else)
     (let ((prompt (skempo-tempo--prompt prompt))
           (map (make-sparse-keymap)))
       (set-keymap-parent map minibuffer-local-map)
       (define-key map skempo-tempo-else-key
         (lambda () (interactive) (throw 'else else)))
       (catch 'else
         (tempo-save-named var (read-from-minibuffer prompt nil map))
         then)))
    (`(:when (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:if (,prompt ,var) (l ,@body) (l)))
    (`(:while (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:when (,prompt ,var) ,@body ,element))))

;;;; PROVIDE

(provide 'skempo)
;;; skempo.el ends here
