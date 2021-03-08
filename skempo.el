;;; skempo.el --- Enhancements for skeleton/tempo + abbrev -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
;; Keywords: convenience
;; Version: 0.1.0
;; URL: https://github.com/xFA25E/skempo
;; Package-Requires: ((emacs "25.1") (parent-mode "2.3"))

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

;; This is an attempt to fuse skeleton, tempo and abbrev.  Emacs has
;; excellent support for templates, but they are not 100% featureful.
;; Tempo has a good "jumping after expansion" and text completion
;; mechanisms.  Skeleton has a good support for recursive templates
;; and abbrev.  This package tries to balance each-other weaknesses by
;; giving a tempo-jumping and completion capability for skeleton and
;; abbrev support for tempo.
;;
;; The main symbols of interests are: skempo-mode, skempo-advice-mode,
;; skempo-complete-tag-or-call-on-region, skempo-forward-mark,
;; skempo-backward-mark, skempo-define-tempo, skempo-define-skeleton
;; and skempo-define-function.  See their docstrings.
;;
;; This package also cleans up some old tempo's and skeleton's code
;; with skempo-advice-mode.

;;; Code:


;;;; REQUIRES

(require 'seq)
(require 'subr-x)
(require 'derived)
(require 'parent-mode)
(require 'mode-local)
(require 'tempo)
(require 'skeleton)


;;;; VARIABLES

(defvar skempo-mode-map (make-sparse-keymap)
  "Map used in `SKEMPO-MODE'.")


;;;; FUNCTIONS

(defalias 'skempo-forward-mark 'tempo-forward-mark)
(defalias 'skempo-backward-mark 'tempo-backward-mark)

(defun skempo--completion-names-for-tag-list (tag-list)
  "Get completion names for templates from `TAG-LIST'."
  (seq-map
   (pcase-lambda (`(,tag . ,fn)) (format "%s (%s)" tag fn))
   tag-list))

(defun skempo--call-template-by-name (name)
  "Expand template by `NAME'.
`NAME' is in a form \"tag (function)\".  Parse a function and
call it."
  (when (string-match (rx "(" (group (*? any)) ")" eos) name)
    (funcall (intern (match-string 1 name)))))


(defun skempo--complete-template (string tag-list)
  "An :override advice function for `TEMPO-DISPLAY-COMPLETIONS'.
Show completion for `STRING' in a `TAG-LIST'.  After selection
expand template.

Rewritten because the original function uses an old way of
displaying completions in a separate buffer, which does not work
anyway.  Now it uses new (compared to the originial tempo
package) and shiny `COMPLETING-READ' interface."
  (let* ((tag-list-names (skempo--completion-names-for-tag-list tag-list))
         (name (completing-read "Skempo: " tag-list-names nil t string)))
    (delete-char (- (length string)))
    (skempo--call-template-by-name name)))

(defun skempo--tags-variable (mode)
  "Return a tempo tags variable's symbol for `MODE'."
  (when mode
    (intern (replace-regexp-in-string
             (rx "-mode" eos) "-skempo-tags"
             (symbol-name mode)))))

(defun skempo--remove-tag-list (tag-list)
  "Remove `TAG-LIST' from `TEMPO-LOCAL-TAGS'."
  (setf (alist-get tag-list tempo-local-tags nil t) nil))

(defun skempo--insert-mark (marker)
  "Insert a `MARKER' to `tempo-marks' while keeping it sorted.
Remove duplicate marks from `TEMPO-MARKS'.  Set to nil removed
markers.  This function is used as an :override advice to
`TEMPO-INSERT-MARK', because the original function does not
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
  "Add `SKELETON-POSITION' positions to `TEMPO-MARKS'."
  (seq-doseq (position skeleton-positions)
    (tempo-insert-mark (set-marker (make-marker) position))))

(defun skempo--add-tag (tag template &optional tag-list)
  "Add a `TEMPLATE' `TAG' to `TAG-LIST' or to `TEMPO-TAGS'.
It is an :override function for `TEMPO-ADD-TAG'.  The original
function does not update identical tags."
  (interactive "sTag: \nCTemplate: ")
  (let ((tag-list (or tag-list 'tempo-tags)))
    (if-let ((value (assoc tag (symbol-value tag-list))))
        (setcdr value template)
      (set tag-list (cons (cons tag template) (symbol-value tag-list)))))
  (tempo-invalidate-collection))


;;;; COMMANDS

;;;###autoload
(defun skempo-complete-tag-or-call-on-region ()
  "Complete skempo tag or provide completion for a template around region.
If the region is active, then provide a completion for a skempo
template.  Otherwise, try to complete with `tempo-comelete-tag'.
If it fails to find a suitable tag, provide template completion."
  (interactive)
  (if (use-region-p)
      (tempo-display-completions "" (tempo-build-collection))
    (unless (tempo-complete-tag)
      (tempo-display-completions "" (tempo-build-collection)))))

;;;###autoload
(define-minor-mode skempo-advice-mode
  "Override some tempo function and add skeleton hook.
New functions enhance default tempo functions without changing
their functionality.  Skeleton hook adds support for
tempo-marks."

  nil "" nil :global t
  (if skempo-advice-mode
      (progn
        (advice-add 'tempo-display-completions :override #'skempo--complete-template)
        (advice-add 'tempo-insert-mark :override #'skempo--insert-mark)
        (advice-add 'tempo-add-tag :override #'skempo--add-tag)
        (add-hook 'skeleton-end-hook #'skempo--add-skeleton-markers))
    (advice-remove 'tempo-display-completions #'skempo--complete-template)
    (advice-remove 'tempo-insert-mark #'skempo--insert-mark)
    (advice-remove 'tempo-add-tag #'skempo--add-tag)
    (remove-hook 'skeleton-end-hook #'skempo--add-skeleton-markers)))

;;;###autoload
(define-minor-mode skempo-mode
  "Minor mode for skempo-templates.
It helps initializing templates for a certain mode and provides a convinient
macro for template definition.  Also, it provides some tempo enhancements for
completion."
  nil " Skempo" skempo-mode-map
  (if skempo-mode
      (thread-last (parent-mode-list major-mode)
        (seq-map #'skempo--tags-variable)
        (seq-filter #'boundp)
        (seq-do #'tempo-use-tag-list))
    (thread-last (parent-mode-list major-mode)
      (seq-map #'skempo--tags-variable)
      (seq-filter #'boundp)
      (seq-do #'skempo--remove-tag-list))))


;;;; MACROS

(defun skempo--modes (mode)
  "Normalize `MODE' argument."
  (cond ((consp mode) mode)
        (mode (list mode))
        (t '(nil))))

(defun skempo--mode-prefix (mode)
  "Return `MODE' name or empty string in NIL."
  (if mode
      (string-trim-right (symbol-name mode) (rx "mode" eos))
    ""))

(defun skempo--abbrev-table (mode)
  "Get abbrev table for `MODE' or `GLOBAL-ABBREV-TABLE' if NIL."
  (if mode
      (derived-mode-abbrev-table-name mode)
    'global-abbrev-table))

(defmacro skempo--common-form (form)
  "Common form for skempo definition macros.
Insert `FORM' in the common parts."
  `(cl-destructuring-bind (name &key tag abbrev docstring mode &aux
                                (docstring (or docstring ""))
                                (name (symbol-name name))
                                (modes (skempo--modes mode)))
       (if (consp name-and-args) name-and-args (list name-and-args))
     `(progn
        ,@(mapcar

           (lambda (mode)
             (let ((mode-prefix (skempo--mode-prefix mode))
                   (tags-var (skempo--tags-variable mode))
                   (abbrev-table (skempo--abbrev-table mode))
                   (template-symbol (gensym "template-symbol")))
               `(progn
                  ,(when (and tag mode)
                     `(defvar ,tags-var nil))

                  ,(when (and abbrev mode)
                     `(define-abbrev-table ',abbrev-table nil))

                  (let ((,template-symbol ,,form))
                    (put ,template-symbol 'no-self-insert t)
                    ,(when tag
                       `(tempo-add-tag ,name ,template-symbol ',tags-var))
                    ,(when abbrev
                       `(define-abbrev ,abbrev-table ,name "" ,template-symbol :system t)))

                  (dolist (buffer (buffer-list))
                    (with-current-buffer buffer
                      (when (and ,(if mode `(derived-mode-p ',mode) t) skempo-mode)
                        (skempo-mode -1)
                        (skempo-mode 1)))))))

           modes))))

;;;###autoload
(defmacro skempo-define-tempo (name-and-args &rest body)
  "Define a tempo template.
This macro defines a new tempo template or updates the old one.
`NAME-AND-ARGS' may be a symbol or a list.  If it is a list it
may take the form (`NAME' [KEY VALUE]...) where each KEY can be
one of `:tag', `:abbrev', `:docstring' or `:mode'.

If `NAME-AND-ARGS' is a symbol then it is equivalent to `NAME'.

If KEY is `:tag', VALUE should be a boolean.  If VALUE is
NON-NIL, then a tempo tag with `NAME' will be created for this
template.

If KEY is `:abbrev', VALUE should be a boolean.  If VALUE is
NON-NIL, then a `NAME' abbrev will be created for this template.

If KEY is `:docstring', VALUE should be a string.

If KEY is `:mode', VALUE should be a list of modes or single
mode.  If this option is provided, than a tempo tag and an abbrev
will be created for these modes, otherwise they will be
global (if `:tag' and `:abbrev' options were provided, of
course).

`BODY' is a sequence of tempo elements that will be passed as a
list directly to `TEMPO-DEFINE-TEMPLATE's second argument.

Example:
\(skempo-define-tempo (defvar :tag t :abbrev t
                              :mode emacs-lisp-mode
                              :docstring \"defvar template\")
  \"(defvar \" (string-trim-right (buffer-name) (rx \".el\" eos)) \"-\" p n>
  r> \")\")"
  (skempo--common-form
   `(tempo-define-template ,(concat mode-prefix name) ',body
                           nil ,docstring nil)))

;;;###autoload
(defmacro skempo-define-skeleton (name-and-args &rest body)
  "Define skeleton template.
See `SKEMPO-DEFINE-TEMPO' for explanation of `NAME-AND-ARGS'.

`BODY' is a sequence of skeleton elements that will be passed
directly to `DEFINE-SKELETON'.

Example:
\(skempo-define-skeleton (defun :tag t :abbrev t
                               :mode (emacs-lisp-mode lisp-interaction-mode)
                               :docstring \"defun template\")
  \"(defun \" str \" (\" @ - \")\" \n
  @ _ \")\" \n)"
  (let ((symbol (gensym "fn")))
    (skempo--common-form
     `(let ((,symbol (define-skeleton ,(intern (concat "skeleton-template-" mode-prefix name))
                       ,docstring ,@body)))
        (set ,symbol `((ignore (,,symbol))))
        ,symbol))))

;;;###autoload
(defmacro skempo-define-function (name-and-args function)
  "Define `FUNCTION' template.
See `SKEMPO-DEFINE-TEMPO' for explanation of `NAME-AND-ARGS'.  It ignores
`:docstring' option, for obvious reasons.

The main purpose of this macro, is to create tempo tags and
abbrevs for existing skeleton templates, such as `sh-case'.

Example:
\(skempo-define-function (shcase :tag t :mode `sh-mode')
  sh-case)"
  (let ((symbol (gensym "fn")))
    (skempo--common-form
     `(let ((,symbol ',function))
        (set ,symbol `((ignore (,,symbol))))
        ,symbol))))

;;;###autoload
(progn
  (put 'skempo-define-tempo 'lisp-indent-function 1)
  (put 'skempo-define-skeleton 'lisp-indent-function 1)
  (put 'skempo-define-function 'lisp-indent-function 1))


;;;; PROVIDE

(provide 'skempo)
;;; skempo.el ends here
