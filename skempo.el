;;; skempo.el --- Enhancements for skeleton/tempo + abbrev -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: abbrev, convenience
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

;;; Code:

;;;; REQUIRES

(require 'cl-lib)
(require 'derived)
(require 'mode-local)
(require 'parent-mode)
(require 'pcase)
(require 'rx)
(require 'skeleton)
(require 'subr-x)
(require 'tempo)

;;;; KEYMAP

(defvar skempo-mode-map (make-sparse-keymap)
  "Keymap for `SKEMPO-MODE'.")

;;;; CUSTOMIZATION

(defgroup skempo nil
  "Skeleton+Tempo+Abbrev."
  :group 'abbrev
  :group 'tempo)

(defcustom skempo-mode-lighter " Skempo"
  "Lighter for `SKEMPO-MODE'."
  :type '(string :tag "Lighter")
  :risky t
  :group 'skempo)

(defcustom skempo-completing-read nil
  "Override default `tempo-display-completions'.
By default it uses a completion buffer to show completions.  This
option overrides this function to use `completing-read' to select
partial skempo tag or complete tag on region.

If you wish to set this variable from ELisp code, you have to
remove `SKEMPO--COMPLETE-TEMPLATE' advice from
`tempo-display-completions' on NIL and add it as on :override
advice on NON-NIL."
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
`SKEMPO--INSERT-MARK' advice from `tempo-insert-mark' on NIL and
add it as on :override advice on NON-NIL."
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
`SKEMPO--ADD-TAG' advice from `tempo-add-tag' on NIL and add it
as on :override advice on NON-NIL."
  :type '(boolean :tag "Override?")
  :set (lambda (variable value)
         (if value
             (advice-add 'tempo-add-tag :override #'skempo--add-tag)
           (advice-remove 'tempo-add-tag #'skempo--add-tag))
         (set-default variable value)
         (when (<= 28 emacs-major-version)
           (message "skempo-update-identical-tags is obsolete since emacs 28")))
  :group 'skempo)

(defcustom skempo-skeleton-marks-support nil
  "Add `tempo-marks' support for skeleton.
This option enables jumping on skeleton points of interest with
`SKEMPO-FORWARD-MARK' nda `SKEMPO-BACKWARD-MARK'.  It reuses
`tempo-marks' functionality.  By default, skeleton does nothing
with its points of interest.

If you want to set this option from ELisp, you have to remove
`SKEMPO--ADD-SKELETON-MARKERS' from `skeleton-end-hook' on NIL
and add it on NON-NIL."
  :type '(boolean :tag "Skeleton marks?")
  :set (lambda (variable value)
         (if value
             (add-hook 'skeleton-end-hook #'skempo--add-skeleton-markers)
           (remove-hook 'skeleton-end-hook #'skempo--add-skeleton-markers))
         (set-default variable value))
  :group 'skempo)

(defcustom skempo-always-create-tag nil
  "Generate tags by default for skempo templates.
Note, you have to set this variable before you define a skempo
template."
  :type '(boolean :tag "Always tag?")
  :group 'skempo)

(defcustom skempo-always-create-abbrev nil
  "Generate abbrevs by default for skempo templates.
Note, you have to set this variable before you define a skempo
template."
  :type '(boolean :tag "Always abbrev?")
  :group 'skempo)

;;;; FUNCTIONS

(defalias 'skempo-forward-mark 'tempo-forward-mark)
(defalias 'skempo-backward-mark 'tempo-backward-mark)

(defun skempo--tags-variable (mode)
  "Return a tempo tags variable's symbol for `MODE'."
  (when mode
    (intern (replace-regexp-in-string
             (rx "-mode" eos) "-skempo-tags"
             (symbol-name mode)))))

(defun skempo--remove-tag-list (tag-list)
  "Remove `TAG-LIST' from `TEMPO-LOCAL-TAGS'."
  (setf (alist-get tag-list tempo-local-tags nil t) nil))

(defun skempo--complete-template (string tag-list)
  "An :override advice function for `TEMPO-DISPLAY-COMPLETIONS'.
Show completion for `STRING' in a `TAG-LIST'.  After selection
expand template.

Rewritten because the original function uses an old way of
displaying completions in a separate buffer, which is not
clickable anyway.  Now it uses new (compared to the originial
tempo package) and shiny `COMPLETING-READ' interface."
  (cl-loop for (tag . fn) in tag-list
           collect (format "%s (%s)" tag fn) into tag-names
           finally
           (let ((name (completing-read "Skempo: " tag-names nil t string)))
             (delete-char (- (length string)))
             (save-match-data
               (pcase name
                 ((rx "(" (let fn (*? any)) ")" eos)
                  (funcall (intern fn))))))))

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
  (dolist (position skeleton-positions)
    (tempo-insert-mark (set-marker (make-marker) position))))

(defun skempo--add-tag (tag template &optional tag-list)
  "Add a `TEMPLATE' `TAG' to `TAG-LIST' or to `TEMPO-TAGS'.
It is an :override function for `TEMPO-ADD-TAG'.  The original
function does not update identical tags."
  (interactive "sTag: \nCTemplate: ")
  (let ((tag-list (or tag-list 'tempo-tags)))
    (if-let ((value (assoc tag (symbol-value tag-list))))
        (setcdr value template)
      (set tag-list (cons (cons tag template) (symbol-value tag-list))))
    (tempo-invalidate-collection (eq tag-list 'tempo-tags))))

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
  (let* ((major-modes (parent-mode-list major-mode))
         (tag-vars (mapcar #'skempo--tags-variable major-modes))
         (bound-tag-vars (cl-delete-if-not #'boundp tag-vars)))
    (if skempo-mode
        (mapc #'tempo-use-tag-list bound-tag-vars)
      (mapc #'skempo--remove-tag-list bound-tag-vars))))

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
  (cl-destructuring-bind (name &key
                               (tag skempo-always-create-tag)
                               (abbrev skempo-always-create-abbrev)
                               docstring mode
                               &aux
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

                 (let ((,template-symbol
                        (tempo-define-template
                         ,(concat mode-prefix name) ',body
                         ,(when tag name) ,docstring ',tags-var)))

                   (put ,template-symbol 'no-self-insert t)
                   ,(when abbrev
                      `(define-abbrev ,abbrev-table ,name "" ,template-symbol :system t)))

                 ,(when tag
                    `(dolist (buffer (buffer-list))
                       (with-current-buffer buffer
                         (when (and ,(if mode `(derived-mode-p ',mode) t) skempo-mode)
                           (skempo-mode -1)
                           (skempo-mode 1))))))))

          modes))))

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
  (cl-destructuring-bind (name &key
                               (tag skempo-always-create-tag)
                               (abbrev skempo-always-create-abbrev)
                               docstring mode
                               &aux
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

                 (let ((,template-symbol
                        (define-skeleton
                          ,(intern (concat "skeleton-template-" mode-prefix name))
                          ,docstring ,@body)))

                   (set ,template-symbol '((ignore (,template-symbol))))
                   ,(when tag
                      `(tempo-add-tag ,name ,template-symbol ',tags-var))
                   ,(when abbrev
                      `(define-abbrev ,abbrev-table ,name "" ,template-symbol :system t)))

                 ,(when tag
                    `(dolist (buffer (buffer-list))
                       (with-current-buffer buffer
                         (when (and ,(if mode `(derived-mode-p ',mode) t) skempo-mode)
                           (skempo-mode -1)
                           (skempo-mode 1))))))))

          modes))))

;;;###autoload
(defmacro skempo-define-function (name-and-args function)
  "Define `FUNCTION' template.
See `SKEMPO-DEFINE-TEMPO' for explanation of `NAME-AND-ARGS'.  It ignores
`:docstring' option, for obvious reasons.

The main purpose of this macro, is to create tempo tags and
abbrevs for existing skeleton templates, such as `sh-case'.

Example:
\(skempo-define-function (shcase :tag t :abbrev t :mode `sh-mode')
  `sh-case')"
  (cl-destructuring-bind (name &key
                               (tag skempo-always-create-tag)
                               (abbrev skempo-always-create-abbrev)
                               mode
                               &aux
                               (name (symbol-name name))
                               (modes (skempo--modes mode)))
      (if (consp name-and-args) name-and-args (list name-and-args))
    `(progn
       ,@(mapcar

          (lambda (mode)
            (let ((tags-var (skempo--tags-variable mode))
                  (abbrev-table (skempo--abbrev-table mode)))
              `(progn
                 ,(when (and tag mode)
                    `(defvar ,tags-var nil))

                 ,(when (and abbrev mode)
                    `(define-abbrev-table ',abbrev-table nil))

                 (put ',function 'no-self-insert t)
                 (set ',function '((ignore (,function))))
                 ,(when tag
                    `(tempo-add-tag ,name ',function ',tags-var))
                 ,(when abbrev
                    `(define-abbrev ,abbrev-table ,name "" ',function :system t))

                 ,(when tag
                    `(dolist (buffer (buffer-list))
                       (with-current-buffer buffer
                         (when (and ,(if mode `(derived-mode-p ',mode) t) skempo-mode)
                           (skempo-mode -1)
                           (skempo-mode 1))))))))

          modes))))

;;;###autoload
(progn
  (put 'skempo-define-tempo 'lisp-indent-function 1)
  (put 'skempo-define-skeleton 'lisp-indent-function 1)
  (put 'skempo-define-function 'lisp-indent-function 1))

;;;; PROVIDE

(provide 'skempo)
;;; skempo.el ends here
