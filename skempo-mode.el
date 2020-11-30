;;; skempo-mode.el --- Easier skempo definition and usage  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
;; Keywords: convenience
;; Version: 0.1.0
;; URL: https://github.com/xFA25E/skempo-mode
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
;; The main symbols of interests are: skempo-mode,
;; skempo-mode-complete-tag-or-call-on-region and
;; skempo-mode-define-templates.  See their docstrings.
;;
;; Also, you should use tempo's tempo-forward-mark and
;; tempo-backward-mark for jumping.
;;
;; This package also cleans up some old tempo's code for mark
;; insersion and completion.

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

(defun skempo-mode-completion-names-for-tag-list (tag-list)
  "Get skempo-templates completion names from `TAG-LIST'."
  (seq-map
   (pcase-lambda (`(,tag . ,fn)) (format "%s (%s)" tag fn))
   tag-list))

(defun skempo-mode-call-name (name)
  "Call skempo-template by `NAME'.
`NAME' is in a form \"tag (function)\".  Parse a function and
call it."
  (when (string-match (rx "(" (group (*? any)) ")" eos) name)
    (funcall (intern (match-string 1 name)))))


(defun skempo-mode-display-completions (string tag-list)
  "An :override advice function for `TEMPO-DISPLAY-COMPLETIONS'.
Show completion for `STRING' in a `TAG-LIST'.  After selection
call tag.

Rewritten because the original function uses an old way of
displaying completions in a separate buffer, which does not work
anyway.  Now it uses new (compared to the originial skempo
package) and shiny `COMPLETING-READ' interface."
  (let* ((tag-list-names (skempo-mode-completion-names-for-tag-list tag-list))
         (name (completing-read "Skempo: " tag-list-names nil t string)))
    (delete-char (- (length string)))
    (skempo-mode-call-name name)))

(defun skempo-mode-tags-var (mode)
  "Return a tempo tags variable's symbol for `MODE'."
  (intern (replace-regexp-in-string
           (rx "-mode" eos) "-skempo-tags" (symbol-name mode))))

(defun skempo-mode-remove-tag-list (tag-list)
  "Remove `TAG-LIST' from `TEMPO-LOCAL-TAGS'."
  (setf (alist-get tag-list tempo-local-tags nil t) nil))

(defun skempo-mode-delete-duplicate-markers (&optional _ignored-mark)
  "Remove duplicate marks from `TEMPO-MARKS'.
Set to nil removed markers.  This function is used as an :after
advice to `TEMPO-INSERT-MARK', because the original function does
not remove duplicate elements and if the buffer gets smaller,
markers could point to the same location.  We don't want that,
because a lot of markers can slow down Emacs.

HACK! This function is relying on `CL-DELETE-DUPLICATES' keeping
all the right arguments in compare function.  So, we can
simultaneously delete and set marker position to nil.  Yeah, I'm
lazy."
  (let ((compare (lambda (a b) (when (= a b) (set-marker a nil)))))
    (setq tempo-marks (cl-delete-duplicates tempo-marks :test compare))))

(defun skempo-mode-add-skeleton-markers ()
  "Add `SKELETON-POSITION' positions to `TEMPO-MARKS'."
  (seq-do
   (lambda (position)
     (tempo-insert-mark (set-marker (make-marker) position)))
   skeleton-positions))


;;;; COMMANDS

;;;###autoload
(defun skempo-mode-complete-tag-or-call-on-region ()
  "Complete tempo tag or provide completion for a template around region.
If the region is active, then provide a completion for a skempo
template.  Otherwise, try to complete with `tempo-comelete-tag'.
If it fails to find a suitable tag, provide template completion."
  (interactive)
  (if (use-region-p)
      (tempo-display-completions "" (tempo-build-collection))
    (unless (tempo-complete-tag)
      (tempo-display-completions "" (tempo-build-collection)))))

;;;###autoload
(define-minor-mode skempo-mode
  "Minor mode for skempo-templates.
It helps initializing templates for a certain mode and provides a convinient
macro for template definition.  Also, it provides some tempo enhancements for
completion."
  nil " Skempo" skempo-mode-map
  (if skempo-mode

      ;; enabled
      (progn
        (advice-add 'tempo-display-completions :override #'skempo-mode-display-completions)
        (advice-add 'tempo-insert-mark :after #'skempo-mode-delete-duplicate-markers)
        (thread-last (parent-mode-list major-mode)
          (seq-map #'skempo-mode-tags-var)
          (seq-filter #'boundp)
          (seq-do #'tempo-use-tag-list))
        (add-hook 'skeleton-end-hook #'skempo-mode-add-skeleton-markers))

    ;; disabled
    (advice-remove 'tempo-display-completions #'skempo-mode-display-completions)
    (advice-remove 'tempo-insert-mark #'skempo-mode-delete-duplicate-markers)
    (thread-last (parent-mode-list major-mode)
      (seq-map #'skempo-mode-tags-var)
      (seq-filter #'boundp)
      (seq-do #'skempo-mode-remove-tag-list))
    (remove-hook 'skeleton-end-hook #'skempo-mode-add-skeleton-markers)))


;;;; MACROS

(defun skempo-mode-template-options-normal-form (options)
  "Normalize template `OPTIONS' to (type-keyword abbrevp doc)."
  (pcase options
    ((pred keywordp) `(,options nil ""))
    (`(,(and (pred keywordp) type)) `(,type nil ""))
    (`(,(and (pred keywordp) type) ,abbrev) `(,type ,abbrev ""))
    (`(,(and (pred keywordp) type) ,abbrev ,(and (pred stringp) doc)) `(,type ,abbrev ,doc))
    (_ (error "Invalid skempo options"))))

;;;###autoload
(defmacro skempo-mode-define-templates (mode &rest templates)
  "Conveniently define skempo `TEMPLATES' for `MODE'.

Example:

\(skempo-mode-define-templates `LISP-INTERACTION-MODE'
  (\"var\" :tempo '(\"(defvar var\" p n> r> \")\"))
  (\"fun\" :tempo '(\"(defun fun\" p \" (\" p \")\" n> r> \")\")))

The above will define two tempo templates with tags \"var\" and
\"fun\" for `LISP-INTERACTION-MODE'.  It will save those tags in
`LISP-INTERACTION-SKEMPO-TAGS' variable.  Two templates will have
names \"lisp-interaction-var\" and \"lisp-interaction-fun\"
respectively.  It will also attempt to refresh buffers with
`LISP-INTERACTION-MODE'.

A second argument :tempo is a shorthand for writing \(:tempo nil
\"\").  Instead of :tempo you could write :skeleton or :function.
A second element (non-nil) of this list indicates whether an
abbrev should be created for this template.  Third element is a
documentation-string."
  (let ((mode-prefix (string-trim-right (symbol-name mode) (rx "mode" eos)))
        (tags-var (skempo-mode-tags-var mode))
        (abbrev-table (derived-mode-abbrev-table-name mode)))
    `(progn
       (defvar ,tags-var nil)
       (define-abbrev-table ',abbrev-table nil)

       ,@(mapcar

          (pcase-lambda (`(,name ,options . ,elements))
            (pcase (skempo-mode-template-options-normal-form options)
              (`(:tempo ,abbrev ,doc)
               (let ((template-symbol (gensym "template-symbol")))
                 `(let ((,template-symbol
                         (tempo-define-template
                          ,(concat mode-prefix name) ',elements ,name ,doc ',tags-var)))
                    (put ,template-symbol 'no-self-insert t)
                    ,(when abbrev
                       `(define-abbrev ,abbrev-table ,name "" ,template-symbol :system t)))))

              (`(:skeleton ,abbrev ,doc)
               (let ((template-symbol (gensym "template-symbol")))
                 `(let ((,template-symbol
                         (define-skeleton ,(intern (concat "skeleton-template-" mode-prefix name))
                           ,doc ,@elements)))
                    (set ,template-symbol `((ignore (,,template-symbol))))
                    (tempo-add-tag ,name ,template-symbol ',tags-var)
                    ,(when abbrev
                       `(define-abbrev ,abbrev-table ,name "" ,template-symbol :system t)))))

              (`(:function ,abbrev ,_)
               (let ((fn (car elements)))
                 `(progn
                    (set ',fn '((ignore (,fn))))
                    (tempo-add-tag ,name ',fn ',tags-var)
                    ,(when abbrev
                       `(define-abbrev ,abbrev-table ,name "" ',fn :system t)))))

              (_ (error "Invalid skempo params"))))

          templates)

       (dolist (buffer (buffer-list))
         (with-current-buffer buffer
           (when (and (derived-mode-p ',mode) skempo-mode)
             (skempo-mode -1)
             (skempo-mode 1)))))))

;;;###autoload
(put 'skempo-mode-define-templates #'lisp-indent-function 1)


;;;; PROVIDE

(provide 'skempo-mode)
;;; skempo-mode.el ends here
