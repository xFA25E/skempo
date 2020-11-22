;;; tempo-mode.el --- Easier tempo definition and usage  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
;; Keywords: convenience
;; Version: 0.1.0
;; URL: https://github.com/xFA25E/tempo-mode
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

;; Conveniently use and define tempo-templates.
;;
;; Call TEMPO-MODE.  Call templates with
;; tempo-mode-complete-tag-or-call-on-region.  Define tempalets with
;; tempo-mode-define-templates.

;;; Code:


;;;; REQUIRES

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'mode-local)
(require 'tempo)


;;;; VARIABLES

(defvar tempo-mode-map (make-sparse-keymap) "Map used in `TEMPO-MODE'.")


;;;; FUNCTIONS

(defun tempo-mode-tag-list-names (tag-list)
  "Get tempo-templates names from `TAG-LIST'."
  (seq-map
   (pcase-lambda (`(_ . ,fn))
     (string-trim-left (symbol-name fn) "tempo-template-"))
   tag-list))

(defun tempo-mode-call-name (name)
  "Call tempo-template by `NAME'."
  (funcall (intern (concat "tempo-template-" name))))

;; Rewritten because the original function uses an old way of displaying
;; completions in a separate buffer, which does not work anyway.  Now it uses
;; new (compared to the originial tempo package) and shiny completing-read
;; interface.
(define-advice tempo-mode-display-completions (:override (string tag-list) read)
  (let* ((tag-list-names (tempo-mode-tag-list-names tag-list))
         (name (completing-read "Tempo: " tag-list-names nil t string)))
    (delete-char (- (length string)))
    (tempo-mode-call-name name)))

(defun tempo-mode-get-mode-genealogy (mode)
  "Get `MODE' forefathers.
Return a list which starts with the oldest forefather and ends with `MODE'."
  (cl-do ((modes nil (cons parent modes))
          (parent mode (get-mode-local-parent parent)))
      ((null parent) modes)))

(defun tempo-mode-tags-var (mode)
  "Return a tempo tags variable's symbol for `MODE'."
  (intern (replace-regexp-in-string (rx "-mode" eos) "-tempo-tags"
                                    (symbol-name mode))))

(defun tempo-mode-remove-tag-list (tag-list)
  "Remove `TAG-LIST' from `TEMPO-LOCAL-TAGS'."
  (setf (alist-get tag-list tempo-local-tags nil t) nil))


;;;; COMMANDS

(defun tempo-mode-complete-tag-or-call-on-region ()
  "Complete tempo tag or provide completion for a template around region.
If the region is active, then provide a completion for a tempo template.
Otherwise, try to complete with `TEMPO-COMELETE-TAG'.  If it fails to find a
suitable tag, provide template completion."
  (interactive)
  (if (use-region-p)
      (tempo-display-completions "" (tempo-build-collection))
    (unless (tempo-complete-tag)
      (tempo-display-completions "" (tempo-build-collection)))))

(define-minor-mode tempo-mode
  "Minor mode for tempo-templates.
It helps initializing templates for a certain mode and provides a convinient
macro for template definition.  Also, it provides some tempo enhancements for
completion."
  nil "Tempo-Mode" tempo-mode-map
  (if tempo-mode
      (thread-last (tempo-mode-get-mode-genealogy major-mode)
        (seq-map #'tempo-mode-tags-var)
        (seq-filter #'boundp)
        (seq-do #'tempo-use-tag-list))
    (thread-last (tempo-mode-get-mode-genealogy major-mode)
      (seq-map #'tempo-mode-tags-var)
      (seq-filter #'boundp)
      (seq-do #'tempo-mode-remove-tag-list))))


;;;; MACROS

;;;###autoload
(defmacro tempo-mode-define-templates (mode &rest templates)
  "Conveniently define tempo `TEMPLATES' for `MODE'.

Example:

\(tempo-define-templates `LISP-INTERACTION-MODE'
  (\"var\" '(\"(defvar var\" p n> r> \")\"))
  (\"fun\" '(\"(defun fun\" p \" (\" p \")\" n> r> \")\")))

The above will define two templates with tags \"var\" and \"fun\" for
`LISP-INTERACTION-MODE'.  It will save those tags in
`LISP-INTERACTION-TEMPO-TAGS' variable.  Two templates will have names
\"lisp-interaction-var\" and \"lisp-interaction-fun\" respectively.  It will
also attempt to refresh buffers with lisp-interaction-mode."
  (let ((name-prefix (string-trim-right (symbol-name mode) (rx "mode" eos)))
        (tags-var (tempo-mode-tags-var mode)))
    `(progn
       (defvar ,tags-var nil)
       ,@(mapcar
          (pcase-lambda (`(,name ,elements))
            `(tempo-define-template
              ,(concat name-prefix name)
              ,elements ,name nil ',tags-var))
          templates)
       (dolist (buffer (buffer-list))
         (with-current-buffer buffer
           (when (and (derived-mode-p ',mode) tempo-mode)
             (tempo-mode -1)
             (tempo-mode 1)))))))

;;;###autoload
(put 'tempo-mode-define-templates #'lisp-indent-function 1)


;;;; PROVIDE

(provide 'tempo-mode)
;;; tempo-mode.el ends here
