;;; auto-minor-mode.el --- Enable minor modes by file name and contents -*- lexical-binding: t -*-
;;
;; Copyright 2017 Joe Wreschnig
;;
;; Author: Joe Wreschnig <joe.wreschnig@gmail.com>
;; Package-Version: 20170601
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package lets you enable minor modes based on file name and
;; contents. To find the right modes, it checks filenames against
;; patterns in `auto-minor-mode-alist' and file contents against
;; `auto-minor-mode-magic-alist'. These work like the built-in Emacs
;; variables `auto-mode-alist' and `magic-mode-alist'.
;;
;; Unlike major modes, all matching minor modes are enabled, not only
;; the first match.
;;
;; A reason you might want to use it:
;;   (add-to-list 'auto-minor-mode-alist '("-theme\\.el\\'" . rainbow-mode))
;;
;; There's intentionally no equivalent of `interpreter-mode-alist'.
;; Interpreters should determine the major mode.  Relevant minor
;; modes can then be enabled by major mode hooks.
;;
;; Minor modes are set whenever `set-auto-mode', the built-in function
;; responsible for handling automatic major modes, is called.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs corresponding minor mode functions.

This is an equivalent of `auto-mode-alist', for minor modes.

Unlike `auto-mode-alist', matching always follows `case-fold-search'.")

;;;###autoload
(defvar auto-minor-mode-magic-alist ()
  "Alist of buffer beginnings vs corresponding minor mode functions.

This is an equivalent of `magic-mode-alist', for minor modes.

Magic minor modes are applied after `set-auto-mode' enables any
major mode, so it's possible to check for expected major modes in
match functions.

Unlike `magic-mode-alist', matching always follows `case-fold-search'.")

(defun auto-minor-mode-enabled-p (minor-mode)
  "Return non-nil if MINOR-MODE is enabled in the current buffer."
  (and (memq minor-mode minor-mode-list)
       (symbol-value minor-mode)))

(defun auto-minor-mode--plain-filename (file-name)
  "Remove remote connections and backup version from FILE-NAME."
  (let ((remote-id (file-remote-p file-name))
        (file-name (file-name-sans-versions file-name)))
    (if (and remote-id (string-match-p (regexp-quote remote-id) file-name))
        (substring file-name (match-end 0))
      file-name)))

;;;###autoload
(defun auto-minor-mode-set (&optional keep-mode-if-same)
  "Enable all minor modes appropriate for the current buffer.

If the optional argument KEEP-MODE-IF-SAME is non-nil, then we
don’t re-activate minor modes already enabled in the buffer."
  (when buffer-file-name
    (let* ((bufname (auto-minor-mode--plain-filename buffer-file-name)))
      (cl-loop for (match . mode) in auto-minor-mode-alist do
               (when (string-match-p match bufname)
                 (unless (and keep-mode-if-same
                              (auto-minor-mode-enabled-p mode))
                   (funcall mode 1))))))

  (save-excursion
    (save-restriction
      (narrow-to-region (point-min)
                        (min (point-max)
                             (+ (point-min) magic-mode-regexp-match-limit)))
      (cl-loop for (match . mode) in auto-minor-mode-magic-alist do
               (goto-char (point-min))
               (when (if (functionp match) (funcall match) (looking-at match))
                 (unless (and keep-mode-if-same
                              (auto-minor-mode-enabled-p mode))
                   (funcall mode 1)))))))

;;;###autoload
(advice-add #'set-auto-mode :after #'auto-minor-mode-set)

(provide 'auto-minor-mode)
;;; auto-minor-mode.el ends here
