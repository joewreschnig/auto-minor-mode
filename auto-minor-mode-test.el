;;; auto-minor-mode-test.el --- Tests for auto-minor-mode  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017  Joe Wreschnig
;;
;; Author: Joe Wreschnig
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
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
;; This file contains test cases for auto-minor-mode.  Unless you’re
;; hacking on it you shouldn’t need to edit or run this file.

;;; Code:

(require 'auto-minor-mode)
(require 'cl-lib)
(require 'ert)

(defvar auto-minor-mode--test 0
  "A counter for mode-re-enabling testing.")

(define-minor-mode auto-minor-mode--test-mode
  "A mode for ‘auto-minor-mode’ test cases."
  nil nil nil
  (setq auto-minor-mode--test (1+ auto-minor-mode--test)))

(defmacro auto-minor-mode--protect (&rest body)
  "Do BODY forms, disabling ‘auto-minor-mode--test-mode’ when done."
  (declare (indent defun))
  `(unwind-protect
       (progn ,@body)
     (let ((is-test (lambda (p) (eq (cdr p) #'auto-minor-mode--test-mode))))
       (setq auto-minor-mode-alist
             (cl-remove-if is-test auto-minor-mode-alist))
       (setq auto-minor-mode-magic-alist
             (cl-remove-if is-test auto-minor-mode-magic-alist)))))

(defun auto-minor-mode--test-suffix-p (suffix)
  "Return non-nil if a file ending with SUFFIX enables the test mode."
  (save-excursion
    (let ((filename (make-temp-file "auto-minor-mode" nil suffix)))
      (find-file filename)
      (unwind-protect
          auto-minor-mode--test-mode
        (kill-buffer)
        (delete-file filename)))))

(defun auto-minor-mode--test-contents-p (contents)
  "Return non-nil if a file containing CONTENTS enables the test mode."
  (with-temp-buffer
    (insert contents)
    (set-auto-mode)
    auto-minor-mode--test-mode))

(defun auto-minor-mode--test-name-p (name)
  "Return non-nil if a buffer named NAME enables the test mode."
  (unwind-protect
      (with-current-buffer (get-buffer-create name)
        (set-auto-mode)
        auto-minor-mode--test-mode)
    (kill-buffer name)))

(ert-deftest auto-minor-mode--test-auto ()
  "Ensure modes match based on suffix."
  (should-not (auto-minor-mode--test-suffix-p ".ammtest"))

  (add-to-list 'auto-minor-mode-alist
               '("\\.ammtest\\'" . auto-minor-mode--test-mode))
  (auto-minor-mode--protect
    (should (auto-minor-mode--test-suffix-p ".ammtest"))
    (should (auto-minor-mode--test-suffix-p ".aMmTeSt"))
    (should-not (auto-minor-mode--test-suffix-p ".xammtest"))
    (should-not (auto-minor-mode--test-suffix-p ".ammtestx"))
    (should-not (auto-minor-mode--test-suffix-p ".txt"))
    (should-not (auto-minor-mode--test-suffix-p "")))
  (should-not (auto-minor-mode--test-suffix-p ".ammtest")))

(ert-deftest auto-minor-mode--test-magic-regex ()
  "Ensure modes match based on initial content regexp."
  (should-not (auto-minor-mode--test-contents-p "ammtest"))

  (add-to-list 'auto-minor-mode-magic-alist
               '("[^x]*ammtest" . auto-minor-mode--test-mode))
  (auto-minor-mode--protect
    (should (auto-minor-mode--test-contents-p "ammtest"))
    (should (auto-minor-mode--test-contents-p "ammtest foo"))
    (should (auto-minor-mode--test-contents-p "ammtestfoo"))
    (should (auto-minor-mode--test-contents-p "ammtest\n"))
    (should (auto-minor-mode--test-contents-p "yammtest\n"))
    (should (auto-minor-mode--test-contents-p "abcammtest"))
    (should (auto-minor-mode--test-contents-p "   ammtest"))

    (should-not (auto-minor-mode--test-contents-p "xammtest"))
    (should-not (auto-minor-mode--test-contents-p "axb ammtest"))
    (should-not (auto-minor-mode--test-contents-p "")))
  (should-not (auto-minor-mode--test-contents-p "ammtest")))

(ert-deftest auto-minor-mode--test-magic-regex-limit ()
  "Ensure matching stops at the auto regexp match limit."
  (should-not (auto-minor-mode--test-contents-p "x"))

  (add-to-list 'auto-minor-mode-magic-alist
               '("a*x" . auto-minor-mode--test-mode))
  (auto-minor-mode--protect
    (let ((limit (make-string (1- magic-mode-regexp-match-limit) ?a)))
      (should (auto-minor-mode--test-contents-p (concat limit "x")))
      (should-not (auto-minor-mode--test-contents-p (concat " " limit "x")))))
  (should-not (auto-minor-mode--test-contents-p "x")))

(ert-deftest auto-minor-mode--test-magic-function ()
  "Ensure magic matching functions work as expected."
  (let ((magicf (lambda () (= (% (length (buffer-name)) 2) 0))))
    (add-to-list 'auto-minor-mode-magic-alist
                 (cons magicf 'auto-minor-mode--test-mode))
    (auto-minor-mode--protect
      (should-not (auto-minor-mode--test-name-p "x"))
      (should-not (auto-minor-mode--test-name-p "xxx"))
      (should (auto-minor-mode--test-name-p "xx"))
      (should (auto-minor-mode--test-name-p "xxxxxx")))
    (should-not (auto-minor-mode--test-name-p "x"))
    (should-not (auto-minor-mode--test-name-p "xx"))))

(ert-deftest auto-minor-mode--test-re-enable ()
  (add-to-list 'auto-minor-mode-alist
               '("\\.ammtest" . auto-minor-mode--test-mode))
  (auto-minor-mode--protect
    (setq auto-minor-mode--test 0)
    (let ((filename (make-temp-file "auto-minor-mode" nil ".ammtest.el")))
      (find-file filename)
      (unwind-protect
          (progn
            (should (eq auto-minor-mode--test 1))
            (set-auto-mode)
            (should (eq auto-minor-mode--test 2))
            (set-auto-mode t)
            (should (eq auto-minor-mode--test 2))
            (set-auto-mode)
            (should (eq auto-minor-mode--test 3)))
        (kill-buffer)))))

;;; auto-minor-mode-test.el ends here
