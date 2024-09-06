;;; crates.el --- Crates.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024  M.R. Siavash Katebzadeh

;; Author: M.R.Siavash Katebzadeh <mr@katebzadeh.xyz>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

(defvar crates-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Crates minor mode.")

(defvar crates-crates-api "https://crates.io/api/v1/crates/")

(defcustom crates-checkmark-symbol "✅"
  "Symbol to indicate that a crate version is up to date."
  :type 'string
  :group 'crates)

(defcustom crates-warning-symbol "⚠️"
  "Symbol to indicate that a crate version is out of date."
  :type 'string
  :group 'crates)

(defun crates-fetch-latest-version (crate)
  "Fetch the latest version of CRATE from crates.io."
  (let ((url-request-method "GET")
        (url (concat crates-crates-api crate)))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (json (json-read))
             (versions (alist-get 'versions json)))
        (alist-get 'num (car versions))))))

(defun crates-update-crate-version (crate new-version)
  "Update the crate CRATE to NEW-VERSION in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^\\s-*%s\\s-*=" crate) nil t)
      (let ((end (line-end-position)))
        (beginning-of-line)
        (delete-region (point) end)
        (insert (format "%s = \"%s\"" crate new-version))))))

(defun crates-update-current-line ()
  "Update the crate in the current line to the latest version."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^\\s-*\\([a-zA-Z0-9_-]+\\)\\s-*=[^=]*" (line-end-position) t)
      (let* ((crate (match-string 1))
             (latest-version (crates-fetch-latest-version crate)))
        (crates-update-crate-version crate latest-version)
        (message "Updated %s to version %s" crate latest-version)))))

(defun crates-check-crate-version (crate current-version)
  "Check if the CRATE version CURRENT-VERSION is up to date."
  (let ((latest-version (crates-fetch-latest-version crate)))
    (if (string= current-version latest-version)
        (concat crates-checkmark-symbol)  ;; Checkmark if up to date
      (concat crates-warning-symbol " (latest: " latest-version ")")))) ;; Warning with the latest version

(defun crates-create-overlay (start end text face)
  "Create an overlay from START to END with TEXT and FACE."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'after-string
                 (propertize text 'face face))
    (overlay-put overlay 'crates-overlay t)
    overlay))

(defun crates-remove-overlays ()
  "Remove all Crates overlays from the buffer."
  (remove-overlays (point-min) (point-max) 'crates-overlay t))

(defun crates-parse-section (section)
  "Parse the SECTION in the current buffer and add overlays for versions."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "\\[%s\\]" section) nil t)
      (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\)\\(?:\\s-*=\\s-*\\(\"[^\"]+\"\\|{[^}]*\\s-+version\\s-*=[^\"]+\"\\)\\)" nil t)
        (let ((crate (match-string 1))
              (version (or (match-string 2) "")))
          (when (string-match "{.*\\bversion\\s*=\\s*\\(\"[^\"]+\"\\)" version)
            (setq version (match-string 1 version)))
          (setq version (replace-regexp-in-string "\"" "" version))
          (let* ((status (crates-check-crate-version crate version))
                 (start (line-end-position))
                 (end (line-end-position))
                 (face (if (string-prefix-p crates-checkmark-symbol status)
                           'success
                         'warning)))
            (crates-create-overlay start end status face)))))))

(defun crates-parse-dependencies ()
  "Parse the dependencies in the current buffer and check versions."
  (crates-remove-overlays)
  (crates-parse-section "dependencies")
  (crates-parse-section "workspace.dependencies")
  (crates-parse-section "dev.dependencies"))

(defun crates-refresh-buffer ()
  "Refresh the buffer and check crate versions again."
  (interactive)
  (crates-parse-dependencies))

;;; Define the minor mode
(define-minor-mode crates-mode
  "Minor mode to check Cargo.toml crate versions."
  :lighter " Crates"
  :keymap crates-mode-map
  (if crates-mode
      (crates-refresh-buffer)
    (crates-remove-overlays)))

(defun maybe-enable-crates-mode ()
  "Enable crates-mode for Cargo.toml files."
  (when (string= (file-name-nondirectory (buffer-file-name)) "Cargo.toml")
    (crates-mode 1)))

(add-hook 'find-file-hook 'maybe-enable-crates-mode)

(provide 'crates)
;;; crates.el ends here
