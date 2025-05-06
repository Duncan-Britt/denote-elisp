;;; denote-elisp.el --- Organize Elisp files using Denote -*- lexical-binding: t -*-

;; Copyright (C) 2025 Duncan Britt

;; Author: Duncan Britt
;; Contact: https://github.com/Duncan-Britt/denote-elisp/issues
;; URL: https://github.com/Duncan-Britt/denote-elisp
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (denote "4.0.0"))
;; Keywords: config, files, elisp

;; This file is NOT part of GNU Emacs.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Code:

(require 'denote)
(require 'vc-git)

;; https://protesilaos.com/codelog/2022-10-30-demo-denote-custom-file-type/

;;;###autoload (put 'denote-elisp-directory 'safe-local-variable (lambda (val) (or (stringp val) (eq val 'local) (eq val 'default-directory))))
(defcustom denote-elisp-directory (expand-file-name "lisp" user-emacs-directory)
  "Directory for storing personal denote-elisp files.

If you intend to reference this variable in Lisp, consider using
the function `denote-elisp=directory' instead."
  :group 'denote-elisp
  :safe (lambda (val) (or (stringp val) (eq val 'local) (eq val 'default-directory)))
  ;; :package-version '(denote-elisp . "1.0.0")
  :type 'directory)

(defun denote-elisp--make-denote-directory ()
  "Make the variable `denote-elisp-directory' and its parents, if needed."
  (when (not (file-directory-p denote-directory))
    (make-directory denote-directory :parents)))

(defun denote-elisp-directory ()
  "Return path of variable `denote-elisp-directory' as a proper directory.
Custom Lisp code can `let' bind the variable `denote-elisp-directory'
to override what this function returns."
  (let ((denote-elisp-directory (file-name-as-directory (expand-file-name denote-elisp-directory))))
    (denote-elisp--make-denote-directory)
    denote-elisp-directory))

(defvar denote-elisp-front-matter
  ";;; --%1$s_%3$s@@%4$s.el --- %1$s -*- lexical-binding: t -*-

;;; Commentary:
;; title: %1$s
;; keywords: %3$s
;; date: %2$s
;; identifier: %4$s
;; signature: %5$s

;;; Code:
"
  "Elisp front matter.
It is passed to ‘format’ with arguments TITLE, DATE, KEYWORDS,
ID.  Advanced users are advised to consult Info node ‘(denote)")

(defun denote-elisp-format-keywords-for-front-matter (keywords)
  "Format front matter KEYWORDS for elisp file type.
KEYWORDS is a list of strings.  Consult the `denote-file-types'
for how this is used."
  (if keywords
      (format ":%s:" (string-join keywords ":"))
    ""))

(defun denote-elisp-date-timestamp (date)
  "Format DATE using the Org inactive timestamp notation."
  (if date
      (format-time-string "[%F %a %R]" date)
    ""))

(defvar denote-elisp-link-format "[[denote:%s][%s]]"
  "Format of Elisp link to note.
The value is passed to `format' with IDENTIFIER and TITLE
arguments, in this order.

Also see `denote-elisp-link-in-context-regexp'.")

(defvar denote-elisp-link-in-context-regexp
  (concat "\\[\\[" "denote:"
          "\\(?1:[^][]*?\\)"
          "\\(?:::.*\\)?" "]"
          "\\[" "\\(?2:" ".*?" "\\)" "]]")
  "Regexp to match an Org link in its context.
The format of such links is `denote-elisp-link-format'.")

(add-to-list 'denote-file-types
             '(elisp :extension ".el"
                     :front-matter denote-elisp-front-matter
                     :title-key-regexp "^;;\\s-*title\\s-*:"
                     :title-value-function denote-format-string-for-org-front-matter
                     :title-value-reverse-function denote-trim-whitespace
                     :keywords-key-regexp "^;;\\s-*keywords\\s-*:"
                     :keywords-value-function denote-elisp-format-keywords-for-front-matter
                     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
                     :signature-key-regexp "^;;\\s-*signature\\s-*:"
                     :signature-value-function denote-format-string-for-org-front-matter
                     :signature-value-reverse-function denote-trim-whitespace
                     :identifier-key-regexp "^;;\\s-*identifier\\s-*:"
                     :identifier-value-function denote-format-string-for-org-front-matter
                     :identifier-value-reverse-function denote-trim-whitespace
                     :date-key-regexp "^;;\\s-*date\\s-*:"
                     :date-value-function denote-elisp-date-timestamp
                     :date-value-reverse-function denote-extract-date-from-front-matter
                     :link denote-elisp-link-format
                     :link-in-context-regexp denote-elisp-link-in-context-regexp))

(defun denote-elisp--after-new-note ()
  "Add footer after creating a new note."
  (when (eq 'elisp (cdr (assq 'file-type denote-current-data)))
    (let* ((file-name (format "--%s%s@@%s.el"
                              (cdr (assq 'title denote-current-data))
                              (if-let (keywords (cdr (assq 'keywords denote-current-data)))
                                (progn
                                  (print keywords)
                                  (concat "__" (string-join keywords "_")))
                              "")
                              (cdr (assq 'id denote-current-data))))
           (full-path (format "%s%s"
                              (cdr (assq 'directory denote-current-data))
                              file-name))
           (module-name (file-name-base file-name)))

      (with-current-buffer (find-file-noselect full-path)
        ;; Update header line
        (goto-char (point-min))
        (when (re-search-forward "^;;; \\([^[:space:]\n]+\\) --- .*$" nil t)
          (replace-match file-name nil nil nil 1))

        ;; Update the provide statement and footer
        (goto-char (point-max))
        (if (re-search-backward "^(provide '\\([^)]+\\))" nil t)
            (replace-match module-name nil nil nil 1)
          (goto-char (point-max))
          (newline)
          (insert (format "(provide '%s)"
                          module-name)))

        (goto-char (point-max))
        (if (re-search-backward "^;;; \\([^[:space:]\n]+\\) ends here$" nil t)
            (replace-match file-name nil nil nil 1)
          (goto-char (point-max))
          (newline)
          (insert (format ";;; %s ends here"
                          file-name)))
        (save-buffer)
        (move-beginning-of-line -1)))))

(defun denote-elisp--after-rename ()
  "Rewrite header and footer lines to match new file name.
Hook to run after the file is renamed."
  (when (eq 'elisp (cdr (assq 'file-type denote-current-data)))
    (let* ((file-name (format "--%s%s@@%s.el"
                              (cdr (assq 'title denote-current-data))
                              (if-let (keywords (cdr (assq 'keywords denote-current-data)))
                                  (progn
                                    (print keywords)
                                    (concat "__" (string-join keywords "_")))
                                "")
                              (cdr (assq 'id denote-current-data))))
           (full-path (format "%s%s"
                              (cdr (assq 'directory denote-current-data))
                              file-name))
           (module-name (file-name-base file-name)))
      (when (file-exists-p full-path)
        (with-current-buffer (find-file-noselect full-path)
          (save-excursion
            ;; Update header line
            (goto-char (point-min))
            (when (re-search-forward "^;;; \\([^[:space:]\n]+\\) --- .*$" nil t)
              (replace-match file-name nil nil nil 1))

            ;; Update the provide statement and footer
            (goto-char (point-max))
            (if (re-search-backward "^(provide '\\([^)]+\\))" nil t)
                (replace-match module-name nil nil nil 1)
              (goto-char (point-max))
              (newline)
              (insert (format "(provide '%s)"
                              module-name)))

            (goto-char (point-max))
            (if (re-search-backward "^;;; \\([^[:space:]\n]+\\) ends here$" nil t)
                (replace-match file-name nil nil nil 1)
              (goto-char (point-max))
              (newline)
              (insert (format ";;; %s ends here"
                              file-name)))
            (save-buffer)))))))

(defun within-user-emacs-directory-p (file-path)
  "Check if FILE-PATH is within the user's Emacs directory."
  (string-prefix-p (expand-file-name user-emacs-directory)
                   (expand-file-name file-path)))

(defun denote-elisp-update-requires (prev-name new-name)
  "Replace (require 'PREV-NAME) with (require 'NEW-NAME)."
  (save-excursion
      (let ((prior-buffers (mapcar #'buffer-name (buffer-list))))
        (dolist (file (directory-files-recursively (denote-elisp-directory) "\\.el$"))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (while (search-forward (concat "(require '" prev-name ")") nil t)
              (replace-match (concat "(require '" new-name ")")))
            (save-buffer)
            (unless (member (buffer-name) prior-buffers)
              (kill-buffer)))))))

(defun denote-elisp-rename-file ()
    "Call denote-rename-file with local directory keywords."
    (interactive)
    (let ((denote-directory default-directory)
          (denote-use-file-type 'elisp)
          (denote-file-name-components-order '(title keywords identifier signature))
          (prev-name (file-name-base (buffer-file-name))))
      (condition-case nil
          (prog1
              (call-interactively 'denote-rename-file)
            (denote-elisp--after-rename)
            (let ((new-name (file-name-base (buffer-file-name))))
              (denote-elisp-update-requires prev-name new-name)))
        (quit nil))))

(defun denote-elisp ()
  "Call denote with local directory."
  (interactive)
  (let ((denote-directory (denote-elisp-directory))
        (denote-use-file-type 'elisp)
        (denote-file-name-components-order '(title keywords identifier signature)))
    (condition-case nil
        (prog1
            (call-interactively 'denote)
          (denote-elisp--after-new-note))
        (quit nil))))

(provide 'denote-elisp)
;;; denote-elisp.el ends here
