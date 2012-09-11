;;; -*- coding: utf-8 -*-
;;; bpe.el --- Blogger Post progrom for Emacs

;; Copyright (C) 2012 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/bpe
;; Version: 0.0.1
;; Keywords: Blogger

;;; License:
;; This program is free software: you can redistribute it and/or modify
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

(eval-when-compile (require 'cl))

;; google blog
(defvar bpe:account   "your-user-account@gmail.com")
(defvar bpe:blog-name "blog-name")

(defvar bpe:removing-list
  '(("^[\n\t]" "")
    ("</p>[\n\t]" "</p>")
    ("[\n\t]<" "<")
    (">[\n\t]" ">")))

(defun bpe:create-html-and-fetch-filename ()
  (let* ((org->html-file-name
          (replace-regexp-in-string
           "org$" "html" buffer-file-truename)))
    (org-export-as-html 23 nil nil nil 'string) ; 23 = HTML
    (bpe:replace-newline org->html-file-name)
    org->html-file-name))

(defun bpe:replace (list)
  (loop for (regexp to-string) in list do
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match to-string nil nil))))

(defun bpe:replace-newline (file)
  (let* ((base (buffer-name)))
    (find-file file)
    (bpe:replace bpe:removing-list)
    (save-buffer)
    (switch-to-buffer base)))

(defun bpe:search-title-word ()
  (goto-char (point-min))
  (if (re-search-forward "^.\+TITLE: \\(.+\\)" nil t)
      (match-string 1)))

(defun bpe:blog-post ()
  (interactive)
  (let* ((title (or (bpe:search-title-word)
                    (read-string "title here: ")))
         (content (if (string-match "\\.org$" (buffer-name))
                      (bpe:create-html-and-fetch-filename)
                    (buffer-string))))
    (async-shell-command
     (concat "google blogger post --draft -u " bpe:account
             " --blog '" bpe:blog-name "'"
             " --title '" title "' " content))))

(provide 'bpe)

;;; bpe.el ends here
