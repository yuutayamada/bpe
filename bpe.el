;;; -*- coding: utf-8 mode: emacs-lisp -*-
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
(require 'org)

;; google blog
(defvar bpe:account   "your-user-account@gmail.com")
(defvar bpe:blog-name "blog-name")
(defvar bpe:lang      "ja_JP.UTF-8")
(defvar bpe:update-by-default nil
  "If this value was non-nil, update article if there are same title's article(s)")

(defvar bpe:removing-list
  '(("\n\\(<p>\\)" 1)
    ("\\(</?ol>\\)\n+" 1)
    ("\\(</li>\\)\n+" 1)
    ("\\(</?tbody>\\)\n+" 1)
    ("\\(<table.+>\\)\n+" 1)
    ("\\(</table>\\)\n+" 1)
    ("\\(</caption>\\)\n+" 1)
    ("\\(</colgroup>\\)\n+" 1)
    ("\\(<col class=.+ />\\)\n+" 1)
    ("\\(</tr>\\)\n+" 1)
    ("\\(</p>\\)\n+" 1)
    ("\\(</ul>\\)\n+" 1)
    ("\\(</h[0-9]>\\)\n" 1)
    ("\\(<div.+>\\)\n+" 1)
    ("\\(</div>\\)\n+" 1)
    ("\\(</pre>\\)\n+" 1)))

(defun bpe:create-html-and-fetch-filename ()
  (let* ((org->html-file-name
          (replace-regexp-in-string
           "org$" "html" buffer-file-truename)))
    (with-no-warnings
      (if (fboundp 'org-html-export-to-html)
          (org-html-export-to-html nil nil nil t)
        (case (string-to-number
               (replace-regexp-in-string "\\." "" org-version))
          ;; 23 = HTML
          (794 (org-export-as-html 23 nil nil t))
          (t   (org-export-as-html 23 nil nil nil 'string)))))
    (bpe:replace-newline org->html-file-name)
    org->html-file-name))

(defun bpe:replace (list)
  (loop with to-str = ""
        for (regexp to-string) in list do
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (typecase to-string
            (number (setq to-str (match-string to-string))
                    (replace-match to-str nil nil))
            (string
             (replace-match to-string nil nil))))))

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
     (concat "LANG=" bpe:lang " google blogger post --draft -u " bpe:account
             " --blog '" bpe:blog-name "'"
             " --title '" title "' " content))))

(provide 'bpe)

;;; bpe.el ends here
