;;; bpe.el --- Blog from Org mode to Blogger

;; Copyright (C) 2012 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/bpe
;; Version: 0.0.1
;; Keywords: Blogger, blog

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
;;; Commentary:

;;; Code:
(eval-when-compile (require 'cl))
(require 'org)
(require 'ox-html nil 'noerror)

;; google blog
(defvar bpe:account   "your-user-account@gmail.com")
(defvar bpe:blog-name "blog-name")
(defvar bpe:lang      (shell-command-to-string "echo -n $LANG"))
(defvar bpe:update-by-default nil
  "If this value was non-nil, update article if there are same title's
article(s)")
(defvar bpe:use-real-post-when-updating nil
  "If non-nil do not attach --draft option when updating.")
(defvar bpe:no-ask nil
  "Attach --yes option when user update(delete old article) if this variable
was non-nil")

(defvar bpe:template "#+TITLE:
#+OPTIONS: toc:nil \\n:nil num:nil
#+TAGS:
#+AUTHOR: "
  "Template for `bpe:insert-template'")

(defvar bpe:command (concat "LANG=" bpe:lang " google blogger "))
(defvar bpe:tag-list
  '("p" "ul" "li" "ol" "tbody" "table.?+" "caption" "tr" "th"
    "colgroup" "div" "pre" "code" "h[0-9]"))

(defun bpe:generate-regexp (tag-list)
  (loop with tag-regexp = '()
        for name in tag-list
        for tag      = (format "<%s>" name)
        for endtag   = (replace-regexp-in-string "^<" "</" tag)
        for left     = `(,(format "\n\\(%s\\)"  tag)    1)
        for right    = `(,(format "\\(%s\\)\n+" tag)    1)
        for endleft  = `(,(format "\n\\(%s\\)"  endtag) 1)
        for endright = `(,(format "\\(%s\\)\n+" endtag) 1)
        do (push left     tag-regexp)
        do (push right    tag-regexp)
        do (push endleft  tag-regexp)
        do (push endright tag-regexp)
        finally return (reverse tag-regexp)))

(defvar bpe:removing-list
  (append
   (bpe:generate-regexp bpe:tag-list)
   '(("\\(</td>\\)\n+" 1)
     ("\\(</table>\\)\n+" 1)
     ("\\(<pre.*>\\)\n+" 1)
     ("\\(<col +class=.+ +/>\\)\n+" 1)
     ("\\(<div.*>\\)\n+" 1))))

;; WIP
;; Google Blogger service is inserting newline to the blog if its HTML file
;; have newline. So I created to minify html that org-mode output. But
;; it didn't solve problem.
;; Because org-mode insert newline into html content's first and end.
;; For example <p>\n CONTENT \n</p>.
;; So this program deleting those newline by Emacs's regexp replacement.
;; Therefore if you use HTML tag in your blog, this program may delete
;; newline from your blog's html tag(for example, in org-src-block's html)
(defvar bpe:tmp-path-file-name "/tmp/emacs-bpe-tmp-file.html")
(defvar bpe:minify-html-path
  nil
  ;; (let ((path
  ;;  (format "%sminify_html.pl" (file-name-as-directory default-directory))))
  ;;  (if (file-exists-p path) path ""))
  )

(defun bpe:create-html-and-fetch-filename ()
  (let* ((org->html-file-name
          (replace-regexp-in-string
           "org$" "html" buffer-file-truename)))
    (bpe:export-html)
    (bpe:replace-newline org->html-file-name)
    org->html-file-name))

(defun bpe:insert-template ()
  "Insert blog template"
  (interactive)
  (insert bpe:template))

(defun bpe:export-html ()
  (interactive)
  (org-html-export-to-html nil nil nil t))

(defun bpe:export-html-old-version ()
  (with-no-warnings
    (let ((version (string-to-number
                    (replace-regexp-in-string "\\." "" org-version))))
      (if (< version 794)
          (funcall 'org-export-as-html 23 nil nil t)
        (funcall 'org-export-as-html 23 nil nil nil 'string)))))

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
    (if (and bpe:minify-html-path
             (file-exists-p bpe:minify-html-path))
        (bpe:replace-newline-by-minify-pl file)
      (find-file file)
      (bpe:replace bpe:removing-list)
      (save-buffer)
      (switch-to-buffer base))))

(defun bpe:replace-newline-by-minify-pl (file)
  (if (file-exists-p file)
      (shell-command (format "perl %s %s %s" bpe:minify-html-path file
                             bpe:tmp-path-file-name))
    (error (format "%s not found" file))))

(defun bpe:get-option (title-or-tag)
  (interactive)
  (goto-char (point-min))
  (let ((option (case title-or-tag
                  (:title "TITLE")
                  (:tag   "TAGS"))))
    (if (re-search-forward
         (format "^#.*%s: \\(.+\\)" option) nil t)
        (match-string 1))))

(defun bpe:format-title (raw-title)
  (let ((title (format "'%s'" raw-title))
        (name  (format "'%s'" bpe:blog-name)))
    (bpe:format "--blog" name "--title" title)))

(defun bpe:get-tags ()
  "Get tag(s) from current file."
  (lexical-let
      ((tags
        (condition-case err
            (mapconcat 'identity
                       (split-string (bpe:get-option :tag) " ") ",")
          (error ""))))
    (if tags (format " --tags \"%s\" " tags) "")))

(defun bpe:get-draft-string ()
  (if (and current-prefix-arg
           bpe:use-real-post-when-updating)
      ""
    " --draft "))

;;;###autoload
(defun bpe:post-article (&optional update)
  "Post current file that converted html to your blog of Google Blogger.
 If you pushed C-u before execute this command, then post article after
delete same title's article."
  (interactive)
  (lexical-let*
      ((title      (or (bpe:get-option :title)
                       (read-string "title here: ")))
       (blog-and-title (bpe:format-title title))
       (content        (bpe:get-content))
       (delete         (bpe:get-delete-string blog-and-title))
       (post           (bpe:get-post-string   blog-and-title content))
       (command        (if (bpe:update-required-p update)
                           (concat delete " && " post)
                         post)))
    (async-shell-command command "*bpe*")))

(defun bpe:get-content ()
  (if (string-match "\\.org$" (buffer-name))
      (bpe:create-html-and-fetch-filename)
    (buffer-string)))

(defun bpe:get-post-string (blog-and-title content)
  (bpe:format bpe:command "post" (bpe:get-draft-string)
              "-u" bpe:account (bpe:get-tags) blog-and-title content))

(defun bpe:get-delete-string (blog-and-title)
  (bpe:format bpe:command "delete" blog-and-title
              (if bpe:no-ask "--yes" "")))

(defun bpe:format (&rest list)
  (mapconcat 'identity list " "))

(defun bpe:update-required-p (&optional force)
  (or bpe:update-by-default current-prefix-arg force))

(defun bpe:update-article ()
  (interactive)
  (bpe:post-article t))

(provide 'bpe)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; bpe.el ends here
