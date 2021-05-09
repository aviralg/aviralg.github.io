(package-initialize)
(require 'ob-css)
(require 'ox-rss)
(require 'ox-publish)

;; This expression bypasses the org-publish cache and publishes everything on
;; every run. Without this, changes to html preamble, postamble and style files
;; do not trigger publishing of pages whose timestamps have not changed since
;; last run.
;; https://emacs.stackexchange.com/questions/44534/org-mode-sitemap-not-updated-after-re-publish
(org-publish-remove-all-timestamps)

(defun create-source-link (filename)
  (concat "./" (file-name-nondirectory filename) ".html"))

(defun create-postamble (options)
  (let ((input-file (plist-get options :input-file)))
    (concat
     "<i class='far fa-copyright fa-sm'></i> Aviral Goel 2021 <a class='source-link' href='"
     (create-source-link input-file)
     "'><i class='fas fa-code'></i></a>")))

(defun plist-keys-helper (plist keyp counter)
  (if (and (> counter 0) (not (null plist)))
      (let ((rest (plist-keys-helper (cdr plist) (not keyp) (- counter 1))))
        (if keyp
            (cons (car plist) rest)
          rest))
    '()))

(defun plist-keys (plist)
  (plist-keys-helper plist 't 10))

(defun create-preamble (options)
  (if (not (string-suffix-p "content/index.org" (plist-get options :input-file)))
      (concat
       "<div id='#preamble'><a href='http://aviral.io'><i class='fas fa-home'></i></a></div>")))

(defun read-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(setq html-head (read-file "templates/head.html"))

;; style files for css code classes are present in css/monokai.css
;; https://stackoverflow.com/questions/12169667/org-mode-uses-current-color-theme-to-publish
(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist `(("articles"
                                   :base-directory "content"
                                   :publishing-directory "www"
                                   :base-extension "org"
                                   :recursive t
                                   :section-numbers nil
                                   :publishing-function org-html-publish-to-html
                                   :with-author nil
                                   :with-toc nil
                                   :html-head-include-scripts nil
                                   :html-head-include-default-style nil
                                   :html-head ,html-head
                                   :html-validation-link nil
                                   :html-footnotes-section "<div class=\"footnotes\" title=\"%s\">%s</div>"
                                   :html-preamble ,'create-preamble
                                   :html-postamble ,'create-postamble)

                                  ("articles-source"
                                   :base-directory "content"
                                   :publishing-directory "www"
                                   :base-extension "org"
                                   :recursive t
                                   :htmlized-source t
                                   :publishing-function org-org-publish-to-org)

                                  ("static"
                                   :base-directory "static"
                                   :publishing-directory "www/static/"
                                   :base-extension any
                                   :recursive t
                                   :publishing-function org-publish-attachment)

                                  ("rss"
                                   :base-directory "content"
                                   :base-extension "org"
                                   :html-link-home "http://aviral.io/"
                                   :html-link-use-abs-url t
                                   :rss-extension "xml"
                                   :publishing-directory "www"
                                   :publishing-function (org-rss-publish-to-rss)
                                   :section-numbers nil
                                   :exclude ".*"
                                   :include ("rss.org")
                                   :table-of-contents nil)

                                  ("all"
                                   :components ("articles" "articles-source" "static" "rss"))))
