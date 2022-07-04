(package-initialize)
(require 'ob-css)
(require 'ox-rss)
(require 'ox-publish)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-file (file-path)
  "Return file-path's content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun replace-substring (str old new)
  (string-replace old new str))

;; This function is supposed to be provided by emacs.
;; I have created my own version because the built-in
;; raises (void-function compat--string-trim-left)
(defun file-name-with-extension (filename ext)
  (concat (file-name-sans-extension filename) "." ext))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq NAVBAR-TEMPLATE (read-file "templates/navbar.html"))
(setq HEADER-ARTICLE-TEMPLATE (read-file "templates/header-article.html"))
(setq HEADER-INDEX-TEMPLATE (read-file "templates/header-index.html"))
(setq HEADER-EMPTY-TEMPLATE (read-file "templates/header-empty.html"))
(setq FOOTER-FULL-TEMPLATE (read-file "templates/footer-full.html"))
(setq FOOTER-EMPTY-TEMPLATE (read-file "templates/footer-empty.html"))
(setq HEAD-TEMPLATE (read-file "templates/head.html"))
(setq INDEX-ENTRY-TEMPLATE (read-file "templates/index-entry.html"))
(setq INDEX-CATEGORY-TEMPLATE (read-file "templates/index-category.html"))
(setq INDEX-SERIES-TEMPLATE (read-file "templates/index-series.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq BASE-DIRECTORY (expand-file-name "./content"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun infer-series-category (path)
  (let ((components (reverse (split-string path  (regexp-quote "/")))))
    (list (nth 2 components) (nth 1 components))))

(defun create-source-link (filename)
  (concat "./" (file-name-nondirectory filename) ".html"))

(defun create-output-link (filename)
  (file-name-with-extension filename "html"))

(defun make-tags (content)
  (let ((tags (split-string content "," t "\s")))
    (mapconcat (lambda (tag) (format "<a class='tag badge' href='/'>#%s</a>" tag)) tags "")))

(defun make-category (category link)
  (if (string= category "")
      ""
    (format "<a class='category' href='%s'>%s</a>" link (string-trim category))))

(defun make-categories (series category)
  (string-join (list (make-category "home" "/")
                     (make-category series (concat "/" series))
                     (make-category category "/"))
               "&nbsp;▶&nbsp;"))

(defun interpolate-template (options template-selector)
  (let* ((metadata (metadata-build options))
         (template (funcall template-selector metadata)))
    (metadata-apply metadata template)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METADATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun metadata-key (key)
    (concat "$" (upcase key) "$"))

(defun metadata-get (metadata key &optional default)
  "Looks up key in metadata and returns '' if key is not present"
  (gethash (metadata-key key) metadata (if default default "")))

(defun metadata-add (metadata key value &optional overwrite)
  "Adds key to value in metadata only if it is absent"
  (let ((key (metadata-key key))
        (match (metadata-get metadata key "")))
    ;; if match is not found or overwrite is true, add metadata
    (when (or (string= "" match) overwrite)
      (puthash key value metadata))))

(defun metadata-print (metadata)
  "Extract user supplied org-mode keywords."
  ;; match against keys of exported-data to find keywords
  (print metadata)
  (maphash (lambda (key value)
             (print (format "%s: %s" key value)))
           metadata))

(defun metadata-apply (metadata content)
  (let ((result content))
    (maphash (lambda (key value)
               (setq result (replace-substring result key value)))
             metadata)
    result))

(defun metadata-build (options)
  "Build metadata from file properties and user supplied org-mode keywords."
  (let* ((metadata (make-hash-table :test 'equal))
         (exported-data (plist-get options :exported-data))
         (input-buffer (plist-get options :input-buffer))
         (input-file (plist-get options :input-file))
         (relative-path (file-relative-name input-file BASE-DIRECTORY))
         (description (plist-get options :description))
         (series-category (infer-series-category input-file))
         (series (nth 0 series-category))
         (category (nth 1 series-category)))

    ;; default values
    (metadata-add metadata "year" (format-time-string "%Y"))
    (metadata-add metadata "author" "Aviral Goel")
    (metadata-add metadata "subtitle" "")
    (metadata-add metadata "date" "")
    (metadata-add metadata "modified" (file-last-modifed-date input-file))

    ;; extract properties provided by ox-publish
    (metadata-add metadata "input-buffer" input-buffer)
    (metadata-add metadata "input-file" input-file)
    (metadata-add metadata "output-file" (create-output-link input-file))
    (metadata-add metadata "formatted-input-file" (create-source-link input-file))
    (metadata-add metadata "description" description)
    (metadata-add metadata "homepage" (string= "index.org" relative-path))

    ;; extract user supplied org-mode keywords.
    (maphash (lambda (key value)
               (pcase key
                 ;; if keyword found update metadata with key and value
                 (`(keyword ,key-data)
                  (let ((key2 (plist-get key-data :key))
                        (value2 (plist-get key-data :value)))
                    (metadata-add metadata key2 value2)))))
             exported-data)

    ;; add series and category
    (metadata-add metadata "series" series)
    (metadata-add metadata "category" category)
    (metadata-add metadata "category-html" (make-categories series category))
    (metadata-add metadata "tags-html" (make-tags (metadata-get metadata "tags" "")))

    metadata))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PREAMBLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-header-template (metadata)
  (let* ((include-header (metadata-get metadata "include-header" "true"))
         (input-buffer (metadata-get metadata "input-buffer" ""))
         (include-p (string= "true" include-header))
         (index-p (string= "index.org" input-buffer))
         (homepage-p (metadata-get metadata "homepage" nil)))
    ;;(metadata-print metadata)
    (cond (homepage-p HEADER-EMPTY-TEMPLATE)
          (index-p HEADER-INDEX-TEMPLATE)
          ((not include-p) HEADER-EMPTY-TEMPLATE)
          (t HEADER-ARTICLE-TEMPLATE))))

(defun create-preamble (options)
  (concat NAVBAR-TEMPLATE
          (interpolate-template options #'select-header-template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POSTAMBLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-footer-template (metadata)
  (let* ((include-footer (metadata-get metadata "include-footer" "true"))
         (include-p (string= "true" include-footer)))
    (if include-p FOOTER-FULL-TEMPLATE FOOTER-EMPTY-TEMPLATE)))

(defun create-postamble (options)
  (interpolate-template options #'select-footer-template))



;; (if (not (string-suffix-p "content/index.org" (plist-get options :input-file)))
;;(concat
;; "<div id='#preamble'><a href='http://aviral.io'><i class='fas
;;fa-home'></i></a></div>")))

;;(defun create-header (metadata)
;;  (format (read-file HEADER_TEMPLATE_FILE)
;;          (gethash "title" metadata "")
;;          (gethash "subtitle" metadata "")
;;          (gethash "author" metadata "Aviral Goel")
;;          (make-categories (gethash "kind" metadata "notes") (gethash "category" metadata ""))
;;          (gethash "date" metadata "")
;;          (make-tags (gethash "tags" metadata ""))))
;;
;;(defun create-preamble (options)
;;  (print options)
;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ;; (let ((data (plist-get options :exported-data)))                                                    ;;
;;  ;;   (print "***********************************Keys************************************************") ;;
;;  ;;   (maphash (lambda (key value)                                                                      ;;
;;  ;;              (pcase key                                                                             ;;
;;  ;;                (`(keyword ,key-data) (print (plist-get key-data :key))                              ;;
;;  ;;                                      (print (plist-get key-data :value)))                           ;;
;;  ;;              ))                                                                                     ;;
;;  ;;            data)                                                                                    ;;
;;  ;;   (print "***********************************END************************************************")  ;;
;;  ;;   )                                                                                                 ;;
;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (let ((navbar (read-file NAVBAR_TEMPLATE_FILE)))
;;    (concat navbar
;;            (let ((metadata (extract-metadata (plist-get options :exported-data))))
;;              (if (string= "true" (gethash "include_header" metadata "true"))
;;                  (create-header metadata)
;;                "")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq org-html-self-link-headlines t)
;;(setq org-html-doctype "xhtml5")

;; style files for css code classes are present in css/monokai.css
;; https://stackoverflow.com/questions/12169667/org-mode-uses-current-color-theme-to-publish
(setq org-html-htmlize-output-type 'css)

;; This expression bypasses the org-publish cache and publishes everything on
;; every run. Without this, changes to html preamble, postamble and style files
;; do not trigger publishing of pages whose timestamps have not changed since
;; last run.
;; https://emacs.stackexchange.com/questions/44534/org-mode-sitemap-not-updated-after-re-publish
(org-publish-remove-all-timestamps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-publish-project-alist `(("articles"
                                   :base-directory ,BASE-DIRECTORY
                                   :publishing-directory "www"
                                   :base-extension "org"
                                   :recursive t
                                   :section-numbers t
                                   :publishing-function org-html-publish-to-html
                                   :with-title nil
                                   :with-author nil
                                   :with-toc nil
                                   :html-doctype "xhtml5"
                                   :html-self-link-headlines t
                                   :html-head-include-scripts nil
                                   :html-head-include-default-style nil
                                   :html-head ,HEAD-TEMPLATE
                                   :html-validation-link nil
                                   :html-footnotes-section "<div class=\"footnotes\" title=\"%s\">%s</div>"
                                   :html-preamble ,'create-preamble ;;,preamble
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

                                  ("icons"
                                   :base-directory "icons"
                                   :publishing-directory "www"
                                   :base-extension any
                                   :recursive t
                                   :publishing-function org-publish-attachment)

                                  ("miscellaneous"
                                   :base-directory "miscellaneous"
                                   :publishing-directory "www"
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
                                   :components ("articles" "articles-source" "static" "icons" "miscellaneous" "rss"))))

;; https://emacs.stackexchange.com/a/57684
(defun build-site ()
  (let ((debug-on-error t))
      (org-publish-all)))
