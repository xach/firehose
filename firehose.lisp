;;;; firehose.lisp

(defpackage #:firehose
  (:use #:cl #:cxml)
  (:export #:blog-post
           #:post-new-entry
           #:blog-list))

(in-package #:firehose)

(defun blog-post (title content &key draft date categories)
  (with-xml-output (make-string-sink)
    (with-namespace (nil "http://www.w3.org/2005/Atom")
      (with-element "entry"
        (when date
          (with-element "published"
            (text (format-time:iso-8601z nil date))))
        (with-element "title"
          (attribute "type" "text")
          (text title))
        (with-element "content"
          (attribute "type" "html")
          (text content))
        (when categories
          (setf categories (if (listp categories)
                               categories
                               (list categories)))
          (dolist (category categories)
            (with-element "category"
              (attribute "scheme" "http://www.blogger.com/atom/ns#")
              (attribute "term" category))))
        (when draft
          (with-namespace ("app" "http://www.w3.org/2007/app")
            (with-element* ("app" "control")
              (with-element* ("app" "draft")
                (text "yes")))))))))

(defun post-new-entry (endpoint body)
  (grout:authenticated-request endpoint
                               :content (sb-ext:string-to-octets body
                                                                 :external-format :utf-8)
                               :method :post
                               :content-type "application/atom+xml"))

(defun post-file (endpoint file)
  (post-new-entry endpoint
                  (alexandria:read-file-into-string file
                                                    :external-format :utf-8)))
                                                                  


(defclass blog ()
  ((title
    :initarg :title
    :accessor title)
   (post-url
    :initarg :post-url
    :accessor post-url)))

(defmethod print-object (blog stream)
  (print-unreadable-object (blog stream :type t :identity t)
    (format stream "~S" (title blog))))

(defvar *blog-list-url* "http://www.blogger.com/feeds/default/blogs")
(defvar *atom-ns* "http://www.w3.org/2005/Atom")


(defun post-link-url (node)
  (and (equal (stp:local-name node) "link")
       (equal (stp:namespace-uri node) *atom-ns*)
       (equal (stp:attribute-value node "rel")
              "http://schemas.google.com/g/2005#post")
       (stp:attribute-value node "href")))

(defun extract-blog-list (source)
  (map 'list
       (lambda (node)
         (let ((titles (stp:filter-children
                        (stp:of-name "title" *atom-ns*)
                        node))
               (links (find-if 'post-link-url
                               (stp:filter-children (stp:of-name "link" *atom-ns*)
                                                    node))))
           (let ((title (stp:data (stp:first-child (aref titles 0)))))
             (make-instance 'blog
                            :title title
                            :post-url (stp:attribute-value links "href")))))
       (stp:filter-recursively (stp:of-name "entry" *atom-ns*) source)))

       

(defun blog-list-xml ()
  (cxml:parse (grout:authenticated-request *blog-list-url*)
              (stp:make-builder)))


(defun blog-list ()
  (extract-blog-list (blog-list-xml)))

