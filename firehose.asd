;;;; firehose.asd

(asdf:defsystem #:firehose
  :depends-on (#:cxml
               #:grout
               #:format-time
               #:cl-ppcre
               #:cxml-stp)
  :serial t
  :components ((:file "firehose")))

