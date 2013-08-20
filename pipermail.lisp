(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(drakma cl-ppcre chipz)))

;; Portmanteau of Kragen and Craven...
;; though considering the site, I don't think this scraping is contemptible behavior. :)
(defpackage :kraven
  (:use :cl)
  (:import-from :drakma #:http-request))

(in-package :kraven)

(defparameter *storage-base* "/home/redline/Downloads/kragen/"
  "The directory to store the mailing list transcripts in.")

(defparameter *kragen-lists*
  '((:discuss . "http://lists.canonical.org/pipermail/kragen-discuss/")
    (:forwards . "http://lists.canonical.org/pipermail/kragen-fw/")
    (:hacks . "http://lists.canonical.org/pipermail/kragen-hacks/")
    (:journal . "http://lists.canonical.org/pipermail/kragen-journal/")
    (:thinking-out-loud . "http://lists.canonical.org/pipermail/kragen-tol/"))
  "A mapping of keywords to pipermail URLs for Kragen Sitaker's mailing lists.")

(defun get-tarball-urls (string fn &optional base-url)
  "Find all links pointing to *.gz files and call FN with each link. If BASE-URL
is present, prepend each link with it."
  (cl-ppcre:do-register-groups (result) ("<[A|a] href=\"(.*.gz)\">" string)
    (funcall fn (concatenate 'string base-url result))))

(defun download-gz-file (url directory)
  "Download a gzipped file from URL, decompress it and store it under DIRECTORY."
  (let ((pathname (merge-pathnames (pathname-name url) directory)))
    (with-open-file (out pathname
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (chipz:decompress out 'chipz:gzip (http-request url)))
    (format t "Successfully downloaded and extracted ~a to ~a.~%" url pathname)))

(defun main ()
  "Loop over *kragen-lists*, retrieving the list of gzipped files on each list.
Create a directory named after the list under *STORAGE-BASE*, then download and
decompress each gzipped file into that directory and inform the user."
  (loop for (name . list-url) in *kragen-lists*
     with directory = (string-downcase (string name))
     with path = (merge-pathnames (format nil "~a/" directory) *storage-base*)
     do (ensure-directories-exist path)
        (get-tarball-urls (http-request list-url)
                          (lambda (gz) (download-gz-file gz path))
                          list-url)))
