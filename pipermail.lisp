(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(drakma cl-ppcre chipz)))

;; Portmanteau of Kragen and Craven...
;; though considering the site, I don't think this scraping is contemptible behavior. :)
(defpackage :kraven
  (:use :cl))

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
    (ensure-directories-exist pathname)
    (with-open-file (out pathname
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (chipz:decompress out 'chipz:gzip (drakma:http-request url)))
    (format t "Successfully downloaded and extracted ~a to ~a.~%~%" url pathname)))

(defun download-mailing-list (list-url target-dir)
  "Download the monthly archives at LIST-URL into TARGET-DIR and extract them."
  (get-tarball-urls (drakma:http-request list-url)
                    (lambda (gz) (download-gz-file gz target-dir))
                    list-url))

(defun main ()
  "Loop over *kragen-lists*, retrieving the list of gzipped files on each list.
Create a directory named after the list under *STORAGE-BASE*, then download and
decompress each gzipped file into that directory and inform the user."
  (loop for (list-name . list-url) in *kragen-lists*
     for path = (merge-pathnames (format nil "~(~a~)/" list-name) *storage-base*)
     do (download-mailing-list list-url path)))
