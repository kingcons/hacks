(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(drakma cl-ppcre chipz)))

;; Portmanteau of Kragen and Craven...
;; though considering the site, I don't think scraping is contemptible behavior. :)
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

(defun do-tarball-urls (list-url fn)
  "Find all links pointing to *.gz files and call FN with each one."
  (cl-ppcre:do-register-groups (result)
      ("<[A|a] href=\"(.*.gz)\">" (drakma:http-request list-url))
    (funcall fn (concatenate 'string list-url result))))

(defun download-gz-file (url directory)
  "Download the gzipped file at URL, decompress it and store it under DIRECTORY."
  (let ((pathname (merge-pathnames (pathname-name url) directory)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (chipz:decompress out 'chipz:gzip (drakma:http-request url)))
    (format t "Successfully downloaded and extracted ~a to ~a.~%~%" url pathname)))

(defun main ()
  "Store the monthly archives of each kragen-mail list under *STORAGE-BASE*."
  (loop for (list-name . list-url) in *kragen-lists*
     for path = (merge-pathnames (format nil "~(~a~)/" list-name) *storage-base*)
     do (do-tarball-urls list-url (lambda (gz) (download-gz-file gz path)))))
