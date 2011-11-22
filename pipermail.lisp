(ql:quickload '(drakma cl-ppcre chipz))

;; Portmanteau of Kragen and Craven...
;; though considering the site, I don't think this scraping is contemptible behavior. :)
(defpackage :kraven
  (:use :cl)
  (:import-from :drakma #:http-request))

(in-package :kraven)

(defparameter *storage-base* "/home/bsbutler/docs/books/people/kragen/"
  "The directory to store the mailing list transcripts in.")

(defparameter *kragen-lists*
  '((:discuss . "http://lists.canonical.org/pipermail/kragen-discuss/")
    (:forwards . "http://lists.canonical.org/pipermail/kragen-fw/")
    (:hacks . "http://lists.canonical.org/pipermail/kragen-hacks/")
    (:journal . "http://lists.canonical.org/pipermail/kragen-journal/")
    (:thinking-out-loud . "http://lists.canonical.org/pipermail/kragen-tol/"))
  "A mapping of keywords to pipermail URLs for Kragen Sitaker's mailing lists,
serving as his blog and thinking grounds.")

(defun get-tarball-urls (string)
  "Find all links pointing to *.gz files."
  (let ((results nil))
    (cl-ppcre:do-register-groups (result)
        ("<[A|a] href=\"(.*.gz)\">" string)
      (push result results))
    results))

;; TODO
;; Read LOOP for Black Belts and apply.
;; Seems I don't understand loop :with scoping.
(defun main ()
  "Loop over *kragen-lists*, retrieving the list of gzipped files on each list.
Create a directory named after the list under *STORAGE-BASE*, then download and
decompress each gzipped file into that directory and inform the user."
  (loop for (name . list-url) in *kragen-lists*
     with directory = (string-downcase (string name))
     with gz-files = (get-tarball-urls (http-request list-url))
     with path = (merge-pathnames (concatenate 'string directory "/") *storage-base*)
     do (ensure-directories-exist path)
        (loop for gz-file in gz-files
           do (let ((out-file (merge-pathnames (pathname-name gz-file) path)))
                (unless (probe-file out-file)
                (with-open-file (out out-file :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
                  (chipz:decompress out 'chipz:gzip
                                    (http-request (concatenate 'string list-url gz-file))))
                (format t "Successfully downloaded and extracted ~
~a to ~a.~%" gz-file out-file))))))
