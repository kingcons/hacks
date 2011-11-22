;; Compile to an executable and place on $PATH with...
;; sbcl --load sotd.lisp ; then
;; (sb-ext:save-lisp-and-die "sotd" :toplevel #'sotd::main :executable t)

;; TODO:
;; This is great for naggum + kragen but...
;; * Needs a "don't recurse" flag.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (ql:quickload '(split-sequence cl-pdf-parser cl-fad cl-store))))

(defpackage :sotd
  (:use :cl)
  (:import-from :cl-fad #:walk-directory
                        #:directory-pathname-p)
  (:import-from :split-sequence #:split-sequence))

(in-package :sotd)

(defvar *recurse-p* t)
(defvar *pdfs* (make-hash-table :test #'equal))

(defun pdf-page-count (path)
  ;; Note that we patched read-pdf-file with a handler-case to skip ERRORs
  (let ((pdf (pdf::read-pdf-file path)))
    (when pdf
      (length (slot-value (slot-value pdf 'pdf::root-page) 'pdf::pages)))))

(defun count-pdf-pages (&optional (path "/home/bsbutler/docs/books/Papers/"))
  (walk-directory path
                  (lambda (path)
                    (when (and (not (directory-pathname-p path))
                               (string= "pdf" (pathname-type path)))
                      (let ((page-count (pdf-page-count path)))
                        (when page-count
                          (cond ((< page-count 41)
                                 (push path (gethash :small *pdfs*)))
                                ((< page-count 101)
                                 (push path (gethash :medium *pdfs*)))
                                ((> page-count 100)
                                 (push path (gethash :large *pdfs*))))))))))

(defun save-pdf-index ()
  (with-open-file (out "/home/bsbutler/projects/scripts/papers.db"
                       :direction :output :if-does-not-exist :create
                       :if-exists :supersede :element-type '(unsigned-byte 8))
    (cl-store:store *pdfs* out)))

(defun load-pdf-index ()
  (with-open-file (in "/home/bsbutler/projects/scripts/papers.db"
                      :element-type '(unsigned-byte 8))
    (setf *pdfs* (cl-store:restore in))))

(defun choose (items)
  (nth (random (length items) (make-random-state t)) items))

(defun choose-pdf (&optional path)
  (let ((results nil))
    (flet ((valid-p (path)
             (when (and (not (directory-pathname-p path))
                        (member path (gethash :small *pdfs*) :test #'equal))
               (push path results))))
      (when path
        (loop for file in (cl-fad:list-directory path) do (valid-p file)))
      (if *recurse-p*
          (choose (gethash :small *pdfs*))
          (choose results)))))

(defun choose-code (&optional (path (sb-posix:getcwd)) whitelist)
  (let ((whitelist (or whitelist
                       '("c" "h" "cpp" "hpp" "py" "lisp" "js" "asm" "lua"
                         "sml" "clj" "pl" "lhs" "scm" "haml" "rb" "java"
                         "scala" "ml" "el" "tex")))
        (results nil))
    (flet ((valid-p (path)
             (when (and (not (directory-pathname-p path))
                        (member (pathname-type path)
                                whitelist :test #'equal))
               (push (namestring path) results))))
      (if *recurse-p*
          (walk-directory path #'valid-p)
          (loop for file in (cl-fad:list-directory path) do (valid-p file))))
    (choose results)))

(defun choose-file (&optional (path (sb-posix:getcwd)))
  (let ((results nil))
    (flet ((valid-p (path)
             (when (not (directory-pathname-p path))
               (push (namestring path) results))))
      (if *recurse-p*
          (walk-directory path #'valid-p)
          (loop for file in (cl-fad:list-directory path) do (valid-p file))))
    (choose results)))

(defun display-help ()
  (format t "~
Usage:
If --no-recurse is the last arg, only search on PWD or PATH.~%
  sotd         # return a randomfile under the current directory
  sotd PATH    # return a randomfile under PATH
  sotd -c PATH # return a source code file under PATH
  sotd -p      # return a pdf from the index
  sotd -u      # update pdf index. takes 1-2 minutes.
  sotd -h      # print this help message. :)~%~%"))

(defun main ()
  (let* ((args (rest sb-ext:*posix-argv*))
         (option (first args))
         (path (second args)))
    (when (member "--no-recurse" args :test #'equal)
        (setf *recurse-p* nil))
    (cond ((null option)
           (format t "~a~%" (choose-file)))
          ((string= "-h" option)
           (display-help))
          ((and (string= "-c" option) path)
           (format t "~a~%" (if (third args)
                                (choose-code path (list (third args)))
                                (choose-code path))))
          ((string= "-p" option)
           (load-pdf-index)
           (format t "~a~%" (if path
                                (choose-pdf path)
                                (choose-pdf))))
          ((string= "-u" option)
           (count-pdf-pages)
           (save-pdf-index))
          ((probe-file option)
           (format t "~a~%" (choose-file option)))
          (t (display-help)))
    (sb-ext:quit)))

#|
Papers: sotd -p or sotd -p ~/docs/books/Papers --no-recurse
Favorites: sotd -c ~/projects/favorite_hackers/
# This handles most use cases. Only real problem is that reading/favorites is biased to larger projects.
# What to do? Check for git repo or a level higher with *NO* files.
Reading: sotd -c ~/projects/reading/
Naggum: sotd ~/projects/reading/naggum
Kragen: sotd ~/projects/reading/kragen
Hacks: sotd ~/projects/reading/kragen/hacks/
T-O-L: sotd ~/projects/reading/kragen/thinking-out-loud
|#
