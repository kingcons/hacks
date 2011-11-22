;#!/usr/bin/sbcl --script
;(declaim (sb-ext:muffle-conditions style-warnings))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (ql:quickload '(trivial-raw-io osicat external-program
                    split-sequence bordeaux-threads))))

(defpackage :everyday-songs
  (:use :common-lisp
        :trivial-raw-io
        :external-program
        :split-sequence
        :osicat
        :bordeaux-threads)
  (:import-from :trivial-raw-io :get-char))

(in-package :everyday-songs)

;;; NOTES:
;;; favorite improv levels: 0, 3. Second favorites: 6, 8
;;; about 30 megs for all the oggs.
;;; Implement phrase and sample classes. Implement play-it generic, defmethods.

;;;; DESIGN:
;;;; I've got some ideas for classes. I could have a sound class which sample and phrase classes inherit from and a defgeneric play on sound.
;;;; I should design a "workspace" class and I can have a "level" class inherit from that for fucks sake.
;;;; What about looping and the threading needs of that? I'm against the hard-coding of folders. Osicat halp?

;; Data Structures

(defvar *loops* (make-hash-table))
(defvar *samples* (make-array 10 :fill-pointer 0))
(defvar *sampleset* nil)
(defparameter *folders* #("cloudlevel/" "heartlevel/" "lightlevel/"
                          "linelevel/" "newsoccerlevel/" "retrolevel/"
                          "robotlevel/" "soccerlevel/" "somanywayslevel/"))
(defvar *unused-keys* '(#\w #\e #\r #\t #\i #\o #\p #\[
                        #\s #\d #\f #\g #\k #\l #\; #\'))

(defun reset ()
  (setf *samples* (make-array 10 :adjustable t :fill-pointer 0))
  (setf *sampleset* nil))

;; Classes

;;; TODO!
;; PHRASES SUPPORT, particularly get-samples storage...
;; For phrases, samples?

;; Init Code

(defun get-samples (path)
  (loop for folder across *folders* do
    (let ((table (make-hash-table)))
      (add-files folder table path)
      (vector-push (cons folder table) *samples*)))
  (vector-push (cons "workspace" (make-hash-table)) *samples*)
  (setf *sampleset* (cdr (aref *samples* 0))))

(defun add-files (folder table path)
  (let ((abspath (concatenate 'string path folder))
        (keys '(#\w #\e #\r #\t #\i #\o #\p #\[
                #\a #\s #\d #\f #\j #\k #\l #\;
                #\z #\x #\c #\v #\n #\m #\, #\.
                #\g #\b #\y #\u)))
    (walk-directory abspath
                    #'(lambda (x)
                        (let ((filename (native-namestring
                                         (absolute-pathname x))))
                          (when (ogg-p filename)
                            (let ((char (pop keys)))
                              (setf (gethash char table) filename))))))))

(defun ogg-p (path)
  (string= "ogg" (subseq path (- (length path) 3))))

;; Main Loop

(defun play (&optional (path "~/Desktop/jonathan_mak/Everyday Shooter/es/"))
  (reset)
  (get-samples (native-namestring (truename path))) ;; for now...
  (loop for key = (get-char) until (char= key #\q) do
       (parse-key key))
  (format t "Thanks for playing!~%")
  (start "killall" '("ogg123")))

;; Parser

(defun parse-key (char)
  (cond ((char= #\h char) (progn
                            (show-keys)
                            (show-folders)))
        ((char= #\` char) (start "killall" '("ogg123")))
        ((char= #\\ char) (record-phrase))
        ((char= #\= char) (setf (cdr (aref *samples* 9)) (make-hash-table)))
        ((char= #\- char) (loop-phrase))
        ((and (char>= char #\0)
              (char<= char #\9)) (setf *sampleset* (cdr (aref *samples*
                                                              (parse-integer (string char))))))
        (t (let ((value (gethash char *sampleset*)))
             (if (listp value)
                 (parse-phrase value)
                 (play-sample value))))))

(defun parse-phrase (keytimes)
  ;; it seems like there is an extra list around the path in each pair...but only coming from loop-phrase...
  (loop for pair in keytimes do
       (sleep (car pair))
       (play-sample (cdr pair)))) ;; ugly

;; Playback.

(defun play-sample (path)
  (start "ogg123" (list path)))

(defun loop-sample (path)
  (loop (run "ogg123" (list path))))

(defun loop-phrase () ;; can't exit cleanly from loop-phrase...
  (let* ((key (read-char))
         (keytimes (gethash key (cdr (aref *samples* 9)))))
    (multiple-value-bind (value present) (gethash key *loops*)
      (if present
          (progn
            (destroy-thread value) ;; we would really like to stop the loop more gracefully
            (remhash key *loops*))
          (setf (gethash key *loops*)
                (make-thread #'(lambda ()
                                 (loop (parse-phrase keytimes)))))))))

;; UI/Help Functions

(defun show-keys ()
  (maphash #'(lambda (k v)
               (format t "The ~a key will play ~a.~%" k
                       (last (split-sequence #\/ v))))
           *sampleset*))

(defun show-folders ()
  (let ((count 0))
    (loop for pair across *samples* do
      (when (not (null count))
        (format t "The ~a key will change to folder ~a.~%" count (car pair)))
      (incf count))))

;; Custom Loops

(defun record-phrase ()
  (let ((phrase nil)
        (start (/ (get-internal-real-time) 1000.0)))
    (loop named outer for key = (read-char) do
      (cond ((char= key #\q) (progn
                               (save-phrase (reverse phrase))
                               (return-from outer 'done)))
            ((char= key #\\) (setf phrase nil
                                   start (/ (get-internal-real-time) 1000.0)))
            (t (progn
                 (let ((current (/ (get-internal-real-time) 1000.0)))
                   (play-sample (gethash key *sampleset*))
                   (push (cons (- current start) (gethash key *sampleset*))
                         phrase)
                   (setf start current))))))))

(defun save-phrase (keytimes)
  (let ((phrase (remove-if #'(lambda (x) (null (cdr x))) keytimes))) ;; hideous hack, move into record-phrase? break up record-phrase?
    (setf (gethash (pop *unused-keys*)
                   (cdr (aref *samples* 9)))
          phrase)))

#+sbcl(play)
