(ql:quickload 'coleslaw)
(ql:quickload 'plump)

;; Make sure to use a pathname (the #p) for this, plump won't treat a plain string as a file to be opened.
(defvar *wp-xml* #p"/Users/brit/projects/linode-retirement/wordpress.2010-09-14.xml")

(defvar *doc* (plump:parse *wp-xml*))

(defvar *posts* (plump-dom:get-elements-by-tag-name *doc* "item"))

;; For those wondering, :test #'equal is there so we can use string keys. Read Practical Common Lisp or google to learn more.
(defvar *post-id-map* (make-hash-table :test #'equal))

(defun extract (key node)
  (let ((value (first (plump-dom:get-elements-by-tag-name node key))))
    (plump:text value)))

;; Yes, I'm using a private coleslaw function here but I wrote coleslaw so ... uh ... do what I want!
;; And in case you were wondering, handler-case is lisp's try/catch equivalent and I'm pretty much doing "rescue nil" here.
(defun fixed-title (title)
  (handler-case (coleslaw::slugify title)
    (simple-error () :junk)))

(loop for post in *posts*
      do (let ((title (extract "title" post))
               (id (extract "wp:post_id" post)))
           (setf (gethash id *post-id-map*) (fixed-title title))))

(ql:quickload 'cl-ppcre)

;; Can you tell I did all this in one repl session and just copy pasted it into this blog post?
(coleslaw::load-config "/Users/brit/projects/improvedmeans/")
(coleslaw::load-content)

;; Cute sidenote, since ppcre turns this regex into to a tree of closures which SBCL compiles, this can be #'disassemble-d.
;; Also, it's pretty fast.
(defvar *wp-url-regex*
  (cl-ppcre:create-scanner "w{0,3}?\.?redlinernotes\.com/blog/\\?p=(\\d+)"))

(defstruct counts
  (invalid-slug   0 :type fixnum)
  (post-not-found 0 :type fixnum)
  (updated-url    0 :type fixnum))

(defvar *results* (make-counts))

(defun invalid-slug-p (slug)
  (or (null slug)
      (every #'digit-char-p slug)))

(defun mismatch-p (slug)
  (let ((key (make-pathname :directory '(:relative "posts")
                            :name slug :type "html")))
    (null (gethash key coleslaw::*site*))))

(defun slug-for-match (text start end match-start match-end reg-start reg-end)
  (declare (ignore start end))
  (let* ((id (subseq text (aref reg-start 0) (aref reg-end 0)))
         (match (subseq text match-start match-end))
         (slug (gethash id *post-id-map*))
         (new-url (concatenate 'string "blog.kingcons.io/posts/" slug ".html")))
    (cond ((invalid-slug-p slug)
           (incf (counts-invalid-slug *results*))
           (format t "Couldn't find valid slug for post id: ~d~%" id)
           "/&")
          ((mismatch-p slug)
           (incf (counts-post-not-found *results*))
           (format t "Not found in site content: ~A~%" slug)
           "/&")
          (t
           (incf (counts-updated-url *results*))
           (format t "Replacing ~A with ~A~%" match new-url)
           new-url))))

(coleslaw::do-files (path "/Users/brit/projects/improvedmeans/" "post")
  (let* ((text (alexandria:read-file-into-string path))
         (updated-post (cl-ppcre:regex-replace-all *wp-url-regex* text #'slug-for-match)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "~A" updated-post))))

(format t "~%~%===RESULTS===~%")
(dolist (type '(invalid-slug post-not-found updated-url))
  (let ((symb (alexandria:symbolicate 'counts- type)))
    (format t "~A: ~D Posts~%" type (funcall symb *results*))))
