(in-package :cl-user)

;;; With thanks to Will Halliburton.
(defun print-instance-usage (&key match (space :dynamic))
  "Return a sorted list of heap memory usage information of the form:
   (instance-type instance-count bytes-used)"
  (let ((totals (make-hash-table :test 'eq))
        (total-objects 0) (total-bytes 0))
    (declare (unsigned-byte total-objects total-bytes))
    (sb-sys:without-gcing
      (sb-vm::map-allocated-objects
       (lambda (obj type size)
         (when (eql type sb-vm:instance-header-widetag)
           (incf total-objects)
           (let* ((classoid (sb-kernel:layout-classoid
                             (sb-kernel:%instance-ref obj 0)))
                  (found (gethash classoid totals))
                  (size size))
             (declare (fixnum size))
             (incf total-bytes size)
             (cond
               (found
                (incf (the fixnum (car found)))
                (incf (the fixnum (cdr found)) size))
               (t (setf (gethash classoid totals) (cons 1 size)))))))
       space))
    (sort
     (loop for classoid being each hash-key of totals using (hash-value what)
          for name = (let ((*package* (find-package :cl-user)))
                       (prin1-to-string
                        (sb-kernel:classoid-proper-name classoid)))
          when (or (null match)
                   (search (string-downcase match)
                           (string-downcase name) :test #'string=))
          collect (list name (car what) (cdr what)))
     #'< :key #'third)))
