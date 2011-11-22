(in-package :swank)

(defun system-containing (namestring)
  (loop for (key . system) being the hash-values in asdf::*defined-systems*
     when  (search (namestring (asdf:system-source-directory system))
                   namestring)
     return (slot-value system 'asdf::name)))

(defun find-current-system (&optional path)
  (if path
      (system-containing (namestring path))
      (let* ((pkg-def (swank:find-definition-for-thing *package*))
             (pkg-path (getf (getf pkg-def :location) :file)))
        (system-containing (namestring pkg-path)))))

(export 'find-current-system)
