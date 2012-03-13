;; Lovingly stolen from 6502 on Stackoverflow
;; See: http://stackoverflow.com/questions/4068340/is-there-a-mocking-stubbing-framework-for-common-lisp
;; (declaim (notinline function-to-patch)) ; may come in handy if with-function-patch appears busted

(defpackage :easymock
  (:use :cl)
  (:export #:with-function-patch))

(in-package :easymock)

(defmacro with-function-patch (patch &rest body)
  "Takes a PATCH form like a FLET clause, i.e. (fn-name (lambda-list) body),
evaluates BODY in an environment with fn-name rebound to the PATCH form and
uses UNWIND-PROTECT to safely restore the original definition afterwards."
  (let ((oldfn (gensym))
        (result (gensym))
        (name (car patch))
        (args (cadr patch))
        (pbody (cddr patch)))
    `(let ((,oldfn (symbol-function ',name)))
       (setf (symbol-function ',name) (lambda ,args ,@pbody))
       (unwind-protect (progn ,@body)
         (setf (symbol-function ',name) ,oldfn))
       ,result)))

;; What about for other types? Compound Structures, Classes+Methods, Types themselves, etc?
;; What if you need to mock a macro? o_O
