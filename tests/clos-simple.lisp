;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(eval-now (setf (find-class 'clos-simple-test nil) nil))


(defclass clos-simple-test (self-ref)
  ((x :initform #λ2)
   (square :initform ↑#λ(* ¤x ¤x)))
  (:metaclass mvc-class))


(with-object (make-instance 'clos-simple-test)
  (assert (equalp (list 2 4) (list ¤x ¤square)))
  (incf ¤x)
  (assert (equalp (list 3 9) (list ¤x ¤square))))

(eval-now (setf (find-class 'clos-simple-test nil) nil))