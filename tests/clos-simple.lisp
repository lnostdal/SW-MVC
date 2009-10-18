;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim (optimize (speed 0) (safety 2)))


(defclass clos-simple-test-1 (self-ref)
  ((x :initform 2)
   (square :initform ↑λF(* ¤x ¤x)))
  (:metaclass mvc-class))


(with-object (make-instance 'clos-simple-test-1)
  (assert (equalp (list 2 4) (list ¤x ¤square)))
  (incf ¤x)
  (assert (equalp (list 3 9) (list ¤x ¤square)))
  (catch :abort
    (with-sync ()
      (incf ¤x)
      (assert (equalp (list 4 16) (list ¤x ¤square)))
      (throw :abort nil)))
  (assert (equalp (list 3 9) (list ¤x ¤square))))


;;; Starting out with unbound slots.

(defclass clos-simple-test-2 ()
  ((x)
   (square))
  (:metaclass mvc-class))

(with-object (make-instance 'clos-simple-test-2)
  (setf ¤x 2)
  (setf ¤square λF(* ¤x ¤x))
  (assert (equalp (list 2 4) (list ¤x ¤square)))
  (incf ¤x)
  (assert (equalp (list 3 9) (list ¤x ¤square))))



(eval-now (nilf (find-class 'clos-simple-test-1 nil)
                (find-class 'clos-simple-test-2 nil))
          (unintern 'clos-simple-test-1)
          (unintern 'clos-simple-test-2))
