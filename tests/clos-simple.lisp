;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass clos-simple-test-1 (self-ref)
  ((x :initform #λ2)
   (square :initform ↑#λ(* ¤x ¤x)))
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


;;; Same, but using the custom slot metaclass here.

(defclass clos-simple-test-2 (self-ref)
  ((x :cell-p t :initform 2)
   (square :cell-p t :initform ↑λλ(* ¤x ¤x)))
  (:metaclass mvc-class))

(with-object (make-instance 'clos-simple-test-2)
  (assert (equalp (list 2 4) (list ¤x ¤square)))
  (incf ¤x)
  (assert (equalp (list 3 9) (list ¤x ¤square)))
  (catch :abort
    (with-sync ()
      (incf ¤x)
      (assert (equalp (list 4 16) (list ¤x ¤square)))
      (throw :abort nil)))
  (assert (equalp (list 3 9) (list ¤x ¤square))))



(eval-now (nilf (find-class 'clos-simple-test-1 nil)
                (find-class 'clos-simple-test-2 nil))
          (unintern 'clos-simple-test-1)
          (unintern 'clos-simple-test-2)
          (unintern 'x)
          (unintern 'square))