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
  ((x :cellp t :initform 2)
   (square :cellp t :initform ↑λλ(* ¤x ¤x)))
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


;;; Starting out with unbound slots.

(defclass clos-simple-test-3 ()
  ((x :cellp t)
   (square :cellp t))
  (:metaclass mvc-class))

(with-object (make-instance 'clos-simple-test-3)
  (setf ¤x 2)
  (setf ¤square λλ(* ¤x ¤x))
  (assert (equalp (list 2 4) (list ¤x ¤square)))
  (incf ¤x)
  (assert (equalp (list 3 9) (list ¤x ¤square))))


;;; Try storing a function as a value in a slot marked as a cell by :CELLP.

(let ((closure (lambda () 42)))
  (defclass clos-simple-test-4 ()
    ((function-as-value :cellp t :initform (as-value closure))
     (function-as-formula :cellp t :initform closure))
    (:metaclass mvc-class))

  (with-object (make-instance 'clos-simple-test-4)
    (assert (eq ¤function-as-value closure))
    (assert (= 42 ¤function-as-formula))))



(eval-now (nilf (find-class 'clos-simple-test-1 nil)
                (find-class 'clos-simple-test-2 nil)
                (find-class 'clos-simple-test-3 nil)
                (find-class 'clos-simple-test-4 nil))
          (unintern 'clos-simple-test-1)
          (unintern 'clos-simple-test-2)
          (unintern 'clos-simple-test-3)
          (unintern 'clos-simple-test-4))
