;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(let* ((i #~2)
       (x #λ(+ 40 ~i)))
  (catch :abort
    (with-sync ()
      (setf (formula-of ~x) (lambda () :fail))
      (throw :abort nil)))
  (incf ~i) ;; The transaction was aborted, so we're back to the old formula.
  (assert (= 43 ~x)))