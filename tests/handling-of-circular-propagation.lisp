;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(let* ((x #λ0)
       (calc #λ(incf ~x)))        ;; 0 -> 1

  (assert (equalp (list ~x ~calc)
                  (list 1 1)))
  (incf ~x)                       ;; 1 -> 2
  (assert (equalp (list ~x ~calc)
                  (list 2 2))))
