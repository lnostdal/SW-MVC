;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(let* ((x #~0)
       (y #λ(+ ~x 1))
       (z #λ(+ ~y 1)))
  (assert (equalp (list ~x ~y ~z)
                  (list 0 1 2)))
  (incf ~x)
  (assert (equalp (list ~x ~y ~z)
                  (list 1 2 3))))
