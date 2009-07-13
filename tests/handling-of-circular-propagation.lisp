;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(let* ((x λv0)
       (calc λi(incf ~x)))

  (assert (equalp (list ~x ~calc)
                  (list 1 1)))
  (incf ~x)
  (assert (equalp (list ~x ~calc)
                  (list 2 2))))
