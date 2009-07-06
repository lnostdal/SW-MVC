;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(let* ((x λv0)
       (calc λi(incf ~x))) ;; 0 -> 1
  (assert (equalp (list ~x ~calc)
                  (list 1 1)))
  (incf ~x)                ;; 1 -> 2, then 2 -> 3
  (assert (equalp (list ~x ~calc)
                  (list 3 3))))
