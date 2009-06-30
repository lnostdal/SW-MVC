;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(let* ((x #~0)
       (y #~0)
       (calc #λ(if (evenp ~x)
                   (+ ~x ~x)
                   (+ ~x ~y))))
      (incf ~x)
      (incf ~y)
      (assert (= 2 ~calc)))
