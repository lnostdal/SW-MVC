;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)
(in-readtable sw-mvc)


(let* ((x λv0)
       (y λv0)
       (calc λi(if (evenp ~x)
                   (+ ~x ~x)
                   (+ ~x ~y))))
      (incf ~x)
      (incf ~y)
      (assert (= 2 ~calc)))
