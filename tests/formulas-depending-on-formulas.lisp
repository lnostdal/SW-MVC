;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)


(let* ((x λV0)
       (y λI(+ ~x 1))
       (z λI(+ ~y 1)))
  (assert (equalp (list ~x ~y ~z)
                  (list 0 1 2)))
  (incf ~x)
  (assert (equalp (list ~x ~y ~z)
                  (list 1 2 3))))
