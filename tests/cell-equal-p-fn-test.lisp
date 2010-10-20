;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
(in-readtable sw-mvc)


(let* ((counter 0)
       (x λV2)
       (square λI(progn
                   (incf counter)
                   (* ~x ~x))))
  (setf ~x 2)
  (assert (= counter 1))
  (assert (= ~x 2))
  (assert (= ~square 4)))
