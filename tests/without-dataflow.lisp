;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
(in-readtable sw-mvc)


(let* ((x λV2)
       (square λI(prog1 (* ~x ~x)
                   (cell-mark-as-dead =cell=))))
  (assert (= ~x 2))
  (assert (= ~square 4))
  (incf ~x)
  (assert (= ~x 3))
  (assert (= ~square 4)) ;; still 4.
  (incf ~x)
  (assert (= ~x 4))
  (assert (= ~square 4))) ;; still 4.
