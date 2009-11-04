;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)


(let* ((x λV0)
       (calc λI(incf ~x)))        ;; 0 -> 1

  (assert (equalp (list ~x ~calc)
                  (list 1 1)))
  (incf ~x)                       ;; 1 -> 2
  (assert (equalp (list ~x ~calc)
                  (list 2 2))))
