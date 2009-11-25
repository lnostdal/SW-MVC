;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass container (multiple-value-model)
  ()

  (:metaclass mvc-class))
