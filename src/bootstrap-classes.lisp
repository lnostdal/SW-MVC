;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(class-forward-reference view-base)


(class-forward-reference node
  (:metaclass mvc-class))


(class-forward-reference model)
(class-forward-reference multiple-value-model)
