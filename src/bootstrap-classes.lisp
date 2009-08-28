;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(class-forward-reference view-base)

#|(class-forward-reference cell
  (:metaclass stm-class))|#


(class-forward-reference direct-cell-slot)
(class-forward-reference effective-cell-slot)


(class-forward-reference dlist
  (:metaclass mvc-class))

(class-forward-reference dlist-node
  (:metaclass mvc-class))
