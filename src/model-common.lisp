;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defmacro pulse (place &optional (value nil value-supplied-p))
  "Sets PLACE to T (or VALUE if supplied), then NIL.
Used for dataflow \"event pulses\"."
  `(prog1 ,(if value-supplied-p
               `(setf ,place ,value)
               `(tf ,place))
          (nilf ,place)))
(export 'pulse)
