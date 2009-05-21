;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass single-value-model ()
  ()
  (:documentation "
A Model which is a subtype of this is a simpler kind of model in that it only
represents or holds a \"single value\" in some way.
This means that DEREF or ~ will most likely work as expected on this model."))

