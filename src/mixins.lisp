;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass model ()
  ((node :accessor node-of
         :initform nil
         :documentation "
If this object represents a value stored in a DLIST-NODE in a DLIST this would
be a pointer to the DLIST-NODE instance."))

  (:metaclass stm-class)
  (:documentation "
Common base class for all Models."))


(defmethod node-of ((view view-base))
  (node-of (model-of view)))



(defclass single-value-model (model)
  ()

  (:metaclass stm-class)
  (:documentation "
A Model which is a subtype of this is a simpler kind of model in that it only
represents or holds a \"single value\" in some way.
This means that DEREF or ~ will most likely work as expected on this model."))



(defclass multiple-value-model (model)
  ()
  (:metaclass stm-class)
  (:documentation "
This usually means this is or represents some sort of container.
DEREF or ~ will most likely return a list of values, or further models in turn."))
