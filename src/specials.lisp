;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defvar *get-cell-p* nil)


(defvar *get-formula-p* nil)


(defvar *creating-formula* nil)


(defvar *simulate-slot-set-event-p* nil)


(defvar *event-stack* nil)


(defparameter *touched-observers* nil
  "Used to detect circularity as stuff propagate.")
