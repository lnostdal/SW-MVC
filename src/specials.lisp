;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defvar *creating-formula* nil)

(defvar *simulate-slot-set-event-p* nil)
