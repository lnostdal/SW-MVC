;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


;;; CELL ;;;
;;;;;;;;;;;;

(defvar *get-cell-p* nil)
(defvar *get-formula-p* nil)

(declaim (list *source-cells*))
(defvar *source-cells* nil)

(declaim ((or null cell) *target-cell*))
(defvar *target-cell* nil)
