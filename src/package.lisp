;;;; http://nostdal.org/ ;;;;


(defpackage :sw-mvc
  (:use :sw-stm))
(in-package :sw-mvc)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unintern 'insert)
  (unintern 'remove)
  (unintern 'exchange))
