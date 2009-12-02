;;;; http://nostdal.org/ ;;;;


(defpackage :sw-mvc
  (:use :sw-stm))
(in-package :sw-mvc)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'insert)
  (shadow 'remove)
  (shadow 'exchange))
