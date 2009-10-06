;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)
(in-readtable sw-mvc)


(let* ((item λv42)
       (container-1 (dlist item))
       (container-2 (dlist container-1)))
  (assert (eq (container-of (node-in-context-of container-1 item))
              container-1))
  (assert (eq (container-of (node-in-context-of container-2 container-1))
              container-2)))


(unintern 'item)
(unintern 'container-1)
(unintern 'container-2)
