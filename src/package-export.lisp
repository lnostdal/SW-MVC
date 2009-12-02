;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
(in-readtable sw-mvc)


;; TODO: Describe why I do this in some blog-post or something. Short; this also exports symbols from CL, so users should use the SW-MVC package instead of the CL package. The point -- besides me being lazy, is that conflicts can be dealt with _once_ and the result/solution trickles town to sub-sub-sub..-packages.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym)
    (export sym))
  (export '(nil))) ;; CL pitfall..
