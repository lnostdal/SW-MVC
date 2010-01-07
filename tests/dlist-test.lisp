;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)
(in-readtable sw-mvc)

(declaim (optimize (speed 0) (safety 2)))


(assert (sequence-of-length-p (let ((c (dlist λV0 λV1))
                                    (new λV2))
                                (assert (not (dbg-prin1 (node-in-context-of c new))))
                                (catch :fail
                                  (with-sync ()
                                    (insert new :in c)
                                    (assert (node-in-context-of c new))
                                    (throw :fail nil)))
                                ~c)
                              2))
