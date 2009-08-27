;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(assert (sequence-of-length-p (let ((c (dlist #λ0 #λ1))
                                    (new #λ2))
                                (assert (not (node-of new)))
                                (catch :fail
                                  (with-sync ()
                                    (insert new :in c)
                                    (assert (node-of new))
                                    (throw :fail nil)))
                                (assert (not (node-of new)))
                                ~c)
                              2))
