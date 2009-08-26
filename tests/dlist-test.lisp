;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(assert (sequence-of-length-p (let ((c (dlist #λ0 #λ1)))
                                (catch :fail
                                  (with-sync ()
                                    (insert #λ2 :in c)
                                    (throw :fail nil)))
                                ~c)
                              2))