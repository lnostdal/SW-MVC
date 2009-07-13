;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

#|
SW-MVC: Dealing with user input
===============================


            (sync-back)
      - - - - - - - - - - - -
      |                     |
      v                     |
  USER-INPUT -> PARSED-X -> X
                            |
                            v
                       SQUARE-OF-X


PARSED-X is the anonymous CELL created by MK-NUMBER-PARSER+MK-VALIDATOR below.

The point here is that changes to our Model (X) coming from "something external" (i.e, not USER-INPUT) should cause
the View (USER-INPUT) to update. At the same time, stuff should not get stuck propagating in circles.
|#


(let* ((x #λ0)
       (user-input #λ~x) ;; X -> USER-INPUT (sync-back).
       (square #λ(* ~x ~x)))

  ;; USER-INPUT -> X.
  (forward-cell (mk-validator #'evenp (mk-number-parser user-input)) x)

  (setf ~user-input "2")
  (assert (string= "2" ~user-input))
  (assert (= 2 ~x))
  (assert (= 4 ~square))

  (setf ~x 4)
  (assert (= 4 ~x))
  (assert (= 4 ~user-input))
  (assert (= 16 ~square)))
