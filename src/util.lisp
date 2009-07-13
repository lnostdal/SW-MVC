;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defmacro with-formula (lifetime &body body)
  "Creates a new formula (CELL) that'll stick around for at least as long as the
LIFETIME object exists."
  `(with-lifetime ,lifetime
     #λ,@body))


(defun forward-cell (source target)
  (declare (cell source target))
  "Forward changes done to SOURCE to TARGET, unless the change to SOURCE was
caused by a change to TARGET."
  (with-formula target
    (unless (member target *source-cells*)
      (setf ~target ~source))))


(defun sync-cells (x y)
  "Keep the values in X and Y in sync.
X will update if Y changes, unless the change to Y was caused by a change to X.
Y will update if X changes, unless the change to X was caused by a change to Y."
  (declare (cell x y))
  (forward-cell x y)
  (forward-cell y x))
