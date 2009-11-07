;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)


(defmethod optimizations (&key context)
  (declare (ignore context))
  '(optimize (speed 3) (space 0) (safety 2) (debug 3) (compilation-speed 0))
  ;;'(optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0))
  )