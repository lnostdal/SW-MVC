;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)


(defmacro with-formula (lifetime &body body)
  "Creates a new formula (CELL) that'll stick around for at least as long as the
LIFETIME object exists."
  (with-gensyms (formula-res)
    `(letp1 ((,formula-res #λ,@body))
       (with-lifetime ,lifetime ,formula-res))))


(defun forward-cell (source target)
  (declare (cell source target))
  "Forward changes done to SOURCE to TARGET, unless the change to SOURCE was
caused by a change to TARGET."
  (with-formula target
    (setf ~target ~source)))


(defun sync-cells (x y)
  "Keep the values in X and Y in sync.
X will update if Y changes, unless the change to Y was caused by a change to X.
Y will update if X changes, unless the change to X was caused by a change to Y."
  (declare (cell x y))
  (forward-cell x y)
  (forward-cell y x))


(defun sync-back (back middle front)
  "Any change to MIDDLE is forwarded to BACK, which will in turn forward its change to FRONT.
Any change to BACK is forwarded to FRONT."
  (forward-cell middle back)
  (forward-cell back front))


(defun add-slot-observers (instance fn)
  "Observe all slots in INSTANCE.
FN is a function accepting 3 arguments; INSTANCE, SLOT-NAME (symbol) and NEW-VALUE."
  ;; TODO: I seem to be repeating this pattern a lot, e.g. in MAKE-INSTANCE for MVC-CLASS etc..
  (assert (subtypep (class-of (class-of instance)) (find-class 'mvc-class)))
  (dolist (class (moptilities:superclasses (class-of instance) :proper? nil))
    (when (subtypep (class-of class) (find-class 'mvc-class))
      (dolist (dslotd (class-direct-slots class))
        (with (cell-of (slot-value instance (slot-definition-name dslotd)))
          (with-formula instance
            (funcall fn instance (slot-definition-name dslotd) (cell-deref it))))))))
