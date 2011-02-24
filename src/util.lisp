;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(define-variable +null-model+
    :value λV"+NULL-MODEL+")


(defmacro with-formula (lifetime &body body)
  "Creates a new formula (CELL) that'll stick around for at least as long as the
LIFETIME object exists."
  (with-gensyms (formula-res)
    `(letp1 ((,formula-res (mk-icell ,@body)))
       (with-lifetime ,lifetime ,formula-res))))


(defmacro without-dataflow (&body body)
  "\Cancel out\" any further dataflow wrt. the closest (dynamic scope) \"formula\" (WITH-FORMULA); or to be exact, this causes new dataflow bindings to not be created;
observers of already monitored CELLs will still be notified."
  `(let ((*without-dataflow-p* t))
     ,@body))


(defun forward-cell (source target)
  (declare (cell source target))
  "Forward changes done to SOURCE to TARGET, unless the change to SOURCE was
caused by a change to TARGET."
  (with-formula target
    (setf (cell-deref target)
          (cell-deref source))))


(defun sync-cells (x y)
  "Keep the values in X and Y in sync.
X will update if Y changes, unless the change to Y was caused by a change to X.
Y will update if X changes, unless the change to X was caused by a change to Y."
  (declare (cell x y))
  (forward-cell x y)
  (forward-cell y x))


(defun sync-back (back middle front)
  "Any change to MIDDLE is forwarded to BACK, which will in turn forward its change to FRONT.
Any (direct) change to BACK is forwarded to FRONT."
  (forward-cell middle back)
  (forward-cell back front))


(defun add-slot-observers (instance fn &optional (eslotd-type 'mvc-class-eslotd))
  "Observe all slots in INSTANCE.
FN is a function accepting 3 arguments; INSTANCE, ESLOTD and NEW-VALUE.
A list of CELL instances is returned. Their lifetime (GC) is bound to INSTANCE. CELL-MARK-AS-DEAD can be used to
stop observing."
  (declare (function fn))
  (let ((class (class-of instance)))
    (check-type class mvc-class)
    (collecting
      (dolist (eslotd (class-slots class))
        (when (typep eslotd eslotd-type)
          (collect (with-formula instance
                     (funcall fn instance (slot-definition-name eslotd)))))))))


(defmacro with-observer (binding-event &body handler)
"Lifetime of HANDLER will be based on the object mentioned in BINDING-EVENT.

Syntax:

  (with-observer (on-click-of some-button)
    ..)

  (with-observer ((value (on-enterpress-of some-text-input)))
    ..)"
  (with-gensyms (instance)
    (let ((binding) (event))
      (if (listp (car binding-event))
          (progn
            (setf binding (caar binding-event))
            (setf event (cadar binding-event)))
          (progn
            (setf binding (gensym "EVENT-ARGS"))
            (setf event binding-event)))
      `(let ((,instance ,(second event)))
         (with-formula ,instance
           (let ((,binding (,(first event) ,instance))) ;; This is a bit paranoid, but ok.
             (declare (ignorable ,binding))
             (without-dataflow
               ,@handler)))))))
