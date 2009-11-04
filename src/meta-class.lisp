;;;; http://nostdal.org/ ;;;;

(in-package sw-mvc)
(in-readtable sw-mvc)
(declaim #.(optimizations))


(defclass mvc-class (standard-class)
  ()

  (:documentation "
This metaclass enables slot access to have CELL-like features.

Initialization of unbound slots has one special property; if initialized with a
pointer (MK-PTR or #&) pointing to a CELL, that CELL will be used to represent
the slot value. The λf reader macro can be used to construct a CELL wrapped in
a pointer easily.

If a pointer is to be stored as a value, just wrap it in another pointer.

The CELL-OF macro allows one to refer directly to a CELL used to represent a
slot:

    (cell-of (slot-value some-object 'some-slot)) => #<CELL ...>

    (setf (cell-of (slot-value some-object 'some-slot))
          :replace-the-cell-itself)


This will also work for accessor methods (i.e., not just SLOT-VALUE)."))


(defmethod validate-superclass ((class mvc-class) (superclass standard-class))
  t)

(defmethod validate-superclass ((class mvc-class) (superclass stm-class))
  #| Because doing this will cause access to slots in the superclass to change behavior when done "from" an instance
  of the subclass. |#
  nil)

(defmethod validate-superclass ((class stm-class) (superclass mvc-class))
  #| I'm not sure what would happen here, so we disallow it for now. |#
  nil)


(defclass mvc-class-dslotd (standard-direct-slot-definition)
  ())


(defclass mvc-class-eslotd (standard-effective-slot-definition)
  ())


(defmethod direct-slot-definition-class ((class mvc-class) &key)
  (find-class 'mvc-class-dslotd))


(defmethod compute-effective-slot-definition ((class mvc-class) name dslotds)
  (if-let (dslotd (find-if (lambda (dslotd) (typep dslotd 'mvc-class-dslotd)) dslotds))
    (apply #'make-instance 'mvc-class-eslotd
           (sb-pcl::compute-effective-slot-definition-initargs class dslotds)) ;; TODO: closer-mop?
    (call-next-method)))


(defmethod allocate-instance ((class mvc-class) &key)
  (with1 (call-next-method)
    (dolist (eslotd (class-slots class))
      (when (typep eslotd 'mvc-class-eslotd)
        (unless (really-slot-boundp it eslotd)
          ;; We now know the slot is to be represented by a CELL; even in its unbound state.
          (setf (standard-instance-access it (slot-definition-location eslotd))
                (mk-vcell '%unbound)))))))


(defmethod compute-slots ((class mvc-class))
  "This'll ensure that the :TYPE slot option will (somewhat) work."
  (with1 (call-next-method)
    (dolist (eslotd it)
      (when (typep eslotd 'mvc-class-eslotd)
        (when-let ((type-check-fn (sb-pcl::slot-definition-type-check-function eslotd)))
          (setf (sb-pcl::slot-definition-type-check-function eslotd)
                (lambda (value)
                  #| TODO: This isn't perfect, but it is better than no type-checking (or full failure wrt. :TYPE)
                  at all. |#
                  (typecase value
                    (cell (funcall type-check-fn (cell-deref value)))
                    (t (funcall type-check-fn value))))))))))


(defmethod (setf slot-value-using-class) :around (new-value (class mvc-class) instance (eslotd mvc-class-eslotd))
  #| This is done as the CELL might be "output triggered" and thus cause additional CELL instances used to represent
  slots to be extracted later in the call stack. |#
  (let ((get-cell-p *get-cell-p*)
        (*get-cell-p* nil))
    (if get-cell-p
        (setf (standard-instance-access instance (slot-definition-location eslotd))
              new-value)
        (call-next-method))))


(defmethod (setf slot-value-using-class) (new-value (class mvc-class) instance (eslotd mvc-class-eslotd))
  (flet ((doit ()
           ;; Extract CELL and deref+set it here as S-V-U-C might call SLOT-UNBOUND otherwise.
           (with (cell-of (slot-value-using-class class instance eslotd))
             (setf (cell-deref it) new-value))))
    (declare (inline doit))
    (typecase new-value
      ;; This implements the λF syntax commonly used in DEFCLASS forms for inline "formulas" there.
      (cons (if (eq '%formula (car new-value))
                (progn
                  (check-type (cdr new-value) cell)
                  (setf (cell-of (slot-value-using-class class instance eslotd))
                        (cdr new-value)))
                (doit)))
      (t (doit)))))


(defmethod slot-value-using-class :around ((class mvc-class) instance (eslotd mvc-class-eslotd))
  #| This is done as the CELL might be "output triggered" and thus cause additional CELL instances used to represent
  slots to be extracted later in the call stack. |#
  (let* ((get-cell-p *get-cell-p*)
         (*get-cell-p* nil))
    (if get-cell-p
        (standard-instance-access instance (slot-definition-location eslotd))
        (call-next-method))))


(defmethod slot-value-using-class ((class mvc-class) instance (eslotd mvc-class-eslotd))
  (with1 (cell-deref (call-next-method))
    (when (eq it '%unbound)
      (slot-unbound class instance (slot-definition-name eslotd)))))


(defmethod slot-boundp-using-class ((class mvc-class) instance (eslotd mvc-class-eslotd))
  (and (call-next-method) ;; Called by our MAKE-INSTANCE, so need to give up early while constructing.
       (not (eq '%unbound
                ;; Extract CELL and deref it here as S-V-U-C might call SLOT-UNBOUND otherwise.
                (cell-deref (cell-of (slot-value-using-class class instance eslotd) :errorp t))))))


(defmethod slot-makunbound-using-class ((class mvc-class) instance (eslotd mvc-class-eslotd))
  "Make the CELL used to represent SLOTD in INSTANCE unbound (SW-MVC-based unboundness).

If CELL-OF is used, instead make the slot used to hold the CELL unbound (CLOS-based unboundness). Note that this
removes the thread safe properties wrt. this slot as going from unbound state back to bound state is now racy."
  (let ((get-cell-p *get-cell-p*)
        (*get-cell-p* nil))
    (if get-cell-p
        (call-next-method)
        ;; Go via our SVUC for thread-safety (STM).
        (setf (slot-value-using-class class instance eslotd)
              '%unbound))))


(defmethod touch-using-class (instance (class mvc-class))
  (dolist (eslotd (class-slots class))
    (when (typep eslotd 'mvc-class-eslotd)
      (with (standard-instance-access instance (slot-definition-location eslotd))
        (check-type it cell)
        (touch it)))))
