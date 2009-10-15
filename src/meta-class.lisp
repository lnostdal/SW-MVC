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


(flet ((body (instance)
         #| Unbound slots should be CELLs also. This ensures thread safety when the user later wants to go from an
         unbound state to a bound state. |#
         (let ((mvc-class (find-class 'mvc-class)))
           (assert (subtypep (class-of (class-of instance)) mvc-class)
                   nil "SW-MVC: Trying to create an instance with a meta-class not a sub-type of MVC-CLASS.")
           (loop :for class :in (moptilities:superclasses (class-of instance) :proper? nil)
              :when (subtypep (class-of class) mvc-class)
              :do (dolist (slot (class-direct-slots class))
                    (unless (slot-boundp instance (slot-definition-name slot))
                      (setf (cell-of (slot-value instance (slot-definition-name slot)))
                            (mk-vcell '%unbound))))))))
  (declare (inline body)
           (optimize speed))

  (defmethod make-instance ((class mvc-class) &key)
    (with1 (call-next-method)
      (body it)))

  (defmethod make-instance ((class (eql 'mvc-class)) &key)
    (with1 (call-next-method)
      (body it))))


(defmethod compute-slots ((class mvc-class))
  (with1 (call-next-method)
    (dolist (slot it)
      (when-let ((type-check-fn (sb-pcl::slot-definition-type-check-function slot)))
        (setf (sb-pcl::slot-definition-type-check-function slot)
              (lambda (value)
                ;; TODO: This isn't perfect, but it is better than no type-checking at all.
                (typecase value
                  (cell (funcall type-check-fn (cell-deref value)))
                  (t (funcall type-check-fn value)))))))))


(defmethod (setf slot-value-using-class) (new-value (class mvc-class) instance slotd)
  ;; This is done as the CELL might be "output triggered" and thus cause additional resources to be deref'ed.
  (let ((get-cell-p *get-cell-p*)
        (*get-cell-p* nil))
    (if get-cell-p
        (call-next-method)
        ;; TODO: This seems rather hackish; we do it to avoid reading from the CELL representing the slot.
        (if (slot-boundp-using-class (find-class 'sb-pcl::std-class) instance slotd)
            ;; Extract CELL and deref+set it here as S-V-U-C might call SLOT-UNBOUND otherwise.
            (with (cell-of (slot-value-using-class class instance slotd) :errorp t)
              (setf (cell-deref it) new-value))
            ;; The slot is _really_ unbound; no CELL with an '%UNBOUND value or anything.
            (call-next-method
             (typecase new-value
               (pointer (with (ptr-value new-value)
                          (typecase it
                            (cell it) ;; "Formula".
                            (t (mk-vcell it))))) ;; A pointer-to-a-pointer is needed to really store a pointer.
               (t (mk-vcell new-value)))
             class instance slotd)))))


(defmethod slot-value-using-class ((class mvc-class) instance slotd)
  ;; This is done as the CELL might be "output triggered" and thus cause additional resources to be deref'ed.
  (let* ((get-cell-p *get-cell-p*)
         (*get-cell-p* nil))
    (if get-cell-p
        (call-next-method)
        (with1 (cell-deref (call-next-method))
          (when (eq it '%unbound)
            (slot-unbound class instance (slot-definition-name slotd)))))))


(defmethod slot-boundp-using-class ((class mvc-class) instance slotd)
  (and (call-next-method) ;; Called by our MAKE-INSTANCE, so need to give up early while constructing.
       (not (eq '%unbound
                ;; Extract CELL and deref it here as S-V-U-C might call SLOT-UNBOUND otherwise.
                (cell-deref (cell-of (slot-value-using-class class instance slotd) :errorp t))))))


(defmethod slot-makunbound-using-class ((class mvc-class) instance slotd)
  "Make the CELL used to represent SLOTD in INSTANCE unbound (SW-MVC-based unboundness).

If CELL-OF is used, instead make the slot used to hold the CELL unbound (CLOS-based unboundness). Note that this
removes the thread safe properties wrt. this slot as going from unbound state back to bound state is now racy."
  (let ((get-cell-p *get-cell-p*)
        (*get-cell-p* nil))
    (if get-cell-p
        (call-next-method)
        (setf (slot-value-using-class class instance slotd)
              '%unbound))))