;;;; http://nostdal.org/ ;;;;

(amx:define-package :sw-mvc
    :use (:amx :sw-stm))
(in-package sw-mvc)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unintern 'insert)
  (unintern 'remove)
  (unintern 'exchange))


(export
 '(abort-mvc-event

   ;; bootstrap.lisp
   mk-vcell mk-icell mk-ocell mk-ccell
   as-formula
   cell-of

   ;; cell.lisp
   cell
   with-cells
   cell-force-update
   cell-mark-as-dead
   equal-p-fn-of
   assign-condition skip-cell
   cell-condition cell-eval-condition cell-equal-p-condition
   condition-of
   accepts-conditions-p-of
   touch

   ;; meta-class.lisp
   mvc-class mvc-stm-class
   with-callbacks callback observer
   callback
   get-object-callbacks
   add-object-callback
   add-simple-object-callback
   remove-object-callback
   get-slot-callbacks
   add-slot-callback
   add-simple-slot-callback
   remove-slot-callback
   as-value
   mvc-class-dslotd mvc-class-eslotd

   ;; mixins.lisp
   model
   single-value-model
   multiple-value-model
   node-in-context-of

   ;; boolean-model.lisp
   boolean-model
   ;;value value-of
   old-t old-t-of
   toggle

   ;; model-container.lisp
   container
   empty-p-of

   ;; model-container-null.lisp
   null-container

   ;; model-container-list.lisp
   dlist-node dlist-of parent-of left-of right-of value-of
   dlist head-of tail-of
   list<-
   dlist<-
   transform-into

   ;; event.lisp
   event
   handle
   observables-of
   event-router
   event-of

   ;; event-container.lisp
   container-event
   container container-of
   container-event-single-object
   object object-of objects-of

   ;; event-container-remove.lisp
   remove remove-from remove-all
   container-remove container-remove-all

   ;; event-container-insert.lisp
   insert
   container-insert
   relative-position relative-position-of
   relative-node relative-node-of

   ;; event-container-exchange.lisp
   exchange
   container-exchange
   target-position-of

   ;; view-base.lisp
   view-base
   model model-of set-model
   view-constructor
   view-constructor-fn view-constructor-fn-of
   add-formulas
   formula-of

   ;; model-base.lisp
   model-base
   mk-view
   views-in-context view-in-context-of

   ;; specials.lisp
   *event-stack*
   *formula*

   ;; symbol-macros.lisp
   =cell=
   =cell-value= =cell-old-value=
   =cell-boundp=

   ;; model-common.lisp
   pulse
   feedback-event-of

   ;; input-translators.lisp
   mk-input-handler
   mk-validator
   mk-integer-parser
   mk-number-parser
   add-input-handler

   ;; util.lisp
   with-formula
   without-dataflow
   forward-cell
   sync-cells
   sync-back
   add-slot-observers
   +null-model+

   ;; conditions.lisp
   mvc-signal condition-of
   mvc-error

   ;; event-slot.lisp
   slot-event
   context context-of
   object object-of
   slot-name slot-name-of
   slot-set
   ))
