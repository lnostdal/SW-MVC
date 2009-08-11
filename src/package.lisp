;;;; http://nostdal.org/ ;;;;

(defpackage #:sw-mvc
  (:use #:cl
        #:closer-mop
        #:aromyxo
        #:cl-utilities
        #:alexandria
        #:sw-stm)

  (:shadow
   ;;#:list #:push #:pop
   #:insert #:remove #:exchange
   ;;#:dolist
   )

  (:shadowing-import-from #:alexandria
                          #:with-gensyms
                          #:compose
                          #:with-unique-names
                          #:copy-array
                          #:once-only)


  (:export

   #:abort-mvc-event

   ;; cell.lisp
   #:cell #:mk-cell
   #:with-cells
   #:cell-force-update
   #:cell-mark-as-dead
   #:equal-p-fn-of
   #:assign-condition #:skip-cell
   #:cell-eval-error #:condition-of

   ;; meta-class.lisp
   #:mvc-class #:mvc-stm-class
   #:with-callbacks #:callback #:observer
   #:callback
   #:get-object-callbacks
   #:add-object-callback
   #:add-simple-object-callback
   #:remove-object-callback
   #:get-slot-callbacks
   #:add-slot-callback
   #:add-simple-slot-callback
   #:remove-slot-callback
   #:cell-of
   #:as-value

   ;; mixins.lisp
   #:single-value-model

   ;; boolean-model.lisp
   #:boolean-model
   ;;#:value #:value-of
   #:old-t #:old-t-of
   #:toggle

   ;; model-container.lisp
   #:container

   ;; model-container-list.lisp
   #:dlist-node #:dlist-of #:parent-of #:left-of #:right-of #:value-of
   #:dlist #:head-of #:tail-of
   #:list<-
   #:dlist<-
   #:transform-into

   ;; event.lisp
   #:event
   #:handle
   #:observables-of
   #:event-router
   #:event-of

   ;; event-container.lisp
   #:container-event
   #:container #:container-of
   #:container-event-single-object
   #:object #:object-of #:objects-of

   ;; event-container-remove.lisp
   #:remove #:remove-from #:remove-all
   #:container-remove

   ;; event-container-insert.lisp
   #:insert
   #:container-insert
   #:relative-position #:relative-position-of
   #:relative-object #:relative-object-of

   ;; event-container-exchange.lisp
   #:exchange
   #:container-exchange
   #:target-position-of

   ;; view-base.lisp
   #:view-base
   #:model #:model-of
   #:view-constructor
   #:view-constructor-fn #:view-constructor-fn-of
   #:add-formulas
   #:formula-of

   ;; model-base.lisp
   #:model-base
   #:mk-view
   #:views-in-context #:view-in-context-of

   ;; specials.lisp
   #:*event-stack*
   #:*formula*

   ;; symbol-macros.lisp
   #:=cell=
   #:=cell-value= #:=cell-old-value=
   #:=cell-boundp=

   ;; model-common.lisp
   #:pulse
   #:feedback-event-of

   ;; input-translators.lisp
   #:mk-input-handler
   #:mk-validator
   #:mk-integer-parser
   #:mk-number-parser

   ;; util.lisp
   #:with-formula
   #:forward-cell
   #:sync-cells
   #:sync-back
   ))


(in-package #:sw-mvc)
