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
   #:propagate

   ;; event-container.lisp
   #:container-event
   #:container #:container-of
   #:container-event-single-object
   #:object #:object-of #:objects-of

   ;; event-container-remove.lisp
   #:remove #:remove-from #:remove-all
   #:container-remove
   #:remove-event-of

   ;; event-container-insert.lisp
   #:insert
   #:container-insert
   #:relative-position #:relative-position-of
   #:relative-object #:relative-object-of
   #:insert-event-of

   ;; event-container-exchange.lisp
   #:exchange
   #:container-exchange
   #:exchange-event-of
   #:target-position-of

   ;; event-slot-set.lisp
   #:slot-set
   #:instance #:instance-of
   #:slot-name #:slot-name-of
   #:old-value #:old-value-of
   #:new-value #:new-value-of
   #:slot-set-event-of

   ;; view-base.lisp
   #:view-base
   #:model #:model-of
   #:handle-model-event
   #:handle-model-slot-set-event
   #:handle-view-set-object-model
   #:handle-view-set-slot-model
   #:view-constructor
   #:view-constructor-fn #:view-constructor-fn-of
   #:add-formula
   #:formula-cells-of

   ;; model-base.lisp
   #:model-base
   #:mk-view
   #:views-in-context #:view-in-context-of

   ;; formula.lisp
   #:creating-formula-p
   #:formula #:value-of
   #:mk-formula #:mk-fcell
   #:formula-of
   #:with-ignored-sources

   ;; specials.lisp
   #:*event-stack*
   #:*formula*

   ;; symbol-macros.lisp
   #:=event=
   #:=event-model=
   #:=slot-set-event-p=
   #:=old-value= #:=new-value=

   ;; model-common.lisp
   #:input-translator-of #:input-translator-restart
   #:translate-input
   #:input-validator-of
   #:feedback-event-of
   #:pulse
   #:integer-input-translator
   #:number-input-translator
   #:model-equal-p
   ))


(in-package #:sw-mvc)
