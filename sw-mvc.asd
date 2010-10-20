;;;; http://nostdal.org/ ;;;;

(defsystem sw-mvc
  :description "
This was originally intended to be a Model View Controller (MVC) framework, but
has turned into a dataflow thing."

  :author "Lars Rune Nøstdal <larsnostdal@gmail.com> http://nostdal.org/"
  :licence "AGPLv3"

  :depends-on (:aromyxo :sw-stm :moptilities)

  :serial t
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "config")
     (:file "bootstrap")
     (:file "read-macros")

     (:file "conditions")
     (:file "bootstrap-macros")

     (:file "specials")
     (:file "symbol-macros")
     (:file "cell")
     (:file "mvc-class")
     (:file "bootstrap-classes")
     (:file "mixins")
     (:file "model-common")


     (:file "input-translators")
     (:file "util")
     (:file "mvc-class-observer")
     (:file "model-boolean")
     (:file "model-container")

     (:file "event")
     (:file "event-slot")
     (:file "event-container")
     (:file "event-container-remove")
     (:file "event-container-insert")
     (:file "event-container-exchange")

     (:file "model-container-null")
     (:file "model-container-list")
     (:file "model-container-pair")
     (:file "model-container-with-1-active-item")
     (:file "view-base")

     (:file "package-export")
     ))


   (:module tests
    :serial t
    :components
    ((:file "formulas-depending-on-formulas")
     (:file "detection-of-new-dependencies")
     (:file "handling-of-circular-propagation")
     (:file "set-formula-@-transaction")
     (:file "input-translation-simple")
     (:file "clos-simple")
     (:file "model-container-list")
     (:file "user-input-sync-back")
     (:file "model-view-connection")
     (:file "dlist-test")
     (:file "container-of")
     (:file "cell-equal-p-fn-test")
     (:file "cell-mark-as-dead")
     ))
   ))
