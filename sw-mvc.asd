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
     (:file "conditions")
     (:file "config")
     (:file "read-macros")
     (:file "bootstrap")
     (:file "bootstrap-classes")

     (:file "specials")
     (:file "symbol-macros")
     (:file "model-common")
     (:file "mvc-class")
     (:file "mixins")
     (:file "cell")

     (:file "input-translators")
     (:file "util")
     (:file "model-boolean")
     (:file "model-container")

     (:file "event")
     (:file "event-slot")
     (:file "event-container")
     (:file "event-container-remove")
     (:file "event-container-insert")
     (:file "event-container-exchange")

     (:file "model-container-list")
     (:file "model-container-pair")
     (:file "model-container-with-1-active-item")
     (:file "view-base")
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
     ))
   ))
