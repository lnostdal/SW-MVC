;;;; http://nostdal.org/ ;;;;


(defsystem sw-mvc
  :description "
This was originally intended to be a Model View Controller (MVC) framework, but
has turned into a dataflow thing."

  :author "Lars Rune Nøstdal <larsnostdal@gmail.com> http://nostdal.org/"
  :licence "AGPLv3"

  :depends-on (:aromyxo :sw-stm)

  :serial t
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "config")
     (:file "bootstrap")
     (:file "bootstrap-classes")
     (:file "read-macros")
     (:file "specials")
     (:file "symbol-macros")
     ;;(:file "cell-decls")
     (:file "cell")
     (:file "meta-class")
     ;;(:file "formula")
     (:file "mixins")
     (:file "model-common")
     (:file "input-translators")
     (:file "model-boolean")
     (:file "model-container")
     (:file "event")
     (:file "event-container")
     (:file "event-container-remove")
     (:file "event-container-insert")
     (:file "event-container-exchange")
     (:file "model-container-list")

     (:file "view-base")
     #|
     (:file "event-slot-set")
     (:file "model-container-pair")

     |#
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
     ))
   ))
