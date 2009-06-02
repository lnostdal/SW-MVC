;;;; http://nostdal.org/ ;;;;


(defsystem sw-mvc
  :description "
This was originally intended to be a Model View Controller (MVC) framework, but
has recently turned into a dataflow thing."

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
     (:file "read-macros")
     (:file "specials")
     (:file "meta-class")
     (:file "formula")
     (:file "bootstrap-classes")
     (:file "mixins")
     (:file "model-boolean")
     (:file "model-container")
     (:file "event")
     (:file "event-container")
     (:file "event-container-remove")
     (:file "event-container-insert")
     (:file "event-container-exchange")
     (:file "event-slot-set")
     (:file "model-container-pair")
     (:file "model-container-list")
     (:file "cell")
     (:file "view-base")
     ))))
