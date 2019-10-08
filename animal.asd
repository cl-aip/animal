(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  )
(defpackage :animal-system
  (:use :common-lisp :asdf))

(in-package :animal-system)

(defsystem :animal
  :name "ANIMAL"
  :author "Patrick Winston"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.1"
  :license "None"
  :description "Simple Productiom System by Winston, but modernized by Seiji"
  :long-description "Animal quize in Common Lisp from 'Artificial Intelligense' by Patrick Winston. This is modernized according to modern lisp by Seiji Koide."
  :components
  ((:file "animal")))