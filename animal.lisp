;;;-*- Mode: common-lisp; syntax: common-lisp; package: ps; base: 10 -*-
;;;
;;;; A Simple Rule-base System
;;;
;;; This program is a Common Lisp version of expert system in Chapter 18 of 
;;; Winston's LISP, 1st edition.
;;;

(defpackage :ps
  (:documentation "Common Lisp version of a expert system in Chapter 18, Winston's LISP, 1st edition."))

(in-package :ps)

(defvar *facts* nil "list of fact data.")

(defun remember (new)
  "push <new> into *facts* if <new> is a new fact."
  (cond ((member new *facts* :test #'equal) nil)
        (t (push new *facts*)
           new)))

(defun recall (fact)
  "returns <fact> if it is known as fact in *facts*."
  (when (member fact *facts* :test #'equal)
    fact))

(defparameter *rules*
  '((rule id1
          (if (animal has hair))
          (then (animal is mammal)))
    (rule id2
          (if (animal gives milk))
          (then (animal is mammal)))
    (rule id3
          (if (animal has feathers))
          (then (animal is bird)))
    (rule id4
          (if (animal flies)
              (animal lays eggs))
          (then (animal is bird)))
    (rule id5
          (if (animal eats meat))
          (then (animal is carnivore)))
    (rule id6
          (if (animal has pointed teeth)
              (animal has claws)
              (animal has forward eyes))
          (then (animal is carnivore)))
    (rule id7
          (if (animal is mammal)
              (animal has hoofs))
          (then (animal is ungulate)))
    (rule id8
          (if (animal is mammal)
              (animal chews cud))
          (then (animal is ungulate)
                (even toed)))
    (rule id9
          (if (animal is mammal)
              (animal is carnivore)
            (animal has tawny color)
            (animal has dark spots))
          (then (animal is cheetah)))
    (rule id10
          (if (animal is mammal)
              (animal is carnivore)
            (animal has tawny color)
            (animal has black stripes))
          (then (animal is tiger)))
    (rule id11
          (if (animal is ungulate)
              (animal has long neck)
            (animal has long legs)
            (animal has dark spots))
          (then (animal is giraffe)))
    (rule id12
          (if (animal is ungulate)
              (animal has black stripes))
          (then (animal is zebra)))
    (rule id13
          (if (animal is bird)
              (animal does not fly)
              (animal has long neck)
              (animal has long legs)
              (animal is black and white))
          (then (animal is ostrich)))
    (rule id14
          (if (animal is bird)
              (animal does not fly)
              (animal swims)
              (animal is black and white))
          (then (animal is penguin)))
    (rule id15
          (if (animal is bird)
              (animal flies well))
          (then (animal is albatross))))
  "This includes 15 rules for identifying animals.")

(defun if-part-of (rule)
  (cdaddr rule))

(defun then-part-of (rule)
  (cdr (cadddr rule)))

(defun id-of (rule)
  (cadr rule))

;;;
;;; Forward Chaining
;;;

#|
(in-package :ps)
(setq *facts*
      '((animal has hair)
        (animal has pointed teeth)
        (animal has claws)
        (animal has forward eyes)
        (animal has tawny color)
        (animal has dark spots)))
(deduce)
|#

(defun deduce ()
    "Top level command for forward-chaining."
  (loop while (stepforward)))

(defun stepforward ()
  "proceeds one step of inference from IF-part to THEN-part of rules."
  (loop for rule in *rules*
      when (tryrule rule)
      return t))

(defun tryrule (rule)
  "tests IF-part of <rule> and establishes THEN-part with one step inference."
  (when (testif rule) (usethen rule)))

(defun testif (rule)
  "returns true if every IF-part of <rule> is satisfied with *facts*."
  (every #'recall (if-part-of rule)))

(defun usethen (rule)
  "establishes every THEN-part of <rule>. When a new one established, a message is printed."
  (let ((success nil))
    (loop for then in (then-part-of rule)
        when (remember then)
        do (format t "~%Rule ~S deduces ~S." (id-of rule) then)
          (setq success t))
    success))

;;;
;;; Backward Chaining
;;;

#|
(in-package :ps)
(setq *facts* nil)
(diagnose)
|#

(defparameter *hypotheses*
  '((animal is albatross)
    (animal is penguin)
    (animal is ostrich)
    (animal is zebra)
    (animal is giraffe)
    (animal is tiger)
    (animal is cheetah))
  "This includes several animal hopotheses.")

(defvar *asked* nil "list of asked assertions.")

(defun diagnose ()
  "Top level command for backward-chaining inference."
  (setq *asked* nil)
  (let ((verified (some #'verify *hypotheses*)))
    (when verified
      (format t "~%Hypothesis ~S is true." verified)
      verified)))

(defun verify (fact)
  "verifies <fact> with backward inference in the following steps. 
1) if <fact> is directly known as fact, then returns true.
2) if no rules relevant to <fact>, question on <fact> is made and asked to users.
   Then, yes/no answer establishes <fact> or not.
3) first, relevant rules are applied to try establish <fact> in one step inference.
   If it is so, immediately returns with established <fact>.
4) otherwise, relevant rules are applied to try establish <fact> in multi-step inference.
   Note that invoking as verify -> tryrule+ -> testif+ -> verify with recursion"
  (if (recall fact) fact
    (let* ((relevant1 (inthen fact))
           (relevant2 relevant1))
      (unless relevant1
        (cond ((member fact *asked* :test #'equal) (return-from verify nil))
              ((y-or-n-p "~%Is this true: ~S? " fact)
               (remember fact)
               (return-from verify fact))
              (t (push fact *asked*)
                 (return-from verify nil))))
      (loop for rel in relevant1
          when (tryrule rel)
          do (return-from verify fact))
      (loop for rel in relevant2
          when (tryrule+ rel)
          do (return-from verify fact))
      nil)))

(defun inthen (fact)
  "collects rules whose THEN-part is relevant to <fact>."
  (remove-if-not #'(lambda (r) (then-p fact r)) *rules*))
#|
(in-package :ps)
(inthen '(animal is cheetah))
|#

(defun then-p (fact rule)
  "returns true if <fact> is in THEN-part of <rule>."
  (member fact (then-part-of rule) :test #'equal))

(defun tryrule+ (rule)
  "tests IF-part of <rule> and establishes THEN-part with multi-step inference."
  (when (testif+ rule) (usethen rule)))

(defun testif+ (rule)
  "returns true if every IF-part of <rule> is verified as truth with inference by rules."
  (every #'verify (if-part-of rule)))
    