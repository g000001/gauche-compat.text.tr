;;;; package.lisp

(cl:in-package :cl-user)

(defpackage gauche-compat.text.tr
  (:nicknames :text.tr)
  (:export :tr :transliterate :string-tr :string-transliterate
           :build-transliterator)
  )

(defpackage :gauche-compat.text.tr.internal
  (:use :gauche-compat.text.tr :cl :named-readtables :fiveam
        :srfi-11 :srfi-8)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :map :member :assoc :lambda :loop :read-char))
