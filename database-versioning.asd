(defpackage :database-versioning.sysdef
  (:use :common-lisp :asdf))

(in-package :database-versioning.sysdef)

(defsystem :database-versioning
  :name "Database Versioning"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "System to version the database in roughly the same way rails migrations work."
  :depends-on (:postmodern)
  :components ((:file "versioning")))