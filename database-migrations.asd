(defpackage :database-migrations.sysdef
  (:use :common-lisp :asdf))

(in-package :database-migrations.sysdef)

(defsystem :database-migrations
  :name "Database Migrations"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "System to version the database in roughly the same way rails migrations work.  Differences are that only one database is really supported (but hacking around that is trivial) and that migrations are not needed to be stored in separate files."
  :depends-on (:postmodern)
  :components ((:file "migrations")))