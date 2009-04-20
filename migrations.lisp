(defpackage :database-versioning
  (:use :common-lisp
	:postmodern)
  (:export :*db-connection-parameters*
	   :def-migration
	   :def-query-migration
	   :def-queries-migration
	   :upgrade
	   :downgrade
	   :run-unregistered-migrations))

(defpackage :migration-user
  (:use :common-lisp
	:database-versioning))

(in-package :database-versioning)

(defparameter *migrations* nil
  "Contains all the migrations that can be executed")

(defparameter *db-connection-parameters* '("database" "user" "password" "host")
  "Set the connection settings here, is it will be used to connect to the needed database")
(setf *db-connection-parameters* '("mycar_development" "mycar_dev" "mycar" "localhost"))

(defmacro with-db (&body body)
  `(postmodern:with-connection *db-connection-parameters*
     ,@body))

(defstruct migration
  number subject execute revert)

(defun def-migration (&key number subject execute revert)
  "Creates the most basic form of a new migration.
A migration is referred to by its number and subject.  Each migration should differ in that combination.
Execute must be the function to be called when the migration is applied.
Revert must be the function to be called when the migration is reverted."
  (push (make-migration :number number :execute execute :revert revert :subject subject) *migrations*))
(defmacro def-query-migration (number subject &key execute revert)
  "Creates a migration for an sql query.
This only differs with def-migration in the way execute and revert are defined. In definition both execute and revert are wrapped inside (lambda () (with-db (execute <here be supplied content>))) which allows you to only write the query and not the extra stuff surrounding it. with-db sets up a database environment for the variables set in *db-connection-paramters*"
  `(def-migration :number ,number :subject ,subject 
		  :execute (lambda () (with-db (execute ,execute)))
		  :revert (lambda () (with-db (execute ,revert)))))
(defmacro def-queries-migration (number subject &key ((:execute execute-queries)) ((:revert revert-queries)))
  "Creates a migration for a range of sql queries.
This is similar to def-migration, but instead of allowing one query, it allows you to supply a range of queries.
(def-queries-migration 1239888485 \"example queries migration\"
		       :execute (\"CREATE TABLE application_user ( user_id SERIAL PRIMARY KEY, name text, email text )\"
				 \"CREATE TABLE user_books ( book_id SERIAL PRIMARY KEY, owner_id INTEGER REFERENCES application_user, title text )\")
		       :revert (\"DROP TABLE application_user, user_books CASCADE\"))"
  `(def-migration :number ,number :subject ,subject
		  :execute (lambda () ,@(map 'list 
					     (lambda (query)
					       `(with-db (execute ,query)))
					     execute-queries))
		  :revert (lambda () ,@(map 'list 
					    (lambda (query)
					      `(with-db (execute ,query)))
					    revert-queries))))

(defun schema-has-migration-p (migration)
  "Checks whether or not the schema contains the given migration.  The migration is stored by its number and its description."
  (with-db
      (handler-case (not (zerop (query (:select (:count '*)
						:from 'schema_version
						:where (:and (:= 'number (migration-number migration))
							     (:= 'subject (migration-subject migration))))
				       :single)))
	(database-error ()
	  (format *debug-io* "No table schema_version found, creating...")
	  (with-transaction (setup-schema-version)
	    (query (:create-table "schema_version" ((number :type integer) (subject :type text))))
	    nil)))))

(defun migration-schema-register (migration)
  "Register a migration as to pretend it has been executed"
  (with-db (query (:insert-into 'schema_version :set 'number (migration-number migration) 'subject (migration-subject migration)))))
(defun migration-schema-revert (migration)
  "Revert a migration as to pretend it was not executed"
  (with-db (query (:delete-from 'schema-version 
				 :where (:and (:= 'number (migration-number migration))
					      (:= 'subject (migration-subject migration)))))))

(defun run-migration (migration &optional revert-p (register-p T))
  "Runs a single migration with all possible options"
  (format T "~&~A ~A:~A ... " (if revert-p "Revert" "Execute") (migration-number migration) (migration-subject migration))
  (if revert-p
      (if (or (not register-p) (schema-has-migration-p migration))
	  (progn 
	    (funcall (migration-revert migration))
	    (when register-p (migration-schema-revert migration))
	    (format T "DONE~%"))
	  (format T "FAILED (migration not present)~%"))
      (if (or (not register-p) (not (schema-has-migration-p migration)))
	    (progn 
	      (funcall (migration-execute migration))
	      (when register-p (migration-schema-register migration))
	      (format T "DONE~%"))
	    (format T "FAILED (migration already present)~%"))))
		  

(defun run-unregistered-migrations (from to &optional (revert nil revert-p))
  "USED ONLY FOR DEBUGGING
Forcibly runs the given migrations without writing the changes in the schema.
This is something you should never really need to run."
  (unless revert-p
    (setf revert (>= to from)))
  (loop for migration in (sort (copy-list *migrations*) (if revert '> '<) :key 'migration-number) do
       (if (funcall (if revert '>= '<=) from (migration-number migration) to)
	   (run-migration migration revert nil))))

(defmacro downgrade (&optional (version 0))
  "Downgrades the database to the given version.
This will revert all migrations that have been run with a migration-number greater than <version>.
The migrations are downgraded from the greatest available migration-number, to the lowest.
When no version is given, 0 is assumed as that will clear the database (migration_schema will continue to exist)."
  `(progn ,@(loop for migration in (sort (copy-list *migrations*) '> :key 'migration-number) collect
		 `(if (and ,(<= version (migration-number migration)) (schema-has-migration-p ,migration))
		      (run-migration ,migration T T)))))

(defmacro upgrade (&optional (version nil))
  "Upgrades the database to the given version.
This will run all migrations that haven't been ran yet and that have a smaller migration-number than <version>.
The migrations are upgraded from the smallest available migration-number to the largest available one (with a cap on <version>).
When no version is given, all available migrations are executed."
  `(progn ,@(loop for migration in (sort (copy-list *migrations*) '< :key 'migration-number) collect
		 `(if (and (not (schema-has-migration-p ,migration))
			   ,(or (not version) (>= version (migration-number migration))))
		      (run-migration ,migration nil T)))))
