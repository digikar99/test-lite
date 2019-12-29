(in-package :cl-user)
(defpackage test-lite
  (:documentation
   "A prove-inspired minimal testing framework based around is- forms and 
grouping tests on the basis of packages.")
  (:use :cl :iterate)
  ;; (:import-from :prove.output
  ;;               :*test-result-output*)
  ;; (:import-from :prove.report
  ;;               :test-report-p
  ;;               :passed-test-report
  ;;               :failed-test-report
  ;;               :error-test-report
  ;;               :skipped-test-report
  ;;               :comment-report
  ;;               :composed-test-report
  ;;               :failed-report-p
  ;;               :duration)
  ;; (:import-from :prove.reporter
  ;;               :format-report
  ;;               :*indent-level*
  ;;               :*additional-indent*)
  (:import-from :alexandria
                :with-gensyms
                :once-only
                :ensure-gethash
                :hash-table-values)
  (:export :*default-test-function*
           :*debug-on-error*

           :is
           :is-values
           :is-print
           :is-condition
           :is-error
           :is-type
           :is-like
           :is-expand

           :define-test
           :run-test
           :run-test-package
           :run-test-all
           :remove-test
           :remove-test-all))

(in-package :test-lite)


;;; The following gensym- entities are required by is-expand.

(defvar *gensym-prefix* "$")
(defvar *gensym-alist*)

(defun gensymp (val)
  (and (symbolp val)
       (string= (subseq (symbol-name val) 0 (length *gensym-prefix*)) *gensym-prefix*)))

(defgeneric gensym-tree-equal (x y)
  (:method (x y)
    (if (and (gensymp y) (symbolp x))
        (if (assoc y *gensym-alist*)
            (eq x (cdr (assoc y *gensym-alist*)))
            (unless (rassoc x *gensym-alist*)
              (setf *gensym-alist* `((,y . ,x) ,@*gensym-alist*))
              t))
        (equal x y)))
  (:method ((x cons) (y cons))
    (loop for a in x for b in y
       always (gensym-tree-equal a b))))

;;; Next three forms define various is- forms

(defmacro define-is-clause (name got-form-function &optional custom-test-fn)
  "GOT-FORM is a lambda that takes one argument - the GOT as an argument 
and returns the actual form that is to be tested against EXPECTED.
Uses CUSTOM-TEST-FN if supplied else provides the user with a choice
defaulted to EQUAL."
  ;; A better way to deal with "cascading gensyms"??
  (let ((return-value (gensym))
        (test 'test))
    `(let ((,return-value (gensym)))
       (defmacro ,name ,(if custom-test-fn
                            `(&whole whole got expected)
                            `(&whole whole got expected &key (,test ''equal)))
         `(let ((,,return-value (gensym)))
            (if (funcall ,,(if custom-test-fn
                               custom-test-fn
                               test)
                         (setq ,,return-value
                               (handler-case ,(funcall ,got-form-function got)
                                 (error (e)
                                   (incf *num-errored*)
                                   (format nil "~D" e))))
                         ,expected)
                (incf *num-passed*)
                (progn
                  (format t "    ~D did not pass. Got ~D.~%"
                          ',whole
                          ,,return-value)
                  (incf *num-failed*))))))))

(defmacro define-is-clauses (&rest clauses)
  `(progn
     ,@(loop for clause in clauses
          collect `(define-is-clause ,@clause))))

(define-is-clauses
  (is #'identity)
  (is-values (lambda (got) `(multiple-value-list ,got)))
  (is-print (lambda (got)
              `(with-output-to-string (*standard-output*)
                 ,got))
            '#'string=)
  (is-condition (lambda (form)
                  (let ((error (gensym)))
                    `(handler-case ,form
                       (condition (,error) ,error))))
                '#'typep)
  (is-type #'identity #'typep)
  (is-like #'identity (lambda (x y) (not (null (ppcre:scan y x)))))
  (is-expand (lambda (got) `(macroexpand-1 ,got))
             '(symbol-function 'gensym-tree-equal))
  (is-error (lambda (form)
              (let ((error (gensym)))
                `(handler-case ,form
                   (condition (,error) (type-of ,error)))))
            '(symbol-function 'subtypep)))

;;; The remaining forms help define and run the tests at the global level.

(defvar *package-tests* (make-hash-table))
(defvar *num-errored*)
(defvar *num-passed*)
(defvar *num-failed*)

(defmacro define-test (name &body test-forms)
  "Defines a test comprising of TEST-FORMS with NAME under *PACKAGE*."
  (with-gensyms (tests)
    `(let ((,tests (ensure-gethash *package* *package-tests* (make-hash-table))))
       (setf (gethash ',name ,tests)
             (lambda ()
               ;; this defines all the work (including background work) that
               ;; should happen when a test is run
               (format t "~A~%" ',name)
               ,@test-forms
               (let* ((base-string
                       (format nil "  ~D passed ~D failed ~D errored."
                               *num-passed* *num-failed* *num-errored*))
                      (rem-length (- 80 (length base-string)))
                      (num-hyphens (- rem-length 14)))
                 (write-string base-string)
                 (unless (= 0 *num-failed* *num-errored*)
                   (loop initially (write-char #\space)
                        (write-char #\<)
                      for i below (max num-hyphens 0)
                      do (write-char #\-)
                      finally (write-char #\space)
                        (write-string "not passing")))
                 (terpri)))))))

(defun run-test (name)
  (let ((tests (gethash *package* *package-tests*)))
    (when tests (funcall (gethash name tests)))))

(defun run-test-package (package-designator)
  "Run tests under package designated by PACKAGE-DESIGNATOR."
  (let ((functions (hash-table-values (gethash (find-package package-designator)
                                               *package-tests*)))
        (total-passed 0)
        (total-errored 0)
        (total-failed 0))
    (declare (special *num-passed* *num-errored* *num-failed*))
    (dolist (function functions)
      (let ((*num-passed* 0)
            (*num-errored* 0)
            (*num-failed* 0))
        (funcall function)
        (incf total-passed *num-passed*)
        (incf total-errored *num-errored*)
        (incf total-failed *num-failed*)))
    (if (= 0 total-errored total-failed)
        (format t "~%ALL TESTS PASSED!~%")
        (format t "~%ALL TESTS DID NOT PASS!~%~D PASSED ~D FAILED ~D ERRORED.~%"
                total-passed total-failed total-errored))))

(defun remove-test (name)
  "Remove test with NAME under *PACKAGE*."
  (let ((tests (gethash *package* *package-tests*)))
    (when tests (remhash name tests))))


