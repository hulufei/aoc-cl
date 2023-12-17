(defpackage :utils
  (:use :cl)
  (:export #:get-input-lines
           #:get-sample-lines
           #:read-lines
           #:parse-integer-in-line
           #:make-grid
           #:string->list
           #:inside-p
           #:curry
           #:dbg
           #:assert-equal))

(in-package :utils)

(defun remote-fetch (year day)
  "Fetch input from adventofcode with curl, save to `year/dayN.input`.
  Note: Require environment variable AOC_COOKIE setted to your login session id.)"
  (uiop:run-program 
    (format 
      nil
      "~{~A~^ ~}"
      (list "curl"
            "-H"
            (format nil "'Cookie: session=~a'" (uiop:getenv "AOC_COOKIE"))
            (format nil "https://adventofcode.com/~d/day/~d/input" year day))
      )
    :OUTPUT (format nil "~d/day~d.input" year day)))

(defun get-input-lines (year day)
  "Get lines of input for the specific day, if the input file doesn't exist, it will fetch from the website.
  Note: Require environment variable AOC_COOKIE setted to your login session id.)"
  (let ((filename (format nil "~d/day~d.input" year day)))
    (if (probe-file filename)
        (uiop:read-file-lines filename)
        (progn (remote-fetch year day)
               (get-input-lines year day)))))

(defun get-sample-lines (year day)
  (uiop:read-file-lines
    (format nil "~d/day~d.sample" year day)))

(defun parse-integer-silent (str)
  (parse-integer str :JUNK-ALLOWED t))

(defun parse-integer-in-line (line &key (separator " "))
  (remove 
    nil
    (mapcar
      #'parse-integer-silent
      (uiop:split-string line :SEPARATOR separator))))

(defun read-lines (year day &key sample)
  (funcall 
    (if sample
        #'get-sample-lines
        #'get-input-lines)
    year day))

(defun make-grid (lines)
  (let* ((rows (length lines))
         (cols (length (first lines))))
    (make-array
      (list rows cols)
      :INITIAL-CONTENTS lines)))

(defun string->list (s)
  (loop for c across s collect c))

(defun inside-p (grid i j)
  (destructuring-bind (n m) (array-dimensions grid)
    (and (>= i 0)
         (>= j 0)
         (< i n)
         (< j m))))

;; https://bese.common-lisp.dev/docs/arnesi/html/api/function_005FIT.BESE.ARNESI_003A_003ACURRY.html
(defun curry (function &rest initial-args)
  "Returns a function which will call FUNCTION passing it
  INITIAL-ARGS and then any other args.

 (funcall (curry #'list 1) 2) ==> (list 1 2)"
  (lambda (&rest args)
    (apply function (append initial-args args))))

(defmacro dbg (v test &body body)
  `(cond ((funcall ,test ,v) ,@body ,v)
         (t ,v)))

(defun assert-equal (actual expected)
  (assert 
    (= actual expected)
    nil
    "Expect ~a, got ~a" expected actual))
