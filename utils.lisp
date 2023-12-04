(defpackage :utils
  (:export #:get-input-lines
           #:get-sample-lines))

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