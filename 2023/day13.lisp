(defpackage :2023/DAY13
  (:use :cl))

(in-package :2023/DAY13)

(defparameter *lines* (utils:read-lines 2023 13 :SAMPLE nil))

(defun parse-patterns (lines)
  (reduce
    #'(lambda (line patterns)
        (if (string= line "")
            (push nil patterns)
            (push line (first patterns)))
        patterns)
    lines
    :FROM-END t
    :INITIAL-VALUE '(nil)))

(defparameter *patterns* (parse-patterns *LINES*))

(defun mirror-row-index (pattern)
  (let ((mirror-indexes
        (loop
          for lines = pattern then (rest lines)
          for line1 = (first lines)
          for line2 = (second lines)
          for i = 1 then (+ i 1)
          while line2
          when (string= line1 line2)
          collect i))
        )
    (loop
      for i in mirror-indexes
      for reflection = (reflection-p pattern i)
      when reflection
      collect i)))

(defun mirror-col-index (pattern)
  (mirror-row-index
    (apply
      #'map
      'list
      #'(lambda (&rest vchars)
          (concatenate 'string vchars))
      pattern)))

(defun reflection-p (lines mirror-index)
  (loop
    with len = (length lines)
    for offset = 1 then (+ 1 offset)
    for i = (- mirror-index offset 1)
    for j = (+ mirror-index offset)
    while (and (>= i 0) (< j len))
    when (string/= (elt lines i)
                   (elt lines j))
    return nil
    finally (return t)))

(defun part1 ()
  (loop
    for pattern in *PATTERNS*
    for row = (or (first (mirror-row-index pattern)) 0)
    for col = (or (first (mirror-col-index pattern)) 0)
    sum (+ (* 100 row) col)))

(assert (equal (part1) 33122))
