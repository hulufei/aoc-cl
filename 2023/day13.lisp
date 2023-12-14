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

(defun col->row (pattern)
  (apply
    #'map
    'list
    #'(lambda (&rest vchars)
        (concatenate 'string vchars))
    pattern))

(defun mirror-col-index (pattern)
  (mirror-row-index
    (col->row pattern)))

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

(defun diff-count (line1 line2)
  (count nil
         (map 'list #'char= line1 line2)))

(defun find-smudge (lines)
  (loop
    for rows = lines then (rest rows)
    for line = (first rows)
    for remains = (rest rows)
    for i = 0 then (1+ i)
    while remains
    append
    (loop
      for target in remains
      for j = (1+ i) then (1+ j)
      when (= (diff-count line target) 1)
      collect (cons i j))))

(defun reflection-between (lines i j)
  (loop
    for start = (1+ i) then (1+ start)
    for end = (1- j) then (1- end)
    while (< start end)
    when (string/= (elt lines start)
                   (elt lines end))
    return nil
    finally (return start)))

(defun reflection-around-p (lines i j)
  (loop
    for start = (1- i) then (1- start)
    for end = (1+ j) then (1+ end)
    while (and (>= start 0) (< end (length lines)))
    when (string/= (elt lines start)
                   (elt lines end))
    return nil
    finally (return t)))

(defun part2 ()
  (loop 
    for pattern in *PATTERNS*
    ;; for stop = 0 then (1+ stop)
    ;; while (< stop 1)
    sum (or 
          (loop
            with lines = pattern
            for (i . j) in (find-smudge lines)
            for row = (reflection-between lines i j)
            if (and row (reflection-around-p lines i j))
            return (* 100 row))
          (loop 
            with lines = (col->row pattern)
            for (i . j) in (find-smudge lines)
            for col = (reflection-between lines i j)
            when (and col (reflection-around-p lines i j))
            return col))
    ))

;; (untrace find-smudge reflection-between reflection-around-p col->row)
;; (assert (equal (part2) 32305))
