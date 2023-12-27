(defpackage :2023/DAY18
  (:use :cl))

(in-package :2023/DAY18)

(defun parse-line (line)
  (let ((parts (uiop:split-string line)))
    (setf (second parts) 
          (parse-integer (second parts)))
    (setf (third parts)
          (string-trim "()" (third parts)))
    parts))

(assert
  (equal (parse-line "R 6 #70c710")
         (list "R" 6 "#70c710")))

(defun move-offset (direction)
  (cdr (assoc
         direction
         '(("U" . (-1 . 0))
           ("R" . (0 . 1))
           ("D" . (1 . 0))
           ("L" . (0 . -1)))
         :TEST #'equal)))

(defun move (pos direction n)
  (let ((offset (move-offset direction)))
    (cons (+ (car pos) (* (car offset) n))
          (+ (cdr pos) (* (cdr offset) n)))))

(defun reduce-move (acc plan)
  (let ((direction (first plan))
        (n (second plan))
        (pos (first (first acc)))
        (count (second acc)))
    (push (move pos direction n) (first acc))
    (setf (second acc) (+ count n))
    acc))

(defun parse-plans ()
  (mapcar
    #'parse-line
    (utils:read-lines 2023 18 :SAMPLE nil)))

(defun move-tracks (plans)
  (reduce
    #'reduce-move
    plans
    :INITIAL-VALUE (list (list '(0 . 0)) 0)))

(defun shoelace (points)
  (loop
    for xs = points then (rest xs)
    while (second xs)
    for (x1 . y1) = (first xs)
    for (x2 . y2) = (second xs)
    sum (* x1 y2) into a
    sum (* x2 y1) into b
    finally (return (/ (- a b) 2))))

(defun part1 ()
  (destructuring-bind 
      (points len) 
      (move-tracks (parse-plans))
    (+ (shoelace points)
       (/ len 2)
       1)))

(assert (equal (part1) 34329))

(defun digit->direction (d)
  (ecase d
    (#\0 "R")
    (#\1 "D")
    (#\2 "L")
    (#\3 "U")))

(defun color->plan (color)
  "Decode hexadecimal code color to dig plan"
  (list
    (digit->direction 
      (char color (1- (length color))))
    (parse-integer
      color
      :START 1
      :END 6
      :RADIX 16)))

(assert (equal (color->plan "#70c710") '("R" 461937)))
(assert (equal (color->plan "#0dc571") '("D" 56407)))

(defun decode-color (plans)
  (mapcar
    #'(lambda (plan)
        (color->plan (third plan)))
    plans))

(defun part2 ()
  (destructuring-bind 
      (points len) 
      (move-tracks (decode-color (parse-plans)))
    (+ (shoelace points)
       (/ len 2)
       1)))

(assert (equal (part2) 42617947302920))
