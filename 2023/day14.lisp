(defpackage :2023/day14
  (:use :cl))

(in-package :2023/day14)

(defparameter *lines* (utils:read-lines 2023 14 :SAMPLE nil))

(defun calc-load (&rest seq)
  (loop
    with gaps = 0
    for rows from (length seq) downto 1
    for rock in seq
    when (char= rock #\.)
    do (incf gaps)
    when (char= rock #\#)
    do (setf gaps 0)
    when (char= rock #\O)
    sum (+ rows gaps)))

(defun part1 ()
  (reduce
    #'+
    (apply 
      #'map
      'list
      #'calc-load
      *LINES*)))

(assert (equal (part1) 110407))
