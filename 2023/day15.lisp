(defpackage :2023/DAY15
  (:use :cl))

(in-package :2023/DAY15)

(defun hash (str)
  (reduce
    #'(lambda (n current-value)
        (rem (* 17 (+ n current-value)) 256))
    (map 'list #'char-int str)
    :INITIAL-VALUE 0))

(defun parse-line (line)
  (uiop:split-string line :SEPARATOR ","))

(defun sum-hash-steps (line)
  (reduce #'+ (mapcar #'hash (parse-line line))))

(assert 
  (equal (sum-hash-steps "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7") 1320))

(defun part1 ()
  (sum-hash-steps (first (utils:read-lines 2023 15))))

(assert
  (equal (part1) 510273))
