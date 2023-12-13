(defpackage :2023/DAY12
  (:use :cl))

(in-package :2023/DAY12)

(defparameter *lines* (utils:read-lines 2023 12 :SAMPLE nil))

(defun parse-line (line)
  (let ((pair (uiop:split-string line :SEPARATOR " ")))
    (list (first pair)
          (utils:parse-integer-in-line 
            (second pair)
            :SEPARATOR ","))))

(defun matches (seq sizes)
  (cond ((and (null sizes)
              (zerop (count #\# seq))) 1)
        ((string= seq "") 0)
        ((null sizes) 0)
        ((char= (char seq 0) #\.) 
         (matches (subseq seq 1) sizes))
        ((char= (char seq 0) #\?)
         (+ (progn
              (setf (char seq 0) #\.)
              (matches seq sizes))
            (progn
              (setf (char seq 0) #\#)
              (matches seq sizes))))
        ((char= (char seq 0) #\#)
         (loop
           with size = (first sizes)
           with len = (length seq)
           for i from 0 below size
           when (or (< len size)
                    (char= (char seq i) #\.))
           return 0
           finally
           (if (and (> len size)
                    (char= (char seq size) #\#))
               (return 0)
               (return
                 (matches 
                     (subseq
                       seq 
                       (if (> len size) (1+ size) size))
                   (rest sizes))))))
        (t 0)))

(assert (equal (matches "???.###" '(1 1 3)) 1))
(assert (equal (matches ".??..??...?##." '(1 1 3)) 4))
(assert (equal (matches "?#?#?#?#?#?#?#?" '(1 3 1 6)) 1))
(assert (equal (matches "????.#...#..." '(4 1 1)) 1))
(assert (equal (matches "????.######..#####." '(1 6 5)) 4))
(assert (equal (matches "?###????????" '(3 2 1)) 10))

(defun part1 ()
  (loop
    for (seq sizes) in (mapcar #'parse-line *LINES*)
    sum (matches seq sizes)))

(part1)
