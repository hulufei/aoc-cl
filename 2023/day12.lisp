(defpackage :2023/DAY12
  (:use :cl))

(in-package :2023/DAY12)

(defparameter *lines* (utils:read-lines 2023 12 :SAMPLE nil))

(defun join-str (s n &key (separator ""))
  (let ((control-str (format nil "~~{~~a~~^~a~~}" separator)))
    (format
      nil
      control-str
      (loop repeat n collect s)
      separator)))

(defun parse-line (line &optional (unfold 1))
  (let ((pair (uiop:split-string line :SEPARATOR " ")))
    (list (join-str (first pair) unfold :SEPARATOR "?")
          (utils:parse-integer-in-line 
            (join-str (second pair) unfold :SEPARATOR ",")
            :SEPARATOR ","))))

(assert
  (equal (parse-line ".# 1") '(".#" (1))))
(assert
  (equal (parse-line ".# 1" 5) '(".#?.#?.#?.#?.#" (1 1 1 1 1))))

(defun matches (seq sizes &optional cache)
  (let ((key (cons seq sizes)))
    (or 
      (and cache (gethash key cache))
      (let ((result
              (cond
                ((and (null sizes)
                      (zerop (count #\# seq))) 1)
                ((string= seq "") 0)
                ((null sizes) 0)
                ((char= (char seq 0) #\.) 
                 (matches (subseq seq 1) sizes cache))
                ((char= (char seq 0) #\?)
                 (+ (progn
                      (setf (char seq 0) #\.)
                      (matches seq sizes cache))
                    (progn
                      (setf (char seq 0) #\#)
                      (matches seq sizes cache))))
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
                           (rest sizes)
                           cache)))))
                (t 0))))
        (when (and cache (not (gethash key cache)))
          (setf (gethash key cache) result))
        result))))

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

(defun parse-line-unfold (line)
  (parse-line line 5))

(defun part2 ()
  (loop
    with cache = (make-hash-table :TEST #'equal)
    for (seq sizes) in (mapcar #'parse-line-unfold *LINES*)
    sum (matches seq sizes cache)))

(assert (equal (part2) 60681419004564))
