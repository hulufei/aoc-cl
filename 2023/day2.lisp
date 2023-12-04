(defpackage :2023/day2
  (:import-from :utils #:get-input-lines)
  (:export #:part1
           #:part2))

(in-package :2023/day2)

(defparameter *bag* '(("red" . 12)
                      ("green" . 13)
                      ("blue" . 14)))

(defun parse-line (line)
  "Return (GameID . ((b . n) (r . n) (g . n)) ...)"
  (let* ((colon-index (position #\: line))
        (id (parse-integer (subseq line 5 colon-index))))
    (cons
      id 
      (loop for turns in (split-turns line)
            collect 
            (loop for (color) in *bag*
                  collect 
                  (cons 
                    color
                    (parse-color-count turns color))))))
  )

(defun split-turns (line)
  (uiop:split-string line :SEPARATOR ";"))

(defun parse-color-count (str color)
  "Return count value of specified color in str"
  (let ((pos (search color str)))
    (cond ((null pos) 0)
          (t (loop for i from pos downto 0
                   for c = #\space then (aref str i)
                   with digits = nil
                   if (digit-char-p c) do
                   (push c digits)
                   else if (plusp (length digits)) do
                   (return (parse-integer
                             (concatenate 'string digits)))))))
  )

(defun get-color-count (color data)
  (cdr (assoc color data :test #'string=)))

(defun possible-round-p (turn)
  (every #'(lambda (color)
             (>= (get-color-count color *bag*)
                 (get-color-count color turn)))
             '("green" "red" "blue")))

(defun possible-p (turns)
  (every #'possible-round-p turns))

(defun part1 ()
  (loop for line in (get-input-lines 2023 2)
        for (id . turns) = (parse-line line)
        when (possible-p turns)
        sum id))

(defun max-color-count (color turns)
  (apply #'max
         (mapcar 
           #'(lambda (turn)
               (get-color-count color turn))
           turns)))

(defun part2 ()
  (loop for line in (get-input-lines 2023 2)
        for (nil . turns) = (parse-line line)
        for r = (max-color-count "red" turns)
        for g = (max-color-count "green" turns)
        for b = (max-color-count "blue" turns)
        sum (* r g b)))

;; (part1)
;; (part2)
