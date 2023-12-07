(defpackage :2023/DAY6
  (:use :cl)
  (:export #:part1
           #:part2))

(in-package :2023/DAY6)

(defparameter *lines* (utils:get-input-lines 2023 6))
(defparameter *time-list* (utils:parse-integer-in-line (first *lines*)))
(defparameter *distance-list* (utils:parse-integer-in-line (second *lines*)))

(defun zip (x y)
  (mapcar #'cons x y))

(defun part1 ()
  (apply
    #'*
    (loop for (time . distance) in (zip *TIME-LIST* *DISTANCE-LIST*)
          collect 
          (loop for hold from 1 to (- time 1)
                for new-distance = (* hold (- time hold))
                count (> new-distance distance)))))

(utils:assert-equal (part1) 1084752)

(defun join-integer (numbers)
  (parse-integer (format nil "~{~a~^~}" numbers)))

;; bx - x^2 > c -> x^2 - bx + c <=0
;; https://en.wikipedia.org/wiki/Quadratic_equation
(defun solve-quadratic-equation (a b c)
  (sort 
    (mapcar
      #'(lambda (op) 
          (/ (funcall
               op 
               (- b)
               (sqrt (- (expt b 2) (* 4 a c))))
             (* 2 a)))
      '(+ -))
    #'<)
  )

(defun part2 ()
  (let* ((time (join-integer *TIME-LIST*))
         (distance (join-integer *DISTANCE-LIST*))
         (ans (solve-quadratic-equation 1 (- time) distance)))
    (- (second ans)
       (first ans))))

(utils:assert-equal (part2) 28228952)
