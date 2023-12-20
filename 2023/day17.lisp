(defpackage :2023/DAY17
  (:use :cl))

(in-package :2023/DAY17)

(defparameter *directions* '((N . (-1 . 0))
                             (S . (1 . 0))
                             (E . (0 . 1))
                             (W . (0 . -1))))

(defstruct reached-point
  pos
  (come-from nil)
  (moves '(0 0))
  (cost 0))

(defun value (grid pos)
  (aref grid (car pos) (cdr pos)))

(defun move (moves d)
  (let ((v (first moves))
        (h (second moves)))
    (ecase d
      ((N S) (list (+ v 1) 0))
      ((W E) (list 0 (+ h 1))))))

(defun valid-moves-p (moves)
  (and (< (first moves) 4)
       (< (second moves) 4)))

(defun neighbors (grid point)
  (loop
    with pos = (reached-point-pos point)
    with acc-moves = (reached-point-moves point)
    with come-from = (reached-point-come-from point)
    with cost-so-far = (reached-point-cost point)
    with (i . j) = pos
    for d in '(N S W E)
    for (x . y) = (cdr (assoc d *directions*))
    for new-pos = (cons (+ i x) (+ j y))
    for new-moves = (move acc-moves d)
    when (and (not (equal come-from new-pos))
              (utils:inside-p grid new-pos)
              (valid-moves-p new-moves))
    collect (make-reached-point
              :POS new-pos
              :MOVES new-moves
              :COST (+ (value grid new-pos) cost-so-far)
              :COME-FROM pos)))

(defun path-find (grid start goal)
  (let ((cost-so-far (make-hash-table :TEST #'equal)))
    (loop
      for queue = (list (make-reached-point :POS start)) then (rest queue)
      for current = (first queue)
      while current
      for pos = (reached-point-pos current)
      for cost = (reached-point-cost current)
      when (equal pos goal)
      return cost
      do (loop
           for next in (neighbors grid current)
           for new-pos = (reached-point-pos next)
           for new-cost = (reached-point-cost next)
           for key = (cons new-pos pos)
           when (< new-cost (gethash key cost-so-far most-positive-fixnum)) do 
           (setf (gethash key cost-so-far) new-cost)
           (nconc queue (list next))
           finally
           (sort (rest queue) #'< :KEY #'reached-point-cost)
           ))
    ))

(defun make-grid ()
  (utils:make-grid
    (loop
      for line in (utils:read-lines 2023 17 :SAMPLE nil) collect
      (loop 
        for c across line
        collect (parse-integer (string c))))))

(defun part1 ()
  (let ((grid (make-grid)))
    (destructuring-bind (n m) (array-dimensions grid)
      (path-find
        grid
        '(0 . 0)
        (cons (1- n) (1- m)))
      )))

(assert (equal (part1) 817)) ; too high
