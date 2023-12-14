(defpackage :2023/day14
  (:use :cl))

(in-package :2023/day14)

(defparameter *directions* '((N . (-1 . 0))
                             (W . (0 . -1))
                             (S . (1 . 0))
                             (E . (0 . 1))))

(defparameter *lines* (utils:read-lines 2023 14 :SAMPLE t))

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

;; (assert (equal (part1) 110407))

(defun scan-order (grid direction)
  (destructuring-bind (n m) (array-dimensions grid)
    (ecase direction
      ('N (loop 
            for i from 0 below n collect
            (loop
              for j from 0 below m collect (cons i j))))
      ('W (loop 
            for j from 0 below m collect
            (loop
              for i from 0 below n collect (cons i j))))
      ('S (reverse (scan-order grid 'N)))
      ('E (reverse (scan-order grid 'W))))))

(scan-order (utils:make-grid *LINES*) 'S)

(defun swap (grid i j x y)
  (psetf (aref grid i j) (aref grid x y)
         (aref grid x y) (aref grid i j)))

(defun bound-p (x y n m)
  (and (>= x 0)
       (>= y 0)
       (< x n)
       (< y m)))

(defun move-direction (direction)
  (cdr (assoc direction *DIRECTIONS*)))

(defun roll-rock (grid i j direction)
  (destructuring-bind (n m) (array-dimensions grid)
    (loop
      with (offset-i . offset-j) = (move-direction direction)
      for x = (+ i offset-i) then (+ x offset-i)
      for y = (+ j offset-j) then (+ y offset-j)
      while (bound-p x y n m)
      unless (char= (aref grid x y) #\.)
      return (swap grid i j (- x offset-i) (- y offset-j))
      finally
      (return (swap grid i j (- x offset-i) (- y offset-j))))))

(defun tilt (grid direction)
  (loop
    for group in (scan-order grid direction) do
    (loop
      for (i . j) in group
      when (char= (aref grid i j) #\O) do
      (roll-rock grid i j direction)))
  )

(defun grid->string (grid)
  (destructuring-bind (n m) (array-dimensions grid)
    (format
      nil
      "~%~{~a~^~&~}~%"
      (loop
        for i from 0 below n collect
        (concatenate
          'string
          (loop
            for j from 0 below m
            collect (aref grid i j)))))))

(defun cycle (grid)
  (loop
    for direction in '(N W S E)
    do (tilt grid direction))
  grid)

(defun part2 ()
  (let* ((grid (utils:make-grid *LINES*)))
    (loop repeat 1000000000
          (cycle grid))
    (grid->string grid)))

;; (part2)
