(defpackage :2023/day16
  (:use :cl))

(in-package :2023/day16)

(defun make-grid ()
  (utils:make-grid
    (utils:read-lines 2023 16 :SAMPLE nil)))

(defun reverse-direction (d)
  (ecase d
    (N 'S)
    (S 'N)
    (W 'E)
    (E 'W)))

(defun encounter (thing source)
  "Return a list of out directions"
  (cond ((char= thing #\\)
         (list
           (ecase source
             (N 'E)
             (S 'W)
             (E 'N)
             (W 'S))))
        ((char= thing #\/)
         (list
           (ecase source
             (N 'W)
             (S 'E)
             (E 'S)
             (W 'N))))
        ((char= thing #\|)
         (case source
           ((W E) '(N S))
           (otherwise (list (reverse-direction source)))))
        ((char= thing #\-)
         (case source
           ((N S) '(W E))
           (otherwise (list (reverse-direction source)))))
        (t (list (reverse-direction source)))))

(assert
  (equal (encounter #\. 'E) '(W)))
(assert
  (equal (encounter #\\ 'E) '(N)))
(assert
  (equal (encounter #\\ 'W) '(S)))
(assert
  (equal (encounter #\/ 'N) '(W)))
(assert
  (equal (encounter #\| 'W) '(N S)))
(assert
  (equal (encounter #\| 'N) '(S)))

(defun next-pos (pos out)
  (let ((i (car pos))
        (j (cdr pos)))
    (ecase out
      (N (cons (1- i) j))
      (S (cons (1+ i) j))
      (E (cons i (1+ j)))
      (W (cons i (1- j))))))

(defun beam-through (grid pos source energized)
  "Return a list of next beam targets"
  (let ((i (car pos))
        (j (cdr pos))
        (key pos))
    (when (and (utils:inside-p grid pos)
               (not (member source (gethash key energized))))
      (push source (gethash key energized nil))
      (loop
        for out in (encounter (aref grid i j) source) do
        (beam-through
          grid
          (next-pos pos out)
          (reverse-direction out)
          energized)))))

(defun solve (grid start direction)
  (let ((energized (make-hash-table :TEST #'equal)))
    (beam-through
      grid
      start
      direction
      energized)
    (hash-table-count energized)))

(defun part1 ()
  (solve (make-grid) '(0 . 0) 'W))

(assert (equal (part1) 6361))

(defun part2 ()
  (let ((grid (make-grid)))
    (destructuring-bind (n m) (array-dimensions grid)
      (max 
        (loop
          for j from 0 below m
          maximize (max (solve grid (cons 0 j) 'N)
                        (solve grid (cons (1- n) j) 'S)))
        (loop
          for i from 0 below n
          maximize (max (solve grid (cons i 0) 'W)
                        (solve grid (cons i (1- m)) 'E)))))
    ))

(assert (equal (part2) 6701))
