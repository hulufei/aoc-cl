(defpackage :2023/DAY11
  (:use :cl)
  (:import-from :utils #:string->list))

(in-package :2023/DAY11)

(defparameter *LINES* (utils:read-lines 2023 11 :SAMPLE nil))

(defun parse-universe (lines)
  (utils:make-grid 
    (expand-cols
      (expand-rows lines))))

(defun galaxies (universe)
  (destructuring-bind (n m) (array-dimensions universe)
    (loop
      for row from 0 below n append
      (loop
        for col from 0 below m
        when (char= (aref universe row col) #\#)
        collect (list row col)))))

(defun expansion-p (&rest chars)
  (every
    #'(lambda (c) (char= #\. c))
    chars))

(defun expand-rows (lines)
  (loop
    for line in lines
    when (apply #'expansion-p (string->list line))
    append (list line line)
    else collect line))

(defun expand-cols (lines)
  (let ((expands (apply
                   #'mapcar
                   #'expansion-p 
                   (mapcar #'string->list lines))))
    (loop
      for line in lines collect
      (concatenate 
        'string
        (loop
          for expand in expands
          for c across line
          when expand
          append (list c c)
          else collect c))
      )))

(defun pair-galaxies (galas)
  (loop
    for remains = galas then (rest remains)
    for g1 = (first remains)
    while (rest remains) append
    (loop for g2 in (rest remains) collect (cons g1 g2))))

(defun neighbors (point bound)
  (loop
    with rows = (first bound)
    with cols = (second bound)
    for (i j) in '((-1 0) (0 1) (1 0) (0 -1))
    for x = (+ i (first point))
    for y = (+ j (second point))
    when (and (>= x 0)
              (>= y 0)
              (< x rows)
              (< y cols))
    collect (list x y)))

(defun manhattan-distance (start end)
  (+ (abs (- (first start) (first end)))
     (abs (- (second start) (second end)))))

;; Slow DP solution
;; (defun shortest (start end bound)
;;   (loop
;;     with visited = nil
;;     for queue = (list (cons start 0)) then (rest queue)
;;     for (target . steps) = (first queue)
;;     while target
;;     when (equal end target)
;;     minimize steps
;;     unless (find target visited :TEST #'equal) do
;;     (push target visited)
;;     (nconc queue (mapcar
;;                    #'(lambda (x)
;;                        (cons x (+ steps 1)))
;;                    (neighbors target bound)))
;;     ))

(defun part1 ()
  (let ((universe (parse-universe *LINES*)))
    (reduce 
      #'+
      (mapcar
        #'(lambda (pair)
            (manhattan-distance 
              (car pair)
              (cdr pair)))
        (pair-galaxies (galaxies universe))))))

(assert (equal (part1) 9647174))

(defun expansion-count-between (n m checklist)
  (loop
    for i from (1+ (min n m)) below (max n m)
    when (find i checklist)
    count i))

(defun expansion-rows (lines)
  (loop
    for line in lines
    for x = 0 then (1+ x)
    when (apply #'expansion-p (string->list line))
    collect x))

(defun expansion-cols (lines)
  (let ((expands (apply
                   #'mapcar
                   #'expansion-p 
                   (mapcar #'string->list lines))))
    (loop
      for expand in expands
      for y = 0 then (1+ y)
      when expand
      collect y)))

(defun shortest (start end scale exp-rows exp-cols)
  (let* ((x1 (first start))
         (y1 (second start))
         (x2 (first end))
         (y2 (second end))
         (expansion-rows-count
           (expansion-count-between x1 x2 exp-rows))
         (expansion-cols-count
           (expansion-count-between y1 y2 exp-cols)))
    (+ (- (abs (- x1 x2)) expansion-rows-count)
       (- (abs (- y1 y2)) expansion-cols-count)
       (* scale expansion-rows-count)
       (* scale expansion-cols-count))))

(defun part2 ()
  (let ((universe (utils:make-grid *LINES*))
        (exp-rows (expansion-rows *LINES*))
        (exp-cols (expansion-cols *LINES*)))
    (reduce 
      #'+
      (mapcar
        #'(lambda (pair)
            (shortest 
              (car pair)
              (cdr pair)
              1000000
              exp-rows
              exp-cols))
        (pair-galaxies (galaxies universe))))))

(assert (equal (part2) 377318892554))
