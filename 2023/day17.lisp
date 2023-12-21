(defpackage :2023/DAY17
  (:use :cl))

(in-package :2023/DAY17)

;;; Search implementation references:
;;; https://www.redblobgames.com/pathfinding/a-star/introduction.html
;;; https://github.com/norvig/paip-lisp/blob/main/docs/chapter6.md#64-a-set-of-searching-tools

(defparameter *directions* '((N . (-1 . 0))
                             (S . (1 . 0))
                             (E . (0 . 1))
                             (W . (0 . -1))))

(defun reverse-direction (d)
  (ecase d
    (N 'S)
    (S 'N)
    (W 'E)
    (E 'W)))

(defstruct reached-point
  (pos)
  (come-from nil)
  (moves '(0 0))
  (cost 0))

(defun tree-search (states goal-p successors combiner)
  "Find a states that satisfies goal-p. Start with states,
  and search according to successors and combiner."
  (cond ((null states) 'fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors combiner))))

(defun reached-p (grid)
  (destructuring-bind (n m) (array-dimensions grid)
    #'(lambda (point) 
        (equal (reached-point-pos point)
               (cons (1- n)
                     (1- m))))))

(defun value (grid pos)
  (aref grid (car pos) (cdr pos)))

(defun visited-key (point)
  (list (reached-point-pos point)
        (reached-point-moves point)
        (reached-point-come-from point)))

(defun record-if-new-low-cost (visited p)
  (when (< (reached-point-cost p)
           (gethash (visited-key p) visited most-positive-fixnum))
    (setf (gethash (visited-key p) visited)
          (reached-point-cost p))
    p)
  )

(defun move (grid visited point d &optional (baby-steps 1))
  (let* ((moves (reached-point-moves point))
         (pos (reached-point-pos point))
         (cost-so-far (reached-point-cost point))
         (offset (cdr (assoc d *directions*)))
         (v (first moves))
         (h (second moves)))
    (labels ((steps (mov)
               (if (zerop mov) baby-steps 1))
             (next-pos (start mov)
               (cons (+ (car start) (* (car offset)
                                       (steps mov)))
                     (+ (cdr start) (* (cdr offset)
                                       (steps mov)))))
             (next-cost (start move-steps cost)
               (cond ((zerop move-steps) cost)
                     (t (let ((next (next-pos start 1)))
                          (next-cost 
                            next
                            (1- move-steps)
                            (+ cost (value grid next))))))))
      (destructuring-bind (new-pos new-moves move-steps)
          (ecase d
            ((N S) (list (next-pos pos v)
                         (list (+ v (steps v)) 0)
                         (steps v)))
            ((W E) (list (next-pos pos h)
                         (list 0 (+ h (steps h)))
                         (steps h))))
        (when (utils:inside-p grid new-pos)
          (record-if-new-low-cost
            visited
            (make-reached-point
              :POS new-pos
              :MOVES new-moves
              :COST (next-cost pos move-steps cost-so-far)
              :COME-FROM (reverse-direction d))))))))

(defun successors (grid point min-moves max-moves visited)
  (loop
    with (v h) = (reached-point-moves point)
    with come-from = (reached-point-come-from point)
    for d in (cond ((= v max-moves) '(W E))
                   ((= h max-moves) '(N S))
                   (t (remove come-from '(N S W E))))
    for next-point = (move grid visited point d min-moves)
    when next-point
    collect next-point
    ))

(defun gen-successors (grid min-moves max-moves)
  (let ((visited (make-hash-table :TEST #'equal)))
    #'(lambda (point)
        (successors grid point min-moves max-moves visited))))

(defun crucible-successors (grid)
  (gen-successors grid 1 3))

(defun ultral-crucible-successors (grid)
  (gen-successors grid 4 10))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn"
  #'(lambda (new old)
      (sort (append new old) #'< :KEY cost-fn)))

(defun make-grid ()
  (utils:make-grid
    (loop
      for line in (utils:read-lines 2023 17 :SAMPLE nil) collect
      (loop 
        for c across line
        collect (parse-integer (string c))))))

(defun part1 ()
  (let ((grid (make-grid)))
    (tree-search
      (list (make-reached-point :POS '(0 . 0)))
      (reached-p grid)
      (crucible-successors grid)
      (sorter #'reached-point-cost))))

(defun part2 ()
  (let ((grid (make-grid)))
    (tree-search
      (list (make-reached-point :POS '(0 . 0)))
      (reached-p grid)
      (ultral-crucible-successors grid)
      (sorter #'reached-point-cost))))

;; (assert (equal (reached-point-cost (part1)) 791))

;; (time (part2)) ; 153 seconds

;; https://github.com/40ants/cl-flamegraph
;; Upload following profile to https://www.speedscope.app/
;; We can identify the bottleneck is sort function
;; (flamegraph:save-flame-graph ("2023/day17-part1-fl.stack")
;;   (part1))
