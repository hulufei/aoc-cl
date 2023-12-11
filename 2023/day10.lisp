(defpackage :2023/day10
  (:use :cl))

(in-package :2023/day10)

(defparameter
  *directions* '((N . (-1 0))
                 (E . (0 1))
                 (S . (1 0))
                 (W . (0 -1)))
  "Clock-wise directions")

(defparameter
  *in-outs-mapping* '((N . "|LJ")
                      (E . "-LF")
                      (S . "|7F")
                      (W . "-J7"))
  "In-out pipes mapping")

(defun reverse-dir (dir)
  (ecase dir
    ('N 'S)
    ('S 'N)
    ('E 'W)
    ('W 'E)))

(assert (equal (reverse-dir 'N) 'S))

(defun visited-p (pipe entry visited-queue)
  (assoc
    entry
    (gethash pipe visited-queue)
    :TEST #'equal))

(defun push-visited (pipe entry steps visited-queue)
  (push
    (cons entry steps)
    (gethash
      pipe visited-queue)))

(defun push-pipe-queue (pipe entry steps pipe-queue)
  (nconc
    pipe-queue 
    (list (cons pipe (cons entry steps))))
  )

(defun match-pipe (in-pipe dir out-pipe)
  (or (and
        (char= in-pipe #\S)
        (char/= out-pipe #\.))
      (and
        (find
          in-pipe
          (cdr (assoc dir *in-outs-mapping*)))
        (find
          out-pipe
          (cdr (assoc (reverse-dir dir) *in-outs-mapping*))))))

(assert (not (match-pipe #\| 'N #\L)))
(assert (match-pipe #\| 'S #\L))
;; (assert (match-pipe #\J #\F 'N))

(defun next-pipe (pipe dir)
  (mapcar #'+ pipe (cdr (assoc dir *DIRECTIONS*))))

(assert (equal (next-pipe '(2 3) 'N) '(1 3)))

(defun visit (grid pipe-queue visited-queue)
  "Visit pipe from entry cost `steps`"
  (destructuring-bind (n m) (array-dimensions grid)
    (loop
      for (pipe entry . steps) in pipe-queue
      unless (visited-p pipe entry visited-queue) do
      (push-visited pipe entry steps visited-queue)
      (loop
        for direction in (remove entry (mapcar #'car *directions*)) do
        (let* ((out-pipe (next-pipe pipe direction))
               (i (first out-pipe))
               (j (second out-pipe)))
          (when (and
                  (>= i 0)
                  (>= j 0)
                  (< i n)
                  (< j m)
                  (match-pipe
                    (apply #'aref grid pipe) ; in-pipe
                    direction
                    (apply #'aref grid out-pipe)))
            (push-pipe-queue
              out-pipe
              (reverse-dir direction)
              (+ steps 1)
              pipe-queue))))
      )))

(defun parse-grid (lines)
  (let* ((rows (length (first lines)))
         (cols (length (first lines))))
    (make-array
      (list rows cols)
      :INITIAL-CONTENTS lines)))

(defun start-position (grid)
  (destructuring-bind (n m) (array-dimensions grid)
    (loop
      for i from 0 below n append
      (loop
        for j from 0 below m
        when (char= (aref grid i j) #\S)
        return (list i j)))))

(defun part1 ()
  (let* ((grid (parse-grid (utils:read-lines 2023 10 :SAMPLE nil)))
         (start (start-position grid))
         (visited-queue (make-hash-table :TEST #'equal))
         (pipe-queue '()))
    (loop
      for dir in '(N S W E) do
      ;; nconc nil pipe-queue WON'T modify pipe-queue
      (setf pipe-queue
            (push-pipe-queue start dir 0 pipe-queue)))
    (visit grid pipe-queue visited-queue)
    ;; (loop
    ;;   for k being the hash-key
    ;;   using (hash-value v) of visited-queue
    ;;   maximize (apply
    ;;              #'max 
    ;;              (mapcar #'cdr v)))
    (/ (hash-table-count visited-queue) 2)))

(assert (equal (part1) (/ 13902 2)))
