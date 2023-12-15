(defpackage :2023/DAY15
  (:use :cl))

(in-package :2023/DAY15)

(defparameter *lines* (utils:read-lines 2023 15))

(defun hash (str)
  (reduce
    #'(lambda (n current-value)
        (rem (* 17 (+ n current-value)) 256))
    (map 'list #'char-int str)
    :INITIAL-VALUE 0))

(defun parse-line (line)
  (uiop:split-string line :SEPARATOR ","))

(defun sum-hash-steps (line)
  (reduce #'+ (mapcar #'hash (parse-line line))))

(assert 
  (equal (sum-hash-steps "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7") 1320))

(defun part1 ()
  (sum-hash-steps (first *LINES*)))

(assert
  (equal (part1) 510273))

(defun label-pos (label box)
  (position label box :TEST #'equal :KEY #'car))

(defun get-box (label)
  (let ((index (hash label)))
    (or (aref *BOXES* index)
        (setf (aref *BOXES* index)
              (make-array 0 :FILL-POINTER 0)))))

(defun dash (op)
  (let* ((label (subseq op 0 (1- (length op))))
         (box (get-box label))
         (pos (label-pos label box)))
    (when pos
      (setf (aref box pos) nil))))

(defun equal-sign (op)
  (let* ((sign-pos (position #\= op :FROM-END t))
         (label (subseq op 0 sign-pos))
         (focal-length (parse-integer
                         (subseq op (1+ sign-pos))))
         (box (get-box label))
         (pos (label-pos label box))
         (item (cons label focal-length)))
    (cond (pos
           (setf (aref box pos) item))
          (t
           (vector-push-extend item box)))))

(defun perform (op)
  (cond ((uiop:string-suffix-p op "-")
         (dash op))
        ((find #\= op)
         (equal-sign op))))

(defun focus-power (box-number box)
  (loop
    for (nil . focal-length) across (remove nil box)
    for slot = 1 then (1+ slot)
    sum (* box-number slot focal-length)))

(defun part2 (line)
  (defparameter *boxes* (make-array 256 :INITIAL-ELEMENT nil))
  (loop
    for op in (parse-line line)
    do (perform op))
  (loop
    for box across *BOXES*
    for i = 1 then (1+ i)
    when box
    sum (focus-power i box))
  )

(assert
  (equal (part2 "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7") 145))

(assert
  (equal (part2 (first *LINES*)) 212449))
