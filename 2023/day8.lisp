(defpackage :2023/DAY8
  (:use :cl))

(in-package :2023/DAY8)

(defun parse-input ()
  (let* ((lines (utils:read-lines 2023 8 :SAMPLE nil))
         (instructions (first lines))
         (nodes (cddr lines)))
    (values instructions
            (mapcar #'parse-node nodes))))

(defun parse-node (line)
  "Convert AAA = (BBB, CCC) to a-list"
  (let ((l-paren (position #\( line))
        (r-paren (position #\) line)))
    (cons
      (subseq line 0 3)
      (cons
        (subseq line (+ l-paren 1) (+ l-paren 4))
        (subseq line (- r-paren 3) r-paren)))))

(assert (equal 
          (parse-node "AAA = (BBB, CCC)")
          '("AAA" . ("BBB" . "CCC"))))

(defun next-node (instruction network from)
  "Get next node of from by instruction R/L"
  (let ((target (cdr (assoc from network :TEST #'string=))))
    (ecase instruction
      (#\R (cdr target))
      (#\L (car target))
      )))

(assert (string= 
          (next-node #\R '(("AAA" . ("BBB" . "CCC"))) "AAA")
          "CCC"))

(defun nav-steps (instructions network from &key next stop)
  (if (or (zerop #1=(length instructions))
          (funcall stop from))
      0
      (loop 
        for i = 0 then (mod (+ i 1) #1#)
        ;; Use and with multiple variables to be evaluated before
        ;; any of the variables is given its new value
        and node = from then 
        (funcall
          next (char instructions i) network node)
        until (funcall stop node)
        count node)))

(defun part1 ()
  (multiple-value-bind (instructions network) (parse-input)
    (nav-steps 
      instructions
      network
      "AAA"
      :NEXT #'next-node
      :STOP (utils:curry #'string= "ZZZ")
    ))
  )

(assert (= (part1) 12361))

(defun suffix-a-p (str)
  (uiop:string-suffix-p str "A"))

(assert (suffix-a-p "11A"))
 
(defun suffix-z-p (str)
  (uiop:string-suffix-p str "Z"))

(defun part2 ()
  (multiple-value-bind (instructions network) (parse-input)
    (let ((start-nodes
            (remove-if-not
              #'suffix-a-p 
              (mapcar #'car network))))
      (apply 
        #'lcm
        (mapcar
          #'(lambda (from)
              (nav-steps 
                instructions
                network
                from
                :NEXT #'next-node
                :STOP #'suffix-z-p
                ))
          start-nodes)
        ))
    ))

;; (untrace batch-stop)
(assert (equal (part2) 18215611419223))
