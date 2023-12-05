(defpackage :2023/DAY4
  (:export #:part1
           #:part2))

(in-package :2023/DAY4)

(defun parse-integer-silent (str)
  (parse-integer str :JUNK-ALLOWED t))

(defun parse-numbers (str)
  (remove 
    nil 
    (mapcar 
      #'parse-integer-silent
      (uiop:split-string str :SEPARATOR " ")))
  )

(defun parse-line (line)
  "Return winning numbers and rest numbers"
  (let* ((winning-start (search ":" line))
         (rest-start (search "|" line))
         (winning-part (subseq line winning-start rest-start))
         (rest-part (subseq line rest-start)))
    (cons (parse-numbers winning-part)
          (parse-numbers rest-part))))

(defun part1 ()
  (loop for line in (utils:get-input-lines 2023 4)
        for (winning . numbers) = (parse-line line)
        for match-count = (length (intersection winning numbers))
        sum (if (zerop match-count) 0
                (expt 2 (1- match-count)))))

;; (part1)

(defun pick (n lst)
  "Pick n items from list"
  (loop for i from 1 to n
        for xs = lst then (rest xs)
        collect (first xs)))

(defun sum-to-nth (n xs)
  (reduce #'+ (pick n xs)))

(defun count-cards (matches stack)
  (if (zerop matches)
      (cons 1 stack)
      (cons (1+ (sum-to-nth matches stack)) stack)))

(defun part2 ()
  (let ((cards 
          (loop for line in (utils:get-input-lines 2023 4)
                for (winning . numbers) = (parse-line line)
                collect (length (intersection winning numbers))
                )))
    (apply 
      #'+
      (reduce
        #'count-cards
        cards
        :INITIAL-VALUE nil
        :FROM-END t))))

;; (part2)
