(defpackage :2023/DAY7
  (:export #:part1
           #:part2))

(in-package :2023/DAY7)

(defparameter *order-cards* "AKQJT98765432")
(defparameter *order-cards-with-joker* "AKQT98765432J")
(defparameter *order-types* '((5)
                              (4 1)
                              (3 2)
                              (3 1 1)
                              (2 2 1)
                              (2 1 1 1)
                              (1 1 1 1 1)))

(defun parse-hands (&key (sample nil))
  (let ((lines (utils:read-lines 2023 7 :sample sample)))
    (loop for line in lines
          for pair = (uiop:split-string line :SEPARATOR " ")
          collect (cons (first pair)
                        (parse-integer (second pair))))))

(defun count-cards (hand)
  (reduce
    #'(lambda (counts card)
        (if #1=(assoc card counts)
            (progn (incf (cdr #1#)) counts)
            (push (cons card 1) counts)))
    hand
    :INITIAL-VALUE nil))

(defun hand-type (hand &key joker)
  "Return one of *ORDER-TYPES*"
  (let* ((counts (count-cards hand))
         (h-type (sort (mapcar #'cdr counts) #'>)))
    (cond 
      ;; Add joker counts to the remain max item to archive best hand type.
      (#1=(and 
            joker
            (> (length h-type) 1)
            (cdr (assoc #\J counts)))
       (let ((sorted (remove #1# h-type :COUNT 1)))
         (setf (car sorted) ; first is max
               (+ (or (car sorted) 0) #1#))
         sorted))
      (t h-type))))

(defun hand-type-rank (h-type)
  "Higher stronger"
  (- (position 
       h-type
       *ORDER-TYPES* 
       :test #'equal)))

(defun card-rank (card &optional (order-cards *ORDER-CARDS*))
  "Higher stronger"
  (- (position card order-cards)))

(defun same-type-lessp (ha hb &optional (order-cards *ORDER-CARDS*))
  (loop for ca across ha
        for cb across hb
        unless (char= ca cb)
        return (< (card-rank ca order-cards)
                  (card-rank cb order-cards))
        )
  )

(defun hand< (ha hb &key joker)
  (let ((ranka (hand-type-rank (hand-type ha :joker joker)))
        (rankb (hand-type-rank (hand-type hb :joker joker))))
    (cond ((= ranka rankb) 
           (same-type-lessp 
             ha hb (if joker 
                       *ORDER-CARDS-WITH-JOKER* 
                       *ORDER-CARDS*)))
          (t (< ranka rankb)))))

(defun solution (&key joker)
  (loop for (nil . bid) in 
        (sort (parse-hands) 
              #'(lambda (a b)
                  (hand< a b :JOKER joker))
              :key #'car)
        for rank = 1 then (1+ rank)
        sum (* bid rank)))

(defun part1 ()
  (solution))

(utils:assert-equal (part1) 251121738)

(defun part2 ()
  (solution :joker t))

(utils:assert-equal (part2) 251421071)
