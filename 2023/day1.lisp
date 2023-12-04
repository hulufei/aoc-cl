(defpackage :2023/day1
  (:import-from :utils #:get-input-lines)
  (:export #:part1
           #:part2))

(in-package :2023/day1)

(defun parse-line-digits (line)
  "Return the number by first-digit and last-digit of the line or 0"
  (let* ((digits (remove-if #'alpha-char-p line))
         (len (length digits)))
    (if (zerop len)
        0
        (parse-integer
          (format nil "~d~d"
                  (aref digits 0)
                  (aref digits (- len 1)))))))

(defun part1 ()
  (loop for line in (get-input-lines 2023 1)
        sum (parse-line-digits line)))

(defparameter *numeric*
  (loop for i from 1 to 9
        collect (cons #1=(format nil "~R" i)
                      (format nil "~d~a" i
                              (uiop:last-char #1#)))))

(defun string->digit (str &rest _)
  (cdr (assoc str *NUMERIC* :test #'string=)))

(defun replace-numberic (line)
  (ppcre:regex-replace-all
    (format nil "(~{~A~^|~})"
            (mapcar #'car *NUMERIC*))
    line
    #'string->digit
    :SIMPLE-CALLS t)
  )

;; "eighthree" is 83 and for "sevenine" is 79.
(defun part2 ()
  (loop for line in (get-input-lines 2023 1)
        sum (parse-line-digits 
              (replace-numberic
                (replace-numberic line)))))

;; TODO: Trie implementation
(defun insert-trie (trie path value)
  (loop with len = (length path)
        for c across path
        for i from 1 to len
        for node = trie then (cdr (assoc c node))
        if (null node) do
        (setf node (list (cons c nil)))
        if (= i len) do
        (push (cons 'v value) node)
        finally (return trie))
  )
