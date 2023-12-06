(defpackage :2023/day5
  (:export #:part1
           #:part2))

(in-package :2023/day5)

(defparameter *lines* (utils:get-input-lines 2023 5))

(defparameter 
  *seeds*
  (utils:parse-integer-in-line (first *lines*)))

(defun collect-maps (acc line)
  (cond ((string= line "") acc)
        ((search "map:" line) (cons nil acc))
        (t (push 
             (utils:parse-integer-in-line line)
             (first acc))
           acc)))

(defparameter 
  *maps*
  (nreverse
    (reduce
      #'collect-maps 
      (rest *lines*)
      :INITIAL-VALUE nil)))

(defun destination (source dest-start src-start range)
  (when (and (>= source src-start)
             (< source (+ src-start range)))
    (+ dest-start
       (- source src-start))))

(defun seed-location (seed)
  (reduce
    #'(lambda (source mappings)
        (or (some #'(lambda (item)
                      (apply #'destination source item))
                  (reverse mappings))
            source))
    *maps*
    :INITIAL-VALUE seed)
  )

(defun part1 ()
  (apply
      #'min
      (mapcar #'seed-location *seeds*)))

(utils:assert-equal (part1) 26273516)

;; |____________|
;;     |_____|
(defun intersect-mappings (src range mappings)
  (cond ((null mappings) (list src range))
        ((<= range 0) nil)
        (t
         (destructuring-bind (m-dest m-src m-range) (first mappings)
           (let ((src-end (+ src range)) ; Exclude endpoint
                 (m-src-end (+ m-src m-range)))
             (cond ((< m-src src)
                    (cond ((< m-src-end src)
                           (intersect-mappings 
                             src range (rest mappings)))
                          ((< m-src-end src-end)
                           (append (list 
                                     (apply #'destination src (first mappings))
                                     (- m-src-end src))
                                   (intersect-mappings
                                     m-src-end
                                     (- src-end m-src-end)
                                     (rest mappings))))
                          (t 
                           (list 
                             (apply #'destination src (first mappings))
                             range))))
                   ((< m-src src-end)
                    (append (intersect-mappings
                              src
                              (- m-src src)
                              (rest mappings))
                            (list 
                              m-dest
                              (min (- src-end m-src) m-range))
                            (intersect-mappings
                              m-src-end
                              (- src-end m-src-end)
                              (rest mappings))))
                   (t
                    (intersect-mappings 
                      src range (rest mappings))))
             ))))
  )

(defun range-seed-location (plist-seeds)
  (reduce
    #'(lambda (plist-seeds mappings)
        (loop for lst = plist-seeds then (cddr lst)
              for src = (first lst)
              for range = (second lst)
              while range
              append (intersect-mappings
                       src range (reverse mappings))))
    *maps*
    :INITIAL-VALUE plist-seeds))

(defun min-range-location (plist-location)
  (loop for lst = plist-location then (cddr lst)
        while lst
        minimize (first lst)))

(defun part2 ()
  (min-range-location
    (range-seed-location *seeds*)))

;; 34039469
(utils:assert-equal (part2) 34039469)
