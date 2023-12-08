(defsystem "aoc"
  :depends-on ("cl-ppcre")
  :components ((:file "utils")
               (:module "2023"
                  :depends-on ("utils")
                  :components
                    ((:file "day1")
                     (:file "day2")
                     (:file "day3")
                     (:file "day4")
                     (:file "day5")
                     (:file "day6")
                     (:file "day7")
                     (:file "day8")))))
