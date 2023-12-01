(defsystem "aoc"
  :depends-on ("cl-ppcre")
  :components ((:file "utils")
               (:module "2023"
                  :depends-on ("utils")
                  :components
                    ((:file "day1")))))
