(defpackage :day1
  (:use #:cl
        #:alexandria))

(in-package :day1)

(defun main (&key (part 2))
  (let ((input (read-file-into-string "input.txt"))
        (story 0))
    (loop for c across input
          for i = 0 then (1+ i)
          when (and (= part 2) (= story -1))
            return i
          finally (return story)
          do (case c
               (#\( (incf story))
               (#\) (decf story))))))
