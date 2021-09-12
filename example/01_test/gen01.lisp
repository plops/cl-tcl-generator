(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-tcl-generator")
  (ql:quickload "alexandria"))

(in-package :cl-tcl-generator)

(progn
  (defparameter *path* "/home/martin/stage/cl-tcl-generator/example/01_test")
  (defparameter *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))

  (write-source
   (format nil "~a/source/test01.tcl" *path*)
   `(do0
     (space package require Tk)
     (button .b :text (quote Push Me)
	     :command (quote tk_messageBox))
     ))
  )
