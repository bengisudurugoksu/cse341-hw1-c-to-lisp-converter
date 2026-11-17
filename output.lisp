(declaim (ftype (function (integer integer) integer) sum))
; Unhandled line type: 
(defun sum (a b)
(+ a b)
)
; Unhandled line type: 
(defun main ()
(let ((x 10))
(let ((y 20))
(let ((result (sum x y)))
; Unhandled line type:     
(if (> result 25)
    (progn
(format t "Result is greater than 25~%")

(setf x 5)
))
; Unhandled line type: 
(loop for i from 0 to 10 do)
(format t "~a~%", i)

)
; Unhandled line type: 
0
)
