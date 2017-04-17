					;Recursive-Method :returns the number of elements in a list of numbers that are followed by a larger integer

(defun follows-l(lst)
  (cond
   ((null lst) 0)
   ((null (cdr lst)) 0)
   ((< (cadr lst) (car lst)) (follows-l(cdr lst)))
   (t (+ (follows-l(cdr lst)) 1))
   )
  )


					
