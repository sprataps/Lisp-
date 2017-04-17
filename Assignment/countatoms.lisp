					; counts the number of atoms in the list
(defun countatoms(lst)
  (cond
   ((null lst) 0)
   ((listp (car lst)) (+ (countatoms (car lst))(countatoms(cdr lst))))
   ((not (listp (car lst))) (+ 1 (countatoms(cdr lst))))
   )
  )
