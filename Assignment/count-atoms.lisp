					; returns the number of atoms at the top level of the list
(defun count-atoms(lst)
  (cond
   ((null lst) nil)
   ((listp (car lst)) 0)
   ((not (listp (car lst))) (+ (count-atoms(cdr lst)) 1))
   )
  )
