					;returns all the atomic elements of the list
(defun fringe(lst)
  (cond
  ((null lst) nil)
  ((not (listp (car lst))) (cons (car lst) (fringe(cdr lst))))
  ((listp (car lst)) (append (fringe (car lst)) (fringe (cdr lst))))
  )
  )
