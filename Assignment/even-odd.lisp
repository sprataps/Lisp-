					;function takes input a list integers as arguments and returns a list where even appear first and then odd integers appear

(defun even-odd(lst)
  (cond
   ((null lst) nil))
   (append (even-list lst) (odd-list lst))
  
  )

(defun even-list(lst)
  (cond
   ((null lst) nil)
   ((= (mod (car lst) 2) 0) (cons (car lst)(even-list(cdr lst))))
   (t (even-list(cdr lst)))
   )
  )

(defun odd-list(lst)
  (cond
   ((null lst) nil)
   ((= (mod (car lst) 2) 1) (cons (car lst) (odd-list(cdr lst))))
   (t (odd-list(cdr lst)))
   )
  )
