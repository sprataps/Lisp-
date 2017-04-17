;Name: SIDDHARTH PRATAP SINGH
(defun sample-test ()
; This is an example call to TravelPlannerAgent
   (TravelPlannerAgent "newark" "bangor" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP))


 (defun TravelPlannerAgent
  (start-city  goal-city goal-test? get-successors get-goal-estimate) 

  (defun search-graph (open-closed)
    ;(write open-closed)
       (cond((null (car open-closed)) nil)
          (t (let((selected-node (caar open-closed)))
                 (terpri)
         
                 (format t 
                    "The nodes, f-values, and actions on open list are ~A" 
                     (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (car open-closed)))
                 (terpri)
                 (format t 
                     "The nodes, f-values, and actions on closed list are ~A" 
                      (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (cadr open-closed)))
                 (terpri) (terpri)
                 (format t "Select a new node from open list")
                 (terpri) 
                (format t "The selected node is ~A" 
                          (caar open-closed))
                (terpri)
                (format t "Check if this node is the goal node")
                (terpri)
                (cond((funcall goal-test? selected-node goal-city)
                          (terpri)
                          (format t "This is the goal node")
                          (terpri)
                          (format t "Here is the list of actions and total path cost in the solution")
                          (terpri)
                          (get-path-and-total-cost selected-node))
                     (t (let ((successors (funcall get-successors
                                                   selected-node)))
                        (format t "This is NOT the goal node")
                        (terpri)
                         (format t "Its successors (and their arc costs) are ~A"
                                  successors)
                        (terpri)

                        (search-graph
                           (expand-graph 
                             successors
                             selected-node
                             (list (cdr (car open-closed))
                                   (cons selected-node 
                                         (cadr open-closed)))
                             get-successors
                             get-goal-estimate 
                             goal-city)))))))))
; create a node for start-city and begin the search
  (search-graph 
   (list(list (create-node start-city nil nil 0 
                           get-goal-estimate goal-city))
   nil)))
      
 (defun expand-graph
     (succs parent-node open-closed succ-fn est-goal goal-city)
  ; (break "The open closed list is ~A" (list (car open-closed) (cdr open-closed)))
   (cond ((null succs) open-closed)
	 (t 
;         process the next successor
           (let* ((state (caar succs))
                  (node-name 
                      (intern (concatenate 'string 
                                 "Node-" state)))
 		  (arccost (caddar succs))
                  (action (list (caar succs) (cadar succs)))
 		  (cost (+ (get parent-node 'best-path-cost)
			    arccost)))
              (format t "     The next successor is ~A" (car succs))
              (terpri)
              ;(break "in expand-graph")
              (cond ((and (not (state-on state (car open-closed)))
			  (not (state-on state (cadr open-closed))))
; this successor is not on open or closed list
                       (format t "this successor is not on open or closed list") 
                       (terpri)    
                       (expand-graph (cdr succs)
                                      parent-node
                                     (list (add-to-open 
                                           (create-node (caar succs) 
                                                     action
                                                     parent-node 
                                                     cost 
                                                     est-goal 
                                                     goal-city)
                                            (car open-closed))
                                         (cadr open-closed))
                                      succ-fn
                                      est-goal
                                      goal-city))
		    ((and (state-on state (car open-closed))
                          (< cost (get node-name 'best-path-cost)))
; this successor is already on open list and we have
;    found a better path to it
                     (format t "**** ON OPEN AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                   (update-node-open node-name
                                                      parent-node
                                                      succ-fn
                                                      cost
                                                      action
                                                      open-closed)
                                    succ-fn
                                    est-goal
                                    goal-city))
                     ((and (state-on state (cadr open-closed))
                           (< cost (get node-name 'best-path-cost)))
; this successor is already on closed list and we have
;    found a better path to it
                     (format t "*** ON CLOSED AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                    (update-node-closed node-name
                                                        parent-node
                                                        succ-fn
                                                        cost
                                                        action
                                                        open-closed)
                                    succ-fn
                                    est-goal
                                    goal-city))
		    (t 
; this successor is already on open or closed and the new path
;   to the node is not better than the existing path
                      (format t "this successor is on open or closed but path is not better")
                      (terpri)
                      (expand-graph (cdr succs)
				    parent-node
				    open-closed 
				    succ-fn
				    est-goal
                                    goal-city)))))))

(defun update-node-open (n parent successor-fn cost-of-short-path action open-closed )
  ;(format t"Entering update-node-open with ~A"n)

  (setf (get n 'parent) parent)
  (setf (get n 'action) action)
  
  (setf (get n 'least-cost-estimate) (+ (- (get n 'least-cost-estimate) (get n 'best-path-cost)) cost-of-short-path))
  (setf (get n 'best-path-cost) cost-of-short-path)
  (adjust-open n (car open-closed))
  ;(format t"HERE")
  ;(format t"OPEN CLOSED ~A"open-closed)
 open-closed
 )

(defun update-node-closed (n parent successor-fn cost-of-short-path action open-closed)

  (setf (get n 'parent) parent)
  (setf (get n 'action) action)
  (setf (get n 'best-path-cost) cost-of-short-path)
  (setf (get n 'least-cost-estimate) (+ (get n 'best-path-cost) (get n 'cost-to-goal-estimate)))
  
  ;(break "Node Updated ~A" n)
  (list (car  (change-open-list n (funcall successor-fn n) successor-fn cost-of-short-path open-closed)) (change-value-closed-node n parent (funcall successor-fn n) cost-of-short-path action (cadr open-closed)))
  )

(defun change-value-closed-node(n parent successor-list cost-of-short-path action closed)
  ;(break "inside change-value-closed-node")
  ;(format t"Successor-list ~A"successor-list)
  (let ((firstsuccs (intern (concatenate 'string "Node-" (caar successor-list)))))
 
  (cond
   ((null successor-list) closed)
   ((not ( null(member firstsuccs closed))) (change-value-closed-node n parent (cdr successor-list) cost-of-short-path action (change-closed-list (car successor-list) n cost-of-short-path (cadar successor-list) closed )))
   (t (change-value-closed-node n parent (cdr successor-list) cost-of-short-path action closed))
   )
  )
  )

(defun update-successor-value-closed-node(n parent cost-of-short-path action closed)
					  (setf (get n 'action) action)
				          (setf (get n 'parent) parent)
					  (setf (get n 'best-path-cost) cost-of-short-path)
					  (setf (get n 'least-cost-estimate) (+ (get n 'cost-to-goal-estimate) cost-of-short-path))
					 ; closed
   )

(defun change-closed-list ( successor-list parent cost-of-short-path action closed)
  (format t"Inside change-closed-list")
  (format t"Successor ~A" successor-list)
  (format t"CADAR ~A" (cadr successor-list))
    (let* ((cost (cond
		  ((string= (cadr successor-list) "fly") (+ (caddr successor-list) cost-of-short-path))
		  ((string= (cadr successor-list) "take-bus") (cond
								((< (caddr successor-list) 400) (+ (caddr successor-list) cost-of-short-path))
								(t (+ (* (caddr successor-list) 2) cost-of-short-path))))
		  ((string= (cadr successor-list) "take-train") (cond
								  ((< (caddr successor-list) 800) (+ (caddr successor-list) cost-of-short-path))
								  (t (+ (* (caddr successor-list) 1.5) cost-of-short-path))))
		  ))
								
		  
       (node-name (intern (concatenate 'string "Node-" (car successor-list)))))
;	)
   ; (change-closed-list  (cdr successor-list) n  cost-of-short-path (update-successor-value-node node-name n cost (cadar successor-list) closed)))
      (update-successor-value-closed-node node-name  parent cost (cadr successor-list) closed)
      closed
    ))							     

(defun change-open-list(n  successor-list successor-fn  cost-of-short-path open-closed)
 ; (break "Entering change-open-list")
 ; (format t "Values in the open and closed list ~A" open-closed)
 ; (format t"Values: node ~A successor-list ~A"n successor-list)
  ;(break "Inside change-open-list")
  ;(format t"Successor list ~A"successor-list)
  (cond
   ((null successor-list) open-closed)
   (t  (let* ((firstsuccs (intern (concatenate 'string "Node-" (caar successor-list)))))
    (cond
     ((not (null (member firstsuccs (car open-closed))))
         (let* ((cost (cond
		  ((string= (cadar successor-list) "fly") (+ (caddar successor-list) cost-of-short-path))
		  ((string= (cadar successor-list) "take-bus") (cond
								((< (caddar successor-list) 400) (+ (caddar successor-list) cost-of-short-path))
								(t (+ (* (caddar successor-list) 2) cost-of-short-path))))
		  ((string= (cadar successor-list) "take-train") (cond
								  ((< (caddar successor-list) 800) (+ (caddar successor-list) cost-of-short-path))
								  (t (+ (* (caddar successor-list) 1.5) cost-of-short-path))))
		  )))
	
       
	   
	   (change-open-list n (cdr successor-list) successor-fn cost-of-short-path (update-node-open firstsuccs n successor-fn cost (cadar successor-list) open-closed))))
     (t (change-open-list n (cdr successor-list) successor-fn cost-of-short-path open-closed))
     )
    )
       )  
   )
  )
 
(defun state-on (state lst)

;(break "entering state-on")
; state is a city represented as a string such as "denver"
; lst is an open or closed list
; return true if a node on lst has this city as its state
; YOU MUST WRITE THIS FUNCTION
  (cond ((null lst) nil)
	(t (cond
	    ((string= (get (car lst) 'state) state) t)
	    (t (state-on state (cdr lst))))
	    
	    )
	   )
  
)
       
(defun add-to-open (n open)
; n is a node and open is the open list
; add n to the open list in the correct position 
; return the new open list
; YOU MUST WRITE THIS FUNCTION
;  (break "entering add-to-open")
 ; (write n)
  ;(write open)
  ;(write (get (car open) 'least-cost-estimate))
  (cond ((null open) (list n))
	(t
	 (cond
	  ((< (get (car open) 'least-cost-estimate) (get n 'least-cost-estimate)) (append (list (car open)) (add-to-open n (cdr open))))
       	((> (get (car open) 'least-cost-estimate) (get n 'least-cost-estimate)) (cons n open))
	((= (get (car open) 'least-cost-estimate) (get n 'least-cost-estimate)) (cons n open))
	)
	 )
	)
)


(defun adjust-open (n open)
;(break "entering adjust-open")
; n is a node and open is the open list
; make sure that n is in its proper position on open list, and if not
;   move it to the proper position
; the reason that n may not be in its proper position is that a better
;   path to it may have been found, thereby changing its f value
; return the new open list
					; YOU MUST WRITE THIS FUNCTION
  (cond ((null open) nil)
	((string= (get n 'state) (get (car open) 'state)) (adjust-open n (cdr open)))
	(t (cons (car open) (adjust-open n (cdr open))))
	)
  )

(defun check-member (n open)
  (
   
;  (let* ((new-open (list(car open)
 ; (cond ((null open) nil )
;	((not (string= (get (cadr open) 'state) (get n 'state))) (adjust-open n (cdr open))); (let* ((new-open (list (car open) (adjust-open n (cdr open) )))) (add-to-open n new-open)))
;	(t (cddr open))

	)
  
	
  )


(defun create-node 
  (city action parent cost-of-short-path est-goal goal-city)
  ; city is a string representing the name of a city.
  ;   Create a new node with this city as its state and
  ;   with the appropriate properties
  ; action is the action that moved from parent to city.  
  ; parent is the parent node.
  ; cost-of-short-path is the cost of the path from the
  ;   initial state to the state represented by this new
  ;   node and goes through parent.
  ; goal-city is a string representing the goal city
  ; est-goal is a parameter giving the function for estimating
  ;   the cost of getting to the goal from this new node 
  ; create a new node with the appropriate properties
  ; return the created node.
 ; (format t "The city ~A, action ~A, parent ~A, cost ~D, estimate ~D, goal city ~A" city action parent cost-of-short-path est-goal goal-city)
  (let ((node (intern (concatenate 'string 
                                 "Node-" 
                                 city
                                 ))))
  (setf (get node 'state) city)
  (setf (get node  'parent) parent)
  (setf (get node 'action) action)
  (setf (get  node 'best-path-cost) cost-of-short-path)
  (setf (get node 'cost-to-goal-estimate) (funcall est-goal city goal-city)) 
  (setf (get  node `least-cost-estimate)
        (+ cost-of-short-path (get node 'cost-to-goal-estimate)))
  ;(write node)
  node))
    
(defun get-path-and-total-cost (node)
; node is a node in the graph
; return a list consisting of two elements: the path (in terms of 
;    a list of successive actions) that was taken to get to node   
;    and cost of that path
; YOU MUST WRITE THIS FUNCTION.
 ; (break "inside get-path=and-total-cost")
  (cond
   ((null node) nil)
   (t
    (list (calculate-path (get node 'parent) (list (get node 'action)) ) (get node 'best-path-cost))
   ))
  )

(defun calculate-path(node path-list)
  ;(write node )
 ; (write path-list)
  (cond
   ((null (get node 'parent))  path-list)
    (t (calculate-path (get node 'parent) (append (list (get node 'action)) path-list)))
    )
   )


(defun successors-TP (cnode)
  
  (let* ((flylist (generate-fly-children cnode (get (intern (get cnode 'state))'fly)))
	(buslist (generate-bus-children cnode (get (intern (get cnode 'state)) 'take-bus)))
	(trainlist (generate-train-children cnode (get (intern (get cnode 'state))'take-train))))

  
 (append flylist buslist trainlist))
  
  )


(defun generate-fly-children(cnode flylist)
					;generates a list of 3 tuple entries containing the cities accesible by flights from cnode
  (cond
  ((null flylist) nil)

   (t  (append (list (list (car flylist) (intern "fly") (get-distance-cost  "fly" (car flylist) (get (intern (get cnode 'state)) 'distance)))) (generate-fly-children cnode (cdr flylist)))
      )
   )
  )

(defun generate-bus-children (cnode buslist)
;generates a list of 3 tuple entries containing the cities accesible by bus from cnode
  (cond
   ((null buslist) nil)
   (t (append  (list (list (car buslist)(intern "take-bus") (get-distance-cost "take-bus" (car buslist) (get (intern (get cnode 'state)) 'distance)))) (generate-bus-children cnode (cdr buslist)))
      )
   )
  )

(defun generate-train-children (cnode trainlist)
;generates a list of 3 tuple entries containing the cities accesible by train from cnode
  (cond
  ((null trainlist) nil )
   (t (append( list (list (car trainlist) (intern "take-train") (get-distance-cost "take-train" (car trainlist) (get (intern (get cnode 'state)) 'distance)))) (generate-train-children cnode (cdr trainlist)))
      )
   )
  )

(defun get-distance-cost (mode city city-from-cnode)
					;gets the distance of the city from the node cnode
  (cond
    ((null city-from-cnode) nil)
    ((string= city (caar city-from-cnode)) (cond
					    ((string= mode "fly")(cadar city-from-cnode))
					    ((string= mode "take-bus")(cond
								       ((< (cadar city-from-cnode) 400) (cadar city-from-cnode))
								       (t (* (cadar city-from-cnode) 2))))
					    ((string= mode "take-train") (cond
									  ((< (cadar city-from-cnode) 800) (cadar city-from-cnode))
									  (t (* (cadar city-from-cnode) 1.5))))
					    ))
					
    (t (get-distance-cost mode  city (cdr city-from-cnode)))
    )
   )
 
 




(defun goal-test-TP? (node goal-city)
; node is a node and goal-city is a string giving the name of the 
;    goal city
; return True if the city for this node is goal-city
; YOU MUST WRITE THIS FUNCTION
  (cond
   ((string= (get node 'state) goal-city) t)
 ;  (t (nil))
   )
  )

(defun get-goal-estimate-TP (city goal-city)
; city and goal-city are both strings giving city names
; return an estimate of the cost of getting from city to goal-city
; YOU MUST WRITE THIS FUNCTION
  (cond
    ((string= city goal-city) 0)
    (t ( get-goal-distance (get (intern city) 'distance) goal-city))
    )

  )

(defun get-goal-distance (goallst goal)
  (cond
   ((null goallst)  nil)

   ((string= (caar goallst) goal) (cadar goallst))
   (t (get-goal-distance (cdr goallst) goal))
  )
)
