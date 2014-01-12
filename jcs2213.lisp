;;; AI Assignment 3
;;; Automated Theorem Prover
;;; John Sizemore

(defstruct func (name nil) (args nil)) ; Function structure definition. The "args" parameter must be of form LIST

(defun varp (item)   ; function to determine whether or not something is a variable or function
  (if (symbolp item)
      (equal #\* (elt (symbol-name item) 0))
      (func-p item)
      )
  )

; Unification functions that provides a mapping between variables and other variables/constants.
(defun unify (x y &optional theta)
  (cond ((eql theta 'fail) 'fail)
	((eql x y) theta)
	((varp x) (unify-var x y theta))
	((varp y) (unify-var y x theta))
	((and (consp x) (consp y))
	 (unify (cdr x) (cdr y) (unify (car x) (car y) theta)))
	(t 'fail)))

(defun unify-var (var x theta)
  (let ((vb (assoc var theta))
	(xb (assoc x theta)))
    (cond (vb (unify (cdr vb) x theta))
	  (xb (unify var (cdr xb) theta))
	  ((occurs-p var x theta) 'fail)
	  (t (cons (cons var x) theta)))))

(defun occurs-p (var x theta)
  (cond ((eql var x) t)
	((and (varp x) (assoc x theta))
	 (occurs-p var (cdr (assoc x theta)) theta))
	((consp x) (or (occurs-p var (car x) theta)
		       (occurs-p var (cdr x) theta)))
	(t nil)))

; Function to return a mapped value of an input
(defun get-mapping (var theta)
	   (cond
	     ((and (listp var) (equal 1 (list-length var))) (get-mapping (car var) theta))
	     ((listp var) (append (get-mapping (car var) theta) (get-mapping (cdr var) theta)))
	     ((null (assoc var theta)) (list var))
	     ((null (cdr (assoc var theta))) (list var))
	     ((get-mapping (cdr (assoc var theta)) theta))
	     )
	   )

;;; QUEUEING FUNCTIONS

(defstruct q
	(enqueue #'identity)
	(last nil)
	(elements nil))

; Function checking if the queue is empty.
(defun q-emptyp (q)
	(= (length (q-elements q)) 0))

; Function returning what is at the front of the queue.
(defun q-front (q)
	(elt (q-elements q) 0))

; Function removing the head element of the queue.
(defun q-remove (q)
	(when (list (q-elements q))
		(pop (q-elements q))))

; Function inserting an element into the queue. Calls the queueing function to do so; in this case, the A* enqueuing function defined above.
(defun q-insert (q items)
	(funcall (q-enqueue q) q items)
	q)

;;; KB's and NQ's

; Coral Reef Club
(defparameter kb (list 
	  (list (make-func :name 'WS :args '(*x)) (make-func :name 'SD :args '(*x)))
	  (list (list '~ (make-func :name 'SD :args '(*y))) (list '~ (make-func :name 'LIKES :args '(*y Waves))))
	  (list (list '~ (make-func :name 'WS :args '(*z))) (make-func :name 'LIKES :args '(*z Warm)))
	  (list (list '~ (make-func :name 'LIKES  :args '(Laura *w))) (list '~ (make-func :name 'LIKES :args '(Jacob *w))))
	  (list (make-func :name 'LIKES :args '(Jacob *w)) (make-func :name 'LIKES :args '(Laura *w)))
	  (list (make-func :name 'LIKES :args '(Jacob Warm)))
	  (list (make-func :name 'LIKES :args '(Jacob Waves)))))

; Smuggler
(defparameter kb2 (list 
	   (list 
	    (list '~ (make-func :name 'E :args '(*x))) 
	    (make-func :name 'V :args '(*x)) 
	    (make-func :name 'S :args (cons '*x (cons (make-func :name 'F :args '(*x)) nil))))
	   (list 
	    (list '~ (make-func :name 'E :args '(*x))) 
	    (make-func :name 'V :args '(*x)) 
	    (make-func :name 'C :args (list (make-func :name 'F :args '(*x)))))
	   (list (make-func :name 'P :args '(C)))
	   (list (make-func :name 'E :args '(C)))
	   (list (list '~ (make-func :name 'S :args '(C *y))) (make-func :name 'P :args '(*y)))
	   (list (list '~ (make-func :name 'P :args '(*z))) (list '~ (make-func :name 'V :args '(*z))))))

; Colonel West
(defparameter kb3 
  (list 
   (list 
    (list '~ '#S(FUNC :NAME AMERICAN :ARGS(*X))) 
    (list '~ '#S(FUNC :NAME WEAPON :ARGS(*Y))) 
    (list '~ '#S(FUNC :NAME SELLS :ARGS(*X *Y *Z))) 
    (list '~ '#S(FUNC :NAME HOSTILE :ARGS(*z))) 
    '#S(FUNC :NAME CRIMINAL :ARGS(*X)))
   (list
    '#S(FUNC :NAME AMERICAN :ARGS(WEST)))
   (list
    (list '~ '#S(FUNC :NAME MISSILE :ARGS(*X))) 
    '#S(FUNC :NAME WEAPON :ARGS(*X)))
   (list
    '#S(FUNC :NAME MISSILE :ARGS(M1)))
   (list
    (list '~ '#S(FUNC :NAME MISSILE :ARGS(*X))) 
    (list '~ '#S(FUNC :NAME OWNS :ARGS(FOO *X))) 
    '#S(FUNC :NAME SELLS :ARGS(WEST *X FOO)))
   (list
    '#S(FUNC :NAME OWNS :ARGS(FOO M1)))
   (list
    (list '~ '#S(FUNC :NAME ENEMY :ARGS(*X AMERICA))) 
    '#S(FUNC :NAME HOSTILE :ARGS(*X)))
   (list
    '#S(FUNC :NAME ENEMY :ARGS(FOO AMERICA)))))

(defparameter kb4 (list
		   (list
		    (make-func :name 'Animal :args '(#S(FUNC :NAME F :ARGS (*X))))
		    (make-func :name 'Loves :args '(#S(FUNC :NAME G :ARGS (*X)) *X)))
		   (list
		    (list '~ (make-func :name 'Loves :args '(*X #S(FUNC :NAME F :ARGS (*X)))))
		    (make-func :name 'Loves :args '(#S(FUNC :NAME G :ARGS (*X)) *X)))
		   (list
		    (list '~ (make-func :name 'Loves :args '(*Y *X)))
		    (list '~ (make-func :name 'Animal :args '(*Z)))
		    (list '~ (make-func :name 'Kills :args '(*X *Z))))
		   (list
		    (list '~ (make-func :name 'Animal :args '(*X)))
		    (make-func :name 'Loves :args '(Jack *X)))
		   (list
		    (make-func :name 'Kills :args '(Jack Tuna))
		    (make-func :name 'Kills :args '(Curiosity Tuna)))
		   (list
		    (make-func :name 'Cat :args '(Tuna)))
		   (list
		    (list '~ (make-func :name 'Cat :args '(*X)))
		    (make-func :name 'Animal :args '(*X)))))

(defparameter nq4 (list
		   (list
		    (list '~ (make-func :name 'Kills :args '(Curiosity Tuna))))))

; Coral Reef Club negated query
(defparameter nq (list 
	  (list 
	   (list '~ (make-func :name 'SD :args '(*v))) (make-func :name 'WS :args '(*v)))))

; Smuggler negated query
(defparameter nq2 (list 
	   (list 
	    (list '~ (make-func :name 'P :args '(*w))) (list '~ (make-func :name 'C :args '(*w))))))

; Colonel West negated query
(defparameter nq3 
  (list
   (list
    (list '~ (make-func :name 'CRIMINAL :args '(WEST))))))

(defparameter ass (list
		    (list
		     (make-func :name 'CRIMINAL :args '(WEST)))))

; Compliment check. Checks if two functions are compliments of one another for elimination purposes.
; For a compliment, one item must be a function and the other must be a list (because of the leading ~ character)
(defun complp (func1 func2)
  (cond
    ((or (and (func-p func1) (func-p func2)) (and (listp func1) (listp func2))) nil)
    ((and (listp func1) (func-p func2))
     (cond
       ((and (equal '~ (car func1)) (equal (func-name (cadr func1)) (func-name func2))) t)
       ))
    ((and (func-p func1) (listp func2))
     (cond
       ((and (equal '~ (car func2)) (equal (func-name (cadr func2)) (func-name func1))) t)
       ))
    )
  )

; Checks if two functions are the same; can be complementary.
(defun samep (func1 func2)
  (cond
    ((and (func-p func1) (func-p func2))
     (equal (func-name func1) (func-name func2)))
    ((and (listp func1) (listp func2))
     (equal (func-name (cadr func1)) (func-name (cadr func2))))
    ((and (func-p func1) (listp func2))
     (equal (func-name func1) (func-name (cadr func2))))
    ((and (listp func1) (func-p func2))
     (equal (func-name (cadr func1)) (func-name func2)))
    )
  )

; Checks if two functions are identical in name. Must not be complimentary.
(defun identp (func1 func2)
  (cond
    ((and (func-p func1) (func-p func2))
     (equal (func-name func1) (func-name func2)))
    ((and (listp func1) (listp func2))
     (equal (func-name (cadr func1)) (func-name (cadr func2))))
    (t nil))
  )

; Node structure definition. Contains the kb and nq sentences used in deriving resolvent as well as the resolvent itself. Terms-removed is how many complementary
; pairs were removed to achieve the current resolvent. Leftovers counts the number of clauses in the resolvent. Theta is the binding list, and parent is the parent
; node i.e. which resolvent was used to create the current resolvent.
(defstruct resolvent
  (kb-sentence nil)
  (nq-sentence nil)
  (resolvent nil)
  (terms-removed 0)
  (leftovers nil)
  (theta nil)
  (parent nil))

; Insertion function into the queue. Resolvents are queued in ascending order based on (terms-removed + leftovers) or g(n) + h(n). Terms-removed is how many complementary
; pairs were used to create a resolvent; leftovers is the number of clauses in the resolvent (i.e. the heuristic)
(defun sos-insert (q resolvents)
  (let
      ((resolvent (car resolvents))
       (inserted nil))
    (unless (null resolvent)
      (loop for i in (q-elements q) do
	   (when (and (null inserted) (<= (+ (resolvent-terms-removed resolvent) (resolvent-leftovers resolvent)) (+ (resolvent-terms-removed i) (resolvent-leftovers i))))
	     (setf inserted t)
	     (setf (q-elements q) (append (subseq (q-elements q) 0 (position i (q-elements q))) (list resolvent) (subseq (q-elements q) (position i (q-elements q)))))   
	     )
	   )
      (when (null inserted)
	(setf (q-elements q) (append (q-elements q) (list resolvent)))
	)
      )
    )
  (unless (null (cdr resolvents))
    (sos-insert q (cdr resolvents))
    )
  )

; Wrapper function to insert resolvents into queue
(defun enqueue-sos (q resolvents)
  (sos-insert q resolvents)
) 

;;; ATP FUNCTIONS

(defun map-args (args theta) ; function mapping values from theta and modifying the argument lists of functions to reflect bindings
  (let 
      ((return-list nil))
    (loop for arg in args do
	 (cond
	   ((func-p arg) 
	    (let 
		((copy (copy-func arg)))
		 (setf (func-args copy) (map-args (func-args copy) theta))
		 (setf return-list (append (list copy) return-list))
		   )
	    )
	   (t (setf return-list (append (get-mapping arg theta) return-list)))
	   )
	 )
    (reverse return-list)
    )
  )

; Function which takes a kb-sentence and an nq-sentence and returns all possible resolvents from that pair.
(defun resolve (kb-sentence nq-sentence &optional (parent nil))
  (let 
      ((remove-list nil)             ; list for complementary pairs; used in pruning
       (resolved-set nil)            ; list for all remaining pairs used in resolvent
       (orig-kb-sent kb-sentence)    ; copy original kb and nq since modifications later on change certain values
       (orig-nq-sent nq-sentence))

    (let                ; building theta
	((theta nil))
      (loop for kb-clause in kb-sentence do
	   (loop for nq-clause in nq-sentence do
		(when (identp kb-clause nq-clause)
		  (let
		      ((kb-args 
			(if (func-p kb-clause)
			    (func-args kb-clause)
			    (func-args (cadr kb-clause))))
		       (nq-args
			(if (func-p nq-clause)
			    (func-args nq-clause)
			    (func-args (cadr nq-clause)))))
		    (setf theta (unify nq-args kb-args theta))
		    )
		  )
		)
	   )
		    
      (loop for kb-clause in kb-sentence do   ; check for complements; if any exist, add them to the remove list
	   (loop for nq-clause in nq-sentence do
		(when (complp kb-clause nq-clause)
		  (let
		      ((kb-args 
			(if (func-p kb-clause)
			    (func-args kb-clause)
			    (func-args (cadr kb-clause))))
		       (nq-args
			(if (func-p nq-clause)
			    (func-args nq-clause)
			    (func-args (cadr nq-clause)))))
		    (unless (equal 'fail (unify nq-args kb-args theta)) ; pruning resolvents that have unsatisfiable thetas
		      (setf remove-list (append (list (list kb-clause nq-clause (unify nq-args kb-args theta))) remove-list)))
		    )
		  )
		)
	   )
      )

    (unless (null remove-list)       ; disables expanding nodes that don't actually remove terms
      (let                           ; copy nq structures
	  ((copy-list nil))
	(loop for nq-clause in nq-sentence do
	     (if (func-p nq-clause)
		 (setf copy-list (append copy-list (list (copy-func nq-clause))))
		 (setf copy-list (append copy-list (list (list '~ (copy-func (cadr nq-clause))))))
		 )
	     )
	(setf nq-sentence copy-list)
	)

      (let                           ; copy kb structures
	  ((copy-list nil))
	(loop for kb-clause in kb-sentence do   
	     (if (func-p kb-clause)
		 (setf copy-list (append copy-list (list (copy-func kb-clause))))
		 (setf copy-list (append copy-list (list (list '~ (copy-func (cadr kb-clause))))))
		 )
	     )
	(setf kb-sentence copy-list)
	)

      (loop for remove-item in remove-list do ; build the resolvent set by subtracting complementary terms from the kb and nq sentences.
	   (let
	       ((new-kb (remove (car remove-item) kb-sentence :test #'equalp))
		(new-nq (remove (cadr remove-item) nq-sentence :test #'equalp)))
	     (cond
	       ((and (null new-kb) (null new-nq)) (setf resolved-set (append (list (list nil (caddr remove-item))) resolved-set)))
	       ((null new-kb) (setf resolved-set (append (list (list new-nq (caddr remove-item))) resolved-set)))
	       ((null new-nq) (setf resolved-set (append (list (list new-kb (caddr remove-item))) resolved-set)))
	       (t (setf resolved-set (append (list (list (append new-kb new-nq) (caddr remove-item))) resolved-set))))
	     )
	   )

      (loop for resolved-item in resolved-set do   ; map arguments from theta
	   (loop for resolved-func in (car resolved-item) do
		(let
		    ((temp-theta (car (last resolved-item))))
		    (let
			((args (if (listp resolved-func)
				   (func-args (cadr resolved-func))
				   (func-args resolved-func))))
		      (setf args (map-args args temp-theta))
		      (if (listp resolved-func)
			  (setf (func-args (cadr resolved-func)) args)
			  (setf (func-args resolved-func) args)
			  )
		      )
		    )
		)
	   )
	   

      (let 
	  ((resolvents nil))  ; create the list of resolvents to queue
	(loop for resolved-item in resolved-set do
	     (let*
		 ((resolvent (car (remove (car (last resolved-item)) resolved-item)))
		  (terms-removed (/ (- (+ (list-length orig-kb-sent) (list-length orig-nq-sent)) (list-length resolvent)) 2)))
	       (loop for clause in resolvent do
		    (when (> (count (car resolvent) resolvent :test #'samep) 1)
		      (setf resolvent (remove clause resolvent))))
	       (unless (null parent)
		 (setf terms-removed (+ terms-removed (resolvent-terms-removed parent))))
	       (setf resolvents (append (list (make-resolvent :kb-sentence orig-kb-sent 
							      :nq-sentence orig-nq-sent
							      :resolvent (list resolvent)
							      :terms-removed terms-removed
							      :leftovers (list-length resolvent)
							      :theta (car (last resolved-item)) 
							      :parent parent)) resolvents))
	       )
	     )
	resolvents
	)
      )
    )
  )

; Wrapper function for "resolve" that returns all resolvents from a particular kb and nq state.
(defun get-resolvents (kb nq)
  (let
      ((resolvent-list nil)
       (parent (if (resolvent-p nq)
		   nq
		   nil)))
    (when (resolvent-p nq)
      (setf nq (resolvent-resolvent nq)))
    (loop for kb-sentence in kb do
	 (loop for nq-sentence in nq do
	      (let
		  (
		   (resolvents (resolve kb-sentence nq-sentence parent)))
		(when (not (null resolvents))
		  (setf resolvent-list (append resolvents resolvent-list))
		  )))
	 )
    resolvent-list
    )
  )

; in the event a nil resolvent is found, this function returns all of the parents with their kb, nq, and theta values
(defun resolve-sequence (resolvent)
  (cond
    ((null (resolvent-parent resolvent)) (list (list (resolvent-kb-sentence resolvent) (resolvent-nq-sentence resolvent) (resolvent-resolvent resolvent) (resolvent-theta resolvent))))
    (t (append (resolve-sequence (resolvent-parent resolvent)) (list (list (resolvent-kb-sentence resolvent) (resolvent-nq-sentence resolvent) (resolvent-resolvent resolvent) (resolvent-theta resolvent)))))
    )
  )

; recursive function for prove; a resolvent is popped from the queue and first checked to see if the resolvent is nil. If it is, the list of parents
; is returned to show the path to the proved theorem. Otherwise, the resolvent generated new resolvents to queue and the process begins anew.
(defun prove (kb queue closed) 
  (unless (q-emptyp queue)
    (let ((resolvent (q-remove queue)))
      (cond ((null (car (resolvent-resolvent resolvent)))
	     (resolve-sequence resolvent))
	    ((member (resolvent-resolvent resolvent) closed
		     :test #'equalp :key #'resolvent-resolvent)
	     (prove kb queue closed))
	    (t (let ((resolvents (get-resolvents kb resolvent)))
		 (prove kb (q-insert queue resolvents)
			       (cons resolvent closed)))
	       ))
      )))

; Initializes the queue with all resolvents from the base negated query and the KB. After this is completed, the recursive "prove" function is called.
(defun init (kb nq &key (enqueue #'identity))
  (let
      ((queue (make-q :enqueue enqueue)))
    (q-insert queue (get-resolvents kb nq))
    (prove kb queue nil)
    )
  )

; Wrapper ATP function to start the proving process
(defun atp (kb nq)
  (init kb nq :enqueue #'enqueue-sos)
  )

;;; SNF FUNCTIONS

; this function removes unnecessary lists that are a result of the converting process
(defun remove-unnecessary-lists (sentence) 
  (loop for i in sentence do
       (cond
	 ((and (listp i) (listp (car i)) (null (cdr i)))
	   (setf (nth (position i sentence) sentence) (car i)))
	 ((listp i) (remove-unnecessary-lists i))))
  sentence
  )

; this function distributes "OR" operators to yield a result in disjunctive normal form
(defun distribute (sentence)
  (let*
      ((or-pos (when (listp sentence)
		 (position '|| sentence)))
       (or-before (unless (null or-pos)
		 (elt sentence (1- or-pos))))
       (or-after (unless (null or-pos)
		(elt sentence (1+ or-pos))))
       (sent-before (unless (null or-pos)
		      (car (subseq sentence 0 or-pos))))
       (sent-after (unless (null or-pos)
		     (car (subseq sentence (1+ or-pos) (list-length sentence))))))
    (cond
      ((null sentence) nil)
      ((and (null or-pos) (listp sentence)) (append (list (distribute (car sentence))) (distribute (cdr sentence))))
      ((and (listp or-before) (member '&& or-before))
       (let*
	   ((and-pos (position '&& or-before))
	    (and-before (elt or-before (1- and-pos)))
	    (and-after (elt or-before (1+ and-pos))))
	 (list (append sent-after (list '||) (list and-before)) '&& (list (append sent-after (list '||) (list and-after))))))
      ((and (listp or-after) (member '&& or-after))
       (let*
	   ((and-pos (position '&& or-after))
	    (and-before (elt or-after (1- and-pos)))
	    (and-after (elt or-after (1+ and-pos))))
	 (list (append sent-before (list '||) (list and-before)) '&& (list (append sent-before (list '||) (list and-after))))))
      (t sentence)
      )
    )
  )
       
; this function removes bi-direction operators. Recursively searches lists and removes like the following: (a <> b) -> ((a -> b) && (b -> a))
(defun remove-bi (clause)
  (if (atom clause)
      clause
      (let*
	  ((bi (position '<> clause))
	   (before (unless (null bi)
		     (elt clause (1- bi))))
	   (after (unless (null bi)
		    (elt clause (1+ bi)))))
	(cond
	  ((and (null bi) (append (list (remove-bi (car clause))) (remove-bi (cdr clause)))))
	  ((and (listp before) (listp after)) (list (list (remove-bi before) '-> (remove-bi after)) '&& (list (remove-bi after) '-> (remove-bi before))))
	  ((and (listp before)) (list (list (remove-bi before) '-> after)  '&& (list after '-> (remove-bi before))))
	  ((and (listp after)) (list (list before '-> (remove-bi after)) '&& (list (remove-bi after) '-> before)))
	  (t (list (list before '-> after) '&& (list after '-> before))))
	)))

; this function removes all implications. Recursively searches lists and removes like the following: (a -> b) -> (~a v b)
(defun remove-impl (clause)
  (if (atom clause)
      clause
      (let*
	  ((impl (position '-> clause))
	   (before (unless (null impl)
		     (elt clause (1- impl))))
	   (after (unless (null impl)
		    (elt clause (1+ impl)))))
	(cond
	  ((and (null impl) (append (list (remove-impl (car clause))) (remove-impl (cdr clause)))))
	  ((and (listp before) (listp after)) (list (list '~ (remove-impl before)) '|| (remove-impl after)))
	  ((and (listp before)) (list (list '~ (remove-impl before)) '|| after))
	  ((and (listp after)) (list (list '~ before) '|| (remove-impl after)))
	  (t (list (list '~ before) '|| after)))
	)))

; This function performs DeMorgan's law and moves all NOT operators that can be moved inward. Doing so allows for sentences to be converted to disjunctive normal form.
; Because DeMorgan's law is iterative, this function can be called serveral times until all NOT's are satisfied.
(defun demorgan (clause)
  (cond
    ((and (listp clause) (listp (car clause)) (> (list-length (car clause)) 2) (equal (caar clause) '~) (equal (cadar clause) 'univ))
		(append (list (setf (nth (position (car clause) clause) clause) (append (list 'exist) (cddar clause))))
			(setf (nth (1+ (position (car clause) clause)) clause) (list (append (list '~) (nth (1+ (position (car clause) clause)) clause))) 
			)))
    ((and (listp clause) (listp (car clause)) (> (list-length (car clause)) 2) (equal (caar clause) '~) (equal (cadar clause) 'exist)) 
		(append (list (setf (nth (position (car clause) clause) clause) (append (list 'univ) (cddar clause)))) 
			(setf (nth (1+ (position (car clause) clause)) clause) (list (append (list '~) (nth (1+ (position (car clause) clause)) clause))) 
			)))
    ((and (listp clause) (equal (car clause) '~) (equal (cadr clause) '~)) (setf clause (cddr clause)))
    ((and (listp clause) (equal (car clause) '~) (> (list-length clause) 2))
      (let
	  ((convert (cdr clause)))
	(loop for i in convert do
	     (cond
	       ((and (listp i) (equal (car i) 'univ)) (setf (nth (position i convert) convert) (append (list 'exist) (cdr i))))
	       ((and (listp i) (equal (car i) 'exist)) (setf (nth (position i convert) convert) (append (list 'univ) (cdr i))))
	       ((and (listp i) (> (list-length i) 2) (equal (car i) '~)) (demorgan (cdr i)))
	       ((and (listp i) (equal 2 (list-length i))) (equal (car i ) '~) (setf (nth (position i convert) convert) (cadr i)))
	       ((equal '&& i) (setf (nth (position i convert) convert) '||))
	       ((equal '|| i) (setf (nth (position i convert) convert) '&&))
	       (t (setf (nth (position i convert) convert) (list '~ i)))))
	convert))
    ((and (listp clause) (equal (car clause) '~) (listp (cadr clause)))
     (let
	 ((convert (cadr clause)))
       (setf clause (append (list '~) convert))))
    (t
     (let
	 ((temp clause))
       (loop for i in temp do
	    (when (listp i)
	      (setf (nth (position i temp) temp) (demorgan i))
	      )
	    )
       temp
       ))
    )
  )

; Wrapper function performing DeMorgan's law until satisfied (i.e. no additional changes)
(defun do-demorgan (sentence)
  (let*
      ((before (remove-impl sentence))
       (after (demorgan (remove-impl sentence))))
    (loop while (not (equalp before after)) do
      (setf before (remove-impl after))
      (setf after (demorgan (remove-impl after))))
    before))

; Function removing duplicate entries that sometimes occur in the skolemization binding list.
(defun remove-dups (binding-list)
  (let
      ((return-list nil))
    (loop for binding in binding-list do
	 (when (not (member binding return-list))
	   (setf return-list (append (list binding) return-list))))
    return-list
    )
  )

; Predicate function determining whether or not to standardize variables. If a duplicate variable appears in the quantifier list, then standardization must be done.
(defun standardizep (quants)
  (let
      ((standardize nil))
    (loop for quant in quants do
	 (when (> (count quant quants :test #'equalp) 1)
	   (setf standardize t)
	   )
	 )
    standardize
    )
  )

; Function which standardizes bindings for skolemization; makes sure dependent existential variables are given unique constant or function names.
(defun standardize-bindings (binding-list)
  (let
      ((func-names '(f g h i j k l m))
       (const-names '(C1 C2 C3 C4 C5 C6 C7 C8)))
    (loop for binding in binding-list do
	 (if (func-p (cadr binding))
	     (if (member (func-name (cadr binding)) func-names)
		 (pop func-names)
		 (setf (func-name (cadr binding)) (pop func-names)))
	     (if (member (cadr binding) const-names)
		 (pop const-names)
		 (setf binding (list (car binding) (pop const-names))))
	     )
	 )
    )
  binding-list
  )

; This function skolemizes variables. For each variable in the binding list, the sentence is searched and each applicable variable is replaced
; with its skolemized equivalent.
(defun skolemize (sentence binding-list)
  (loop for i in sentence do
       (cond
	 ((listp i) (skolemize i binding-list))
	 ((func-p i)
	  (let
	      ((args (func-args i)))
	    (loop for arg in args do
		 (when (not (null (assoc arg binding-list)))
		   (setf (nth (position arg args) args) (cadr (assoc arg binding-list)))))
	    (setf (func-args i) args))
	  )
	 )
       )
  sentence
  )

; Searches a skolemization binding-list for variables which need constants and adds them accordingly.
(defun set-constants (quants binding-list)
  (loop for quant in quants do
       (unless (equal (cadr quant) 'univ)
	 (let ((cant-add nil))
	   (loop for binding in binding-list do
	      (when (equal (car binding) (car quant))
		(setf cant-add t)))
	   (when (null cant-add)
	     (setf binding-list (append (list (list (car quant) 'C)) binding-list)))
	   )
	 )
       )
  binding-list
  )

; This is a convenience function which reverses the quantifier list for easier use in performing skolemization
(defun reverse-quants (quants)
  (loop for i in quants do
       (setf (nth (position i quants) quants) (reverse i)))
  quants)

; This function is what obtains the skolem bindings for existential operators. An applicable function or constant will be bound to each applicable skolemized variable.
(defun get-skolem-bindings (sentence quants &optional (binding-list nil))
  (cond
    ((null sentence) nil)
    ((and (listp sentence) (func-p (car sentence)))
     (let
	 ((args (func-args (car sentence)))
	  (univ-args nil)
	  (exist-args nil))
       (loop for arg in args do
	    (when (equal 'univ (cadr (assoc arg quants)))
	      (setf univ-args (append (list arg) univ-args)))
	    (when (equal 'exist (cadr (assoc arg quants)))
	      (setf exist-args (append (list arg) exist-args)))
	    )
       (when (and (not (null univ-args)) (not (null exist-args)))
	 (loop for exist-arg in exist-args do
	      (setf binding-list (append (list (list exist-arg (make-func :name 'F :args univ-args)))))))
       )
     (setf binding-list (append (get-skolem-bindings (cdr sentence) quants binding-list)))
     )
    ((listp sentence) (setf binding-list (append (get-skolem-bindings (car sentence) quants binding-list) (get-skolem-bindings (cdr sentence) quants binding-list) binding-list)))
    )
  binding-list
  )

; This function removes quantifiers. Changes the list with the quantifier to simply the cdr of that list.
(defun remove-quants (sentence)
  (cond
    ((null sentence) sentence)
    ((and (listp sentence) (listp (car sentence)) (or (equal (caar sentence) 'exist) (equal (caar sentence) 'univ))) (setf sentence (remove-quants (cdr sentence))))
    ((listp sentence) (setf sentence (append (list (remove-quants (car sentence))) (remove-quants (cdr sentence)))))
    (t sentence)))

; Function which actually changes the variable values when standardizing.
(defun change-vars (quants)
  (let
      ((variables '(*x *y *z *w *p *q *t *u *v *s *r *m *n)))
    (loop for i in quants do
	 (when (> (count i quants :test #'equalp) 1)
	   (setf (nth (position i quants) quants) (list (car i) (elt variables (1+ (position (cadr i) variables)))))))
    quants))

; Function which recursively searches a sentence and returns all quantifiers. Used in skolemization and standardization.
(defun get-quants (sentence)
  (let
      ((quant-list nil))
    (loop for i in sentence do
	 (cond
	   ((and (listp i) (or (equal (car i) 'exist) (equal (car i) 'univ))) (setf quant-list (append (list i) quant-list)))
	   ((listp i) (setf quant-list (append (get-quants i) quant-list)))))
    quant-list))

; Function which converts the argument lists of various functions with bindings present in the skolem variable binding list.
(defun change-func-vars (new-var funclist)
  (let
      ((binding-list (list (cadar funclist) (cadr new-var)))
       (functions (cadr funclist)))
    (loop for func in functions do
	 (let
	     ((args (func-args func)))
	   (loop for arg in args do
		(when (equal arg (car binding-list))
		  (setf (nth (position arg args) args) (cadr binding-list)))
		)
	   )
	 )
    functions
    )
  )

; Function which standardizes variables within a sentence. For each quantifier in the sentence, its presence is checked in the quantifier list and then removed from the
; quantifier list. If a quantifier is found to not be present in the quantifier list, then it needs to be standardized. The variable of the quantifier is then changed to
; something unique.
(defun standardize (sentence quants)
  (loop for i in sentence do
       (cond
	 ((and (listp i) (or (equal (car i) 'exist) (equal (car i) 'univ)))
	  (if (member i quants :test #'equalp)
	      (setf quants (remove i quants :test #'equalp))
	      (if (equal (car i) 'exist)
		  (let
		      ((replacement nil))
		    (loop for j in quants do
			 (when (and (null replacement) (equal (car j) 'exist))
			   (setf replacement j)
			   (setf quants (remove j quants :test #'equalp))
			   (change-func-vars j sentence)
			   (setf (nth (position i sentence) sentence) j))))
		  (let
		      ((replacement nil))
		    (loop for j in quants do
			 (when (and (null replacement) (equal (car j) 'univ))
			   (setf replacement j)
			   (setf quants (remove j quants :test #'equalp))
			   (change-func-vars j sentence)
			   (setf (nth (position i sentence) sentence) j))))
		  )
	      )
	  )
	 ((listp i) (setf quants (standardize i quants))))
       )
  quants
  )

; Wrapper function handling all quantifier business. Executes in the following order: Standardizes variables, removes quantifiers, and skolemizes variables.
(defun handle-quants (sentence)
  (let
      ((quants (get-quants sentence))
       (binding-list nil))
    (unless (null (standardizep quants))
      (setf quants (change-vars quants))
      (standardize sentence quants)
      )
    (setf sentence (remove-quants sentence))
    (setf quants (reverse-quants quants))
    (setf binding-list (get-skolem-bindings sentence quants))
    (setf binding-list (remove-dups binding-list))
    (setf binding-list (set-constants quants binding-list))
    (setf binding-list (standardize-bindings binding-list))
    (setf sentence (skolemize sentence binding-list))
    sentence
  )
)

; wrapper function for converting a clause to SNF. Executes the following: removes bi-directional operators, removes implications, performs DeMorgan's law until all NOT operators are
; satisfied, standardizes variables if need be, removes quantifiers, skolemizes variables, and finally distributes OR operators to create a set in disjunctive normal form.
(defun convert-clause (s)
  (setf s (remove-bi s))
  (setf s (remove-impl s))
  (setf s (do-demorgan s))
  (setf s (handle-quants s))
  (setf s (remove-unnecessary-lists s))
  (when (and (listp s) (listp (car s)) (null (cdr s)))
    (setf s (car s)))
  (setf s (distribute s))
  s
  )

; wrapper function for converting to snf. loops through all clauses and appends the converted clause to a list.
(defun snf (s)
  (let
      ((return-list nil))
    (loop for i in s do
	 (setf return-list (append (list (convert-clause i)) return-list))
	 )
    return-list
    )
  )

;;; SNF TEST PARAMETERS

; smuggler sentences
(defparameter smuggle1 (list '(univ *x) 
			     (list 
			      (list 
			       (make-func :name 'E :args '(*X)) 
			       '&& 
			       (list '~ (make-func :name 'V :args '(*X)))
			       )
			      '->
			      (list '(exist *y)
				    (list
				     (make-func :name 'S :args '(*X *Y))
				     '&&
				     (make-func :name 'C :args '(*Y))
				     )
				    )
			      )
			     )
  )

(defparameter smuggle2 (list '(exist *x)
			     (list
			      (list
			       (make-func :name 'P :args '(*X))
			       '&&
			       (make-func :name 'E :args '(*X))
			       )
			      '&&
			      (list '(univ y)
				    (list
				     (make-func :name 'S :args '(*x *y))
				     '->
				     (make-func :name 'P :args '(*y))
				     )
				    )
			      )
			     )
  )

(defparameter smuggle3 (list '(univ *x)
			     (list
			      (make-func :name 'P :args '(*X))
			      '->
			      (list '~ (make-func :name 'V :args '(*X)))
			      )
			     )
  )

; smuggler
(defparameter fol1 (list smuggle1 smuggle2 smuggle3))

(defparameter animals (list '(univ *x)
			    (list
			     (list '(univ *y)
				   (list
				    (make-func :name 'Animal :args '(*y))
				    '->
				    (make-func :name 'Loves :args '(*x *y)))
				   )
				  '->
				  (list '(exist *y)
					(list
					 (make-func :name 'Loves :args '(*y *x))
					 )
					)
				  )
			    )
  )

;coral reef club sentences
(defparameter coral1 (list
		     '(univ *x)
		     (list
		      (make-func :name 'WS :args '(*x))
		      '||
		      (make-func :name 'SD :args '(*x))
		      )
		     )
  )

(defparameter coral2 (list
		      '(~ exist *x)
		      (list
		       (make-func :name 'SD :args '(*y))
		       '&&
		       (make-func :name 'Likes :args '(*y Waves))
		       )
		      )
  )

(defparameter coral3 (list
		      '(univ *x)
		      (list
		       (make-func :name 'WS :args '(*z))
		       '->
		       (make-func :name 'Likes :args '(*z Warm))
		       )
		      )
  )

(defparameter coral4 (list
		      '(univ *w)
		      (list
		       (make-func :name 'Likes :args '(Laura *w))
		       '<>
		       (list '~ (make-func :name 'Likes :args '(Jacob *w)))
		       )
		      )
  )

(defparameter coral5 (list (make-func :name 'Likes :args '(Jacob Warm))))
(defparameter coral6 (list (make-func :name 'Likes :args '(Jacob Waves))))

;coral reef club
(defparameter fol2 (list coral1 coral2 coral3 coral4 coral5 coral6))

; colonel west sentences
(defparameter west1 (list
		     '(univ *x)
		     '(univ *y)
		     '(univ *z)
		     (list
		      (list
		       (make-func :name 'American :args '(*x))
		       '&&
		       (make-func :name 'Weapon :args '(*y))
		       '&&
		       (make-func :name 'Sells :args '(*x *y *z))
		       '&&
		       (make-func :name 'Hostile :args '(*z))
		       )
		      '->
		      (list
		       (make-func :name 'Criminal :args '(*x))
		       )
		      )
		     )
  )

(defparameter west2 (list
		     '(exist *x)
		     (list
		      (make-func :name 'Owns :args '(Foo *x))
		      '&&
		      (make-func :name 'Missile :args '(*x))
		      )
		     )
  )

(defparameter west3 (list
		     '(univ *x)
		     (list
		      (list
		       (make-func :name 'Missile :args '(*x))
		       '&&
		       (make-func :name 'Owns :args '(Foo *x))
		       )
		      '->
		      (make-func :name 'Sells :args '(West *x Foo))
		      )
		     )
  )

(defparameter west4 (list
		     '(univ *x)
		     (list
		      (make-func :name 'Missile :args '(*x))
		      '->
		      (make-func :name 'Weapon :args '(*x))
		      )
		     )
  )

(defparameter west5 (list
		     '(univ *x)
		     (list
		      (make-func :name 'Enemy :args '(*x America))
		      '->
		      (make-func :name 'Hostile :args '(*x))
		      )
		     )
  )

(defparameter west6 (list
		     (make-func :name 'American :args '(West))
		     )
  )

(defparameter west7 (list
		     (make-func :name 'Enemy :args '(Foo America))
		     )
  )

; colonel west
(defparameter fol3 (list west1 west2 west3 west4 west5 west6 west7))