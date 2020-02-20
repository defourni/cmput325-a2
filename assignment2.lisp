
(defun safeGCD (x y)
  (cond ((= y 0) x)
	(t (safeGCD y (mod x y)))
	)
  )

(defun euclid (x y)
  (cond ((= x 0) 'GCD-ERROR)
	((= y 0) 'GCD-ERROR)
	(t (safeGCD x y))
	)
  )

(defun simplifyfraction (F)
  (cond ((= (cdr F) 0) 'ZeroDivide-Error)
	((= (cdr F) 1) (car F))
	((= (car F) 0) 0)
	((= (euclid (car F) (cdr F)) 1)
	 (cons (car F) (cdr F)))
	(t (simplifyfraction
	    (cons (/ (car F) (euclid (car F) (cdr F)))
		  (/ (cdr F) (euclid (car F) (cdr F))))))
	)
  )

(defun singleop (L)
  (cond ((eql (car L) 'zeroDivide-error) 'zeroDivide-error)
	((eql (caddr L) 'zeroDivide-error) 'zeroDivide-error)
	((eql (cadr L) '*) (* (car L) (caddr L)))
	((eql (cadr L) '+) (+ (car L) (caddr L)))
	((eql (cadr L) '-) (- (car L) (caddr L)))
	((eql (cadr L) '/) (if (= (caddr L) 0)
			       'zeroDivide-Error
			       (/ (car L) (caddr L))))
	))


(defun binarylist (L)
  (cond ((atom L) L)
	((atom (cdr L)) (if (= (cdr L) 0)
			    'zeroDivide-Error
			    (/ (car L) (cdr L))))
	(t (binarylist (if (not (atom (car L)))
			   (cons (binarylist (car L)) (cdr L))
			   (if (not (atom (caddr L)))
			       (reverse (cons (binarylist (car (reverse L))) (cdr (reverse L))))
			       (singleop L)))))
  ))

(defun simplifybinary (E)
  (cond ((atom E) (if (numberp E)
		      (simplifyfraction (cons (numerator E) (denominator E)))
		    E))
	((atom (cdr E)) (simplifyfraction E))
	((eql (binarylist E) 'zerodivide-error) 'zerodivide-error)
        ((listp (cdr E)) (simplifyfraction (cons (numerator (binarylist E)) (denominator (binarylist E)))))
	(t E)
	)
  )


;binarizes an infix expression that already has division and multiplication binarized
;uses recurive calls to group left to right.
(defun binarize_add (E)
  (cond ((not (or (find '+ E) (find '- E))) E)
        (t (binarize_add (cons (list (car E) (cadr E) (caddr E)) (cdddr E)))))
  )

;binarizes the multiplication and division of an infix expression - recursively calls itself left to right, ignoring any + or - operators and stopping when out of / and *
;Both binarize_multi and binarize_add only worry about the current depth of list given (i.e. do not worry about deeper nested lists)
(defun binarize_multi (E)
  (cond ((= (list-length E) 1 ) E)
	((not (or (find '* E) (find '/ E))) E)
	((or (eql (cadr E) '*) (eql (cadr E) '/))
	 (binarize_multi (cons (list (car E) (cadr E) (caddr E)) (cdddr E))))
	(t (cons (car E) (cons (cadr E) (binarize_multi (cddr E))))))
	
  )

;Helper function returns true if a list has further nested lists and nil if it is a flat list (one level)
(defun isNested (E)
  (cond ((atom E) nil)
	((atom (car E)) (isNested (cdr E)))
	((atom (cdar E)) (isNested (cdr E)))
	(t t)))

;caller function used to binarize flat lists. if no multiplication or division operators are found, the bin_multi step is skipped to save time. Also deals with atomic values (i.e numbers)
(defun binarize_after_nest (E)
  (cond ((atom E) E)
        ((or (position '* E) (position '/ E)) (car (binarize_add (binarize_multi E))))
	((or (position '+ E) (position '- E)) (car (binarize_add E)))
	(t nil)
	))

;function used to explore deeper levels of nesting and correctly group them back together.
;Works similar to building a binary tree bottom-up, but requires a few more conditions to group binexpressions properly.
;list-length 3 is a sort of base case, where list is in form (x op y). These can be joined together with a simple list function as there wont be more operators to add on later.
;The other cases take the form (x op1 y op2 z ...) these are split into  (x) and (y op 2 z op3 ...).
;because these ultimately should be in the same depth, append fuctions and either adding or removing depth (cons '() or car) is necessary.
(defun explore_nesting (E)
  (cond ((numberp E) E)
	((atom E) E)
	((not (listp (cdr E))) E)
	((not (isNested E)) (binarize_after_nest E))
	((= (list-length E) 3) (list
			        (explore_nesting (car E))
				(cadr E)
				(binarize_after_nest (explore_nesting (caddr E)))))
	
	((and (isNested (car E)) (isNested (cddr E)))
	 (binarize_after_nest (append
			       (explore_nesting (car E))
			       (cons (cadr E) '())
			       (explore_nesting (cddr E)))))
	
	((isNested (car E))
	 (binarize_after_nest (append
			       (explore_nesting (car E))
			       (cons (cadr E) '())
			       (cons (explore_nesting (cddr E)) '()))))
      	
	((isNested (cddr E))
	 (append (cons (explore_nesting (car E)) '())
		 (cons (cadr E) '())
		 (explore_nesting (cddr E))))
	
	(t (append
	    (cons (explore_nesting (car E)) '())
	    (cons (cadr E) '())
	    (explore_nesting (cddr E))))
	))

;handles atomic values and gives an extra binarize call after the nesting is resolved for cases like ((1 + 2 + 3) / (4 + 5 + 6) * (7 + * 9)) to group the shallowest depth.
(defun binarize (E)
  (cond ((atom E) E)
	((atom (cdr E)) E)
	(t (binarize_after_nest (explore_nesting E)))
  ))

(defun simplify (E)
  (simplifybinary (binarize E)))


(defun sub_var_flat (Bindings E)
    (cond ((not Bindings) E)
	((atom E) (if (eql (caar Bindings) E)
		      (cadar Bindings)
		      (sub_var_flat (cdr Bindings) E)))
	((not (find (caar Bindings) E))
	 (sub_var_flat (cdr Bindings) E))
	((position (caar Bindings) E)
	  (setf
	   (nth
	    (position (caar Bindings) E)
	    E)
	   (simplify (cadar Bindings)))
	  (sub_var_flat Bindings E))
	)
)

(defun substitutevar (Bindings E)
  (cond 
	((not (isNested E)) (sub_var_flat Bindings E))
	 
	((and (isNested (car E)) (isNested (cddr E)))
	 (append (cons (substitutevar Bindings (car E)) '()) (cons (cadr E) '()) (cons (substitutevar Bindings (cddr E)) '())))
	((and (isNested (car E)) (cddr E))
	 (append (cons (substitutevar Bindings (car E)) '()) (cons (cadr E) '()) (cons (sub_var_flat Bindings (cddr E)) '())))
	((isNested (car E))
	 (substitutevar Bindings (car E)))
	((isNested (cddr E))
	 (list (sub_var_flat Bindings (car E)) (cadr E) (substitutevar Bindings (cddr E))))
	((cddr E) (append (cons (sub_var_flat Bindings (car E)) '()) (cons (cadr E) '()) (substitutevar Bindings (cddr E))))
	(t (sub_var_flat Bindings (car E)))
	
	))

(defun simplifyvar (Bindings E)
  (simplify (substitutevar Bindings E)))
       
