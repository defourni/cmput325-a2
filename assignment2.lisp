
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

(defun binarize_add (E)
  (cond ((not (or (find '+ E) (find '- E))) E)
        (t (binarize_add (cons (list (car E) (cadr E) (caddr E)) (cdddr E)))))
  )

(defun binarize_multi (E)
  (cond ((= (list-length E) 1 ) E)
	((not (or (find '* E) (find '/ E))) E)
	((or (eql (cadr E) '*) (eql (cadr E) '/))
	 (binarize_multi (cons (list (car E) (cadr E) (caddr E)) (cdddr E))))
	(t (cons (car E) (cons (cadr E) (binarize_multi (cddr E))))))
	
  )

(defun isNested (E)
  (cond ((atom E) nil)
	((atom (car E)) (isNested (cdr E)))
	((atom (cdar E)) (isNested (cdr E)))
	(t t)))

(defun binarize_after_nest (E)
  (cond ((atom E) E)
        ((or (position '* E) (position '/ E)) (car (binarize_add (binarize_multi E))))
	((or (position '+ E) (position '- E)) (car (binarize_add E)))
	(t nil)
	))

(defun explore_nesting (E)
  (cond ((numberp E) E)
	((atom E) E)
	((not (isNested E)) (binarize_after_nest E))
	((> (list-length (cddr E)) 1)   (binarize_after_nest (list
					(car (explore_nesting (car E)))
					(cadr E)
					(car (explore_nesting (cddr E))))))
	(t (binarize_after_nest (list
				 (explore_nesting (car E))
				 (cadr E)
				 (caddr E))))
	))

(defun first_nest (E)
  (cond ((numberp E) E)
	((atom E) E)
	((not (isNested E)) (binarize_after_nest E))
	((> (list-length (cddr E)) 1)   (binarize_after_nest (list
					(explore_nesting (car E))
					(cadr E)
					(explore_nesting (cddr E)))))
	(t (binarize_after_nest (list
				 (explore_nesting (car E))
				 (cadr E)
				 (caddr E))))
	))

;(defun binarize_nest (E)
;   (cond ((atom E) E)
;	 ((isNested E) (binarize_after_nest
;			(cond
;			 ((not (cddr E))
;			  (cons (binarize_nest (car E)) '()))
;			 ((isNested (binarize_nest (cddr E)))
;			  (append
;			   (list (binarize_nest (car E)) (cadr E))
;			   (binarize_nest (cddr E))))
;			 (t
;			  (list
;			   (binarize_nest (car E))
;			   (cadr E)
;			   (car (binarize_nest (cddr E)))))))
;	  )
;	 
;       ((or (position '* E) (position '/ E)) (car (binarize_add (binarize_multi E))))
;	((or (position '+ E) (position '- E)) (car (binarize_add E)))
;	(t E)
; ))

(defun binarize (E)
  (cond ((atom E) E)
	(t (binarize_after_nest (first_nest E)))
	;((isNested E) (if (cddr E)
	;		  (binarize_after_nest (list (binarize (car E)) (cadr E) (binarize (cddr E))))
	;		  (binarize_after_nest (binarize (car E)))))
        ;((or (position '* E) (position '/ E)) (car (binarize_add (binarize_multi E))))
	;((or (position '+ E) (position '- E)) (car (binarize_add E)))
	;(t (binarize_after_nest (binarize_nest E)))
  ))
