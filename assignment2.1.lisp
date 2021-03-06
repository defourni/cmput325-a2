
(defun safeGCD (x y)
  (cond ((= y 0) x)
	(t (safeGCD y (mod x y)))
	)
  )

(defun euclid (x y)
  (cond ((= x 0) 'GCD_ERROR)
	((= y 0) 'GCD_ERROR)
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
  (cond ((eql (cadr L) '*) (* (car L) (caddr L)))
	((eql (cadr L) '+) (+ (car L) (caddr L)))
	((eql (cadr L) '-) (- (car L) (caddr L)))
	((eql (cadr L) '/) (cons (car L) (caddr L)))
	;((eql (cadr L) '/) (/ (car L) (caddr L)))
	))
	

(defun binarylist (L)
  (cond ((atom L) L)
	((= (list-length L) 3) (binarylist
				(if (not (atom (car L)))
				   (cons (binarylist (car L)) (cdr L))
				   (if (not (atom (car (reverse L))))
				       (reverse (cons (binarylist (car (reverse L))) (cdr (reverse L))))
				       (singleop L)))))
	(t L))
  )

(defun simplifybinary (E)
  (cond ((atom E) E)
        ((listp (cdr E)) (binarylist E))
	(t (simplifyfraction E))
	)
  )

