(defun singleop (L)
  (cond ((eql (cadr L) '*) (* (car L) (caddr L)))
	((eql (cadr L) '+) (+ (car L) (caddr L)))
	((eql (cadr L) '-) (- (car L) (caddr L)))
	((eql (cadr L) '/) (/ (car L) (caddr L)))
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
