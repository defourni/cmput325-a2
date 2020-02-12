
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
  
