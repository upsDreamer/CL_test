(defun our-listp(x)
  (or (null x) (consp x)))

(defun our-equal(x y)
  (or (equal(x y)
	    (and (consp x)
		 (consp y)
		 (our-equal(car x) (car y))
		 (our-equal(cdr x) (cdr y))))))

(defun our-copy-list(lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list(cdr lst)))))

(defun n-elts(elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun compr(elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts n elt)
		  (compr next 1 (cdr lst)))))))

(defun compress(x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun our-nthcdr(n lst)
  (if (zerop n)
      lst
      (our-nthcdr(-1 n) (cdr lst))))

(defun len(lst)
  (if (null lst)
      0
      (+ (len(cdr lst)) 1)))

