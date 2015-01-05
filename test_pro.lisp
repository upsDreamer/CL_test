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

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))

(defun show-squares(start end)
  (do ((i start (+ i 1)))
      ((> i end) 'overall)
    (format t "~A ~A~%" i (* i i))))



(defun my-squre(start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (let ((mid (/ len 2)))
	   (equal (subseq s 0 mid)
		  (reverse (subseq s mid)))))))

(defun nthmost (n lst)
   (nth (- n 1)
       (sort (copy-list lst) #'>)))

(defun our-reverse(lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))


(defun proper-list? (x)
  (or (null x)
      (and (consp x)
	   (proper-list? (cdr x)))))

(defun our-assoc(key alist)
  (and (consp alist)
       (let ((pair (car alist)))
	 (if (eql key (car alist))
	     pair
	     (our-assoc key (cdr alist))))))

(defun shortest-path(start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net))
		   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))

