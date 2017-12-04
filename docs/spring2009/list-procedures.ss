;;; 
;;; list-procedures.ss
;;; David Evans
;;; 22 January 2009
;;;
;;; List procedures from Chapter 5.
;;;

(define (list-append p q)
  (if (null? p) 
      q
      (cons (car p) (list-append (cdr p) q))))

(define (list-get-element p n) 
  (if (= n 1)
      (car p)
      (list-get-element (cdr p) (- n 1))))

(define (list-length p)
   (if (null? p) 
       0 
       (+ 1 (list-length (cdr p)))))

(define (list-sum p)
   (if (null? p) 
       0 
       (+ (car p) (list-sum (cdr p)))))

(define (list-map f p)
  (if (null? p) 
      null
      (cons (f (car p)) 
            (list-map f (cdr p)))))

(define (list-filter test p)
  (if (null? p) 
      null
      (if (test (car p))
          (cons (car p) (list-filter test (cdr p)))
          (list-filter test (cdr p)))))

(define (intsto n)
  (if (= n 0) 
      null 
      (list-append (intsto (- n 1)) (list n))))

(define (list-flatten p)
  (if (null? p) 
      null
      (list-append (car p) (list-flatten (cdr p)))))

(define (list-reverse p)
  (if (null? p) 
      null 
      (list-append (list-reverse (cdr p)) (list (car p)))))
