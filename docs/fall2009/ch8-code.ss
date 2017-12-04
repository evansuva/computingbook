(load "list-procedures.ss")
(require (lib "trace.ss"))

(define (pick-better cf p1 p2)
  (if (cf p1 p2) p1 p2))

(define (list-find-best cf p)
  (if (null? (cdr p))
      (car p)
      (pick-better cf (car p) (list-find-best cf (cdr p)))))

(define (list-delete p el)
  (if (null? p) 
      null
      (if (equal? (car p) el) ; found match, delete this element
          (cdr p)
          (cons (car p) (list-delete (cdr p) el)))))


(define (list-sort-best-first cf p)
  (if (null? p) 
      null
      (cons (list-find-best cf p) 
            (list-sort-best-first cf (list-delete p (list-find-best cf p))))))

(define (list-sort-best-first-nodup cf p)
   (if (null? p) 
       null
       ((lambda (best)
          (cons best (list-sort-best-first-nodup cf (list-delete p best))))
        (list-find-best cf p))))

(define (list-sort-best-first-let cf p)
  (if (null? p) 
      null
      (let ((best (list-find-best cf p)))
        (cons best (list-sort-best-first-let cf (list-delete p best))))))

(define (list-insert-one cf el p) ; pre: p must be in sorted order by cf
  (if (null? p) 
      (list el)
      (if (cf el (car p)) 
          (cons el p)
          (cons (car p) (list-insert-one cf el (cdr p))))))

(define (list-sort-insert cf p)
  (if (null? p) 
      null
      (list-insert-one cf (car p) (list-sort-insert cf (cdr p)))))

(define (make-random-list n max)
  (if (= n 0)
      null
      (cons (random max) (make-random-list (- n 1) max))))

;;; quicksort

(define (list-extract p start end)
  (if (= start 0)
      (if (= end 0)
          null
          (cons (car p) (list-extract (cdr p) start (- end 1))))
      (list-extract (cdr p) (- start 1) (- end 1))))

(define (list-first-half p)
  (list-extract p 0 (floor (/ (list-length p) 2))))
    
(define (list-second-half p)
  (list-extract p (floor (/ (list-length p) 2)) (list-length p)))

(define (list-insert-one-halves cf el p)
  (if (null? p) 
      (list el)      
      (if (null? (cdr p))
          (if (cf el (car p))
              (cons el p)
              (list (car p) el))
          (let ((front (list-first-half p))
                (back (list-second-half p)))     
            (if (cf el (car back))
                (list-append (list-insert-one-halves cf el front) back)
                (list-append front (list-insert-one-halves cf el back)))))))

(define (list-sort-insert-halves cf p)
  (if (null? p) 
      null
      (list-insert-one-halves cf (car p) (list-sort-insert-halves cf (cdr p)))))

;;;
;;; Binary Trees
;;;

(define (make-tree left element right)
  (cons element (cons left right)))

(define (tree-element tree) (car tree))
(define (tree-left tree) (car (cdr tree)))
(define (tree-right tree) (cdr (cdr tree)))

(define (tree-insert-one cf el tree)
  (if (null? tree)
      (make-tree null el null)
      (if (cf el (tree-element tree))
          (make-tree (tree-insert-one cf el (tree-left tree))
                     (tree-element tree)
                     (tree-right tree))
          (make-tree (tree-left tree)
                     (tree-element tree)
                     (tree-insert-one cf el (tree-right tree))))))

(define (tree-extract-elements tree)
  (if (null? tree) 
      null
      (list-append (tree-extract-elements (tree-left tree))
                   (cons (tree-element tree) 
                         (tree-extract-elements (tree-right tree))))))

(define (list-to-sorted-tree cf p) 
  (if (null? p) 
      null
      (tree-insert-one cf (car p) (list-to-sorted-tree cf (cdr p)))))

(define (list-sort-tree cf p)
  (tree-extract-elements (list-to-sorted-tree cf p)))

;;;
;;; Quicksort
;;;

(define (list-quicksort cf p)
  (if (null? p) 
      null
      (list-append (list-quicksort cf (list-filter (lambda (el) (cf el (car p))) (cdr p)))
                   (cons (car p)
                         (list-quicksort cf (list-filter (lambda (el) (not (cf el (car p)))) (cdr p)))))))

(define list1 (make-random-list 1000 1000))
(define list2 (make-random-list 2000 1000))

;;;
;;; Searching
;;;

;;;
;;; Unstructured Sorting
;;;

(define (list-search ef p)
  (if (null? p)
      false ; Not found
      (if (ef (car p))
          (car p)
          (list-search ef (cdr p)))))

;;;
;;; Binary Search
;;;

(define (binary-tree-search ef cf tree)
  (if (null? tree)
      false
      (if (ef (tree-element tree))
          (tree-element tree)
          (if (cf (tree-element tree))
              (binary-tree-search ef cf (tree-left tree))
              (binary-tree-search ef cf (tree-right tree))))))

(define (binary-tree-has-number btree target)
 (if (binary-tree-search (lambda (el) (= target el))
                         (lambda (el) (< target el)) 
                         btree)
     true
     false))

;;;
;;; Indexed Searching
;;;

;; divide a string into a list of (word pos) pairs
(define (text-to-word-positions s)
  (define (text-to-word-positions-iter p w pos)
    (if (null? p)
        (if (null? w) 
            null
            (list (cons (list->string w) pos)))
        (if (not (char-alphabetic? (car p))) ; finished word
            (if (null? w) ; no current word
                (text-to-word-positions-iter (cdr p) null (+ pos 1))
                (cons (cons (list->string w) pos)
                      (text-to-word-positions-iter (cdr p) null (+ pos (list-length w) 1))))
            (text-to-word-positions-iter (cdr p) (list-append w (list (char-downcase (car p)))) pos))))
  (text-to-word-positions-iter (string->list s) null 0))
            
(define (insert-into-index index wp)
  (if (null? index)
      (make-tree null (cons (car wp) (list (cdr wp))) null)
      (if (string=? (car wp) (car (tree-element index)))
          (make-tree (tree-left index)
                     (cons (car (tree-element index))
                           (list-append (cdr (tree-element index)) (list (cdr wp))))
                     (tree-right index))
          (if (string<? (car wp) (car (tree-element index)))
              (make-tree (insert-into-index (tree-left index) wp)
                         (tree-element index)
                         (tree-right index))
              (make-tree (tree-left index)
                         (tree-element index)
                         (insert-into-index (tree-right index) wp))))))
  
(define (insert-all-wps index wps)
  (if (null? wps) 
      index
      (insert-all-wps 
       (insert-into-index index (car wps))
       (cdr wps))))

(define (index-document url text)
  (printf "Indexing page: ~a~n" url)
  (insert-all-wps
   null
   (list-map (lambda (wp) (cons (car wp) (cons url (cdr wp)))) (text-to-word-positions text))))

(require (lib "url.ss" "net"))

(define (read-all-chars port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        null
        (cons c (read-all-chars port)))))
         
(define (web-get url)
  (list->string (read-all-chars (get-pure-port (string->url url)))))

(define (tree-size tree)
  (if (null? tree) 
      0
      (+ 1 (tree-size (tree-left tree))
           (tree-size (tree-right tree)))))

(define (merge-indexes d1 d2)
  (define (merge-elements p1 p2)
    (if (null? p1)
        p2
        (if (null? p2)
            p1
            (if (string=? (car (car p1)) (car (car p2)))
                (cons (cons (car (car p1))
                            (list-append (cdr (car p1)) (cdr (car p2))))
                      (merge-elements (cdr p1) (cdr p2)))
                (if (string<? (car (car p1)) (car (car p2)))
                    (cons (car p1)
                          (merge-elements (cdr p1) p2))
                    (cons (car p2)
                          (merge-elements p1 (cdr p2))))))))
  (if (null? d1) ;; for efficiency, we have a special case for null indexes
      d2
      (if (null? d2)
          d1
          (begin
            (printf "Merging indexes: ~a + ~a~n"
                    (tree-size d1) (tree-size d2))
            (list-to-sorted-tree
             (lambda (e1 e2) (string<? (car e1) (car e2)))
             (merge-elements
              (tree-extract-elements d1)
              (tree-extract-elements d2)))))))
          
(define (index-pages p)
  (if (null? p)
      null
      (merge-indexes 
       (let ((index (index-document (car p) (web-get (car p)))))
         (printf "  Entries: ~a   Word occurances: ~a~n"
                 (tree-size index)
                 (list-sum (list-map
                            (lambda (entry) (list-length (cdr entry)))
                            (tree-extract-elements index))))
         index)
       (index-pages (cdr p)))))
      
(define (index-pages-merge-tree p)
  (define (index-tree tree)
    (if (null? tree)
        null
        (merge-indexes 
         (merge-indexes (index-tree (tree-left tree))
                        (index-document (tree-element tree) (web-get (tree-element tree))))
         (index-tree (tree-right tree)))))
  (index-tree (list-to-sorted-tree string<? p)))

(define (make-bills-index)
  (index-pages
   (list-map
    (lambda (play) (string-append "http://shakespeare.mit.edu/" play "/full.html"))
    (list    
     ;;; Comedies
     "allswell" ; All's Well That Ends Well
     "asyoulikeit" ; As You Like It
     "comedy_errors" ; The Comedy of Errors
     "cymbeline" ; Cymbeline
     "lll" ; Love's Labours Lost
     "measure" ; Measure for Measure
     "merry_wives" ; The Merry Wives of Windsor
     "merchant" ; The Merchant of Venice
     "midsummer" ; A Midsummer Night's Dream
     "much_ado" ; Much Ado About Nothing
     "pericles" ; Pericles, Prince of Tyre
     "taming_shrew" ; Taming of the Shrew
     "tempest" ; The Tempest
     "troilus_cressida" ; Troilus and Cressida
     "twelfth_night" ; Twelfth Night
     "two_gentlemen" ; Two Gentlemen of Verona
     "winters_tale" ; Winter's Tale
     
     ;;; Histories
     "1henryiv" ; Henry IV, part 1
     "2henryiv" ; Henry IV, part 2
     "henryv" ; Henry V
     "1henryvi" ; Henry VI, part 1
     "2henryvi" ; Henry VI, part 2
     "3henryvi" ; Henry VI, part 3
     "henryviii" ; Henry VIII
     "john" ; King John
     "richardii" ; Richard II
     "richardiii" ; Richard III</td>
     
     ;;; Tragedies
     "cleopatra" ; Antony and Cleopatra
     "coriolanus" ; Coriolanus
     "hamlet" ; Hamlet
     "julius_caesar" ; Julius Caesar
     "lear" ; King Lear
     "macbeth" ; Macbeth
     "othello" ; Othello
     "romeo_juliet" ; Romeo and Juliet
     "timon" ; Timon of Athens
     "titus" ; Titus Andronicus
     ))))
  
;; (define shakespeare-index (time (make-bills-index)))

(define (search-in-index index word)
  (binary-tree-search
   (lambda (el) (string=? word (car el)))
   (lambda (el) (string<? word (car el)))
   index))

(define (index-histogram index)
  (list-quicksort 
   (lambda (e1 e2) (> (cdr e1) (cdr e2)))
   (list-map (lambda (el)
               (cons (car el)
                     (length (cdr el))))
             (tree-extract-elements index))))
