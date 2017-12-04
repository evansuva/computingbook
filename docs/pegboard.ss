;;; Pegboard Solver
;;; David Evans
;;; 22 January 2009
;;;
;;; Described in Chapter 5: Data
;;;

(load "list-procedures.ss")

(define (make-taggedlist tag p) (cons tag p))
(define (taggedlist-get-tag p) (car p))

(define (taggedlist-get-element tag p n)
  (if (eq? (taggedlist-get-tag p) tag)
      (list-get-element (cdr p) n)      
      (error (format "Bad list tag: ~a (expected ~a)" (taggedlist-get-tag p) tag))))

;;; Position

(define (make-position row col) (make-taggedlist 'Position (list row col)))
(define (position-get-row posn) (taggedlist-get-element 'Position posn 1))
(define (position-get-column posn) (taggedlist-get-element 'Position posn 2))

(define (make-list-of-constants n val)
  (if (= n 0) 
      null
      (cons val (make-list-of-constants (- n 1) val))))

;;; Board

(define (make-board rows) 
  (if (= rows 0) 
      null
      (list-append (make-board (- rows 1))
                   (list (make-list-of-constants rows true)))))

(define (board-rows board) (length board))

(define (board-contains-peg? board pos)
  (list-get-element (list-get-element board (position-get-row pos)) 
                    (position-get-column pos)))

(define (row-replace-peg pegs col val)
  (if (= col 1) 
      (cons val (cdr pegs))
      (cons (car pegs) (row-replace-peg (cdr pegs) (- col 1) val))))

(define (board-replace-peg board row col val)
  (if (= row 1) 
      (cons (row-replace-peg (car board) col val)
            (cdr board))
      (cons (car board)
            (board-replace-peg (cdr board) (- row 1) col val))))

(define (board-add-peg board pos)
  (if (board-contains-peg? board pos)
      (error (format "Board already contains peg at position: ~a" pos))
      (board-replace-peg board (position-get-row pos) (position-get-column pos) true)))

(define (board-remove-peg board pos)
  (if (not (board-contains-peg? board pos))
      (error (format "Board does not contain peg at position: ~a" pos))
      (board-replace-peg board (position-get-row pos) (position-get-column pos) false)))

(define (board-number-of-pegs board)
  (list-sum
   (list-map
    (lambda (peg) (if peg 1 0))
    (list-flatten board))))

;;; Direction

(define (make-direction down right) (make-taggedlist 'Direction (list down right)))
(define (direction-get-vertical dir) (taggedlist-get-element 'Direction dir 1))
(define (direction-get-horizontal dir) (taggedlist-get-element 'Direction dir 2))

(define (direction-add dir1 dir2)
  (make-direction (+ (direction-get-vertical dir1) (direction-get-vertical dir2))
                  (+ (direction-get-horizontal dir1) (direction-get-horizontal dir2))))

(define direction-up (make-direction -1 0))
(define direction-down (make-direction 1 0))
(define direction-left (make-direction 0 -1))
(define direction-right (make-direction 0 1))
(define direction-up-left (direction-add direction-up direction-left))
(define direction-down-right (direction-add direction-down direction-right))

(define all-directions
	(list direction-up direction-down direction-left direction-right
        direction-up-left direction-down-right))

(define (direction-step pos dir)
  (make-position (+ (position-get-row pos) (direction-get-vertical dir))
                 (+ (position-get-column pos) (direction-get-horizontal dir))))

;;; Move

(define (make-move start direction) (make-taggedlist 'Move (list start direction)))
(define (move-get-start move) (taggedlist-get-element 'Move move 1))
(define (move-get-direction move) (taggedlist-get-element 'Move move 2))
(define (move-get-jumpee move) (direction-step (move-get-start move) (move-get-direction move)))
(define (move-get-landing move) (direction-step (move-get-jumpee move) (move-get-direction move)))


;;; Solving the Puzzle

(define (execute-move board move)
  (board-add-peg 
   (board-remove-peg (board-remove-peg board (move-get-start move)) 
                     (move-get-jumpee move))
   (move-get-landing move)))

(define (all-positions board)
  (list-flatten
   (list-map (lambda (row)
               (list-map (lambda (col)
                           (make-position row col))
                         (intsto row)))
             (intsto (board-rows board)))))

(define (all-conceivable-moves board)
  (list-flatten
   (list-map
    (lambda (pos)
      (list-map
       (lambda (dir)
         (make-move pos dir))
       all-directions))
    (all-positions board))))

(define (valid-position? board pos)
  ; position is valid if its row <= # of rows in board and col <= row
  (and
   (>= (position-get-row pos) 1)
   (>= (position-get-column pos) 1)
   (<= (position-get-row pos) (board-rows board))
   (<= (position-get-column pos) (position-get-row pos))))

(define (all-possible-moves board)
  (list-filter
   (lambda (move)
     (valid-position? board (move-get-landing move)))          
   (all-conceivable-moves board)))

(define (all-legal-moves board)
  (list-filter
   (lambda (move)
     (and (board-contains-peg? board (move-get-start move))
          (board-contains-peg? board (move-get-jumpee move))
          (not (board-contains-peg? board (move-get-landing move)))))
   (all-possible-moves board)))

(define (is-winning-board? board)
  (= (board-number-of-pegs board) 1))

(define (solve-pegboard board) ; Board -> List of Moves
  (if (is-winning-board? board)
      null ; no Moves needed to reach winning position
      (try-moves board (all-legal-moves board))))

(define (try-moves board moves)
  (if (null? moves)
      false ; didn't find a winner 
      (if (solve-pegboard (execute-move board (car moves)))
          (cons (car moves) (solve-pegboard (execute-move board (car moves))))
          (try-moves board (cdr moves)))))