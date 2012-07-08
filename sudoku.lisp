;;;; sudoku.lisp
;;; See http://norvig.com/sudoku.html
;;
;; A Sudoku puzzle is a grid of 81 squares; the majority of enthusiasts
;; label the columns 1-9, the rows A-I, and call a collection of nine
;; squares (column, row, or box) a unit and the squares that share a unit
;; the peers.

(in-package #:sudoku)

;; we're playing the useless benchmark-me trick, so well
(declaim (optimize speed))

(defparameter *row-names* "ABCDEFGHI"
  "list of row names in a soduku puzzle")

(defparameter *easy50*
  (asdf:system-relative-pathname :sudoku "easy50.txt"))

(defparameter *top95*
  (asdf:system-relative-pathname :sudoku "top95.txt"))

(defparameter *hardest*
  (asdf:system-relative-pathname :sudoku "hardest.txt"))

;; conditions
(define-condition empty-values (condition) ())
(define-condition unit-contains-contradictory-solution (condition) ())

;;
;; Tools to easily walk a square's peers and units
;;
(defun list-units-containing (row col)
  "list indexes of all units of a given position"
  (declare (type (integer 0 9) row col))
  (values
   ;; unit with peers on the same row
   (loop for pc below 9 unless (eq pc col) collect (cons row pc))

   ;; unit with peers on the same column
   (loop for pr below 9 unless (eq pr row) collect (cons pr col))

   ;; unit with peers in the same box
   (let ((unit-first-row (* 3 (floor (/ row 3))))
	 (unit-first-col (* 3 (floor (/ col 3)))))
     (loop for pr from unit-first-row to (+ 2 unit-first-row)
	append (loop for pc from unit-first-col to (+ 2 unit-first-col)
		  collect (cons pr pc))))))

(defun make-puzzle-units-array ()
  "Return a puzzle units array"
  (let ((u (make-array '(9 9) :element-type 'list)))
    ;; Every square has exactly 3 units and 20 peers
    (loop for r below 9
       do (loop for c below 9
	     do (setf (aref u r c)
		      (multiple-value-bind (peers-row peers-col peers-box)
			  (list-units-containing r c)
			(list peers-row peers-col peers-box)))))
    u))

(defun make-puzzle-peers-array ()
  "Return a puzzle peers array"
  (let ((p (make-array '(9 9) :element-type 'list)))
    ;; Every square has exactly 3 units and 20 peers
    (loop for r below 9
       do (loop for c below 9
	     do (setf (aref p r c)
		      (multiple-value-bind (peers-row peers-col peers-box)
			  (list-units-containing r c)
			(remove-duplicates
			 (delete (cons r c)
				 (append peers-row peers-col peers-box)
				 :test 'equal)
			 :test 'equal)))))
    p))

(defun make-puzzle-values-array ()
  "Return a puzzle values array, to use in the :initform of the puzzle class"
  (let ((v (make-array '(9 9) :element-type 'simple-bit-vector)))
    (loop for r below 9
       do (loop for c below 9
	     do (setf (aref v r c)
		      ;; we lose a bit each time here, allowing for easier
		      ;; index referencing
		      (make-array 10
				  :element-type 'bit
				  :initial-element 1))))
    v))

;; The main inconvenient of using 2-D arrays of bit-vectors here is to have
;; to provide for a copy implementation. The main alternative is using
;; fixnum instead of bit-vector: http://psg.com/~dlamkins/sl/chapter18.html
(defun copy-puzzle-values-array (values)
  "Return a fresh copy of the VALUES array"
  (let ((v (make-array '(9 9) :element-type 'simple-bit-vector)))
    (loop for r below 9
       do (loop for c below 9
	     do (setf (aref v r c) (copy-seq (aref values r c)))))
    v))

;; units and peers are the same for every Sudoku puzzle, so just compute
;; them once
(defparameter *units*
  (make-puzzle-units-array)
  "list of all possible units in a sudoku grid")

(defparameter *peers*
  (make-puzzle-peers-array)
  "list of all possible peers in a sudoku grid")

;; a puzzle is both the current game status and the values to explore still
(defclass puzzle ()
  ((grid :documentation "The sudoku grid we're solving"
	 :initform (make-array '(9 9)
			       :element-type '(integer 0 9)
			       :initial-element 0)
	 :initarg :grid)
   (values :documentation "All possible values for each Sudoku place"
	   :initform (make-puzzle-values-array)
	   :initarg :values)))

;; provide a way to copy a puzzle class instance
(defun copy-puzzle-grid (grid)
  "copy given GRID"
  (let ((g (make-array '(9 9) :element-type '(integer 0 9))))
    (loop for r below 9
       do (loop for c below 9
	     do (setf (aref g r c) (aref grid r c))))
    g))

(defmethod copy-puzzle ((puzzle puzzle))
  "Copy given PUZZLE into a whole new puzzle and return it"
  (with-slots (grid values) puzzle
   (make-instance 'puzzle
		  :grid (copy-puzzle-grid grid)
		  :values (copy-puzzle-values-array values))))

;;
;; Basic testing
;;
(defun test ()
  "A set of basic tests that must pass"
  (let ((p (make-instance 'puzzle)))
    (with-slots (grid values) p
      (assert (equal (array-dimensions grid) '(9 9)))))
  ;; all units must have 3 elements
  (assert (every (lambda (x) (eq x 3))
		 (loop for r below 9
		    append (loop for c below 9
			      collect (length (aref *units* r c))))))
  ;; all peers must have 20 elements
  (assert (every (lambda (x) (eq x 20))
		 (loop for r below 9
		    append (loop for c below 9
			      collect (length (aref *peers* r c)))))))

;;
;; Some tools to deal with the bit-vector representation
;;
(defun count-remaining-possible-values (possible-values)
  "How many possible values are left in there?"
  (declare (type (simple-bit-vector 10) possible-values))
  ;; remember that we don't use the first bit of the bit-vector
  (let ((count (- (count 1 possible-values) 1)))
    (when (eq 0 count)
      (error 'empty-values))
    count))

(defun first-set-value (possible-values &key (start 1) (end 9))
  "Return the index of the first set value in POSSIBLE-VALUES, a bit vector
    of 10 bits, skipping the first bit"
  (declare (type (simple-bit-vector 10) possible-values))
  (loop for i from start to end
     until (eq 1 (aref possible-values i))
     finally (return i)))

(defun only-possible-value-is? (possible-values value)
  "Return a generalized boolean which is true when the only value found in
   POSSIBLE-VALUES is VALUE"
  (declare (type (simple-bit-vector 10) possible-values))
  (and (eq 1 (aref possible-values value))
       (eq 1 (count-remaining-possible-values possible-values))))

(defun list-all-possible-values (possible-values)
  "Return a list of all possible values to explore"
  (declare (type (simple-bit-vector 10) possible-values))
  (loop for i from 1 to 9
     when (eq 1 (aref possible-values i))
     collect i))

;;
;; Constraint propagation
;;
(defmethod assign ((puzzle puzzle) row col value)
  "Assign given VALUE in cell ROWxCOL of PUZZLE, eliminating all other
   values for given cell, and propagating that elimination to peers."
  (with-slots (grid values) puzzle
    (setf (aref grid row col) value)	; maintain the main grid
    (loop for other-value from 1 to 9	; then the unknown possible values
       unless (eq other-value value)
       do (eliminate puzzle row col other-value)))
  puzzle)

(defmethod eliminate-value-in-peers ((puzzle puzzle) row col value)
  "Eliminate given VALUE from all peers of PUZZLE cell at ROWxCOL"
  (loop for (r . c) in (aref *peers* row col)
     do (eliminate puzzle r c value)))

(defmethod list-places-with-single-unit-solution ((puzzle puzzle) row col value)
  "Check given PUZZLE for VALUE having a single choice of placement within
   all peers units, and return the list of of such places as a (cons ROW COL)"
  (with-slots (values) puzzle
    (loop for unit in (aref *units* row col)
       append (destructuring-bind (n positions)
		  (loop for (r . c) in unit
		     when (only-possible-value-is? (aref values r c) value)
		     count t into n and collect (cons r c) into p
		     finally (return (list n p)))
		;; simply ignore n = 0, can't solve yet
		(when (< 1 n)
		  ;; if more than one place in that unit accepts only the
		  ;; given value, that's a contradiction
		  (error 'unit-contains-contradictory-solution))

		(when (eq 1 n)
		  (list (first positions)))))))

(defmethod eliminate-value ((puzzle puzzle) row col value)
  "Eliminate VALUE from the set of possible values at ROWxCOL in PUZZLE"
  (with-slots (values) puzzle
    ;; values is a 9x9 array containing a bit-vector of 10 elements
    (setf (aref (aref values row col) value) 0)))

(defmethod eliminate ((puzzle puzzle) row col value)
  "Eliminate given VALUE from possible values in cell ROWxCOL of PUZZLE, and
   propagate when needed"
  (with-slots (grid values) puzzle
    (let* ((possible-values (aref values row col))
	   (value-is-set? (eq 0 (aref possible-values value))))
      ;; if already unset, work is already done
      (unless value-is-set?
	;; eliminate the value from the set of possible values
	(eliminate-value puzzle row col value)

	;; now if we're left with a single possible value
	(when (eq 1 (count-remaining-possible-values possible-values))
	  (let ((found-value (first-set-value possible-values)))
	    ;; update the main grid
	    (setf (aref grid row col) found-value)

	    ;; eliminate that value we just found in all peers
	    (eliminate-value-in-peers puzzle row col
				      (first-set-value possible-values))))

	;; now check if any unit has a single possible place for that value
	(loop
	   for (r . c)
	   in (list-places-with-single-unit-solution puzzle row col value)
	   do (assign puzzle r c value))))))

;;
;; parse a Grid
;;
(defmethod parse-grid ((puzzle puzzle) grid-string)
  "Parse a given GRID-STRING and fill PUZZLE accordingly"
  (with-slots (grid values) puzzle
    ;; fill in the grid
    (loop
       for index below 80
       for digit = (parse-integer grid-string :start index :end (+ 1 index) :junk-allowed t)
       do (setf (row-major-aref grid index) (or digit 0)))
    ;; now assign the values
    (loop for r below 9
       do (loop for c below 9
	     when (< 0 (aref grid r c))
	     do (assign puzzle r c (aref grid r c))))
    puzzle))

;;
;; print a grid
;;
(defmethod puzzle-to-string ((puzzle puzzle))
  "Return a string representation of puzzle, ready to display"
  (when puzzle
    (with-slots (grid) puzzle
      (with-output-to-string (p)
	(let* (*print-circle* *print-pretty*)
	  (flet ((format-some-cols (grid row &rest cols)
		   (loop for c in cols do (format p "~d " (aref grid row c)))))
	    (loop for r below 9
	       do
		 (when (and (< 0 r) (eq 0 (mod r 3)))
		   (format p "------+-------+------~%"))
		 (format-some-cols grid r 0 1 2)
		 (format p "| ")
		 (format-some-cols grid r 3 4 5)
		 (format p "| ")
		 (format-some-cols grid r 6 7 8)
		 (format p "~%"))))))))

(defmethod print-puzzle ((puzzle puzzle))
  "Print out a puzzle as a Sudoku grid, given its flat string representation"
  (format t "~a" (puzzle-to-string puzzle))
  puzzle)

;;
;; print a puzzle given its text input
;;
(defun make-puzzle (grid-string)
  "Make a Sudoku puzzle given the GRID-STRING input"
  (let ((p (make-instance 'puzzle)))
    (parse-grid p grid-string)))

(defun print-grid (grid-string)
  "Make a Sudoku puzzle given the GRID-STRING input, then print it"
  (let ((p (make-puzzle grid-string)))
    (print-puzzle p)))

;;
;; Search
;;
(defmethod puzzle-solved? ((puzzle puzzle))
  "Return a generalized boolean which is true when all the values in the
   puzzle are known"
  (with-slots (grid) puzzle
    (notany #'zerop
	    (loop for r below 9
	       append (loop for c below 9 collect (aref grid r c))))))

(defmethod find-position-with-fewest-possibilities ((puzzle puzzle))
  "get a sorted list of counts of possible values (count (row . col)) in the
   puzzle grid"
  (with-slots (values) puzzle
    (destructuring-bind (n (row . col))
	(first
	 (sort
	  (loop for r below 9
	     append (loop for c below 9
		       for n = (handler-case
				   (count-remaining-possible-values
				    (aref values r c))
				 (empty-values () 0))
		       when (< 1 n)
		       collect (list n (cons r c))))
	  (lambda (a b)
	    (< (car a) (car b)))))
      (declare (ignore n))
      (cons row col))))

(defmethod search-puzzle ((puzzle puzzle))
  "Using depth-first search and propagation, try all possible values."
  (cond
    ((null puzzle)           nil)	; earlier failure
    ((puzzle-solved? puzzle) puzzle)	; solved
    (t					; search
     ;; Chose the unfilled square s with the fewest possibilities
     (with-slots (values) puzzle
       (destructuring-bind (row . col)
	   (find-position-with-fewest-possibilities puzzle)
	 (some
	  (lambda (c)
	    (handler-case ; skip search errors and continue
		(search-puzzle (assign (copy-puzzle puzzle) row col c))
	      (empty-values () nil)
	      (unit-contains-contradictory-solution () nil)))
	  (list-all-possible-values (aref values row col))))))))

(defun solve-grid (grid-string)
  "Make a puzzle from GRID-STRING, search solutions"
  (search-puzzle (make-puzzle grid-string)))

;; Tests with some grids
(defun read-grids-in-file (filename)
  "Read one grid per line in the given file and return them as a list"
  (with-open-file (s filename :direction :input)
    (loop for line = (read-line s nil)
	  while line
	  collect line)))

(defmacro timing (&body forms)
  "return both how much real time was spend in body and its result"
  (let ((start (gensym))
	(end (gensym))
	(result (gensym)))
    `(let* ((,start (get-internal-real-time))
	    (,result (progn ,@forms))
	    (,end (get-internal-real-time)))
       (values ,result (/ (- ,end ,start) internal-time-units-per-second)))))

(defun solve-grids (name grids)
  "Solve all grids given and report timings"
  (loop for grid in grids
     for (timing solved?) = (multiple-value-bind (puzzle timing)
				(timing (solve-grid grid))
			     (list timing (puzzle-solved? puzzle)))
     sum timing into s
     count t into n
     count solved? into x
     maximize timing into m
     finally
       (return
	 (format nil
		 "Solved ~d of ~d ~a puzzles (avg ~5f sec (~5f Hz), max ~5f secs)."
		 x n name (/ s n) (/ n s) m))))

(defun solve-example-grids ()
  "Solve all the grids given in examples files"
  (format t "~{~a~^~%~}"
	  (loop for (filename report-name) in `((,*easy50*  "easy")
						(,*top95*   "hard")
						(,*hardest* "hardest"))
	     collect (solve-grids report-name (read-grids-in-file filename)))))
