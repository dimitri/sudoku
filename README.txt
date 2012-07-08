= Solving Every Sudoku Puzzle

Peter Norvig presents a nice and simple way to solve every Sudoku puzzle in
http://norvig.com/sudoku.html, and mention some ports of his effort. He
picked python to implement his solver, and there's no Common Lisp port.

Let's begin by copying the introduction as-is:

== Sudoku Notation and Preliminary Notions

First we have to agree on some notation. A Sudoku puzzle is a grid of 81
squares; the majority of enthusiasts label the columns 1-9, the rows A-I,
and call a collection of nine squares (column, row, or box) a unit and the
squares that share a unit the peers. A puzzle leaves some squares blank and
fills others with digits, and the whole idea is:

  A puzzle is solved if the squares in each unit are filled with a
  permutation of the digits 1 to 9.

== Notes about the CL port

The main goal here is to have fun in CL, of course, and then to learn more
of it along the way. So that port is not as straighforward as one would
think, I've been trying to make good use of some CL features.

The first consequence concerns data structures used, as those lists and hash
tables in python are better replaced with two dimensional arrays in CL, and
abstracted away in a class. Probably. At least that's what I did here.

== Performances

That's somewhat useless and generally is responsible for loads of
visibility, so let's have some nice numbers here. Note that I'm using the
Clozure Common Lisp implementation (CCL for short) here.

=== Original python version

I had to change a single line in the Norvig's python code, given that I had
to hack the sodoku.txt in order to get the easy50.txt file content, and
obviously that's not matching what Norvig has been working with:

  -    solve_all(from_file("easy50.txt", '========'), "easy", None)
  +    solve_all(from_file("easy50.txt"), "easy", None)

Now for the results:  

  dim ~/dev/CL/sudoku python sudoku.dim.py 
  python sudoku.dim.py 
  All tests pass.
  Solved 50 of 50 easy puzzles (avg 0.01 secs (151 Hz), max 0.01 secs).
  Solved 95 of 95 hard puzzles (avg 0.02 secs (42 Hz), max 0.12 secs).
  Solved 11 of 11 hardest puzzles (avg 0.01 secs (115 Hz), max 0.01 secs).
  
=== SBCL

And the results with sbcl which is known to be faster than ccl:

  * (sudoku:solve-example-grids)
  (sudoku:solve-example-grids)
  Solved 50 of 50 easy puzzles (avg .0039 sec (257.7 Hz), max 0.041 secs).
  Solved 95 of 95 hard puzzles (avg .2383 sec (4.196 Hz), max  3.34 secs).
  Solved 11 of 11 hardest puzzles (avg .0081 sec (123.6 Hz), max 0.042 secs).
  NIL

Not good. Yet.
