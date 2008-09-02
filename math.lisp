;;; math.lisp --- math and geometry routines

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(in-package :rlx)

(defun roll (rolls &optional (sides 6) (adds 0))
  "Total ROLLS rolls of a SIDES-sided die, then add ADDS.
So 2d6+2 would be (roll 2 6 2)."
  (let ((total 0))
    (+ adds
       (dotimes (r rolls total)
	 (incf total (+ 1 (random sides)))))))

(defun distance (x1 y1 x2 y2)
  "Compute the distance between the points X1,Y1 and X2,Y2."
  (let ((delta-x (- x2 x1))
	(delta-y (- y2 y1)))
    (sqrt (+ (* delta-x delta-x) (* delta-y delta-y)))))

(defvar *compass-directions* (list :north :south :east :west
				   :northeast :southeast
				   :northwest :southwest :here)
  "List of keywords representing the eight compass directions, plus :here.")

(defvar *compass-opposites* (list :north :south
				  :south :north
				  :east :west
				  :west :east
				  :northeast :southwest
				  :southwest :northeast
				  :southeast :northwest
				  :northwest :southeast
				  :here :here)
  "Property list mapping direction keywords to their 180-degree
opposites.")

(defun opposite-direction (direction)
  "Return the direction keyword that is the opposite direction from
DIRECTION."
  (getf *compass-opposites* direction))

(defun random-direction ()
  (nth (random (length *compass-directions*))
       *compass-directions*))

(defun step-in-direction (row column direction)
  "Return the point ROW, COLUMN moved by one square in DIRECTION."
  (ecase direction
    (:here (values row column))
    (:north (values (- row 1) column))
    (:south (values (+ row 1) column))
    (:east  (values row (+ column 1)))
    (:west  (values row (- column 1)))
    (:northeast (values (- row 1) (+ column 1)))
    (:northwest (values (- row 1) (- column 1)))
    (:southeast (values (+ row 1) (+ column 1)))
    (:southwest (values (+ row 1) (- column 1)))))

(defun direction-to (r1 c1 r2 c2)
  "Return general direction of the ray from R1,C1 to R2,C2."
  (if (< r1 r2) ; definitely to the south
      (if (< c1 c2)
	  :southeast
	  (if (> c1 c2)
	      :southwest
	      :south))
      (if (> r1 r2) ;; definitely to the north
	  (if (< c1 c2)
	      :northeast
	      (if (> c1 c2)
		  :northwest
		  :north))
	  ;; rows are equal; it's either east or west
	  (if (< c1 c2)
	      :east
	      :west))))

;;; Functions that trace out shapes

(defun trace-rectangle (trace-function row column height width)
  "Call TRACE-FUNCTION for each point on the rectangle of HEIGHT
and WIDTH with top left corner at ROW COLUMN."
  (block tracing
    (dotimes (r height)
      ;; Are we painting a horizontal?
      (if (or (equal r 0) (equal r (- height 1)))
	  (dotimes (c width)
	    (if (funcall trace-function (+ r row) (+ c column))
		(return-from tracing)))
	  ;; no, it's a row with only verticals
	  (if (or (funcall trace-function (+ r row) column)
		  (funcall trace-function (+ r row) (+ width column -1)))
	      (return-from tracing))))))

(defun trace-octagon (trace-function center-row center-column radius &optional thicken)
  "Call TRACE-FUNCTION for each point on the octagon of radius RADIUS
centered at row ROW, column COLUMN. When THICKEN is non-nil, thicken
the diagonals of the rectangle in order to facilitate raycasting.
It's an ugly hack, but it helps reduce artifacts."
  ;; calculate
  (let* ((origin-row (- center-row radius))
	 (origin-column (- center-column radius))
	 (side-length radius)
	 (angle-length (floor (/ (float radius) 2.0)))
	 (starting-x (+ 1 angle-length)))
    ;; draw top line
    (dotimes (i side-length)
      (funcall trace-function
	       origin-row
	       (+ origin-column starting-x i)))
    ;; draw top angles
    (dotimes (i angle-length)
      ;; left side
      (funcall trace-function
	       (+ 1 origin-row i)
	       (- center-column angle-length i 1))
      ;; right side
      (funcall trace-function
	       (+ 1 origin-row i)
	       (+ center-column angle-length i 1))
      ;;
      (when thicken
	;; left side
	(funcall trace-function
		 (+ 1 origin-row i)
		 (- center-column angle-length i))
	;; right side
	(funcall trace-function
		 (+ 1 origin-row i)
		 (+ center-column angle-length i))))
    ;; fill in diagonal points that are along the sides
    (when thicken
      ;; left side
      (funcall trace-function
	       (+ 1 origin-row angle-length)
	       (+ origin-column 1))
      ;; right side
      (funcall trace-function
	       (+ 1 origin-row angle-length)
	       (+ center-column side-length -1)))
    ;; draw side lines
    (dotimes (i side-length)
      ;; leftside
      (funcall trace-function
	       (+ 1 origin-row angle-length i)
	       origin-column)
      ;; right side
      (funcall trace-function
	       (+ 1 origin-row angle-length i)
	       (+ origin-column (* 2 side-length))))
    ;; fill in diagonal points that are along the sides
    (when thicken
	  ;; left side
	  (funcall trace-function
		   (+ origin-row side-length angle-length)
		   (+ origin-column 1))
	  ;; right side
	  (funcall trace-function
		   (+ origin-row side-length angle-length)
		   (+ center-column side-length -1)))
    ;; draw bottom angles
    (dotimes (i angle-length)
      ;; left side
      (funcall trace-function
	       (+ 1 origin-row angle-length side-length i)
	       (- center-column angle-length (- angle-length i)))
      ;; right side
      (funcall trace-function
	       (+ 1 origin-row angle-length side-length i)
	       (+ center-column angle-length (- angle-length i)))
      (when thicken
	;; left side
	(funcall trace-function
		 (+ 1 origin-row angle-length side-length i)
		 (+ 1 (- center-column angle-length (- angle-length i))))
	;; right side
	(funcall trace-function
		 (+ 1 origin-row angle-length side-length i)
		 (+ center-column angle-length (- angle-length i 1)))))
    ;; draw bottom line
    (dotimes (i side-length)
      (funcall trace-function
	       (+ 1 origin-row side-length (* 2 angle-length))
	       (+ origin-column starting-x i)))))

;;; Line of sight and lighting

;; :. lighting >

;; We use Bresenham's line algorithm to trace out the player's field
;; of vision and determine which squares are lit.

;; See also http://en.wikipedia.org/wiki/Bresenham's\_line\_algorithm

(defun trace-vertical-line (trace-function x y0 y1)
  (dotimes (n (abs (- y1 y0)))
    (funcall trace-function x (+ y0 n))))

(defun trace-line (trace-function x0 y0 x1 y1)
  "Trace a line between X0,Y0 and X1,Y1.
calling TRACE-FUNCTION at each point of the line.

Returns non-nil if tracing was successful, and nil if failed."
  ;; analyze coordinates and prepare them for bresenham's
  (let ((steep (> (abs (- y1 y0))
		  (abs (- x1 x0))))
	(flipped (> x0 x1)))
    ;; reflect steep lines through line y=x
    (when steep
      (rotatef x0 y0)
      (rotatef x1 y1))
    ;; swap points if line is backwards
    (when flipped
      (rotatef x0 x1)
      (rotatef y0 y1))
    (prog1 flipped
      (if (= x1 x0)
	  ;; just trace a vertical line.
	  (if flipped
	      (trace-vertical-line trace-function x1 y0 y1)
	      (trace-vertical-line trace-function x1 y1 y0))
	  ;; ok, use bresenham's
	  (let* ((delta-x (- x1 x0))
		 (delta-y (abs (- y1 y0)))
		 (err 0.0)
		 (delta-err (/ (float delta-y) (float delta-x)))
		 (y y0)
		 (x x0)
		 (step-y (if (< y0 y1) 1 -1)))
	    ;; main loop
	    (block tracing
	      (loop while (/= x x1) do
		 ;; call the supplied trace function.
		 ;; note that trace functions get args in order (row column).
		 ;; terminate with result = nil if it returns non-nil.
		   (if (if steep
			   (funcall trace-function x y)
			   (funcall trace-function y x))
		       (return-from tracing))
		   (incf err delta-err)
		   (when (>= err 0.5)
		     (incf y step-y)
		     (decf err 1.0))
		 ;; for next iteration
		   (incf x))))))))

;;; Random midpoint displacement fractals

;; The following routines create random midpoint displacement fractals
;; on a grid. This is useful for natural-looking world generation.
;; See also http://en.wikipedia.org/wiki/Diamond-square_algorithm

;; My implementation is slow and needs to be improved.

;; First comes the midpoint formula.
;; http://en.wikipedia.org/wiki/Midpoint

(defun midpoint (A B)
  (list (truncate (/ (+ (first A) (first B)) 2))
	(truncate (/ (+ (second A) (second B)) 2))))

;; We need an representation for a rectangle that is appropriate to
;; our problem. Then we must allow recursive subdivision of
;; rectangles.

(defstruct plasma-rect
  A B C D)

(defun subdivide-rect (R)
  "Subdivide rectangle R into four rectangles joined at the
center point of the original R, and return the list of four
rectangles, or NIL if they would be smaller than one pixel."
  (let* ((A (plasma-rect-A R))
	 (B (plasma-rect-B R))
	 (C (plasma-rect-C R))
	 (D (plasma-rect-D R)))
    ;; are they too small?
    (if (> 2 (abs (- (first C) (first A))))
	nil
	(let
	    ((R1 (make-plasma-rect :A A
				   :B (midpoint A B)
				   :C (midpoint A C)
				   :D (midpoint A D)))
	     ;;
	     (R2 (make-plasma-rect :A (midpoint A B)
				   :B B
				   :C (midpoint B C)
				   :D (midpoint B D)))
	     ;;
	     (R3 (make-plasma-rect :A (midpoint A C)
				   :B (midpoint B C)
				   :C C
				   :D (midpoint C D)))
	     ;;
	     (R4 (make-plasma-rect :A (midpoint A D)
				   :B (midpoint B D)
				   :C (midpoint C D)
				   :D D)))
	  (list R1 R2 R3 R4)))))

(defun render-plasma (height width &key (graininess 1.0) array)
  (let* ((grid (or array (make-array (list height width))))
	 (A (list 0 0))
	 (B (list 0 (- height 1)))
	 (C (list (- width 1) 0))
	 (D (list (- width 1) (- height 1)))
	 (Rs (list (make-plasma-rect :A A :B B :C C :D D)))
	 (Ss nil)
	 (S nil)
	 (R nil)
	 (rect-width nil))
    ;; assign random values to corners of grid to prime the algorithm
    (dolist (P (list A B C D))
      (setf (aref grid (second P) (first P)) (random graininess)))
    ;; begin processing rectangles and painting plasma
    (loop while (setf R (pop Rs))
       do
       ;; subdivide rectangle R and push results onto the rectangle list Rs
	 (setf Ss (subdivide-rect R))
	 (if Ss
	     (loop while (setf S (pop Ss)) do
		  (push S Rs)))
       ;; calculate values for midpoints and center of current rectangle R
	 (setf A (plasma-rect-A R))
	 (setf B (plasma-rect-B R))
	 (setf C (plasma-rect-C R))
	 (setf D (plasma-rect-D R))
	 (setf rect-width (abs (- -1 (first C) (first A))))
       ;; do for all edge midpoints and center:
	 (dolist (pair (list (list A B) (list A C)
			     (list B D) (list C D) (list A D)))
	   (let* ((P1 (first pair))
		  (P2 (second pair))
		  (M (midpoint P1 P2))
		  (V (+
		      ;; average value of values at P1 and P2
		      (* 0.5
			 (+ (aref grid (second P1) (first P1))
			    (aref grid (second P2) (first P2))
			    ;; random part smaller as rects get smaller
			    (* graininess (- 0.5 (random 1.0))
			       (sqrt (float rect-width))))))))
	     ;; paint the point
	     (setf (aref grid (second M) (first M)) V))))
    grid))

;;; math.lisp ends here