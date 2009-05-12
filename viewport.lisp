;;; viewport.lisp --- tile engine display widget

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: games

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

;; A world may have one or more viewport widgets. In a standard
;; viewport, cells are represented onscreen by uniformly-sized
;; graphical tiles; worlds are viewed from a bird's-eye
;; perspective. Other viewports may render schematic or compressed
;; views of the world (for example, an auto-map display.)

;;; Code:

(in-package :rlx)

(define-prototype viewport 
    (:parent =widget= :documentation "A map display for RLX worlds.")
  (world :documentation "The world object to be displayed.")
  (margin :initform 6 :documentation "Scroll margin.")
  (origin-x :initform 0 
	    :documentation "The world x-coordinate of the tile at the viewport's origin.")
  (origin-y :initform 0 
	    :documentation "The world y-coordinate of the tile at the viewport's origin.")
  (origin-width :initform 10 :documentation "The width in tiles of the viewport.")
  (origin-height :initform 10 :documentation "The height in tiles of the viewport.")
  (tile-size :initform 16 :documentation "Size in pixels of a tile. They must be square."))

(define-method set-world viewport (world)
  (setf <world> world))
    
(define-method render viewport ()
  [adjust self] ;; hehe
  (let* ((world <world>)
	 (origin-width <origin-width>)
	 (origin-height <origin-height>)
	 (origin-x <origin-x>)
	 (origin-y <origin-y>)
	 (image <image>)
	 (tile-size <tile-size>)
	 objects cell)
    (with-field-values (grid light-grid environment-grid phase-number
			     height width
			     turn-number ambient-light) world
      ;; blank the display
      [clear self]
      ;; draw the tiles
      (dotimes (i origin-height)
	(dotimes (j origin-width)
	  ;; is this square lit? 
	  ;; <: lighting :>
	    (if (and (array-in-bounds-p grid (+ i origin-y) (+ j origin-x))
		     (or (eq :total ambient-light)
			 (= 1 (aref light-grid (+ i origin-y) (+ j origin-x)))))
		(progn 
		  (setf objects (aref grid 
				      (+ i origin-y)
				      (+ j origin-x)))
		  (dotimes (k (fill-pointer objects))
		    (setf cell (aref objects k))
		    (when (object-p cell)
		      (draw-resource-image (field-value :tile cell)
					   (* j tile-size) (* i tile-size)
					   :destination image))))
		;; not in bounds, or not lit; draw blackness
		(draw-resource-image ".blackness" (* j tile-size) (* i tile-size)
				     :destination image))))
      ;; update geometry
      (setf <width> (* tile-size origin-width))
      (setf <height> (* tile-size origin-height)))))

(define-method set-origin viewport (&key x y height width)
  (setf <origin-x> x
	<origin-y> y
	<origin-width> width
	<origin-height> height))

(define-method adjust viewport ()
  "Move the viewport's origin if required to keep the player onscreen."
  (let* ((world <world>)
	 (world-width (field-value :width world))
	 (world-height (field-value :height world))
	 (player (field-value :player world))
	 (player-x (field-value :column player))
	 (player-y (field-value :row player))
	 (origin-x <origin-x>)
	 (origin-y <origin-y>)
	 (origin-height <origin-height>)
	 (origin-width <origin-width>)
	 (margin <margin>))
    ;; are we outside the "comfort zone"?
    (when (or 
	   ;; too far left
	   (> (+ origin-x margin) 
	      player-x)
	   ;; too far right
	   (> player-x
	      (- (+ origin-x origin-width)
		 margin))
	   ;; too far up
	   (> (+ origin-y margin) 
	      player-y)
	   ;; too far down 
	   (> player-y 
	      (- (+ origin-y origin-height)
		 margin)))
      ;; yes. recenter.
      (setf <origin-x> 
	    (max 0
		 (min (- world-width origin-width)
		      (- player-x 
			 (truncate (/ origin-width 2))))))
      (setf <origin-y> 
	    (max 0 
		 (min (- world-height origin-height)
		      (- player-y 
			 (truncate (/ origin-height 2)))))))))


;;; viewport.lisp ends here