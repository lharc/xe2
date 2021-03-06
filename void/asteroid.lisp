(in-package :void)

;;; An asteroid.

(defvar *asteroid-count* 0)

(defcell asteroid
  (name :initform "Asteroid")
  (categories :initform '(:actor :sticky :target))
  (hit-points :initform (make-stat :base 1 :min 0))
  (movement-cost :initform (make-stat :base 10))
  (stuck-to :initform nil)
  (direction :initform :north)
  (stepping :initform t)
  (description :initform "These space rocks often contain Endurium."))

(define-method is-stuck asteroid ()
  <stuck-to>)

(define-method die asteroid ()
  (decf *asteroid-count*)
  [>>say :narrator "You destroyed an asteroid!"]
  [play-sample self "bleep"]
  [drop self (if (equal <tile> "asteroid-red")
		 (random-powerup)
		 (if (zerop (random 2))
		     (clone =crystal=)
		     (clone =small-crystal=)))]
  [stat-effect [get-player *world*] :score 120]
  (when <stuck-to>
    [unstick <stuck-to> self])
  [parent>>die self])

(define-method initialize asteroid (&key speed direction color)
  (incf *asteroid-count*)
  (setf <speed> (make-stat :base speed))
  (setf <direction> direction)
  (setf <tile>
	(ecase color
	  (:orange "asteroid-orange")
	  (:red "asteroid-red")
	  (:blue "asteroid-blue")
	  (:brown "asteroid-brown"))))

(define-method run asteroid ()
  (when (eq :here <direction>)
    (setf <direction> :north))
  (if (<= [stat-value self :hit-points] 0)
      [die self]
      ;; if free, float
      (if (and (not <stuck-to>)  
	       (not [obstacle-in-direction-p *world* <row> <column> <direction>]))
	  [move self <direction>]
	  ;; otherwise bounce (when free)
	  (unless <stuck-to>
	    (setf <direction> (xe2:random-direction))))))

(define-method step asteroid (stepper)
  (when [in-category stepper :player]
    [damage stepper 3]
    [>>say :narrator "You took a hit!"]
    [die self]))

;;; Polaris collects asteroids

(defcell polaris
  (tile :initform "polaris")
  (name :initform "Polaris Mine")
  (speed :initform (make-stat :base 7))
  (asteroids :initform '())
  (stepping :initform t)
  (categories :initform '(:actor :target))
  (hit-points :initform (make-stat :base 5 :min 0 :max 5))
  (direction :initform (xe2:random-direction))
  (description :initform 
"This magnetic mine explodes if damaged, and attracts metallic
asteroids."))

(define-method scan-neighborhood polaris ()
  (dolist (dir *compass-directions*)
    (multiple-value-bind (r c) (xe2:step-in-direction <row> <column> dir)
      (do-cells (cell [cells-at *world* r c])
	(when (and cell [in-category cell :sticky])
	  [stick self cell])))))

(define-method change-direction polaris (direction)
  (dolist (asteroid <asteroids>)
    (assert (clon:object-p asteroid))
    (setf (field-value :direction asteroid) direction))
  (setf <direction> direction))

(define-method move-as-group polaris (direction)
  ;; move self first so that nobody steps on us
  [move self direction]
  ;; now move the stuck asteroids
  (dolist (a <asteroids>)
    [move a direction]))

(define-method run polaris ()
  [scan-neighborhood self]	       
  ;; reset direction for stuck mines
  (when (eq <direction> :here)
    (setf <direction> :north))
  (let ((direction <direction>))	       
    (labels ((obstructed (asteroid)
	       [obstacle-in-direction-p *world*
					(field-value :row asteroid)
					(field-value :column asteroid)
					direction]))
      (let ((timeout 8)) 
	(loop while (and (plusp timeout)
			 (or (some #'obstructed <asteroids>)
			     (obstructed self)))
	   do [change-direction self (xe2:random-direction)]
	     (decf timeout))
	(unless (zerop timeout)
	  ;; it's safe. move as a group. 
	  [move-as-group self <direction>])))))
	      
(define-method stick polaris (asteroid)
  (when (and [in-category asteroid :sticky]
	     (not [is-stuck asteroid]))
    (setf (field-value :stuck-to asteroid) self)
    (setf (field-value :direction asteroid) <direction>)
    ;; put it back where it was
    [move asteroid (xe2:opposite-direction (field-value :direction asteroid))]
    (pushnew asteroid <asteroids>)))

(define-method unstick polaris (asteroid)
  (setf <asteroids> (delete asteroid <asteroids>))
  (when (= 0 (length <asteroids>))
    [stat-effect [get-player *world*] :score 2000]
    [play-sample self "sweep"]))

(define-method explode polaris ()
  (labels ((boom (r c &optional (probability 50))
	     (prog1 nil
	       (when (and (< (random 100) probability)
			  [in-bounds-p *world* r c])
		 [drop-cell *world* (clone =explosion=) r c :no-collisions nil]))))
    (dolist (dir xe2:*compass-directions*)
      (multiple-value-bind (r c)
	  (step-in-direction <row> <column> dir)
	(boom r c 100)))
    ;; randomly sprinkle some fire around edges
    (trace-rectangle #'boom 
		     (- <row> 2) 
		     (- <column> 2) 
		     5 5)))

(define-method die polaris ()
  [explode self]
  [parent>>die self])
