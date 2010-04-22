(in-package :cons-game)

;;; enemy ships a la tac scan

(define-prototype xr7 (:parent =rook=)
  (name :initform "XR-7 Phalanx Interdictor")
  (tile :initform "xr7")
  (speed :initform (make-stat :base 1))
  (description :initform "The deadly XR7 can fire lasers from a distance."))

(define-method fire xr7 (direction)
  [expend-action-points self 30]
  (let* ((world *world*)
	 (player [get-player *world*]))
    (labels ((draw-beam (image)
	       (prog1 t (multiple-value-bind (x0 y0) 
			    [screen-coordinates self]
			  (multiple-value-bind (x1 y1)
			      [screen-coordinates player]
			    (xe2:draw-line (+ 8 x0) (+ 8 y0) (+ x1 8) (+ y1 8)
					   :destination image))))))
      [damage player 2]
      [say self "You sustain 2 damage from the laser."]
      [play-sample self "laser2"]
      [>>add-overlay :viewport #'draw-beam])))

(define-method seek xr7 ()
  (clon:with-field-values (row column) self
    (when (< [distance-to-player *world* row column] <chase-distance>)
      (let ((direction [direction-to-player *world* row column])
	    (world *world*))
	(if (< [distance-to-player self] 8)
	    (progn
	      [fire self direction]
	      (setf <clock> 6
		    <behavior> :fleeing))
	    (if [obstacle-in-direction-p world row column direction]
		(let ((target [target-in-direction-p world row column direction]))
		  (if (and target (not [in-category target :enemy]))
		      (progn nil)
;;			[>>attack self direction]
		      (progn (setf <direction> (random-direction))
			     [>>move self direction])))
		(progn (when (< 7 (random 10))
			 (setf <direction> (random-direction)))
		       [>>move self direction])))))))

;;; bresenham's rail gun

(defsprite rail-trail 
  (image :initform "ring")
  (categories :initform '(:actor :rail))
  (movement-distance :initform 2)
  (clock :initform 6))

(define-method run rail-trail ()
  (decf <clock>)
  [move self (random-direction)]
  (when (minusp <clock>)
    [die self]))

(defsprite rail-particle ()
  (clock :initform 60)
  (team :initform :enemy)
  (speed :initform (make-stat :base 1))
  (categories :initform '(:actor :rail))
  (movement-distance :initform 4)
  (image :initform "rail-particle"))

(define-method impel rail-particle (row column)
  (multiple-value-bind (r c) [grid-coordinates self]
    (setf <direction> (direction-to r c row column))))
;;    [move self <direction>]
  
(define-method run rail-particle ()
  (decf <clock>)
  (setf <image> (car (one-of (list "rail-particle" "rail-particle2"))))
  (if (zerop <clock>)
      [die self]
      (progn [drop-trail self nil]
	     [move self <direction>])))

(define-method drop-trail rail-particle (direction)
  (declare (ignore direction))
  (multiple-value-bind (x y) [xy-coordinates self]
    (let ((trail (clone =rail-trail=)))
      [add-sprite *world* trail]
      [update-position trail x y])))

(define-method do-collision rail-particle (&optional object)
  (cond ([in-category object :obstacle]
	 (unless (same-team object self)
	   [hit object] [die self]))
	((and (not [in-category object :rail])
	       (has-field :team object)
	       (not (eq <team> (field-value :team object))))
	  [hit object self]
	  [die self])))

(defcell rail-cannon
  (name :initform "Rail gun cannon")
  (tile :initform "gun")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:center-bay))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 100))
  (attack-power :initform (make-stat :base 18))
  (attack-cost :initform (make-stat :base 5))
  (energy-cost :initform (make-stat :base 0)))

(define-method fire rail-cannon (row column)
  [expend-action-points <equipper> [stat-value self :attack-cost]]
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((particle (clone =rail-particle=)))
	[play-sample <equipper> "bip"]
	(multiple-value-bind (x y) [xy-coordinates <equipper>]
	    [drop-sprite *world* particle (+ x 8) (+ y 8)]
	    [impel particle row column]))
      [say self "Not enough energy to fire!"]))

;;; the eyeboss

(defparameter *guardic-eye-open-time* 70)
(defparameter *guardic-eye-closed-time* 80)

(defcell guardic-eye
  (name :initform "Guardic eye")
  (tile :initform "guardic")
  (team :initform :enemy)
  (auto-loadout :initform t)
  (hit-points :initform (make-stat :base 20 :max 4 :min 0))
  (open :initform nil)
  (clock :initform (random *guardic-eye-closed-time*))
  (speed :initform (make-stat :base 3))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (energy :initform (make-stat :base 1000 :min 0 :max 1000))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 6))
  (equipment-slots :initform '(:center-bay))
  (firing-with :initform :center-bay)
  (max-items :initform (make-stat :base 2))
  (categories :initform '(:actor :obstacle :enemy :target))
  (description :initform "Invulnerable until red eye opens. Fires particle weapons."))

(define-method loadout guardic-eye ()
  (setf <speed> (make-stat :base 2))
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =rail-cannon=)]])

(define-method run guardic-eye ()
  [expend-default-action-points self]
  ;; open or close eye
  (decf <clock>)
  (if (zerop <clock>)
      (if <open>
	  (progn
	    (setf <open> nil)
	    (setf <tile> "guardic")
	    (setf <clock> *guardic-eye-closed-time*))
	  (progn
	    (setf <open> t)
	    (setf <tile> "guardic-open")
	    (setf <clock> *guardic-eye-open-time*))))
  ;; attack!
  (if (< [distance-to-player self] 20)
      (let ((cannon [equipment-slot self :center-bay]))
	(when <open> 
	    (progn (when [can-see-player self]
		     [fire cannon [player-row *world*]
			   [player-column *world*]]
		     [expend-action-points self 100]))))))
					  
(define-method damage guardic-eye (points)
  ;; only damage when open
  (if <open>
    [parent>>damage self points]
    [say self "Cannot damage closed eye."]))

(define-method hit guardic-eye (&optional object)
  [play-sample self (if <open> "munch1" "ice")]
  [damage self 1])

(defcell guardic 
  (name :initform "Electric eye")
  (tile :initform "godseye"))
  
;;; the bases

(defcell vomac-base 
  (tile :initform "vomac-base")
  (description :initform "Platform for Guardic eye bases."))

(define-method explode vomac-base ()
  [drop self (clone =explosion=)]
  [die self])

(defcell vomac-wires 
  (tile :initform "vomac-wires")
  (description :initform "Deadly live defense wires."))

(define-method step vomac-wires (stepper)
  (when [is-player stepper]
    [play-sample self "spawn"]
    [damage stepper 5]
    [say self "You are shocked by the guard wires!"]))

;;; the vomac ship

;; (define-prototype vomac (:parent =olvac=)
;;   (tile :initform "vomac")
;;   (mode :initform :vehicle)
;;   (name :initform "Vomac XLUX Fighter")
;;   (last-direction :initform :here)
;;   (speed :initform (make-stat :base 9 :min 0 :max 25))
;;   (strength :initform (make-stat :base 12))
;;   (defense :initform (make-stat :base 15))
;;   (hearing-range :initform 15)
;;   (energy :initform (make-stat :base 70 :min 0 :max 70 :unit :gj))
;;   (pollen3a :initform (make-stat :base 0 :min 0 :max 30 :unit :kg))
;;   (endurium :initform (make-stat :base 70 :min 0 :max 140 :unit :kg))
;;   (technetium :initform (make-stat :base 0 :min 0 :unit :ug))
;;   (biosilicate :initform (make-stat :base 0 :min 0 :unit :g))
;;   (hit-points :initform (make-stat :base 70 :min 0 :max 70))
;;   (movement-cost :initform (make-stat :base 8))
;;   (max-items :initform (make-stat :base 2))
;;   (trail-length :initform (make-stat :base 12 :min 0))
;;   (bomb-ammo :initform (make-stat :base 10 :min 0 :max 10))
;;   (oxygen :initform (make-stat :base 200 :min 0 :max 200))
;;   (invincibility-clock :initform 0)
;;   (stepping :initform t)
;;   (attacking-with :initform nil)
;;   (firing-with :initform :center-bay)
;;   (categories :initform '(:actor :player :target :container :light-source :vehicle :repairable))
;;   (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension))
;;   (boost-clock :initform 0)
;;   (description :initform 
;; "The Vomac XLUX Fighter is Arch Gamma's newest mid-range fighter model
;; with 8-way fire and heavy armor."))

;; (define-method damage vomac (points)
;;   [play-sample self "vomac-damage"]
;;   [parent>>damage self points])

;; (define-method update-tile vomac ()
;;   nil)

;; (define-method drop-trail vomac ()
;;   nil)

;; (define-prototype defleptor-trail (:parent =muon-trail=)
;;   (speed :initform (make-stat :base 20))
;;   (tile :initform "defleptor-trail")
;;   (clock :initform 3))

;; (define-method initialize defleptor-trail ()
;;   (setf <direction> :north))

;; (define-method run defleptor-trail ()
;;   [expend-default-action-points self]
;;   (decf <clock>)
;;   (when (minusp <clock>)
;;     [die self]))

;; (define-prototype defleptor-wave (:parent =muon-particle=)
;;   (name :initform "Defleptor wave")
;;   (speed :initform (make-stat :base 90))
;;   (tile :initform "defleptorwave")
;;   (clock :initform 20))

;; (define-method update-tile defleptor-wave ()
;;   (setf <tile> (case <direction> 
;; 		 (:north "defleptorwave")
;; 		 (:south "defleptorwave-south")
;; 		 (:west "defleptorwave-west")
;; 		 (:east "defleptorwave-east")
;; 		 (:northeast "defleptorwave-northeast")
;; 		 (:northwest "defleptorwave-northwest")
;; 		 (:southeast "defleptorwave-southeast")
;; 		 (:southwest "defleptorwave-southwest")
;; 		 (otherwise ".gear"))))
  
;; (define-method drop-trail defleptor-wave (direction)
;;   (declare (ignore direction))
;;   [drop self (clone =defleptor-trail=)])

;; (define-prototype vomac-cannon (:parent =muon-cannon=)
;;   (name :initform "Vomac defleptor wave cannon")
;;   (energy-cost :initform (make-stat :base 1))
;;   (tile :initform "defleptorwave"))

;; (define-method fire vomac-cannon (direction)
;;   (if [expend-energy <equipper> [stat-value self :energy-cost]]
;;       (let (wave)
;; 	(dolist (dir (delete :here xe2:*compass-directions*))
;; 	  (setf wave (clone =defleptor-wave=))
;; 	  [drop <equipper> wave]
;; 	  [play-sample <equipper> "defleptor3"]
;; 	  [impel wave dir])
;; 	[expend-default-action-points self])
;;       [say <equipper> "Not enough energy to fire!"]))

;; (define-method loadout vomac ()
;;   [make-inventory self]
;;   [make-equipment self]
;;   [equip self [add-item self (clone =vomac-cannon=)]])
 
;;; The vaxodrones

(defcell vaxodrone 
  (name :initform "VAXodrone")
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (speed :initform (make-stat :base 7 :min 7))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 3))
  (tile :initform "vaxodrone")
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (xe2:random-direction))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10))
  (description :initform "Swarms of vaxodrones will trap and kill you."))
 
(define-method run vaxodrone ()
  (when (< [distance-to-player self] 18)
    (let ((dir [direction-to-player self]))
      (if (= 0 (random 2))
	  [move self dir]
	  [move self (random-direction)])
      (when [adjacent-to-player self]
	[play-sample self "scree"]
	[attack self dir]))))

(define-method die vaxodrone ()
  [play-sample self "aagh2"]
  [parent>>die self])

(define-method loadout vaxodrone ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =shock-probe=)]])

;;; an arena for vomac combat

(defcell vomac-starfield 
  (name :initform "Star corridor")
  (tile :initform "vomac-starfield"))

(defcell vomac-starfield2
  (name :initform "Star corridor with turbulence")
  (tile :initform "vomac-starfield2"))

(defcell road
  (description :initform "Security vehicle transit area.")
  (tile :initform "darkcyanworld"))

(define-prototype corridor (:parent =sector=)
  (name :initform "Corridor")
  (description :initform 
"These massive tube-like corridors are used to transport materials and
vehicles all over the base. Steam flows upward along the sloped
channel; corridors adjacent to a reactor are especially hot.")
  (level :initform 1)
  ;;
  (floor :initform "corridor-background")
  (barrier :initform "corridor-foreground")
  (accent :initform "corridor-accent")
  (grammar :initform 
	   '((world >> (10 :jump 90 :right 10 :jump =exit= :color :drop))))
  ;;
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(3 m))
  (height :initform 20)
  (width :initform 100)
  (edge-condition :initform :block))

(define-method drop-base corridor (row column &optional (size 5))
  (labels ((drop-panel (r c)
	     (prog1 nil [drop-cell self (clone =vomac-base=) r c])))
    (trace-rectangle #'drop-panel row column size size :fill)
    (dotimes (i 8)
      [drop-cell self (clone =guardic-eye=) 
    		 (+ row (random size)) (+ column (random size)) :loadout t])
    (dotimes (i (* 2 size))
      [drop-cell self (clone =vomac-wires=)
		 (+ row (random size)) (+ column (random size))])))

(define-method begin-ambient-loop corridor ()
  (play-music "vedex" :loop t))

(define-method generate corridor (&rest params)
  [create-default-grid self]
  [parent>>generate self]
  (dotimes (i 5)
    [drop-base self (random <height>) (random <width>) (+ 5 (random 10))])
  (dotimes (i 8)
    (let ((xr7 (clone =xr7=))
	  (row (random <height>))
	  (column (random <width>)))
      [drop-cell self xr7 row column :loadout t])))

;;; Radiation graviceptors leave energy behind when you kill them

(defcell graviceptor
 (tile :initform "gravicept")
 (hit-points :initform (make-stat :base 3 :max 3 :min 0))
 (speed :initform (make-stat :base 1))
 (strength :initform (make-stat :base 10))
 (defense :initform (make-stat :base 10))
 (stepping :initform t)
 (team :initform :enemy)
 (movement-cost :initform (make-stat :base 40))
 (max-items :initform (make-stat :base 2))
 (direction :initform (random-direction))
 (categories :initform '(:actor :obstacle :enemy :target))
 (description :initform 
"The deadly Graviceptor seeks out the player and explodes into a cloud
of poisonous radioactive gas."))

(define-method run graviceptor ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player world row column]))
      (if [adjacent-to-player world row column]
	  [explode self]
	  (if [obstacle-in-direction-p world row column direction]
	      (let ((target [target-in-direction-p world row column direction]))
		(if (and target (not [in-category target :enemy]))
		    [explode self]
		    (progn (setf <direction> (random-direction))
			   [>>move self direction])))
	      (progn (when (< 7 (random 10))
		       (setf <direction> (random-direction)))
		     [>>move self direction]))))))

(define-method drop-gas graviceptor (row column &key
					       (height (+ 3 (random 5)))
					       (width (+ 3 (random 5))))
  (labels ((drop-gas (r c)
	     (prog1 nil
	       [drop-cell *world* (clone =gas=) r c])))
    [play-sample self "pop-ssh"]
    (trace-rectangle #'drop-gas row column height width :fill)))

(define-method explode graviceptor ()
  ;; only when not in space debris... debris are "safe zones" from mines
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
		     5 5)
    ;; release radiation
					;      (when (< 3 (random 10))
    [drop-gas self (- <row> 2) (- <column> 2) :height 5 :width 5]
    [die self]))

(define-method damage graviceptor (points)
  (declare (ignore points))
  [stat-effect [get-player *world*] :score 5000]
  [>>say :narrator "Graviceptor destroyed. 5000 Bonus Points."]
  [explode self])

(define-method hit graviceptor (&optional other)
  [damage self 1]
  (when other [die other]))

;;; A radiation probe releases a trail of toxic graviceptor particles.

(defcell radiation 
  (categories :initform '(:actor))
  (clock :initform 200)
 (team :initform :enemy)
  (description :initform 
"A radiation trail. Don't touch it. Fades after several turns."))
  
(define-method initialize radiation (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "radiation-north")
		 (:south "radiation-south")
		 (:east "radiation-east")
		 (:west "radiation-west")
		 (:northeast "radiation-northeast")
		 (:northwest "radiation-northwest")
		 (:southeast "radiation-southeast")
		 (:southwest "radiation-southwest")
		 (:here "explosion"))))

(define-method run radiation ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (zerop <clock>)
    [die self]))

(define-method damage radiation (points)
  (declare (ignore points))
  [die self])

(define-method die radiation ()
  (when (> 1 (random 80))
    [drop self (clone =graviceptor=)])
  [parent>>die self])

(define-method step radiation (stepper)
  (unless (same-team self stepper)
    [drop self (clone =explosion=)]	       
    [damage stepper 1]))
	   
(defcell probe 
  (tile :initform "probe")
  (hit-points :initform (make-stat :base 3 :max 3 :min 0))
  (speed :initform (make-stat :base 1))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (team :initform :enemy)
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (direction :initform (random-direction))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:actor :obstacle :enemy :target))
  (trail-length :initform (make-stat :base 160))
  (description :initform 
"This automated probe scans the area releasing graviceptor mines.
Avoid its radioactive trail and shoot it from a distance.
Watch out---they can spawn mines even after death!"))

(define-method move probe (direction)
  [drop self (clone =radiation= 
		    :direction direction 
		    :clock [stat-value self :trail-length])]
  [parent>>move self direction])

(define-method run probe ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column])
	   (distance [distance-to-player *world* row column]))
      (if (< distance 8)
	  (progn 
	    (percent-of-time 4 [play-sample self "dtmf1"])
	    (setf <direction> (if (< distance 4)
				  (random-direction)
				  direction))
	    [>>move self <direction>])
	  ;; bounce around 
	  (progn 
	    (when [obstacle-in-direction-p *world* <row> <column> <direction>]
	      (setf <direction> (random-direction)))
	    [>>move self <direction>])))))

(define-method hit probe (&optional other)
  [damage self 1]
  (when other [die other]))

(define-method die probe ()
  [play-sample self "death-alien"]
  [drop self (clone =explosion=)]
  [parent>>die self])
