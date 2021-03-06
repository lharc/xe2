(in-package :cons-game)

(defcell hive-floor 
  (name :initform "Hive floor")
  (tile :initform "sprout-ground")
  (description :initform "This slimy membrane is richly coated with biosilicate excreta."))

(defcell pollen 
  (name :initform "Type 1 biosilicate pollen")
  (tile :initform "pollen")
  (categories :initform '(:sprout-food)))

(defcell pollen2 
  (name :initform "Type 2 biosilicate pollen")
  (categories :initform '(:sprout-food))
  (tile :initform "pollen2"))

(defcell pollen3a
  (name :initform "Type 3A biosilicate pollen")
  (categories :initform '(:item))
  (tile :initform "pollen3a")
  (description :initform "Highly valuable pollen. Collect as much as you can!"))

(define-method step pollen3a (stepper)
  (when (and [is-player stepper]
	     (has-field :pollen3a stepper))
    (let ((amount (random 0.1)))
      [stat-effect stepper :pollen3a amount]
      [say self "Obtained ~d kg Type 3A biosilicate pollen." amount]
      [play-sample self "biosilicate-sound"]
      [die self])))

(defparameter *sprout-tiles*
  '("sprout" "sprout2" "sprout3" "sprout4" "sprout5" "sprout6"))

(defcell sprout 
  (tile :initform "sprout")
  (generation :initform 0)
  (hit-points :initform (make-stat :base 4 :max 7 :min 0))
  (speed :initform (make-stat :base 1))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 20))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle :enemy :target))
  (description :initform 
"Rapidly growing and reproducing creatures. Grows upon eating pollen,
divides after growing 3 times, dies after six. Kill them to collect
biosilicate materials."))

(define-method divide sprout ()
  [play-sample self "munch1"]
  [stat-effect self :hit-points 3]
  [say self "The sprout divides into two, gaining strength in the process."]
  (dotimes (i (if (zerop (random 17))
		  2 1))
    [drop self (clone =sprout=)]))

(define-method damage sprout (points)
  (let ((r <row>)
	(c <column>))
    [parent>>damage self points]))

(define-method hit sprout (&optional other)
  (when (and other [in-category other :particle])
    [die other])
  [damage self 1])

(define-method die sprout ()
  [play-sample self "biodeath"]
  [say self "The sprout dies."]
  [parent>>die self])

(define-method grow sprout ()
  [expend-action-points self 100]
  (incf <generation>)
  (when (= 3 <generation>)
    [divide self])
  (when (> <generation> 5)
    [say self "The sprout becomes overage."]
    [die self])
  (setf <tile> (nth <generation> *sprout-tiles*)))

(define-method find-food sprout (direction)
  (let ((food [category-in-direction-p *world* <row> <column> direction :sprout-food]))
    (when food
      (prog1 food
	[play-sample self (if (= 0 (random 1))
			      "slurp1" "slurp2")]
	[say self "The sprout eats pollen."]
	[delete-from-world food]
	[move self direction]
	[grow self]))))

(define-method run sprout ()
  [move self (random-direction)]
  (if (< [distance-to-player self] 6)
      (progn [move self [direction-to-player self]]
	     (if [adjacent-to-player self]
		 [attack self [direction-to-player self]]))

    ;; otherwise look for food
      (block searching
	(dolist (dir xe2:*compass-directions*)
	  (when (or [in-category self :dead]
		    [find-food self dir])
	    (return-from searching))))))
  
(define-method attack sprout (direction)
  (let ((player [get-player *world*]))
    [play-sample self "munch2"]
    [damage player 1]
    [say self "The sprout hits you."]))

;;; Poison cells that harm the player

(defcell toxic-hazard
  (name :initform "Toxic hazard")
  (clock :initform 180)
  (tile :initform "toxic-hazard")
  (categories :initform '(:target :actor :item :opaque))
  (hit-points :initform (make-stat :base 6 :min 0))
  (description :initform "Poisonous excreta."))

(define-method step toxic-hazard (stepper)
  (when [is-player stepper]
    [say self "TOXIC HAZARD!"]
    [damage stepper 4]))
;;    [add-category stepper :toxic]))

(define-method run toxic-hazard ()
  (decf <clock>)
  [expend-default-action-points self]
  (when (zerop <clock>)
    [die self]))

;;; The excretors

(defcell excretor 
  (tile :initform "excretor")
  (generation :initform 0)
  (hit-points :initform (make-stat :base 18 :max 12 :min 0))
  (speed :initform (make-stat :base 1))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 20))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle :enemy :target))
  (description :initform "This Lovecraftian abomination excretes toxic waste pods."))

(define-method run excretor ()
  [expend-action-points self 1]
  (when (< [distance-to-player self] 25)
    [drop self (clone =toxic-hazard=)]
    (setf <direction> [direction-to-player self])
    (when [obstacle-in-direction-p *world* <row> <column> <direction>]
      (setf <direction> (random-direction)))
    [move self <direction>]))

(defun hive-random-powerup ()
  (clone (ecase (random 4)
	   (0 =repair-module=)
	   (1 =diamond=)
	   (3 =bomb-ammo=)
	   (2 =energy=))))
	      
(define-method hit excretor (&optional other)
  (when (and other [in-category other :particle])
    [die other])
  [damage self 1]
  [play-sample self "blaagh"])

(define-method die excretor ()
  [play-sample self "blaagh4"]
  (dotimes (n 10)
    [drop self (clone =gas=)])
;;  [drop self (hive-random-powerup)]
  [parent>>die self])

;;; The Biome

(define-prototype biome (:parent =sector=)
  (name :initform "Biosilicate Hive Biome")
  (ambient-light :initform :total)
  (floor :initform "reactor-background")
  (barrier :initform "reactor-foreground")
  (accent :initform "reactor-accent")
  (size :initform 10)
  (required-modes :initform '(:vehicle :spacesuit))
  (clusters :initform 10)
  ;;  (factory-count :initform 0) ;; some enemy
  (height :initform 20)
  (width :initform 20)
  (scale :initform '(5 m))
  (edge-condition :initform :block))

(define-method begin-ambient-loop biome ()
  (play-music "neo-eof" :loop t)) 

(define-method drop-cluster biome (row column)
  (labels ((drop-pollen (r c &optional (type 1))
	     [drop-cell self (ecase type
			       (1 (clone =pollen=))
			       (2 (clone =pollen2=)))
			r c])
	   (drop-pollen2 (r c)
	     (drop-pollen r c 2)))
    (trace-octagon #'drop-pollen row column 5)
    (trace-rectangle #'drop-pollen2 
		     (- row 2) (- column 2)
		     5 5 t)
    (dotimes (i 3)
      [drop-cell self (clone =sprout=)
		 (+ (- row 3) 
		    (random 5))
		 (+ (- column 3)
		    (random 5))])))

(define-method generate biome (&key (height 80) (width 80) clusters
				    (sequence-number (random 32768))
				    (size 10)
				    (pollen3a 30)
				    (leeches 10)
				    (excretors 4))
  (setf <height> (or height (+ 20 (* size (random 8)))))
  (setf <width> (or width (+ 30 (* size (random 8)))))
  (setf <clusters> (or clusters (+ 5 (* size 2))))
  [create-default-grid self]
  (clon:with-field-values (height width clusters) self
    ;; drop floor
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =hive-floor=) i j]))
    ;; drop plasma pollen
    (let ((plasma (xe2:render-plasma height width :graininess 0.3))
    	  (value nil))
      (dotimes (i height)
    	(dotimes (j width)
    	  (setf value (aref plasma i j))
    	  (when (< 0.9 value)
    	    [drop-cell self 
    		       =pollen2=
    		       i j]))))
    ;; drop precious pollen3a
    (dotimes (p pollen3a)
      [drop-cell self (clone =pollen3a=) 
		 (+ (truncate (/ height 2))
		    (random 12))
		 (+ (truncate (/ width 2))
		    (random 12))])
    (dotimes (r 2)
      [drop-cell self (clone =rook=) 
		 (truncate (/ height 2))
		 (truncate (/ width 2)) :loadout t])    ;;
    ;; drop clusters
    (dotimes (c clusters)
      [drop-cluster self (random height) (random width)])
     ;; drop excretors
    (dotimes (ex excretors)
      [drop-cell self (clone =excretor=) (random height) (random width)])
    ;; player 
    [drop-cell self (clone =launchpad=) (random 16) (random 16)]))

      

    



  

