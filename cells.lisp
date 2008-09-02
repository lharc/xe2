;;; cells.lisp --- defining roguelike game objects

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

;; "Cells" are CLON objects which represent all in-game entities;
;; player characters, enemies, weapons, items, walls and floors are
;; all different types of cells. Game play occurs in a
;; three-dimensional grid of cells called a "world" (see worlds.lisp).

;; Cells may be stacked along the z-axis, and may also contain other
;; cells. Cells interact by sending messages to one another (with
;; `send-queue'); these messages are queued and processed by the
;; world for delivery to their recipients. 

;;; Code:

(in-package :rlx)

;;; Base cell prototype

;; The base cell prototype's data members and methods build in many basic
;; features of the roguelike engine:

(define-prototype cell
    (:documentation "An RLX game-world object.")
  (weight :documentation "Weight of the cell.")
  (tile :initform ".asterisk" :documentation "Resource name of image.")
  (row :documentation "When non-nil, the current row location of the cell.")
  (column :documentation "When non-nil, the current column of the cell.")
  ;; :. categories >
  (categories :documentation "List of category keyword symbols") 
  ;; :. lighting >
  (light-radius :initform 0 :documentation "Strength of light cast by this object.") 
  ;; :. action-points >
  (actions :initform nil :documentation "List of Action Method keywords.") ;; 
  (speed :initform 10 :documentation "The number of action points alloted each phase.")
  (phase-number :initform 0
	       :documentation "An integer giving the last phase this cell has completed.")
  (action-points :initform 0
		 :documentation "An integer giving the ability of a cell to take turns on a given round.")
  (default-cost :initform '(:base 5 :min nil :max nil :delta nil)
    :documentation "Cost for basic actions.")
  (movement-cost :initform '(:base 10 :min nil :max nil :delta nil)
		 :documentation "Base cost of moving one square.")
  ;; :. knowledge >
  (name :documentation "The name of this cell.")
  (description :documentation "A description of the cell.") 
  (unknown-name :documentation "The name of this cell, when it is unknown.")
  (unknown-description :documentation "A description of the cell, when it is unknown.")
  ;; :. equipment >
  (equipment :documentation "Property list of :slot -> cell pairs.") 
  (equipment-slots :documentation "List of keyword symbols identifying available equipment slots."
		   :initform '(:head :neck :left-hand :right-hand :hands :feet :legs :torso :arms :pack))
  (using-slot :documentation "Keyword symbol of the currently selected equipment slot.")
  (attacking-with :documentation "Keyword symbol of the currently selected weapon.")
  (equip-for :documentation "List of keyword symbols showing where this item may be equipped.")
  ;; :. containers >
  (inventory :documentation "The contents (if any) of the cell.")
  (max-weight :documentation "Maximum weight this container can hold.")
  (max-items :documentation "Maximum number of items this container can hold.")
  (parent-container :documentation "Link to containing cell, if any."))

;;; Statistics 

;; :. stats >

;; Characters and objects may have numeric-valued attributes like
;; Strength and Dexterity that have a minimum and maximum value
;; (perhaps decided on the basis of class) as well as temporary and
;; permanent effects. In this case you want to store a base value,
;; minimum, maximum, and current delta, and compute the value at run
;; time.
;;
;; Stats are just property lists with four different components: :base
;; :min :max and :delta. 

(define-method stat-value cell (stat-name &optional component (clamping t))
  "Compute the current value of the statistic in field STAT-NAME.
If a COMPONENT keyword is provided, return that component of the stat
instead of computing the value."
  (let ((stat (field-value stat-name self)))
    (if (member component '(:base :min :max :delta))
	(getf stat stat-name)
	;; compute the value
	(destructuring-bind (&key base delta min max) stat
	  (let ((val (+ base (if (numberp delta) delta 0))))
	    (when clamping 
	      (when (and (numberp min) (< val min))
		(setf val min))
	      (when (and (numberp max) (> val max))
		(setf val max)))
	    val)))))
	   
(define-method stat-effect cell (stat-name val
					   &optional (component :base) (clamping t))
  "Add VAL, which may be negative, to the COMPONENT part of the stat
field named by STAT-NAME. The default is to change the :base value."
  (let* ((stat (field-value stat-name self))
	 (x (getf stat component)))
    (destructuring-bind (&key base min max &allow-other-keys) stat
      (incf x val)
      ;; ensure base stays within bounds.
      (when (and clamping (eq :base component))
	(when (numberp min)
	  (setf x (max min x)))
	(when (numberp max)
	  (setf x (min max x))))
      ;; update the stat
      (setf (getf stat component) x)
      (setf (field-value stat-name self) stat))))

(defun make-stat (&key base min max delta)
  (assert (numberp base))
  (list :base base :min min :max max :delta delta))

;;; Cell categories

;; :. categories >

;; Cells may be placed into categories that influence their processing
;; by the engine. The field `<categories>' is a set of keyword
;; symbols; if a symbol `:foo' is in the list, then the cell is in the
;; category `:foo'.

;; Although a game built on RLX can define whatever categories are
;; needed, certain base categories are built-in and have a fixed
;; interpretation:

(defparameter *standard-categories*
  '(;; :. action-points > 
    :actor ;; This cell is active and may be controlled by either the
	   ;; user or the CPU. Only actor cells receive `:run'
	   ;; messages every turn. Other cells are purely "reactive".
	   ;; Actor cells participate in the Action Points system.
    :proxy ;; This cell is a proxy for another cell.
    :dead  ;; This cell is no longer receiving run messages.
    :player ;; Only one cell (your player avatar) has this category.
    :obstacle ;; Blocks :. movement >
    ;; :. lighting >
    :light-source ;; This object casts light. 
    :opaque ;; Blocks line-of-sight, casts shadows. 
    ;; :. containers >
    :container ;; This cell contains other cells, and has an <inventory> field
    :contained ;; This cell is contained in another cell (i.e. not in open space on the map)
    :item ;; A potential inventory item. 
    ;; :. equipment >
    :equipper ;; Uses equipment. 
    :equipped ;; This item is currently equipped
    :equipment ;; This item can be equipped. 
    ))   

(define-method in-category cell (category) 
  "Return non-nil if this cell is in the specified CATEGORY."
  (member category <categories>))

(define-method add-category cell (category)
  "Add this cell to the specified CATEGORY."
  (pushnew category <categories>))

(define-method delete-category cell (category)
  "Remove this cell from the specified CATEGORY."
  (setf <categories> (delete category <categories>)))

;;; Action Points

;; :. action-points >

;; The Action Points system is RLX's model of roguelike time; Time is
;; divided into discrete episodes called phases.  Each phase consists
;; of one or more actions, each of which lasts a certain number of
;; action points' worth of time. During an action, the cell may modify
;; its own fields, invoke methods on itself, or send queued messages
;; to other cells in the environment.

;; "Action points" (or "AP") control an actor cell's ability to take
;; actions during a phase. The AP score for a cell's phase starts at
;; [stat-value cell :speed]. The AP cost of an action is determined by
;; the corresponding method's use of `expend-action-points'; see below. 

;; An "action" is a method that may consume action points. Actor cells
;; have an <actions> field; this is a list of method keywords
;; identifying the actions the player can trigger.

;; First your turn comes up, and RLX waits for your input.  Once you
;; issue a command, some AP may be used up. When your AP is gone, the
;; computer's phase begins; the results are displayed, and if you're
;; still alive, the player phase begins again.

;; The queued messages' targets can be keywords like :world, :browser,
;; or :narrator instead of direct references to objects; the world
;; processes the messages before delivery and sends them to the right
;; place. (See also worlds.lisp)

;; Actions should follow these conventions:

;;    1. Do something meaningful when invoked without arguments, 
;;       whatever other arguments may be accepted.
;;    2. Queue appropriate narration messages. :. queueing >
;;    3. Have a short docstring (under 40 characters or so) that can be 
;;       displayed on a single line in a menu system. (See browser.lisp)

;; A cell does not have to be an Actor cell to participate in the
;; Action Points system. 

(define-method get-actions cell ()
  <actions>)

(define-method is-actor cell ()
  [in-category self :actor])

(define-method is-player cell ()
  [in-category self :player])

(defvar *action-points-over-p* nil 
  "When non-nil, ignore action points limit.")

;; The following functions calculate action points.

(define-method begin-phase cell ()
  "Give the cell its allotment of action points to begin a phase.
If the last action of the previous turn brought the AP score into the
negative, then you'll come up that much short."
  (incf <action-points> [stat-value self :speed]))

(define-method can-act cell (phase)
  "Determine whether the cell has enough action points to take some
action during PHASE."
  (and (< <phase-number> phase)
       (plusp <action-points>)))

(define-method expend-action-points cell (points)
  "Expend POINTS action points, possibly going into the negative."
  (decf <action-points> points))

(define-method expend-default-action-points cell ()
  [expend-action-points self [stat-value self :default-cost]])

(define-method end-phase cell ()
  (setf <phase-number> [get-phase-number *active-world*]))

;;; Cell movement

;; :. movement > 

(define-method move cell (direction)
  (multiple-value-bind (r c) 
      (step-in-direction <row> <column> direction)
    (if [obstacle-at-p *active-world* r c]
	[queue>>narrateln :narrator "There is an obstacle in the way."]
	(progn
	  [queue>>expend-action-points self [stat-value self :movement-cost]]
	  [queue>>move-cell :world self r c]))))

;; :. lighting > 
      
(define-method is-light-source cell ()
  [in-category self :light-source])

;;; Containers

;; :. containers >

;; An object's <inventory> field is a vector. Each position of the
;; vector holds either a cell object or nil. The number of available
;; slots is stored in the <max-items> field. When an item is added to
;; an inventory, the first open slot is used. 
;; TODO allow arbitrary placement

(define-method make-inventory cell ()
  "Create an empty <inventory> of length <max-items>."
  (setf <inventory> (make-array [get-max-items self]
				:initial-element nil
				:adjustable nil)))

(define-method make-equipment cell ()
  (setf <equipment> (mapcon #'(lambda (slot)
				(list slot nil))
			    <equipment-slots>)))

(define-method get-max-items cell ()
  (assert <max-items>)
  [stat-value self :max-items])

(define-method set-container cell (container)
  (setf <container> container))

(define-method is-container cell ()
  [in-category self :container])

(define-method is-item cell ()
  [in-category self :item])

(define-method first-open-slot cell ()
  (position nil <inventory>))

(define-method add-item cell (item)
  "Add the ITEM to the cell's <inventory>.
Return the new position if successful, nil otherwise."
  (let ((pos [first-open-slot self]))
    (when (and (numberp pos) [in-category item :item])
      (prog1 pos
	(setf (aref <inventory> pos) item)
	[queue>>add-category item :contained]
	[queue>>set-container item self]))))
      
(define-method remove-item cell (item)
  "Remove ITEM from the <inventory>.
Return ITEM if successful, nil otherwise."
  (let* ((pos (position item <inventory>)))
    (when pos
      (prog1 item
	(setf (aref <inventory> pos) nil)
	[queue>>delete-category item :contained]
	[queue>>set-container item nil]))))

(define-method item-at cell (pos)
  (aref <inventory> pos))

(define-method replace-item-at cell (item pos)
  (setf (aref <inventory> pos) item))

(define-method weight cell ()
  (let ((total 0)
	(inventory <inventory>)
	(cell nil))
    (if [is-container self]
	;; recursively weigh the contents.
	(progn
	  (dotimes (n (length inventory))
	    (setf cell (aref inventory n))
	    (when cell
	      (incf total [weight cell])))
	  total)
	;; base case; just return the weight
	(or <weight> 0))))

;;; Finding and manipulating objects

;; :. finding > 

;; TODO split into find-direction, find-z, find & key :dir :z
(define-method find cell (&key (direction :here) (index :top))
  (let ((world *active-world*))
    (multiple-value-bind (nrow ncol)
	(step-in-direction <row> <column> direction)
      (if [in-bounds-p world nrow ncol]
	  (let* ((cells [cells-at world nrow ncol])
		 (index2 (cond ((and (eq :top index) (eq :here direction))
				;; skip yourself and instead get the item you're standing on
				(- (fill-pointer cells) 2))
			       ((eq :top index)
				(- (fill-pointer cells) 1))
			       ((numberp index) 
				(when (array-in-bounds-p cells index)
				  index))))
		 (cell (aref cells index2)))
	    (values cell nrow ncol index2))))))

(define-method clear-location cell ()
  (setf <row> nil <column> nil))

(define-method delete-from-world cell ()
  [queue>>delete-cell *active-world* self <row> <column>]
  [queue>>clear-location self])
      
(define-method take cell (&key (direction :here) (index :top))
  (multiple-value-bind (cell row column)
      [find self :direction direction :index index]
    (when (and [in-category cell :item]
	       [first-open-slot self])
      [queue>>expend-default-action-points self]
      [queue>>add-item self cell]
      [queue>>delete-from-world cell])))

(define-method resolve cell (reference)
  "Accept a REFERENCE to a cell, and try to get the real cell.
The REFERENCE may be an object, one of the `*compass-directions*', an
equipment slot keyword, or an integer denoting the nth inventory
slot."
  (etypecase reference
    (keyword (if (member reference *compass-directions*)
		 [find self :direction reference]
		 [equipment-slot self reference]))
    (integer [item-at self reference])
    (rlx:object reference)))

;;; Knowledge of objects

;; :. knowledge >
;; TODO port and document this

(define-method name-of cell ()
  (let ((name <name>))
    (if (stringp name)
	name
	(progn
	  (setf name (symbol-name (object-parent self)))
	  (subseq name (1+ (search "=" name))
		  (search "=" name :from-end t))))))

;;; Equipment

;; :. equipment >

(define-method is-equipment cell ()
  [in-category self :equipment])

(define-method equipment-slot cell (slot)
  (assert (member slot <equipment-slots>))
  (getf <equipment> slot))

(define-method equipment-match cell (item)
  (when [is-equipment item]
    (intersection <equipment-slots> 
		  (field-value :equip-for item))))

(define-method add-equipment cell (item &optional slot)
  (let ((match [equipment-match self item]))
    (setf (getf <equipment> 
		(or slot (first match)))
	  item)))
  		
(define-method delete-equipment cell (slot)
  (setf (getf <equipment> slot) nil))

(define-method equip cell (&optional reference slot)
  (let* ((item [resolve self reference])
	 (match [equipment-match self item])
	 (valid [is-equipment item])
	 (slot2 (or slot (when match 
			   (first match))))
	 (open (when slot2
		 (null [equipment-slot self slot2]))))
    (if (and valid match open)
	(progn 
	  [queue>>expend-default-action-points self]
	  [queue>>add-equipment self item]
	  [queue>>add-category item :equipped]
	  ;; remove from inventory
	  [queue>>remove-item self item]
	  ;; notify user of success
	  [queue>>narrate :narrator "You equip "]
	  [queue>>print-object-tag :narrator item]
	  [queue>>newline :narrator])
	(progn
	  ;; explain failure
	  [queue>>narrate :narrator "You cannot equip "]
	  [queue>>print-object-tag :narrator item]
	  [queue>>newline :narrator]
	  (cond
	    ((not valid) 
	     [queue>>narrateln :narrator "This item is not a piece of equipment."])
	    ((and match (not open))
	     [queue>>narrateln :narrator "You must un-equip the ~A first." slot2])
	    ((not match)
	     [queue>>narrateln :narrator "This can only be equipped in one of: ~A"
			       (field-value :equip-for item)]))))))

(define-method dequip cell (slot)
  ;; TODO document
  ;; TODO narration
  (let ((item (getf <equipment> slot)))
    (when item
      [queue>>expend-default-action-points]
      [queue>>delete-equipment self slot]
      [queue>>add-item self item])))

;;; Loadout

;; Automatic inventory and equipment loadout for new cells.
;; See how this is used in worlds.lisp.

(define-method loadout cell ()
  nil) 

;;; Combat

(define-method attack cell (target)
  (let ((weapon [equipment-slot self <attacking-with>]))
    (if (null weapon)
	[queue>>narrateln :narrator "Cannot attack without a weapon in ~A." 
			  <attacking-with>]
      (let* ((attack-cost [stat-value weapon :attack-cost])
	     (accuracy [stat-value weapon :accuracy])
	     (dexterity [stat-value self :dexterity])
	     (to-hit (< (random 100)
			(+ accuracy (/ dexterity 2)))))
	(if to-hit
	    ;; calculate and send damage
	    (let ((damage [stat-value weapon :attack-power]))
	      [queue>>expend-action-points self attack-cost]
	      [queue>>damage [resolve self target] damage])
	    (progn 
	      [queue>>expend-default-action-points self]
	      (when [is-player self]
		[queue>>narrateln :narrator "You missed."])))))))
      
(define-method damage cell (damage-points)
  (if (has-field :hit-points self)
      (progn 
	[stat-effect self :hit-points (- damage-points)]
	(when (zerop [stat-value self :hit-points])
	  [queue>>die self]))
      (when [is-player self]
	[queue>>narrateln :narrator "Nothing happens."])))
	
(define-method die cell ()
  [add-category self :dead]
  [queue>>delete-from-world self])

;;; The asterisk cell is a wildcard

(define-prototype asterisk (:parent =cell=)
  (tile :initform ".asterisk")
  (name :initform "Command"))

(define-prototype gray-asterisk (:parent =cell=)
  (tile :initform ".gray-asterisk")
  (name :initform "System"))

;;; cells.lisp ends here