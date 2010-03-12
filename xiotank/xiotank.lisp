;;; xiotank.lisp --- 7DRL

;; Copyright (C) 2010  David O'Toole

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

;; 

;;; Code:

(defpackage :xiotank
  (:documentation "xiotank is a sonic roguelike combat game.")
  (:use :xe2 :common-lisp)
  (:export xiotank))

(in-package :xiotank)

(setf xe2:*dt* 30)

;;; Text labels

(defcell label 
  (categories :initform '(:drawn :actor :label))
  text stroke-color background-color timeout)

(define-method initialize label (&key text (stroke-color ".white") (background-color ".gray30")
					(style :label) (timeout nil))
  (setf <text> text)
  (setf <stroke-color> stroke-color)
  (setf <background-color> background-color)
  (setf <style> style)
  (setf <timeout> (if (floatp timeout)
		      ;; specify in (roughly) seconds if floating
		      (truncate (* 15 timeout))
		      ;; leave as frames if integer
		      timeout)))
  
(define-method draw label (x y image)
  (clon:with-field-values (text style) self
    (let* ((offset (ecase style
		     (:label 16)
		     (:flat 0)))
	   (x0 x)
	   (y0 y)
	   (x1 (+ x0 offset))
	   (y1 y0)
	   (margin 4)
	   (height (+ (* 2 margin) (apply #'+ (mapcar #'formatted-line-height text))))
	   (width (+ (* 2 margin) (apply #'max (mapcar #'formatted-line-width text)))))
      (draw-box x1 y1 width height 
		:stroke-color <stroke-color>
		:color <background-color>
		:destination image)
      ;; (when (eq style :label)
      ;; 	(draw-line x0 y0 x1 y1 :destination image))
      (let ((x2 (+ margin x1))
	    (y2 (+ margin y1)))
	(dolist (line text)
	  (render-formatted-line line x2 y2 :destination image)
	  (incf y2 (formatted-line-height line)))))))

(define-method run label ()
  [expend-default-action-points self]
  (when (integerp <timeout>)
    (when (minusp (decf <timeout>))
      [die self])))

;;; Sound waves

(defparameter *waveforms* '(:sine :square :saw :bass))
(defparameter *wave-colors* '(:yellow :cyan :magenta :green))

(defparameter *wave-samples*
  '((:sine "A-2-sine" "A-4-sine")
    (:saw "A-2-saw" "A-4-saw")
    (:square "A-2-square" "A-4-square")))

(defun wave-sample (type &optional (note "A-4"))
  (assert (member type *waveforms*))
  (concatenate 'string note "-" (string-downcase (symbol-name type))))

(defparameter *wave-images*
  '((:sine :green "sine-green" :yellow "sine-yellow" :magenta "sine-magenta" :cyan "sine-cyan")
    (:square :green "square-green" :yellow "square-yellow" :magenta "square-magenta" :cyan "square-cyan")
    (:saw :green "saw-green" :yellow "saw-yellow" :magenta "saw-magenta" :cyan "saw-cyan")))

(defun wave-image (type &optional (color :green))
  (assert (and (member type *waveforms*)
	       (member color *wave-colors*)))
  (getf (cdr (assoc type *wave-images*))
	color))

(defparameter *pulse-delay* 8)

(defsprite wave
  (description :initform "A sonic wave.")
  (team :initform :player)
  (color :initform :green)
  (waveform :initform :sine)
  (note :initform "A-4")
  (clock :initform 60)
  (pulse :initform (random *pulse-delay*))
  (image :initform nil)
  (direction :initform nil)
  (speed :initform (make-stat :base 20))
  (movement-distance :initform (make-stat :base 2))
  (movement-cost :initform (make-stat :base 20))
  (categories :initform '(:wave :actor)))

(define-method start wave (&key (note "A-4") (waveform :sine) (direction :north) (team :player) (color :green))
  (setf <waveform> waveform)
  (setf <team> team)
  (setf <note> note)
  [update-image self (wave-image waveform color)]
  (setf <sample> (wave-sample waveform note))
  (setf <direction> direction))

(define-method run wave ()
  (decf <clock>)
  (if (minusp <clock>)
      [die self]
      (progn [expend-action-points self 2]
	     (when <direction> 
	       (multiple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction>
								 [stat-value self :movement-distance])
		 [update-position self x y])
	       ;; decide whether to beep.
	       (if (zerop <pulse>)
		   (progn (setf <pulse> *pulse-delay*)
			  [play-sample self <sample>])
		   (decf <pulse>))))))

(define-method refresh wave ()
  (setf <clock> 60))

(define-method do-collision wave (object)
  (when (and (not [in-category object :wave])
	     [in-category object :target]
	     (has-field :team object)
	     (not (eq <team> (field-value :team object))))
    [hit object self]
    [die self]))

;;; Pulsators 

(defparameter *default-pulsator-delay* 20)

(defparameter *pulsing* nil)

(defcell pulsator 
  (name :initform "Pulsator")
  (description :initform "Pulsates to a given beat, activating nearby objects.")
  (tile :initform "pulsator")
  (delay :initform *default-pulsator-delay*)
  (clock :initform 0)
  (trip :initform nil)
  (team :initform :neutral)
  (state :initform nil)	 
  (categories :initform '(:obstacle :target :actor)))

(define-method update-tile pulsator (&optional pulsing)
  (setf <tile> (if pulsing "pulsator-pulse"
		   (if <state> "pulsator-on" "pulsator"))))

(define-method tap pulsator (delay)
  (setf <delay> delay))

(define-method start pulsator (&optional delay)
  (unless <state>
    (setf <clock> 0)
    (when delay (setf <delay> delay))
    (setf <state> t)
    (setf <trip> nil)
    (setf *pulsing* t)
    [update-tile self]))

(define-method stop pulsator ()
  (unless (null <state>)
    (setf <state> nil)
    (setf *pulsing* nil)
    [update-tile self]
    (setf <clock> 0)))

(define-method run pulsator ()
  [update-tile self]
  (when <state>
    (if (zerop <clock>)
	(progn [play-sample self "pulse"]
	       [update-tile self t]
	       (setf *pulsing* t)
	       (setf <trip> nil)
	       (labels ((do-circle (image)
			  (prog1 t
			    (multiple-value-bind (x y) 
				[viewport-coordinates self]
			      (let ((x0 (+ x 8))
				    (y0 (+ y 8)))
				(draw-circle x0 y0 40 :destination image)
				(draw-circle x0 y0 35 :destination image))))))
		 [>>add-overlay :viewport #'do-circle])
	       (setf <clock> <delay>))
	(progn (if <trip>
		   (setf *pulsing* nil)
		   (progn (setf <trip> t)
			  (setf *pulsing* t)))
	       (decf <clock>)))))
  
(define-method hit pulsator (&optional object)
  (if <state> [stop self] [start self]))

;;; Phonic particles

(defcell particle 
  (tile :initform "particle")
  (direction :initform (car (one-of '(:north :south :east :west))))
  (categories :initform '(:actor))
  (clock :initform (random 20)))

(define-method run particle ()
  (decf <clock>)
  (setf <tile> (car (one-of '("particle" "particle2" "particle3"))))
  ;;[play-sample self "particle-sound-1"]
  (if (minusp <clock>) [die self]
      [move self <direction>]))

;;; Phi particles

(defcell phi
  (tile :initform "phi")
  (direction :initform (car (one-of '(:north :south :east :west))))
  (categories :initform '(:actor))
  (clock :initform (random 20)))

(define-method run phi ()
  (decf <clock>)
  (setf <tile> (car (one-of '("phi" "phi2" "phi3"))))
  ;;[play-sample self "particle-sound-1"]
  (if (minusp <clock>) 
      [die self]
      (progn (percent-of-time 3 [play-sample self (car (one-of '("dtmf1" "dtmf2" "dtmf3")))])
	     [move self <direction>])))

;;; Wave delays that respond to pulses

(defcell delay 
  (tile :initform "delay")
  (wave :initform nil)
  (coords :initform nil)
  (team :initform :neutral)
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :target)))

(define-method hit delay (&optional object)
  (when [in-category object :wave]
    (setf <wave> object)
    (setf <coords> (multiple-value-bind (x y)
		       [xy-coordinates object]
		     (list x y)))
    [remove-sprite *world* object]))

(define-method run delay ()
  [expend-action-points self 10]
  (setf <tile> (if <wave> "delay-on" "delay"))
  (when (and *pulsing* <wave>)
    [add-sprite *world* <wave>]
    (destructuring-bind (x y) <coords>
      [update-position <wave> x y]
      [move <wave> (field-value :direction <wave>) 17]
      [refresh <wave>])
    (setf <wave> nil)))

;;; Turrets

(defcell turret
  (tile :initform "turret-east-on")
  (direction :initform :east)
  (team :initform :neutral) wave
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :target)))

(define-method run turret ()
  [expend-action-points self 10]
  [update-tile self]
  (when *pulsing*
    (setf <wave> (clone =wave=))
    [start <wave> :direction <direction>
	   :team :neutral
	   :waveform :saw]
    [add-sprite *world* <wave>]
    (multiple-value-bind (x y) [xy-coordinates self]
      [update-position <wave> (+ x 4) (+ y 4)]
      [refresh <wave>])))

(define-method update-tile turret ()
  (setf <tile>
	(if *pulsing*
	    (ecase <direction>
	      (:east "turret-east-on")
	      (:west "turret-west-on")
	      (:south "turret-south-on")
	      (:north "turret-north-on"))
	    (ecase <direction>
	      (:east "turret-east")
	      (:west "turret-west")
	      (:south "turret-south")
	      (:north "turret-north")))))
    
(define-method hit turret (&optional object)
  (setf <direction> 
	(ecase <direction>
	  (:east :south)
	  (:south :west)
	  (:west :north)
	  (:north :east)))
  [update-tile self])

;;; Fences

(defcell wire 
  (tile :initform "wire-east")
  (clock :initform 5)
  (speed :initform (make-stat :base 10))
  (direction :initform :east)
  (categories :initform '(:actor :obstacle :target)))

(define-method orient wire (dir &optional (clock 5))
  (setf <direction> dir)
  (setf <clock> clock))

(define-method run wire ()
  (if (zerop <clock>) 
      [die self]
      (progn [move self <direction>]
	     (decf <clock>)
	     ;; possibly kill something
	     (let ((thing [category-in-direction-p *world* <row> <column> <direction> :target]))
	       (when (clon:has-field :hit-points thing)
		 [damage thing 5]))
	     (setf <tile> (ecase <direction>
			    (:east "wire-east")
			    (:south "wire-south")
			    (:west "wire-west")
			    (:north "wire-north"))))))

;; (define-method step wire (stepper)
;;   (when [is-player stepper]
;;     [die stepper]))

(defcell fence
  (tile :initform "fence-east-on")
  (fence-length :initform 4)
  (fence-release :initform 30)
  (fence-release-clock :initform 0)
  (direction :initform :east)
  (sample :initform nil)
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 10))
  (categories :initform '(:actor :obstacle :target)))

(define-method run fence ()
  [expend-action-points self 10]
  [update-tile self]
  (when *pulsing*
    (let ((wire (clone =wire=)))
      [orient wire <direction> <fence-length>]
      [drop self wire])))

(define-method update-tile fence ()
  (setf <tile>
	(if *pulsing*
	    (ecase <direction>
	      (:east "fence-east-on")
	      (:west "fence-west-on")
	      (:south "fence-south-on")
	      (:north "fence-north-on"))
	    (ecase <direction>
	      (:east "fence-east")
	      (:west "fence-west")
	      (:south "fence-south")
	      (:north "fence-north")))))
    
(define-method hit fence (&optional object)
  nil)
  ;; (setf <direction> 
  ;; 	(ecase <direction>
  ;; 	  (:east :south)
  ;; 	  (:south :west)
  ;; 	  (:west :north)
  ;; 	  (:north :east)))
  ;; [update-tile self])


;;; Shield

(defcell shield
  (tile :initform "shield")
  (team :initform :neutral)
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 20))
  (hit-points :initform (make-stat :base 5 :min 0))
  (categories :initform '(:actor :target)))

(define-method hit shield (&optional wave)
  (when [in-category wave :wave]
    [play-sample self "ice"]
    [damage self 1]))

(define-method run shield () nil)

;;; Triggers just play a sample once per hit

(defcell trigger
  (tile :initform "trigger")
  (sample :initform nil)
  (team :initform :neutral)
  wave
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :target)))

(define-method run trigger ()
  [expend-action-points self 10])

(define-method intone trigger (sample)
  (setf <sample> sample))

(define-method hit trigger (&optional object)
  (when <sample> 
    [play-sample self <sample>]
    (dotimes (n 6)
      [drop self (clone =particle=)])))

;;; Oscillators and tone clusters

(defparameter *scale* '("C-2" "C#2" "D-2" "D#2" "E-2" 
			"F-2" "F#2" "G-2" "G#2" "A-2" "A#2"
			"B-2" "C-3"))

(defparameter *scale-white-keys* '("C-2" "D-2" "E-2" 
			"F-2"  "G-2"  "A-2"
			"B-2" "C-3"))

(defun random-note (&optional (scale *scale*))
  (car (one-of scale)))

(defun random-cluster (&optional (size 3) (scale *scale*))
  (let (tones (n 0))
    (loop while (< (length tones) size)
	  do (pushnew (random-note scale) tones :test 'equal))
    tones))

(defparameter *example-cluster* '("C-2" "E-2" "G-2"))

(defvar *notes* nil)

(defun add-note (note)
  (pushnew note *notes* :test 'equal))

(defun remove-note (note)
  (setf *notes* (delete note *notes* :test 'equal)))

(defun note-playing-p (note)
  (member note *notes* :test 'equal))

(defparameter *oscillator-tiles* '((:sine "osc-sine-off" "osc-sine-on")
				   (:square "osc-square-off" "osc-square-on")
				   (:saw "osc-saw-off" "osc-saw-on")))

(defun oscillator-tile (waveform &optional state)
  (assert (member waveform *waveforms*))
  (let ((tiles (cdr (assoc waveform *oscillator-tiles*))))
    (if (null state) (first tiles) (second tiles))))

(defcell oscillator 
  (categories :initform '(:actor :obstacle :target))
  channel
  (team :initform :neutral)
  (waveform :initform :sine)
  (note :initform "A-2")
  (state :initform nil))

(define-method intone oscillator (waveform &optional (note "A-2"))
  [stop self]
  (setf <waveform> waveform)
  (setf <note> note)
  [update-tile self])
  
(define-method update-tile oscillator ()
  (setf <tile> (oscillator-tile <waveform> <state>)))

(define-method start oscillator (waveform &optional (note "A-2"))
  (unless <channel>
    (add-note note)
    [intone self waveform note]
    (setf <state> t)
    [update-tile self]
    (let ((label (clone =label= :text (list (list (list <note>))))))
      [drop self label]))
    (setf <channel> (xe2:play-sample (wave-sample waveform note) :loop t)))

(define-method stop oscillator ()
  (unless (null <channel>)
    (remove-note <note>)
    (setf <state> nil)
    [update-tile self]
    (let ((label [category-at-p *world* <row> <column> :label]))
      (when label
	[die label]))
    (xe2:halt-sample <channel>)
    (setf <channel> nil)))

(define-method run oscillator () nil)

(define-method hit oscillator (&optional object)
  (if <state> [stop self] [start self <waveform> <note>]))
				
;;; Resonators

(defcell resonator 
  (tile :initform "resonator")
  (categories :initform '(:actor :obstacle :target))
  channel
  (team :initform :neutral)
  (waveform :initform :sine)
  (note :initform "A-2")
  (state :initform nil))

(define-method tune resonator (note)
  (setf <note> note))

(define-method run resonator ()
  (setf <tile> (if (note-playing-p <note>)
		   "resonator-on"
		   "resonator"))
  (when (and *pulsing* (note-playing-p <note>))
    (dotimes (n 5)
      [drop self (clone =phi=)])))


(define-method hit resonator (&optional object)
  nil)

;;; The sonic cannon

(defparameter *wave-cannon-reload-time* 20)

(defcell wave-cannon
  (tile :initform "gun")
  (reload-clock :initform 0)
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:center-bay))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 100))
  (attack-power :initform (make-stat :base 12))
  (attack-cost :initform (make-stat :base 10))
  (energy-cost :initform (make-stat :base 1)))

(define-method fire wave-cannon (direction)
  (if (plusp <reload-clock>)
      nil ;; (decf <reload-clock>)
      (progn 
	(setf <reload-clock> *wave-cannon-reload-time*)
	(if [expend-energy <equipper> [stat-value self :energy-cost]]
	    (let ((wave (clone =wave=)))
	      (multiple-value-bind (x y) [viewport-coordinates <equipper>]
		[drop-sprite <equipper> wave (+ x 4) (+ y 4)]
		[start wave :direction direction :team (field-value :team <equipper>)
		       :color (field-value :color <equipper>)
;;		       :note (car (one-of (list "A-4"  "A-2")))
		       :waveform (field-value :waveform <equipper>)]))
	    (when [is-player <equipper>]
	      [say <equipper> "Not enough energy to fire!"])))))

(define-method recharge wave-cannon ()
  (decf <reload-clock>))

;;; The tank!

(defcell tank 
  (tile :initform "tank-north")
  (dead :initform nil)
  (team :initform :player)
  (color :initform :green)
  (waveform :initform :sine)
  (hit-points :initform (make-stat :base 20 :min 0 :max 20))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 1000)
  (energy :initform (make-stat :base 40 :min 0 :max 40 :unit :gj))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (direction :initform :north)
  (attacking-with :initform nil)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :obstacle :player :target :container :light-source :vehicle :repairable))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension)))

(define-method loadout tank ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wave-cannon=)]])

(define-method hit tank (&optional object)
  [play-sample self "ouch"]
  [parent>>damage self 1])

(defparameter *tank-tiles* '(:north "tank-north"
			     :south "tank-south"
			     :east "tank-east"
			     :west "tank-west"
			     :northeast "tank-northeast"
			     :northwest "tank-northwest"
			     :southeast "tank-southeast"
			     :southwest "tank-southwest"))

(define-method move tank (direction)
  (unless <dead>
    (setf <direction> direction)
    (setf <tile> (getf *tank-tiles* direction))
    [parent>>move self direction]))

(define-method fire tank (direction)
  (unless <dead>
    [play-sample self "pop"]
    [parent>>fire self direction]))

(define-method shield tank ()
  (unless <dead>
    (if (>= [stat-value self :energy] 5) 
	(labels ((drop-shield (r c)
		   (prog1 nil
		     [drop-cell *world* (clone =shield=) r c :no-collisions t])))
	  (trace-rectangle #'drop-shield (- <row> 2) (- <column> 2) 5 5)
	  [play-sample self "saddown"]
	  ;;[stat-effect self :energy -10]
	  )
	[say self "Not enough energy to activate shield."])))

(define-method run tank ()
  (let ((cannon [equipment-slot self :center-bay]))
    (when cannon [recharge cannon])))
  
(define-method quit tank ()
  (xe2:quit :shutdown))

(define-method die tank ()
  (unless <dead>
    (setf <tile> "tank-disabled")
    [play-sample self "gameover"]
    [say self "YOU DIED."]
    (setf <dead> t)))

(define-method restart tank ()
  (let ((tank (clone =tank=)))
    [destroy *universe*]
    [set-player *universe* tank]
    [set-character *status* tank]
    [play *universe*
	  :address '(=blue-world=)]
    [loadout tank]))

;;; White noise

(defcell noise 
  (tile :initform (car (one-of '("white-noise" "white-noise2" "white-noise3" "white-noise4"))))
  (categories :initform '(:actor))
  (clock :initform (random 20)))

(define-method run noise ()
  (decf <clock>)
  [play-sample self "noise-white"]
  (if (minusp <clock>) [die self]
      [move self (random-direction)]))

;;; Basic enemy

(defcell shocker 
  (tile :initform "shocker")
  (team :initform :enemy)
  (color :initform :cyan)
  (waveform :initform :square)
  (hit-points :initform (make-stat :base 2 :min 0 :max 45))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 40 :min 0 :max 40 :unit :gj))
  (hit-points :initform (make-stat :base 45 :min 0 :max 45))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (direction :initform :north)
  (attacking-with :initform nil)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :obstacle :target :container :light-source :vehicle :repairable))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension)))

(define-method loadout shocker ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wave-cannon=)]])

(define-method hit shocker (&optional object)
  [die self])

(define-method run shocker ()
  (let ((cannon [equipment-slot self :center-bay]))
    (when cannon [recharge cannon]))
  (let ((dir [direction-to-player self])
	(dist [distance-to-player self]))
    (if (< dist 13)
	(if (> 9 dist)
	    (progn [fire self dir]
		   (xe2:percent-of-time 3 [move self dir]))
	    (if [obstacle-in-direction-p *world* <row> <column> dir]
		[move self (random-direction)]
		[move self dir]))
	(percent-of-time 3 [move self (random-direction)]))))

(define-method die shocker () 
  (dotimes (n 10)
    [drop self (clone =noise=)])
  [play-sample self "yelp"]
  [parent>>die self])  

;;; Basic blue world

(defparameter *xiotank-grammar* 
  '((puzzle >> (:generate-tone-cluster :goto-origin tone+ :goto-east 
		pulsator enemies powerups :goto-random-position :drop-extras player))
    (tone+ >> (tone :goto-east maybe-south tone :goto-east maybe-south tone))
    (tone >> :drop-tone-pair)
    (maybe-south >> :noop :goto-south)
    (enemies >> :drop-shockers)
    (powerups >> :drop-powerups)
    (player >> :drop-player)
    (pulsator >> :drop-pulsator)))

;; (setf xe2:*grammar* *xiotank-grammar*)
;; (xe2:generate 'puzzle)

;; (:DROP-TONE-PAIR :GOTO-EAST :DROP-TONE-PAIR :DROP-SHOCKERS :DROP-POWERUPS
;;  :GOTO-RANDOM-POSITION :DROP-PLAYER)

(defcell block 
  (tile :initform "block")
  (categories :initform '(:obstacle :opaque)))

(defcell blue-space 
  (tile :initform "blue-space"))

(define-prototype blue-world (:parent xe2:=world=)
  gen-row gen-column cluster note
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (spacing :initform 6)
  (scale :initform '(3 m))
  (edge-condition :initform :block))

(defparameter *bass-notes* '("C-2-bass" "C#2-bass" "D-2-bass" "D#2-bass" "E-2-bass" 
	      "F-2-bass" "F#2-bass" "G-2-bass" "G#2-bass" "A-2-bass" "A#2-bass"
	      "B-2-bass" "C-3-bass"))

(define-method generate blue-world (&key (height 27)
					    (width 50)
					    sequence-number)
  (setf *notes* nil)
  (setf <height> height <width> width)
  [create-default-grid self]
  (setf <gen-row> 0 <gen-column 0)
  (dotimes (i height)
    (dotimes (j width)
      [drop-cell self (clone =blue-space=)
		 i j]))
  (setf xe2:*grammar* *xiotank-grammar*)
  (let ((puzzle (generate 'puzzle)))
    (dolist (op puzzle)
      (when (and (keywordp op) (clon:has-method op self))
	(send nil op self)))
    (setf <description> (prin1-to-string puzzle)))
  [drop-cell self (clone =launchpad=) (- height 8) 5])

(define-method drop-shockers blue-world ()
  (dotimes (n 10)
    [drop-cell self (clone =shocker=) (random <height>) (random <width>) :loadout t]))

(define-method drop-pulsator blue-world ()
  (let ((pulse (clone =pulsator=)))
    [drop-cell self pulse <gen-row> <gen-column>]
    [tap pulse 20]))

(define-method drop-extras blue-world ()
  [drop-cell self (clone =turret=) <gen-row> <gen-column>]
  [goto-south self]
  [drop-cell self (clone =fence=) <gen-row> <gen-column>])
  

(define-method goto-origin blue-world ()
  (setf <gen-row> 0) (setf <gen-column> 0))

(define-method goto blue-world (r c)
  (setf <gen-column> (* <spacing> c))
  (setf <gen-row> (* <spacing> r)))

(define-method goto-east blue-world ()
  (incf <gen-column> <spacing>))

(define-method goto-south blue-world ()
  (incf <gen-row> <spacing>))

(define-method noop blue-world ()
  nil)

(define-method goto-random-position blue-world ()
  [goto self (random 5) (random 5)])

(define-method generate-tone-cluster blue-world ()
  (setf <cluster> (random-cluster 3 *scale*)))

(define-method drop-tone-pair blue-world ()
  (let ((note (pop <cluster>))
	(osc (clone =oscillator=))
	(res (clone =resonator=)))
    [drop-cell self osc (+ <gen-row> (random 5)) (+ <gen-column> (random 5))]
    [intone osc :sine note] 
    [drop-cell self res (+ <gen-row> (random 5)) (+ <gen-column> (random 5))]
    [tune res note]))

;;; Splash screen
  
(defvar *pager* nil)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (xe2:draw-resource-image "splash" 0 0 
			   :destination <image>))

(defvar *space-bar-function*)

(define-method dismiss splash ()
  [select *pager* :game]
  (when (functionp *space-bar-function*)
    (funcall *space-bar-function*))
  (xe2:show-widgets))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Player status

(defvar *status* nil)

(define-prototype status (:parent xe2:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method print-stat status (stat-name &key warn-below show-max)
  (let* ((stat (field-value stat-name <character>))
	 (value [stat-value <character> stat-name]))
    (destructuring-bind (&key min max base delta unit) stat
      (let ((color (if (and (numberp warn-below)
			    (< value warn-below))
		       ".red"
		       ".gray40")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self ":["]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	(when show-max
	  [print self (format nil "/~S" max)
		 :foreground ".yellow"
		 :background color])
	(when unit 
	  [print self " "]
	  [print self (symbol-name unit)])
	[print self "]"]
	))))

(defparameter *status-bar-character* " ")

(define-method print-stat-bar status (stat &key 
					   (color ".yellow")
					   (background-color ".gray40")
					   (divisor 1))
  (let ((value (truncate (/ [stat-value <character> stat] divisor)))
	(max (truncate (/ [stat-value <character> stat :max] divisor))))
    (dotimes (i max)
      [print self *status-bar-character*
	     :foreground ".yellow"
	     :background (if (< i value)
			     color
			   background-color)])))

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>))
    (when char
	[print-stat self :hit-points :warn-below 10 :show-max t]
	[print-stat-bar self :hit-points :color ".red" :divisor 2]
	[space self]
	[print-stat self :energy :warn-below 10 :show-max t]
	[print-stat-bar self :energy :color ".yellow" :divisor 2]
	[newline self])))

;;; Custom bordered viewport

(define-prototype view (:parent xe2:=viewport=))

(define-method render view ()
  [parent>>render self]
  (xe2:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>))

(defvar *view* (clone =view=))

;;; Keyboard controls

(define-prototype xiotank-prompt (:parent xe2:=prompt=))

(defparameter *basic-keybindings* 
  '(("KP7" nil "move :northwest .")
    ("KP8" nil "move :north .")
    ("KP9" nil "move :northeast .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP1" nil "move :southwest .")
    ("KP2" nil "move :south .")
    ("KP3" nil "move :southeast .")
    ;;
    ("KP7" (:control) "fire :northwest .")
    ("KP8" (:control) "fire :north .")
    ("KP9" (:control) "fire :northeast .")
    ("KP4" (:control) "fire :west .")
    ("KP6" (:control) "fire :east .")
    ("KP1" (:control) "fire :southwest .")
    ("KP2" (:control) "fire :south .")
    ("KP3" (:control) "fire :southeast .")
    ;;
    ("KP7" (:alt) "fire :northwest .")
    ("KP8" (:alt) "fire :north .")
    ("KP9" (:alt) "fire :northeast .")
    ("KP4" (:alt) "fire :west .")
    ("KP6" (:alt) "fire :east .")
    ("KP1" (:alt) "fire :southwest .")
    ("KP2" (:alt) "fire :south .")
    ("KP3" (:alt) "fire :southeast .")
    ;;
    ("KP7" (:meta) "fire :northwest .")
    ("KP8" (:meta) "fire :north .")
    ("KP9" (:meta) "fire :northeast .")
    ("KP4" (:meta) "fire :west .")
    ("KP6" (:meta) "fire :east .")
    ("KP1" (:meta) "fire :southwest .")
    ("KP2" (:meta) "fire :south .")
    ("KP3" (:meta) "fire :southeast .")))
    ;; ;;
    ;; ("JOYSTICK" (:north :circle) "attack :north .")
    ;; ("JOYSTICK" (:northeast :circle) "attack :northeast .")
    ;; ("JOYSTICK" (:northwest :circle) "attack :northwest .")
    ;; ("JOYSTICK" (:east :circle) "attack :east .")
    ;; ("JOYSTICK" (:west :circle) "attack :west .")
    ;; ("JOYSTICK" (:south :circle) "attack :south .")
    ;; ("JOYSTICK" (:southwest :circle) "attack :southwest .")
    ;; ("JOYSTICK" (:southeast :circle) "attack :southeast .")
    ;; ;;
    ;; ("JOYSTICK" (:north :cross) "move :north .")
    ;; ("JOYSTICK" (:northeast :cross) "move :northeast .")
    ;; ("JOYSTICK" (:northwest :cross) "move :northwest .")
    ;; ("JOYSTICK" (:east :cross) "move :east .")
    ;; ("JOYSTICK" (:west :cross) "move :west .")
    ;; ("JOYSTICK" (:south :cross) "move :south .")
    ;; ("JOYSTICK" (:southwest :cross) "move :southwest .")
    ;; ("JOYSTICK" (:southeast :cross) "move :southeast .")
    ;; ;;
    ;; ("JOYSTICK" (:north :square) "fire :north .")
    ;; ("JOYSTICK" (:northeast :square) "fire :northeast .")
    ;; ("JOYSTICK" (:northwest :square) "fire :northwest .")
    ;; ("JOYSTICK" (:east :square) "fire :east .")
    ;; ("JOYSTICK" (:west :square) "fire :west .")
    ;; ("JOYSTICK" (:south :square) "fire :south .")
    ;; ("JOYSTICK" (:southwest :square) "fire :southwest .")
    ;; ("JOYSTICK" (:southeast :square) "fire :southeast .")))

(defparameter *qwerty-keybindings*
  (append *basic-keybindings*
	  '(("Y" nil "move :northwest .")
	    ("K" nil "move :north .")
	    ("U" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("B" nil "move :southwest .")
	    ("J" nil "move :south .")
	    ("N" nil "move :southeast .")
	    ;;
	    ("Y" (:alt) "fire :northwest .")
	    ("K" (:alt) "fire :north .")
	    ("U" (:alt) "fire :northeast .")
	    ("H" (:alt) "fire :west .")
	    ("L" (:alt) "fire :east .")
	    ("B" (:alt) "fire :southwest .")
	    ("J" (:alt) "fire :south .")
	    ("N" (:alt) "fire :southeast .")
	    ;;
	    ("Y" (:meta) "fire :northwest .")
	    ("K" (:meta) "fire :north .")
	    ("U" (:meta) "fire :northeast .")
	    ("H" (:meta) "fire :west .")
	    ("L" (:meta) "fire :east .")
	    ("B" (:meta) "fire :southwest .")
	    ("J" (:meta) "fire :south .")
	    ("N" (:meta) "fire :southeast .")
	    ;;
	    ("Y" (:control) "fire :northwest .")
	    ("K" (:control) "fire :north .")
	    ("U" (:control) "fire :northeast .")
	    ("H" (:control) "fire :west .")
	    ("L" (:control) "fire :east .")
	    ("B" (:control) "fire :southwest .")
	    ("J" (:control) "fire :south .")
	    ("N" (:control) "fire :southeast .")
	    ;;
	    ("SPACE" nil "shield .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" nil "restart .")
	    ("Q" (:control) "quit ."))))
  
(defparameter *alternate-qwerty-keybindings*
  (append *basic-keybindings*
	  '(("Q" nil "move :northwest .")
	    ("W" nil "move :north .")
	    ("E" nil "move :northeast .")
	    ("A" nil "move :west .")
	    ("D" nil "move :east .")
	    ("Z" nil "move :southwest .")
	    ("X" nil "move :south .")
	    ("C" nil "move :southeast .")
	    ;;
	    ("Q" (:alt) "attack :northwest .")
	    ("W" (:alt) "attack :north .")
	    ("E" (:alt) "attack :northeast .")
	    ("A" (:alt) "attack :west .")
	    ("D" (:alt) "attack :east .")
	    ("Z" (:alt) "attack :southwest .")
	    ("X" (:alt) "attack :south .")
	    ("C" (:alt) "attack :southeast .")
	    ;;
	    ("Q" (:meta) "attack :northwest .")
	    ("W" (:meta) "attack :north .")
	    ("E" (:meta) "attack :northeast .")
	    ("A" (:meta) "attack :west .")
	    ("D" (:meta) "attack :east .")
	    ("Z" (:meta) "attack :southwest .")
	    ("X" (:meta) "attack :south .")
	    ("C" (:meta) "attack :southeast .")
	    ;;
	    ("Q" (:control) "fire :northwest .")
	    ("W" (:control) "fire :north .")
	    ("E" (:control) "fire :northeast .")
	    ("A" (:control) "fire :west .")
	    ("D" (:control) "fire :east .")
	    ("Z" (:control) "fire :southwest .")
	    ("X" (:control) "fire :south .")
	    ("C" (:control) "fire :southeast .")
	    ;;
	    ("ESCAPE" nil "restart .")
	    ("SPACE" nil "shield .")
	    ("P" (:control) "quit ."))))
  
;; g c r
;;  \|/
;; h-.-n
;;  /|\ 
;; m w v

(defparameter *dvorak-keybindings*
  (append *basic-keybindings*
	  '(("G" nil "move :northwest .")
	    ("C" nil "move :north .")
	    ("R" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("N" nil "move :east .")
	    ("M" nil "move :southwest .")
	    ("W" nil "move :south .")
	    ("V" nil "move :southeast .")
	    ;;
	    ("G" (:alt) "fire :northwest .")
	    ("C" (:alt) "fire :north .")
	    ("R" (:alt) "fire :northeast .")
	    ("H" (:alt) "fire :west .")
	    ("N" (:alt) "fire :east .")
	    ("M" (:alt) "fire :southwest .")
	    ("W" (:alt) "fire :south .")
	    ("V" (:alt) "fire :southeast .")
	    ;;
	    ("G" (:meta) "fire :northwest .")
	    ("C" (:meta) "fire :north .")
	    ("R" (:meta) "fire :northeast .")
	    ("H" (:meta) "fire :west .")
	    ("N" (:meta) "fire :east .")
	    ("M" (:meta) "fire :southwest .")
	    ("W" (:meta) "fire :south .")
	    ("V" (:meta) "fire :southeast .")
	    ;;
	    ("G" (:control) "fire :northwest .")
	    ("C" (:control) "fire :north .")
	    ("R" (:control) "fire :northeast .")
	    ("H" (:control) "fire :west .")
	    ("N" (:control) "fire :east .")
	    ("M" (:control) "fire :southwest .")
	    ("W" (:control) "fire :south .")
	    ("V" (:control) "fire :southeast .")
	    ;;
	    ("SPACE" nil "shield .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" nil "restart .")
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings xiotank-prompt ()
  (let ((keys (ecase xe2:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k))))
  ;; ;; we also want to respond to timer events. this is how. 
  ;; [define-key self nil '(:timer) (lambda ()
  ;; 				   [run-cpu-phase *world* :timer])])

;;; Main program. 

(defun generate-level-address (ignore)
  '(=blue-world=))

(defparameter *xiotank-window-width* 800)
(defparameter *xiotank-window-height* 600)

(defvar *viewport*)

(defun xiotank ()
  (xe2:message "Initializing Xiotank...")
  (setf xe2:*window-title* "Xiotank")
  (setf clon:*send-parent-depth* 2) 
  (xe2:set-screen-height *xiotank-window-height*)
  (xe2:set-screen-width *xiotank-window-width*)
  ;; go!
  (let* ((prompt (clone =xiotank-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =tank=))
	 (splash (clone =splash=))
	 (help (clone =formatter=))
	 (quickhelp (clone =formatter=))
	 (viewport (clone =viewport=))
	 (status (clone =status=))
	 (splash-prompt (clone =splash-prompt=))
	 (terminal (clone =narrator=))
	 (stack (clone =stack=)))
    ;;
    (setf *viewport* viewport)
    (setf *status* status)
    ;;
    [resize splash :height (- *xiotank-window-height* 20) :width *xiotank-window-width*]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize *status* :height 20 :width *xiotank-window-width*]
    [move *status* :x 0 :y 0]
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    (labels ((spacebar ()
	       ;;
	       ;; enable pseudo timing
	       ;; (xe2:enable-timer)
	       ;; (xe2:set-frame-rate 30)
	       ;; (xe2:set-timer-interval 1)
	       (xe2:halt-music 1000)
	       (setf xe2:*physics-function* #'(lambda (&rest ignore)
						(when *world* [run-cpu-phase *world* :timer])))

	       (xe2:enable-held-keys 1 3)
	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address (generate-level-address 1)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       ;;
	       [set-character *status* player]
	       ;;
	       [set-tile-size viewport 16]
	       [resize viewport :height 470 :width *xiotank-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *xiotank-window-height* 130) 16))
			   :width (truncate (/ *xiotank-window-width* 16))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    ;;
    ;; [resize help :height 540 :width 800] 
    ;; [move help :x 0 :y 0]
    ;; (let ((text	(find-resource-object "help-message")))
    ;;   (dolist (line text)
    ;; 	(dolist (string line)
    ;; 	  (funcall #'send nil :print-formatted-string help string))
    ;; 	[newline help]))
    ;; ;;
    ;; [resize quickhelp :height 85 :width 250] 
    ;; [move quickhelp :y (- *xiotank-window-height* 130) :x (- *xiotank-window-width* 250)]
    ;; (let ((text	(find-resource-object "quickhelp-message")))
    ;;   (dolist (line text)
    ;; 	(dolist (string line)
    ;; 	  (funcall #'send nil :print-formatted-string quickhelp string))
    ;; 	[newline quickhelp]))
    ;; ;;
    (play-music "purity" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *xiotank-window-width* :height (- *xiotank-window-height* 20)]
    [move stack :x 0 :y 0]
    [set-children stack (list viewport terminal status)]
    ;;
    [resize terminal :height 80 :width *xiotank-window-width*]
    [move terminal :x 0 :y (- *xiotank-window-height* 80)]
    [set-verbosity terminal 0]
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (xe2:install-widgets splash-prompt splash)
    [add-page *pager* :game prompt stack viewport terminal *status* quickhelp]
    [add-page *pager* :help help]))

(xiotank)


;;; xiotank.lisp ends here

