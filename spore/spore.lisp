;;; spore.lisp --- a clone of the c64 game spore
;;; based on xe2/example/example.lisp
;;; See also http://dto.github.com/notebook/developers-guide.html

;;; Packaging

(defpackage :spore
  (:documentation "This is spore")
  (:use :cl :clon :xe2)
  (:export :spore))

(in-package :spore)

(defparameter *room-mx* 40)
(defparameter *room-my* 24)
(defvar *room* nil) ; FIX: how do we communicate between prototypes?
(defvar *player* nil)
(defvar *narrator* nil)
(defvar *clock* 0)
(defvar *clocktimer* (make-array 16 :initial-element 0))

(defmacro check-timer ((pos elapse) &body body)
  (let ((pos (if (numberp pos) `(aref *clocktimer* ,pos) pos)))
    `(let ((diff (- *clock* ,pos)))
       (when (> diff ,elapse)
         (setf ,pos *clock*)
         ,@body))))

(defcell slime-0
  (tile :initform "slime-0"))

(defcell floor 
  (tile :initform "floor-0"))

(defcell spore
  (tile :initform (car (one-of '("spore-0" "spore-1" "spore-2"))))
  (speed :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :spore)))

(define-method run spore ()
  ; animate the spore by changing tile
  (let ((tile (field-value :tile self)))
    (cond
      ((string= tile "spore-0") (setf (field-value :tile self) "spore-1"))
      ((string= tile "spore-1") (setf (field-value :tile self) "spore-2"))
      ((string= tile "spore-2") (setf (field-value :tile self) "spore-0"))))
  ; maybe-move
  (let ((x (field-value :column self))
        (y (field-value :row self))
        (px (field-value :column *player*))
        (py (field-value :row    *player*)))
    (let ((dir (if (field-value :repel *player*)
                 (case (random 2)
                   (0 (cond ((< px x) :east)
                             ((> px x) :west)
                             ((< py y) :south)
                             ((> py y) :north)
                             (t (random-direction))))
                   (1 (cond ((< py y) :south)
                             ((> py y) :north)
                             ((< px x) :east)
                             ((> px x) :west)
                             (t (random-direction)))))
                 (case (random 2)
                   (0 (cond ((> px x) :east)
                             ((< px x) :west)
                             ((> py y) :south)
                             ((< py y) :north)
                             (t (random-direction))))
                   (1 (cond ((> py y) :south)
                             ((< py y) :north)
                             ((> px x) :east)
                             ((< px x) :west)
                             (t (random-direction))))))))
      [move self dir]
      (let ((nx x) (ny y))
        (case dir
          (:east  (incf nx))
          (:west  (decf nx))
          (:south (incf ny))
          (:north (decf ny)))
        ; check if spore is adjacent to a edible wall
        (when (and (> nx -1) (< nx *room-mx*)
                   (> ny -1) (< ny *room-my*))
          (let ((objs (aref (field-value :grid *world*) ny nx)))
            (loop for obj across objs do
              (when (find :edible (field-value :categories obj))
                (let ((tile (field-value :tile obj)) new)
                  (cond
                    ((string= "wall-edible" tile) (setf new "img/wall-edible-1"))
                    ((string= "img/wall-edible-0" tile) (setf new "img/wall-edible-1"))
                    ((string= "img/wall-edible-1" tile) (setf new "img/wall-edible-2"))
                    ((string= "img/wall-edible-2" tile) (setf new "img/wall-edible-3"))
                    ((string= "img/wall-edible-3" tile) (setf new "img/wall-edible-4"))
                    ((string= "img/wall-edible-4" tile) (setf new "img/wall-edible-5"))
                    ((string= "img/wall-edible-5" tile) (setf new "img/wall-edible-6"))
                    ((string= "img/wall-edible-6" tile) (setf new "img/wall-edible-7"))
                    ((string= "img/wall-edible-7" tile)
                      (format t "wall has died~%")
                      [die obj]))
                  (if new (setf (field-value :tile obj) new)))))))))))

(define-method hit spore ()
  (format t "spore-hit!~%")
  [die self])

(macrolet
  ((wall-pair (name file &rest args)
     (let ((cat (append (getf args :categories) '(:obstacle :opaque :exclusive :wall))))
       `(progn
          (defcell ,name
            (tile :initform ,file)
            (categories :initform ',cat)
            (hit-points :initform (make-stat :base 1 :min 0)))
          (define-method hit ,name ())))))
  (wall-pair wall-0 "wall-horiz-0")
  (wall-pair wallbox-0 "wallbox-0")
  (wall-pair wallbox-1 "wallbox-1")
  (wall-pair wallbox-2 "wallbox-2")
  (wall-pair wallbox-3 "wallbox-3")
  (wall-pair grid-0    "grid-0")
  (wall-pair img/wall-edible-0 "img/wall-edible-0" :categories (:edible))
  (wall-pair wall-edible "img/wall-edible-0" :categories (:edible))
  (wall-pair mirror-0  "mirror-0")
  (wall-pair mirror-1  "mirror-1")
  (wall-pair wall-horiz-0 "wall-horiz-0")
  (wall-pair wall-vert-0 "wall-vert-0")
  (wall-pair box-0 "box-0")
  (wall-pair beam-horiz-0 "beam-horiz-0")
  (wall-pair beam-vert-0 "beam-vert-0"))

(defun shutdown-beam (x y dir)
  (flet ((shut (x y)
           (when (and (> x -1) (< x *room-mx*) (> y -1) (< y *room-my*))
             (let ((objs (aref (field-value :grid *world*) y x)))
               (loop for obj across objs do
                 (when (search "beam-horiz" (field-value :tile obj))
                   [die obj]
                   (shutdown-beam x y :horiz)
                   (return))
                 (when (search "beam-vert" (field-value :tile obj))
                   [die obj]
                   (shutdown-beam x y :vert)
                   (return)))))))
    (cond
      ((eq dir :horiz)
        (shut (1+ x) y)
        (shut (1- x) y))
      ((eq dir :vert)
        (shut x (1+ y))
        (shut x (1- y)))
      (t
        (shut (1+ x) y)
        (shut (1- x) y)
        (shut x (1+ y))
        (shut x (1- y))))))

(define-method hit box-0 ()
  (format t "box-0-hit~%")
  (let* ((x (field-value :column self))
         (y (field-value :row self)))
    ; search for adjacent beams
    (shutdown-beam x y nil)))

(define-method hit wall-edible ()
  (let ((tile (field-value :tile self)) new)
    (cond
      ((string= "img/wall-edible-0" tile) (setf new "img/wall-edible-1"))
      ((string= "img/wall-edible-1" tile) (setf new "img/wall-edible-2"))
      ((string= "img/wall-edible-2" tile) (setf new "img/wall-edible-3"))
      ((string= "img/wall-edible-3" tile) (setf new "img/wall-edible-4"))
      ((string= "img/wall-edible-4" tile) (setf new "img/wall-edible-5"))
      ((string= "img/wall-edible-5" tile) (setf new "img/wall-edible-6"))
      ((string= "img/wall-edible-6" tile) (setf new "img/wall-edible-7"))
      ((string= "img/wall-edible-7" tile)
        (format t "wall has died~%")
        [die self]))
    (if new (setf (field-value :tile self) new))))

(define-method hit img/wall-edible-0 ()
  (let ((tile (field-value :tile self)) new)
    (cond
      ((string= "img/wall-edible-0" tile) (setf new "img/wall-edible-1"))
      ((string= "img/wall-edible-1" tile) (setf new "img/wall-edible-2"))
      ((string= "img/wall-edible-2" tile) (setf new "img/wall-edible-3"))
      ((string= "img/wall-edible-3" tile) (setf new "img/wall-edible-4"))
      ((string= "img/wall-edible-4" tile) (setf new "img/wall-edible-5"))
      ((string= "img/wall-edible-5" tile) (setf new "img/wall-edible-6"))
      ((string= "img/wall-edible-6" tile) (setf new "img/wall-edible-7"))
      ((string= "img/wall-edible-7" tile)
        (format t "wall has died~%")
        [die self]))
    (if new (setf (field-value :tile self) new))))

(macrolet
  ((define-box ((name file &rest args) (&body hit-body) (&body run-body))
     (let ((cat (getf args :categories)))
       `(progn
          (defcell ,name
            (tile :initform ,file)
            (categories :initform ',(append cat '(:actor :box)))
            (direction :initform (random-direction))
            (hit-points :initform (make-stat :base 5 :min 0))
            (tile-timer :initform 0))
          (define-method hit ,name ()
            ,@hit-body)
          (define-method run ,name ()
            ,@run-body)))))
  (define-box (generator-0 "generator-0" :categories (:obstacle))
    ((format t "hit-spore-generator!~%")
     [die self])
    ((when (= (random 256) 0)
       (let* ((x (field-value :column self))
              (y (field-value :row self))
              nx ny)
         (case (random 4)
           (0 (setf nx (1+ x)) (setf ny y))
           (1 (setf ny (1+ y)) (setf nx x))
           (2 (setf nx (1- x)) (setf ny y))
           (3 (setf ny (1- y)) (setf nx x)))
         (if (< nx 0) (setf nx 0))
         (if (< ny 0) (setf ny 0))
         (if (>= nx *room-mx*) (setf nx (1- *room-mx*)))
         (if (>= ny *room-my*) (setf ny (1- *room-my*)))
         (when (not [category-at-p *world* ny nx '(:obstacle)])
           [drop-cell *room* (clone =spore=) nx ny :exclusive t :probe t])))))
  (define-box (repel-0 "repel-0")
    ([die self])
    ((check-timer ((field-value :tile-timer self) 100)
       (let ((tile (field-value :tile self)))
         (cond
           ((string= tile "repel-0") (setf (field-value :tile self) "repel-1"))
           ((string= tile "repel-1") (setf (field-value :tile self) "repel-2"))
           ((string= tile "repel-2") (setf (field-value :tile self) "repel-0")))))))
  (define-box (barrel-0 "barrel-0")
    ([die self])
    ())
  (define-box (energy-0 "energy-0")
    ([die self])
    ()))

(define-method step repel-0 (stepper)
  (when [is-player stepper]
    (setf (field-value :repel stepper) 600)
    [die self]))

(define-method step barrel-0 (stepper)
  (when [is-player stepper]
    [die self]))

(define-method step energy-0 (stepper)
  (when [is-player stepper]
    [die self]))

;;; shots
(defcell ball
  (tile :initform "fire-0")
  (categories :initform '(:actor))
  (direction :initform (random-direction))
  (hit-points :initform (make-stat :base 5 :min 0)))

(defvar *ball-active* nil)
(define-method run ball ()
  (when (eq <direction> :here) (setf <direction> (random-direction)))
  (with-fields (direction row column) self
    (multiple-value-bind (r c) (step-in-direction row column direction)
      (if [obstacle-at-p *world* r c]
        (progn
          ;; is it a wall or character? then hit it
          (let ((object [category-at-p *world* r c '(:wall :spore :box)]))
            (when object
              [hit object]
              [play-sample self "snd/fire-hit-0"]
              [damage self 1]))
          (setf *ball-active* nil)
          [die self])
        ;; move along
        [move self direction]))))

(define-method serve ball (direction)
  (setf <direction> direction))

;;; The player

(defcell player
  (tile :initform "player-0")
  (name :initform "Player")
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (energy :initform 400)
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (repel :initform nil)
  (categories :initform '(:actor :player :obstacle)))

(define-method quit player ()
  (quit :shutdown))

(defvar *player-sample* nil)
(define-method run player ()
  (format t "rp0~%")
  (setf *clock* (get-internal-real-time))
  (let ((tile (field-value :tile self))
        (repel (field-value :repel self))
        (x (field-value :column self))
        (y (field-value :row    self)))
  (format t "rp1 ~a ~a~%" x y)
    (check-timer (0 100)
      (setf (field-value :tile self)
            (cond
              ((string= tile "player-0") "player-1")
              ((string= tile "player-1") "player-2")
              ((string= tile "player-2") "player-0"))))
    (when repel
      (decf repel)
      (if (minusp repel) (setf repel nil))
      (setf (field-value :repel self) repel))
    (when (or [category-at-p *world* (1- y) x '(:spore)]
            [category-at-p *world* y (1- x) '(:spore)]
            [category-at-p *world* (1+ y) x '(:spore)]
            [category-at-p *world* y (1+ x) '(:spore)])
      (decf (field-value :energy self))
      (let ((str (format nil "Energy: ~a" (field-value :energy self))))
        [narrateln *narrator* str])
      (if *player-sample*
        (let ((diff (- *clock* *player-sample*)))
          (if (> diff 500) (setf *player-sample* nil))))
      (when (not *player-sample*)
        (ecase (random 3)
          (0 [play-sample self "snd/spore-do-damage-0"])
          (1 [play-sample self "snd/spore-do-damage-1"])
          (2 [play-sample self "snd/spore-do-damage-2"]))
        (setf *player-sample* *clock*))
      [damage self 1])))

(define-method serve-ball player (direction)
  (when (not *ball-active*)
    (setf *ball-active* t)
    (let ((ball (clone =ball=)))
      [drop self ball]
      [play-sample self "snd/fire-0"]
      [serve ball direction])))
  
;;; Non player characters who wander around


;;; The example room

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform "floor-0"))

(define-prototype room (:parent =world=)
  (tile-size :initform 8)
  (height :initform *room-my*)
  (width :initform *room-mx*)
  (edge-condition :initform :block))

(define-method drop-floor room ()
  (with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
        [drop-cell self (clone =floor=) i j]))))

(define-method drop-spore room ()
  (with-field-values (height width) self
    (dotimes (i 10)
      [drop-cell self (clone =spore=) (random height) (random width)])))

(define-method drop-wall room (r0 c0 r1 c1)
  (let ((w (cond
             ((= r0 r1) "wall-vert-0")
             ((= c0 c1) "wall-horiz-0"))))
    (trace-line (lambda (x y)
                  (prog1 nil
                    (when [drop-cell self (let ((wall (clone =wall-0=)))
                                            (setf (field-value :tile wall)
                                                  (cond
                                                    ((or (and (= c0 y) (= r0 x))
                                                         (and (= (1- c1) y) (= (1- r1) x))
                                                         (and (= c1 y) (= (1- r1) x))
                                                         (and (= (1- c1) y) (= r1 x))) "box-0")
                                                              (t w)))
                                            wall)
                                          y x])))
              c0 r0 c1 r1)
    (let ((x r1) (y c1))
      (when (and (= (random 4) 0) (> (- *room-my* 4) r0) (> (- *room-mx* 4) c0))
        (loop for (tile x y) in `(("wallbox-0"      ,x     ,y)
                                  ("wallbox-1" ,(1+ x)     ,y)
                                  ("wallbox-2"      ,x ,(1+ y))
                                  ("wallbox-3" ,(1+ x) ,(1+ y))) do
          (let ((wall (clone =wall-0=)))
            (setf (field-value :tile wall) tile)
            [drop-cell self wall y x]))))))

(define-method drop-edible-wall room (r0 c0 r1 c1)
  (trace-line (lambda (x y)
                (prog1 nil
                  (when [drop-cell self (clone =wall-edible=) y x])))
              c0 r0 c1 r1))

(define-method drop-generator room (x y)
  (let ((gen (clone =generator-0=)))
    [drop-cell self gen y x]))

(define-method drop-repelant room (x y)
  (let ((gen (clone =repel-0=)))
    (setf (field-value :tile gen) (nth (random 3) '("repel-0" "repel-1" "repel-2")))
    [drop-cell self gen y x]))

(define-method generate room (&key (height *room-my*)
                                   (width *room-mx*))
  (setf *room* self)
  (setf <height> height)
  (setf <width> width)
  [create-default-grid self]
  [drop-floor self]
  [drop-spore self]

    [drop-cell *room* (clone =beam-horiz-0=) 0 0]
    [drop-cell *room* (clone =beam-vert-0=) 0 1]
    [drop-cell *room* (clone =box-0=) 0 2]
    [drop-cell *room* (clone =repel-0=) 0 3]
    [drop-cell *room* (clone =barrel-0=) 0 4]
    [drop-cell *room* (clone =energy-0=) 0 5]
    [drop-cell *room* (clone =generator-0=) 0 6]
    [drop-cell *room* (clone =grid-0=) 0 7]
    [drop-cell *room* (clone =mirror-0=) 0 8]
    [drop-cell *room* (clone =mirror-0=) 0 9]
    [drop-cell *room* (clone =spore=) 0 10]
    [drop-cell *room* (clone =slime-0=) 0 13]
    [drop-cell *room* (clone =wallbox-0=) 0 14]
    [drop-cell *room* (clone =wallbox-1=) 0 15]
    [drop-cell *room* (clone =wallbox-2=) 0 16]
    [drop-cell *room* (clone =wallbox-3=) 0 17]
    [drop-cell *room* (clone =wall-horiz-0=) 0 18]
    [drop-cell *room* (clone =wall-vert-0=) 0 19]
    [drop-cell *room* (clone =img/wall-edible-0=) 0 20]
  ; create walls
  (dotimes (i 20)
    (let ((column (1+ (random *room-mx*)))
          (row (1+ (random *room-my*)))
          (len (random (truncate (/ *room-my* 2)))))
      [drop-wall self row column 
                 (min (- height 1)
                      (+ row len))
                 (min (- width 1)
                      column)]
      (destructuring-bind (r c)
        (midpoint (list row column) (list (+ row len) column))
        [drop-wall self r c r (+ column len)])))
  ; create edible walls
  (dotimes (i 20)
    (let ((column (1+ (random *room-mx*)))
          (row (1+ (random *room-my*)))
          (len (random (truncate (/ *room-my* 2)))))
      [drop-edible-wall self row column 
                        (min (- height 1)
                             (+ row len))
                        (min (- width 1)
                             column)]
      (destructuring-bind (r c)
        (midpoint (list row column) (list (+ row len) column))
        [drop-edible-wall self r c r (+ column len)])))
  ; create spore-generators
  (dotimes (i 40)
    (let ((x (1+ (random *room-mx*)))
          (y (1+ (random *room-my*))))
      (if (not [obstacle-at-p *world* y x])
        [drop-generator self x y])))
  ; create some repelants
  (dotimes (i 10)
    (let ((x (1+ (random *room-mx*)))
          (y (1+ (random *room-my*))))
      (if (not [obstacle-at-p *world* y x])
        [drop-repelant self x y])))
  ; create player
  [drop-cell self (clone =drop-point=)
             2 2 :exclusive t :probe t])

(define-method begin-ambient-loop room ())

;;; Controlling the game

(define-prototype room-prompt (:parent =prompt=))

(defparameter *numpad-keybindings* 
  '(("KP7" nil "move :northwest .")
    ("KP8" nil "move :north .")
    ("KP9" nil "move :northeast .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP1" nil "move :southwest .")
    ("KP2" nil "move :south .")
    ("KP3" nil "move :southeast .")
    ;;
    ("KP7" (:control) "serve-ball :northwest .")
    ("KP8" (:control) "serve-ball :north .")
    ("KP9" (:control) "serve-ball :northeast .")
    ("KP4" (:control) "serve-ball :west .")
    ("KP6" (:control) "serve-ball :east .")
    ("KP1" (:control) "serve-ball :southwest .")
    ("KP2" (:control) "serve-ball :south .")
    ("KP3" (:control) "serve-ball :southeast .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
          '(("Y" nil "move :northwest .")
            ("W" nil "move :north .")
            ("U" nil "move :northeast .")
            ("A" nil "move :west .")
            ("D" nil "move :east .")
            ("B" nil "move :southwest .")
            ("S" nil "move :south .")
            ("N" nil "move :southeast .")
            ;;
            ("Y" (:control) "serve-ball :northwest .")
            ("W" (:control) "serve-ball :north .")
            ("U" (:control) "serve-ball :northeast .")
            ("A" (:control) "serve-ball :west .")
            ("D" (:control) "serve-ball :east .")
            ("B" (:control) "serve-ball :southwest .")
            ("S" (:control) "serve-ball :south .")
            ("N" (:control) "serve-ball :southeast .")
            ;;
            ("Q" (:control) "quit ."))))

(define-method install-keybindings room-prompt ()
  (dolist (k (append *numpad-keybindings* *qwerty-keybindings*))
    (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer)
                   (lambda ()
                     [run-cpu-phase *world* :timer])])

;;; Main program. 

(defparameter *room-window-width* 800)
(defparameter *room-window-height* 600)

;(defun load-object-resource-hook (object)
;  (let ((grid (field-value :grid object)))
;    (when grid
;      (walk-grid (grid spot x y)
;        (loop for obj across spot
;              for i from 0 do
;          ;(format t "grid-obj:~a ~%~s~%" (type-of obj) obj)
;          (when (eq (type-of obj) 'symbol)
;            (setf (aref spot i) (clone (symbol-value obj)))
;            ;(let ((obj (aref spot i)))
;            ;  (format t "new grid-obj:~a ~%~s~%" (type-of obj) obj))
;            )))))
;  object)

(defun make-room ()
  (generate =room=))

(defun init-spore ()
  (message "Initializing Spore...")
;  (setf xe2::*load-object-resource-hook* #'load-object-resource-hook)
  (initialize)
  (set-screen-height *room-window-height*)
  (set-screen-width *room-window-width*)
  (let* ((prompt (clone =room-prompt=))
         (universe (clone =universe=))
         (narrator (clone =narrator=))
         (player (clone =player=))
         (viewport (clone =viewport=)))
    (setf *player* player)
    (setf *narrator* narrator)
    (format t "player: ~s~%" *player*)
    (setf (field-value :tile-size viewport) 8)
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    [resize narrator :height 80 :width *room-window-width*]
    [move narrator :x 0 :y (- *room-window-height* 80)]
    [set-verbosity narrator 2]
    ;;
    ;(setf *room* (find-resource-object "level-1"))
    (format t "x0~%")
    (setf *room* (clone =world=))
    (format t "x1~%")
    (assert *room*)
    (format t "~s~%" *room*)
    (format t "x2~%")
    ;[clone-onto *room* (find-resource-object "level-1")]
    ;(format t "x3~%")
    (setf *room* =room=)
    [drop-cell *room* (clone =drop-point=) 0 0]
    ;[drop-cell *room* player               0 0]
    ;(format t "*world* = ~s~%" (world))
    ;[clone-onto level (find-resource-object "level-1")]
    ;(setf *room* level)
    ;(format t "set player drop-point~%")
    ;(format t "player drop-point is set~%")
    [play universe
          :address '(=room=) ;"level-1"
          :player player
          :narrator narrator
          :prompt prompt
          :viewport viewport]
    [resize viewport :height 470 :width *room-window-width*]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 
                :height (truncate (/ (- *room-window-height* 130) 8))
                :width (truncate (/ *room-window-width* 8))]
    [adjust viewport] 
    [narrateln narrator "You are the green guy."]
    [narrateln narrator "Use hjkl or numpad to move; Control-direction to fire."]
    (install-widgets prompt viewport narrator)))

