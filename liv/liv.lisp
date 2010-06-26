;;; liv --- life
;;; based on xe2/example/example.lisp
;;; See also http://dto.github.com/notebook/developers-guide.html

;;; Packaging

(defpackage :liv
  (:use :cl :clon :xe2)
  (:export :liv))

(in-package :liv)

(defparameter *room-mx* 40)
(defparameter *room-my* 24)
(defvar *room* nil) ; FIX: how do we communicate between prototypes?
(defvar *player* nil)
(defvar *narrator* nil)
(defvar *clock* 0)
(defvar *clocktimer* (make-array 16 :initial-element 0))
(defvar *pop* (make-array 256 :initial-element nil))
(defvar *obj* (make-array (* *room-mx* *room-my*) :element-type 'fixnum :initial-element 0))

(defmacro make-liv (x y) `(list ,x ,y (make-net) 100 (random 3)))
(defmacro liv-x  (liv) `(first  ,liv))
(defmacro liv-y  (liv) `(second ,liv))
(defmacro liv-net (liv) `(third  ,liv))
(defmacro liv-energy (liv) `(fourth ,liv))
(defmacro liv-art    (liv) `(fifth  ,liv))
(defmacro set-obj (x y i) `(setf (aref *obj* (+ ,x (* ,y *room-mx*))) ,i))
(defmacro get-obj (x y) `(aref *obj* (+ ,x (* ,y *room-mx*))))

(defcell spore
  (id   :initform nil)
  (tile :initform (car (one-of '("spore-0" "spore-1" "spore-2"))))
  (speed :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :spore)))

(defun make-net ()
  (let ((wo (make-array 40 :element-type 'single-float)))
    (loop for i from 0 below 40 do
      (setf (aref wo i) (* 0.9 (- (/ (random (* 2 8192)) 8192) 1f0))))
    wo))

(defun copy-net-random (dst src1 src2)
  (loop for i from 0 below 40 do
    (let ((f (case (random 2)
               (0 (aref src1 i))
               (1 (aref src2 i)))))
      (setf f (+ f (* 0.1 (- (/ (random 8192) 8192) 0.5))))
      (setf (aref dst i) f))))

(defun run-net (net i0 i1 i2 i3)
  (declare (single-float i0 i1 i2 i3) ((array single-float) net))
  (macrolet ((anet (i)
               `(aref net ,i))
             (a* (i j)
               `(* (aref net ,i) (aref net ,j)))
             (nl (i)
               `(coerce (/ 1 (1+ (exp (- (aref net ,i))))) 'single-float)))
    ; layer 1
    (setf (anet 4) (+ (* i0 (anet 0)) (* i1 (anet 1)) (* i2 (anet 2)) (* i3 (anet 3))))
    (setf (anet 9) (+ (* i0 (anet 5)) (* i1 (anet 6)) (* i2 (anet 7)) (* i3 (anet 8))))
    (setf (anet 14) (+ (* i0 (anet 10)) (* i1 (anet 11)) (* i2 (anet 12)) (* i3 (anet 13))))
    (setf (anet 19) (+ (* i0 (anet 15)) (* i1 (anet 16)) (* i2 (anet 17)) (* i3 (anet 18))))
    (setf (anet 4)  (nl 4))
    (setf (anet 9)  (nl 9))
    (setf (anet 14) (nl 14))
    (setf (anet 19) (nl 19))
    ; layer 2
    (setf (anet 24) (+ (a* 4 20) (a* 9 21) (a* 14 22) (a* 19 23)))
    (setf (anet 29) (+ (a* 4 25) (a* 9 26) (a* 14 27) (a* 19 28)))
    (setf (anet 34) (+ (a* 4 30) (a* 9 31) (a* 14 32) (a* 19 33)))
    (setf (anet 39) (+ (a* 4 35) (a* 9 36) (a* 14 37) (a* 19 38)))
    (setf (anet 24) (nl 24))
    (setf (anet 29) (nl 29))
    (setf (anet 34) (nl 34))
    (setf (anet 39) (nl 39))
    ;(format t "~,2f ~,2f ~,2f ~,2f  :  ~,2f ~,2f ~,2f ~,2f~%" (anet 4) (anet 9) (anet 14) (anet 19) (anet 24) (anet 29) (anet 34) (anet 39))
  ))

(defmacro check-timer ((pos elapse) &body body)
  (let ((pos (if (numberp pos) `(aref *clocktimer* ,pos) pos)))
    `(let ((diff (- *clock* ,pos)))
       (when (> diff ,elapse)
         (setf ,pos *clock*)
         ,@body))))

(defun neighbours (x y)
  (list (if (> x 0)              (get-obj (1- x) y) 0)
        (if (< x (1- *room-mx*)) (get-obj (1+ x) y) 0)
        (if (> y 0)              (get-obj x (1- y)) 0)
        (if (< y (1- *room-my*)) (get-obj x (1+ y)) 0)))

(defun transfer-energy (liv liv2)
  (format t "kill: ~a~%" (= (liv-art liv) (liv-art liv2)))
  (let ((am (/ (liv-energy liv2) 2)))
    (when (> am 0)
      (setf am (truncate am))
      (incf (liv-energy liv) am)
      (decf (liv-energy liv2) am))))

(defun spawn (id liv)
  (let ((spore (clone =spore=))
        (x (liv-x liv))
        (y (liv-y liv)))
    (setf (field-value :tile spore) (ecase (liv-art liv)
                                      (0 "spore-0")
                                      (1 "spore-1")
                                      (2 "spore-2")))
    (setf (aref *pop* id) liv)
    (setf (field-value :id spore) id)
    (set-obj x y id)
    [drop-cell *room* spore y x :exclusive t :probe t]))

(defun breed (liv liv2)
  (when (= (liv-art liv) (liv-art liv2))
    (let ((en1 (truncate (/ (liv-energy liv) 2)))
          (en2 (truncate (/ (liv-energy liv2) 2))))
      (decf (liv-energy liv) en1)
      (decf (liv-energy liv2) en2)
      (format t "liv: ~a~%" (+ en1 en2))
      (let ((x (liv-x liv))
            (y (liv-y liv))
            nx ny n)
        (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1)) do
          (setf nx (+ x dx))
          (setf ny (+ y dy))
          (if (< nx 0) (setf nx 0))
          (if (< ny 0) (setf ny 0))
          (if (>= nx *room-mx*) (setf nx (1- *room-mx*)))
          (if (>= ny *room-my*) (setf ny (1- *room-my*)))
          (when (= (get-obj nx ny) 0)
            (loop for i from 1 below (length *pop*) do
              (when (not (aref *pop* i))
                (setf n i)
                (return)))
            (when n
              (format t "born: objid=~a at: ~x,~x~%" n nx ny)
              (let ((liv3 (make-liv nx ny)))
                (setf (liv-art liv3) (liv-art liv))
                (setf (liv-energy liv3) (+ en1 en2))
                (copy-net-random (liv-net liv3) (liv-net liv) (liv-net liv2))
                (spawn n liv3)))))))))

(defun run-liv (liv i)
  (let* ((x (liv-x liv))
         (y (liv-y liv))
         (art (liv-art liv))
         (net (liv-net liv))
         (objid (get-obj x y))
         (nx 0)
         (ny 0)
         (nei (neighbours x y)))
    (format t "objid: ~a:~a, xy: ~a,~a~%" i objid x y)
    (assert (> objid 0))
    (assert (= objid i))
    (decf (liv-energy liv))
    (when (< (liv-energy liv) 0)
      (set-obj x y 0)
      (setf (aref *pop* i) nil)
      (return-from run-liv))
    (destructuring-bind (w e n s) nei
      (let ((eb 0f0) (rb 0f0) (ee 0f0) (fe 0f0))
        (declare (single-float eb rb ee fe))
        ; energy box
        (when (or (= n -2) (= s -2) (= w -2) (= e -2))
          (incf eb)
          ;(format t "~a: energy!~%" objid)
          (incf (liv-energy liv) 100))
        (when (or (= n -1) (= s -1) (= w -1) (= e -1))
          (incf rb))
        (if (> w 0) (if (= (liv-art (aref *pop* w)) art) (incf fe) (incf ee)))
        (if (> e 0) (if (= (liv-art (aref *pop* e)) art) (incf fe) (incf ee)))
        (if (> n 0) (if (= (liv-art (aref *pop* n)) art) (incf fe) (incf ee)))
        (if (> s 0) (if (= (liv-art (aref *pop* s)) art) (incf fe) (incf ee)))
        (run-net net eb rb ee fe)
        (if (> (aref net 24)  0.7) (setf nx 1))
        (if (< (aref net 24)  0.3) (setf nx -1))
        (if (> (aref net 29)  0.7) (setf ny 1))
        (if (< (aref net 29)  0.3) (setf ny -1))
        (when (> (aref net 34) 0.7)
          (when (> w 0) (transfer-energy liv (aref *pop* w)))
          (when (> e 0) (transfer-energy liv (aref *pop* e)))
          (when (> n 0) (transfer-energy liv (aref *pop* n)))
          (when (> s 0) (transfer-energy liv (aref *pop* s))))
        (when (> (aref net 39) 0.7)
          (when (> w 0) (breed liv (aref *pop* w)))
          (when (> e 0) (breed liv (aref *pop* e)))
          (when (> n 0) (breed liv (aref *pop* n)))
          (when (> s 0) (breed liv (aref *pop* s))))
        ;(format t "nx,ny: ~a,~a~%" nx ny)
        ))
    ;(format t "obj: ~a~%" objid)
    (when (or (/= nx 0) (/= ny 0))
      (setf nx (+ x nx))
      (setf ny (+ y ny))
      (when (and (> nx -1) (> ny -1)
                 (< nx *room-mx*)
                 (< ny *room-my*)
                 (= (get-obj nx ny) 0))
        (set-obj x  y  0) ; mark the old position as free
        (set-obj nx ny objid) ; store the objid at the new position
        ;(format t "move: ~a:~a ~a,~a -> ~a,~a~%" i objid x y nx ny)
        (setf (liv-x liv) nx
              (liv-y liv) ny)))))

(defun run-world ()
  (format t "~a:~%" (get-internal-run-time))
  (let ((poplen (length *pop*)))
    (loop for i from 1 below poplen do
      (let ((liv (aref *pop* i)))
        (if liv (run-liv liv i))))))

(defcell floor 
  (tile :initform "floor-0"))

(define-method run spore ()
  (let* ((id (field-value :id self))
         (liv (if id (aref *pop* id))))
    (if (not liv)
      [die self]
      [move-cell *world* self (liv-y liv) (liv-x liv)])))

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
       (let ((x (field-value :column self))
             (y (field-value :row self))
             nx ny n)
         (case (random 4)
           (0 (setf nx (1+ x)) (setf ny y))
           (1 (setf ny (1+ y)) (setf nx x))
           (2 (setf nx (1- x)) (setf ny y))
           (3 (setf ny (1- y)) (setf nx x)))
         (if (< nx 0) (setf nx 0))
         (if (< ny 0) (setf ny 0))
         (if (>= nx *room-mx*) (setf nx (1- *room-mx*)))
         (if (>= ny *room-my*) (setf ny (1- *room-my*)))
         (when (= (get-obj nx ny) 0)
                 ;(not [category-at-p *world* ny nx '(:obstacle)])
           (loop for i from 1 below (length *pop*) do
             (when (not (aref *pop* i))
               (setf n i)
               (return)))
           (if n (spawn n (make-liv nx ny))))))))
  (define-box (repel-0 "repel-0")
    ([die self])
    ())
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

(define-method run player ()
  (run-world)
  (setf *clock* (get-internal-real-time))
  (let ((tile (field-value :tile self))
        (repel (field-value :repel self))
        (x (field-value :column self))
        (y (field-value :row    self)))
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
              [category-at-p *world* y (1+ x) '(:spore)]))))

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
    (loop for i from 1 below 11 do
      (let* ((spore (clone =spore=))
             (x (random width))
             (y (random height))
             (liv (make-liv x y))
             (art (liv-art liv)))
        (when (= (get-obj x y) 0)
          (format t "set-obj: objid=~a~%" i)
          (set-obj x y i)
          (setf (aref *pop* i) liv)
          (setf (field-value :tile spore) (ecase art
                                            (0 "spore-0")
                                            (1 "spore-1")
                                            (2 "spore-2")))
          (setf (field-value :id spore) i)
          [drop-cell self spore y x])))))

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

;    [drop-cell *room* (clone =beam-horiz-0=) 0 0]
;    [drop-cell *room* (clone =beam-vert-0=) 0 1]
;    [drop-cell *room* (clone =box-0=) 0 2]
;    [drop-cell *room* (clone =repel-0=) 0 3]
;    [drop-cell *room* (clone =barrel-0=) 0 4]
;    [drop-cell *room* (clone =energy-0=) 0 5]
;    [drop-cell *room* (clone =generator-0=) 0 6]
;    [drop-cell *room* (clone =grid-0=) 0 7]
;    [drop-cell *room* (clone =mirror-0=) 0 8]
;    [drop-cell *room* (clone =mirror-0=) 0 9]
;    [drop-cell *room* (clone =spore=) 0 10]
;    [drop-cell *room* (clone =wallbox-0=) 0 14]
;    [drop-cell *room* (clone =wallbox-1=) 0 15]
;    [drop-cell *room* (clone =wallbox-2=) 0 16]
;    [drop-cell *room* (clone =wallbox-3=) 0 17]
;    [drop-cell *room* (clone =wall-horiz-0=) 0 18]
;    [drop-cell *room* (clone =wall-vert-0=) 0 19]
;    [drop-cell *room* (clone =img/wall-edible-0=) 0 20]
  ; create walls
;  (dotimes (i 20)
;    (let ((column (1+ (random *room-mx*)))
;          (row (1+ (random *room-my*)))
;          (len (random (truncate (/ *room-my* 2)))))
;      [drop-wall self row column 
;                 (min (- height 1)
;                      (+ row len))
;                 (min (- width 1)
;                      column)]
;      (destructuring-bind (r c)
;        (midpoint (list row column) (list (+ row len) column))
;        [drop-wall self r c r (+ column len)])))
  ; create edible walls
;  (dotimes (i 20)
;    (let ((column (1+ (random *room-mx*)))
;          (row (1+ (random *room-my*)))
;          (len (random (truncate (/ *room-my* 2)))))
;      [drop-edible-wall self row column 
;                        (min (- height 1)
;                             (+ row len))
;                        (min (- width 1)
;                             column)]
;      (destructuring-bind (r c)
;        (midpoint (list row column) (list (+ row len) column))
;        [drop-edible-wall self r c r (+ column len)])))
  ; create spore-generators
  (dotimes (i 40)
    (let ((x (1+ (random *room-mx*)))
          (y (1+ (random *room-my*))))
      (when (not [obstacle-at-p *world* y x])
        (set-obj x y -1)
        [drop-generator self x y])))
  ; create some repelants
  (dotimes (i 10)
    (let ((x (1+ (random *room-mx*)))
          (y (1+ (random *room-my*))))
      (when (not [obstacle-at-p *world* y x])
        (set-obj x y -2)
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

(defun make-room ()
  (generate =room=))

(defun init-liv ()
  (message "Initializing Liv...")
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
    [resize narrator :height 80 :width *room-window-width*]
    [move narrator :x 0 :y (- *room-window-height* 80)]
    [set-verbosity narrator 2]
    (setf *room* (clone =world=))
    (assert *room*)
    (setf *room* =room=)
    [drop-cell *room* (clone =drop-point=) 0 0]
    [play universe
          :address '(=room=)
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
    [narrateln narrator "Use hjkl or numpad to move; Control-direction to manipulate."]
    (install-widgets prompt viewport narrator)))

