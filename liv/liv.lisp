;;; liv --- life
;;; based on xe2/example/example.lisp
;;; See also http://dto.github.com/notebook/developers-guide.html

(proclaim '(optimize (safety 3) (debug 3) (speed 0)))

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
(defvar *run-world* nil)
; one-way lockless communication channels
; FIX: create some macros and move out this functionally
(defvar *create-spore-request* nil) ; create request isn't critical, ie lossy is fine
(defvar *drop-spore-request-0* nil)
(defvar *drop-spore-request-1* nil)
(defvar *die-spore-request-0* nil)
(defvar *die-spore-request-1* nil)
(defvar *move-spore-request-0* nil)
(defvar *move-spore-request-1* nil)

(defmacro make-liv (x y) `(list ,x ,y (make-net) 100 (random 3) 0 0 0 nil))
(defmacro liv-x  (liv) `(first  ,liv))
(defmacro liv-y  (liv) `(second ,liv))
(defmacro liv-net (liv) `(third  ,liv))
(defmacro liv-energy (liv) `(fourth ,liv))
(defmacro liv-art    (liv) `(fifth  ,liv))
(defmacro get-plague-liv-flag (liv) `(not (zerop (logand (sixth ,liv) 1))))
(defmacro set-plague-liv-flag (liv) `(setf (sixth ,liv) (logior (sixth ,liv) 1)))
(defmacro liv-age (liv) `(nth 6 ,liv))
(defmacro liv-gen (liv) `(nth 7 ,liv))
(defmacro liv-spore (liv) `(nth 8 ,liv))
(defmacro set-obj (x y i) `(setf (aref *obj* (+ ,x (* ,y *room-mx*))) ,i))
(defmacro get-obj (x y) `(aref *obj* (+ ,x (* ,y *room-mx*))))

(defcell spore
  (id   :initform nil)
  (tile :initform (car (one-of '("spore-0" "spore-1" "spore-2"))))
  (speed :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :spore)))

(defcell floor (tile :initform "floor-0"))

(defmacro fassert (form &rest msg)
  `(if (not ,form) (error "assertion failed for form: ~s data: ~a" ',form (format nil ,@msg))))

(defun verify-pop-obj-integrity ()
  (let ((cnta 0) (cntb 0))
    (loop for i from 1 below (length *pop*) do
      (let ((liv (aref *pop* i)))
        (when liv
          (incf cnta)
          (assert (listp liv))
          (let ((x (liv-x liv))
                (y (liv-y liv)))
            (let ((objid (get-obj x y)))
              (fassert (= objid i) "objid=~a /= i=~a" objid i))))))
    (loop for n from 0 below (length *obj*) do
      (let ((objid (aref *obj* n)))
        (when (> objid 0)
          (incf cntb)
          (let ((liv (aref *pop* objid)))
            (assert liv) (assert (listp liv))
            (let ((x (liv-x liv))
                  (y (liv-y liv)))
              (fassert (= n (+ x (* y *room-mx*))) "n=~a, x=~a, y=~a (~a) objid=~a" n x y (+ x (* y *room-mx*)) objid))))))
    (fassert (= cnta cntb) "cnta=~a /= cntb=~a" cnta cntb)))

(defun make-net ()
  (let ((wo (make-array 64 :element-type 'single-float)))
    (loop for i from 0 below 64 do
      (setf (aref wo i) (* 0.9 (- (/ (random (* 2 8192)) 8192) 1f0))))
    wo))

(defun copy-net-random (dst src1 src2)
  (loop for i from 0 below 64 do
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
    (setf (anet 4)  (+ 1f0 (* i0 (anet  0)) (* i1 (anet  1)) (* i2 (anet  2)) (* i3 (anet  3))))
    (setf (anet 9)  (+ 1f0 (* i0 (anet  5)) (* i1 (anet  6)) (* i2 (anet  7)) (* i3 (anet  8))))
    (setf (anet 14) (+ 1f0 (* i0 (anet 10)) (* i1 (anet 11)) (* i2 (anet 12)) (* i3 (anet 13))))
    (setf (anet 19) (+ 1f0 (* i0 (anet 15)) (* i1 (anet 16)) (* i2 (anet 17)) (* i3 (anet 18))))
    (setf (anet 24) (+ 1f0 (* i0 (anet 20)) (* i1 (anet 21)) (* i2 (anet 22)) (* i3 (anet 23))))
    (setf (anet 29) (+ 1f0 (* i0 (anet 25)) (* i1 (anet 26)) (* i2 (anet 27)) (* i3 (anet 28))))
    (setf (anet 4)  (nl 4))
    (setf (anet 9)  (nl 9))
    (setf (anet 14) (nl 14))
    (setf (anet 19) (nl 19))
    (setf (anet 24) (nl 24))
    (setf (anet 29) (nl 29))
    ; layer 2
    (setf (anet 36) (+ 1f0 (a* 4 30) (a* 9 31) (a* 14 32) (a* 19 33) (a* 24 34) (a* 29 35)))
    (setf (anet 43) (+ 1f0 (a* 4 37) (a* 9 38) (a* 14 39) (a* 19 40) (a* 24 41) (a* 29 42)))
    (setf (anet 50) (+ 1f0 (a* 4 44) (a* 9 45) (a* 14 46) (a* 19 47) (a* 24 48) (a* 29 49)))
    (setf (anet 57) (+ 1f0 (a* 4 51) (a* 9 52) (a* 14 53) (a* 19 54) (a* 24 55) (a* 29 56)))
    (setf (anet 36) (nl 36))
    (setf (anet 43) (nl 43))
    (setf (anet 50) (nl 50))
    (setf (anet 57) (nl 57))
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
  ;(format t "kill: ~a~%" (= (liv-art liv) (liv-art liv2)))
  (let ((am (truncate (/ (liv-energy liv2) 4))))
    (when (> am 0)
      (incf (liv-energy liv) am)
      (decf (liv-energy liv2) (ash am 2)))))

(defun yield ()
  (when (not *drop-spore-request-1*)
    (setf *drop-spore-request-1* *drop-spore-request-0*)
    (setf *drop-spore-request-0* nil))
  (when (not *move-spore-request-1*)
    (setf *move-spore-request-1* *move-spore-request-0*)
    (setf *move-spore-request-0* nil))
  (when (not *die-spore-request-1*)
    (setf *die-spore-request-1* *die-spore-request-0*)
    (setf *die-spore-request-0* nil))
  )

(defun spawn (id liv)
  (let (;(spore (clone =spore=))
        (x (liv-x liv))
        (y (liv-y liv))
        (plague (get-plague-liv-flag liv))
        tile)
    ;(setf (liv-spore liv) spore)
    (setf tile (ecase (liv-art liv)
                 (0 (if plague "spore-plague-0" "spore-0"))
                 (1 (if plague "spore-plague-1" "spore-1"))
                 (2 (if plague "spore-plague-2" "spore-2"))))
    (fassert (not (aref *pop* id)) "id=~a *pop*[id]=~a /= nil" id (aref *pop* id))
    (setf (aref *pop* id) liv)
    (set-obj x y id)
    (push (list id tile liv x y) *drop-spore-request-0*)
    (when (not *drop-spore-request-1*)
      (setf *drop-spore-request-1* *drop-spore-request-0*)
      (setf *drop-spore-request-0* nil))))

(defun breed (liv liv2)
  (when (= (liv-art liv) (liv-art liv2))
    (let ((en1 (truncate (/ (liv-energy liv) 2)))
          (en2 (truncate (/ (liv-energy liv2) 2))))
      (decf (liv-energy liv) en1)
      (decf (liv-energy liv2) en2)
      ;(format t "liv: ~a~%" (+ en1 en2))
      (let ((x (liv-x liv))
            (y (liv-y liv))
            nx ny n)
        (verify-pop-obj-integrity)
        (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1)) do
          (setf nx (+ x dx))
          (setf ny (+ y dy))
          (if (< nx 0) (setf nx 0))
          (if (< ny 0) (setf ny 0))
          (if (>= nx *room-mx*) (setf nx (1- *room-mx*)))
          (if (>= ny *room-my*) (setf ny (1- *room-my*)))
          (when (= (get-obj nx ny) 0)
            (setf n nil)
            (loop for i from 1 below (length *pop*) do
              (when (not (aref *pop* i))
                (setf n i)
                (return)))
            (when n
              ;(format t "born: objid=~a at: ~a,~a~%" n nx ny)
              (let ((liv3 (make-liv nx ny)))
                (setf (liv-art liv3) (liv-art liv))
                (setf (liv-energy liv3) (+ en1 en2))
                (copy-net-random (liv-net liv3) (liv-net liv) (liv-net liv2))
                (if (or (get-plague-liv-flag liv) (get-plague-liv-flag liv2))
                  (set-plague-liv-flag liv3))
                (spawn n liv3)
                (setf (liv-gen liv3) (1+ (liv-gen liv)))
                (verify-pop-obj-integrity)
                (return-from breed)))))))))

(defun die-liv (liv i)
  (loop while (not (liv-spore liv)) do (yield) (sleep 0.001))
  (push liv *die-spore-request-0*)
  (when (not *die-spore-request-1*)
    (setf *die-spore-request-1* *die-spore-request-0*)
    (setf *die-spore-request-0* nil))
  (set-obj (liv-x liv) (liv-y liv) 0)
  (setf (aref *pop* i) nil))

(defun run-liv (liv i)
  (let* ((x (liv-x liv))
         (y (liv-y liv))
         (art (liv-art liv))
         (net (liv-net liv))
         (objid (get-obj x y))
         (nx 0)
         (ny 0)
         (nei (neighbours x y)))
    ;(format t "objid: ~a:~a, xy: ~a,~a~%" i objid x y)
    (assert (> objid 0))
    (assert (= objid i))
    (incf (liv-age liv))
    (when (> (liv-age liv) 1000) 
      (die-liv liv i)
      (return-from run-liv))
    (decf (liv-energy liv))
    (if (get-plague-liv-flag liv) (decf (liv-energy liv)))
    (when (< (liv-energy liv) 0)
      (die-liv liv i)
      (return-from run-liv))
    ;(format t "nei: ~s~%" nei)
    (destructuring-bind (w e n s) nei
      (let ((eb 0f0) (rb 0f0) (ee 0f0) (fe 0f0))
        (declare (single-float eb rb ee fe))
        ; energy box
        (when (or (= n -2) (= s -2) (= w -2) (= e -2))
          (incf eb)
          (incf (liv-energy liv) 100))
        (when (or (= n -1) (= s -1) (= w -1) (= e -1))
          (incf rb))
        (if (> w 0) (if (= (liv-art (aref *pop* w)) art) (incf fe) (incf ee)))
        (if (> e 0) (if (= (liv-art (aref *pop* e)) art) (incf fe) (incf ee)))
        (if (> n 0) (if (= (liv-art (aref *pop* n)) art) (incf fe) (incf ee)))
        (if (> s 0) (if (= (liv-art (aref *pop* s)) art) (incf fe) (incf ee)))
        (run-net net eb rb ee fe)
        (setf nx -1) (setf ny -1)
        (if (> (aref net 36)  0.7) (setf nx 1))
        (if (< (aref net 43)  0.7) (setf ny 1))
        (when (> (aref net 50) 0.7)
          (when (> w 0) (transfer-energy liv (aref *pop* w)))
          (when (> e 0) (transfer-energy liv (aref *pop* e)))
          (when (> n 0) (transfer-energy liv (aref *pop* n)))
          (when (> s 0) (transfer-energy liv (aref *pop* s))))
        (when (> (aref net 57) 0.7)
          (when (> w 0) (breed liv (aref *pop* w)))
          (when (> e 0) (breed liv (aref *pop* e)))
          (when (> n 0) (breed liv (aref *pop* n)))
          (when (> s 0) (breed liv (aref *pop* s))))))
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
              (liv-y liv) ny)
        (loop while (not (liv-spore liv)) do (yield) (sleep 0.001))
        (push (list (liv-spore liv) nx ny) *move-spore-request-0*)
        (when (not *move-spore-request-1*)
          (setf *move-spore-request-1* *move-spore-request-0*)
          (setf *move-spore-request-0* nil))
        (verify-pop-obj-integrity)))))

(defun run-world ()
  ;(format t "**** ~a ****~%" (get-internal-run-time))
  (loop for i from 0 below (length *obj*) do
    (if (= (aref *obj* i) -3)
      (setf (aref *obj* i) 0)))
  (let ((x (field-value :column *player*))
        (y (field-value :row    *player*)))
    ; FIX: bug, player moves into busy position
    ;(let ((objid (get-obj x y)))
    ;  (fassert (= objid 0) "place ~a,~a is not empty" x y))
    (if (= (get-obj x y) 0) (set-obj x y -3)))
  (let ((poplen (length *pop*))
        (pops 0)
        (plague 0)
        (maxgen 0))
    (verify-pop-obj-integrity)
    (loop for i from 1 below poplen do
      (let ((liv (aref *pop* i)))
        (when liv 
          (incf pops)
          (if (get-plague-liv-flag liv) (incf plague))
          (if (> (liv-gen liv) maxgen) (setf maxgen (liv-gen liv)))
          (run-liv liv i))))
    (format t "stats: size=~a maxgen=~a plague=~a~%" pops maxgen plague))
  ; handle create spore requests
  (let ((lst *create-spore-request*) n)
    (setf *create-spore-request* nil)
    (loop for (nx ny) in lst do
      (setf n nil)
      (when (= (get-obj nx ny) 0)
        (loop for i from 1 below (length *pop*) do
          (when (not (aref *pop* i))
            (setf n i)
            (return)))
        (when n
          ;(format t "handle-spawn-request~%")
          (spawn n (make-liv nx ny)))))))

(defun run-liv-thread ()
  (when *run-world*
    (run-world)
    (setf *run-world* nil))
  (sleep 0.001)
  (run-liv-thread))

(define-method run spore ()
  )

(define-method hit spore ()
  ;[die self]
  (let* ((id (field-value :id self))
         (liv (if id (aref *pop* id))))
    (when liv
      (format t "plague: ~a,~a~%" (length liv) (sixth liv))
      (set-plague-liv-flag liv)
      (setf (field-value :tile self) (ecase (liv-art liv)
                                       (0 "spore-plague-0")
                                       (1 "spore-plague-1")
                                       (2 "spore-plague-2"))))))

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
    ((when (= (random 16) 0) ; how often to spawn a spore
       (let ((x (field-value :column self))
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
         (push (list nx ny) *create-spore-request*)))))
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
  (setf *run-world* t) ; a bit baroque/primitive message-passing system
  ; FIX: unmark the below loop to run the threads async, though that might hit thread-unsafe code
  ; Having this loop here runs the threads in sync but not parallel and therefor doesn't benefit from threading.
  ;(loop while *run-world* do (sleep 0.001))
  ;(format t "xe2 handle-requests~%")
  (loop for (id tile liv x y) in *drop-spore-request-1* do
    (let ((spore (clone =spore=)))
      (setf (liv-spore liv) spore)
      (setf (field-value :id spore) id)
      (setf (field-value :tile spore) tile)
      [drop-cell *room* spore y x :exclusive t :probe t]))
  (setf *drop-spore-request-1* nil)
  ;(format t "xe2 2~%")
  (loop for liv in *die-spore-request-1* do
    [die (liv-spore liv)])
  (setf *die-spore-request-1* nil)
  ;(format t "xe2 3~%")
  (loop for (spore nx ny) in *move-spore-request-1* do
    [move-cell *world* spore ny nx])
  (setf *move-spore-request-1* nil)
  ;(format t "xe2 done handle-requests~%")
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
          (setf (liv-spore liv) spore)
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
  (dotimes (i 20)
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
  (sb-thread:make-thread 'run-liv-thread)
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

