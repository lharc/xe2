
(in-package :liv)

;;; Turn on timing after SDL init

(add-hook '*initialization-hook*
          (lambda ()
            (enable-timer)
            (set-frame-rate 30)
            (set-timer-interval 1)
            (enable-held-keys 1 3)))

(init-liv)

