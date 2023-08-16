(defun c:DEMANDA ()
  (swc)
)

;; Select Within Curve
;; Alan J. Thompson, 03.31.11
(defun swc (/ _pac add ss i temp i2)
  (vl-load-com)

  (defun _pac (e / l v d lst)
    (setq d (- (setq v (/ (setq l (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))) 100.))))
    (while (< (setq d (+ d v)) l)
      (setq lst (cons (vlax-curve-getPointAtDist e d) lst))
    )
  )

  (if (setq add (ssadd)
        ss (ssget '((-4 . "<OR")
                    (0 . "CIRCLE,ELLIPSE")
                    (-4 . "<AND")
                    (0 . "*POLYLINE")
                    (-4 . "&=")
                    (70 . 1)
                    (-4 . "AND>")
                    (-4 . "OR>")
                    )
            )
      )
    (progn 
      (repeat (setq i (sslength ss))
        (if (setq temp (ssget "_WP" (_pac (ssname ss (setq i (1- i))))))
          (repeat (setq i2 (sslength temp)) (ssadd (ssname temp (setq i2 (1- i2))) add))
        )
      )
      (sssetfirst nil add)
      ; (ssget "_I")
      (sum)
    )
  )
  (princ)
)

;; By Fredy Godinho - Brazil
;; This routine is FREE and can be changed, use but do not abuse, please keep this information!
;; if you change it, send to contato@aditivocad.com
;; thanks!
(defun sum (/ ar_total gr i text value)
  (setq ar_total 0)
  (setq objs (ssget '((0 . "TEXT"))))
  (if (/= objs nil)
    (progn
      (setq gr (ssadd))
      (setq i (sslength objs))
      (setq position 0 value 0.0)
      (repeat i
        (setq text (cdr (assoc 1 (entget (ssname objs position)))))
        (setq value (+ value (atof text)))
        (setq position (+ position 1))
      )
      (draw (setq hh value))
    )
  )
  (princ)
)

;; Gabriel S. Nascimento, Brazil
(defun draw (hh)
  (setq insert_point (getpoint "\nClique no local desejado: "))
  (command "._text" "j" "mc" insert_point 0.000013 hh)
)