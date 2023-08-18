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

;; Gabriel S. Nascimento, 15.08.23
(defun draw (hh)
  (save_osnap)
  (setvar "osmode" 0)
  
  (setq insert_point (getpoint "\nClique no local desejado: "))
  
  (command "._style" "ROMANS" "ROMANS" 0.000008 "1" "0" "N" "N" "N")
  
  (command "layer" "new" "DEMANDA ASS" "color" "1" "DEMANDA ASS" "") 
  (command "layer" "set" "DEMANDA ASS" "") 
  
  (command "._rectang" insert_point "d" 0.00004 0.000066 insert_point)
  
  (setq first_line_break_start (polar insert_point (/ pi 2) (/ 0.000066 4)))
  (setq first_line_break_end (polar first_line_break_start 0 0.00004))
  (setq second_line_break_start (polar first_line_break_start (/ pi 2) (/ 0.000066 4)))
  (setq second_line_break_end (polar second_line_break_start 0 0.00004))
  (setq third_line_break_start (polar second_line_break_start (/ pi 2) (/ 0.000066 4)))
  (setq third_line_break_end (polar third_line_break_start 0 0.00004))
  
  (command "._line" first_line_break_start first_line_break_end "")
  (command "._line" second_line_break_start second_line_break_end "")
  (command "._line" third_line_break_start third_line_break_end "")
    
  (setq center_point (polar insert_point 0 (/ 0.00004 2)))
  
  (setq hh_point (polar center_point (/ pi 2) (* (/ 0.000066 8) 5)))
  (setq percentage_point (polar center_point (/ pi 2) (* (/ 0.000066 8) 3)))
  (setq result_point (polar center_point (/ pi 2) (* (/ 0.000066 8) 1)))
  
  (setq integer_percentage (rtos (* (/ 16 hh) 100) 2 0))
  
  (command "._text" "j" "mc" hh_point 0 hh "")
  (command "._text" "j" "mc" percentage_point 0 (strcat integer_percentage "%") "")
  (command "._text" "j" "mc" result_point 0 16 "")
  
  (restore_osnap)
)

(defun save_osnap ()
  (setq osnap_mode (getvar "osmode"))
)

(defun restore_osnap ()
  (setvar "osmode" osnap_mode)
)