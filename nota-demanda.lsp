(defun c:DEMANDA ()
  (swc)
)

; Select Within Curve
; Alan J. Thompson, 03.31.11
(defun swc (/ _pac add ss i temp i2)
  ; loads Visual LISP extensions to AutoLISP
  (vl-load-com)

  ; takes a curve entity 'e' and divides it into segments by calculating points along the curve at regular intervals
  ; it stores these points in a list 'lst' and continues this process until it reaches the end of the curve
  ; the final result is a list of points representing points sampled along the curve at approximately 1% intervals
  (defun _pac (e / l v d lst)
    (setq d (- (setq v (/ (setq l (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))) 100.))))
    (while (< (setq d (+ d v)) l)
      (setq lst (cons (vlax-curve-getPointAtDist e d) lst))
    )
  )

  ; initializes an empty selection set and a selection set query based on specific criteria: they are either circles or ellipses or closed polylines
  (if (setq add (ssadd) ss (ssget '((-4 . "<OR")
                                    (0 . "CIRCLE,ELLIPSE")
                                    (-4 . "<AND")
                                    (0 . "*POLYLINE")
                                    (-4 . "&=")
                                    (70 . 1)
                                    (-4 . "AND>")
                                    (-4 . "OR>")
                                  ))
      )
      (progn 
        (repeat (setq i (sslength ss))
          ; push all objects within the specified polygon "_WP" to "add"
          (if (setq temp (ssget "_WP" (_pac (ssname ss (setq i (1- i))))))
            (repeat (setq i2 (sslength temp)) (ssadd (ssname temp (setq i2 (1- i2))) add))
          )
        )
        (get_hh_and_ass add)
      )
  )
  (princ)
)

(defun get_hh_and_ass (add / hh_filter ass_filter hh ass)
  (setq hh_filter '((0 . "TEXT") (8 . "HP_HORIZONTAL")))
  (setq ass_filter '((0 . "TEXT") (8 . "ASSINANTE")))
  
  ; select and grip all entities of "add"
  (sssetfirst nil add)
  (setq hh (sum hh_filter))
  
  ; select again
  (sssetfirst nil add)
  (setq ass (sum ass_filter))
  
  (draw hh ass)
)

; By Fredy Godinho - Brazil
; This routine is FREE and can be changed, use but do not abuse, please keep this information!
; if you change it, send to contato@aditivocad.com
; thanks!
(defun sum (filter_list / i text value)
  (setq objs (ssget filter_list))

  (if (= objs nil) (setq objs (ssadd)))
  
  (progn
    (setq i (sslength objs))
    (setq position 0 value 0.0)
    (repeat i
      (setq text (cdr (assoc 1 (entget (ssname objs position)))))
      (setq value (+ value (atof text)))
      (setq position (+ position 1))
    )
    value
  )
)

; Gabriel S. Nascimento, 15.08.23, Brazil
(defun draw (hh ass)
  (save_osnap)
  (setvar "osmode" 0)
  
  ; width = 0.00004
  ; _______
  ; | ASS |
  ; -------
  ; | HH  |
  ; ------- height = 0.000066
  ; |  %  |
  ; -------
  ; | FAC |
  ; -------

  (setq insert_pt (getpoint "\nClique no local desejado: "))
  
  ; set styles and current layer
  (command "._style" "ROMANS" "ROMANS" 0.000008 "1" "0" "N" "N" "N")
  (command "layer" "new" "DEMANDA ASS" "color" "3" "DEMANDA ASS" "") 
  (command "layer" "set" "DEMANDA ASS" "") 
  
  (setq rec_w 0.00004)
  (setq rec_h 0.000066)
  
  (command "._rectang" insert_pt "d" rec_w rec_h insert_pt)
  
  ; line breaks
  (setq lb1_start (polar insert_pt (/ pi 2) (/ rec_h 4)))
  (setq lb1_end (polar lb1_start 0 rec_w))
  (setq lb2_start (polar lb1_start (/ pi 2) (/ rec_h 4)))
  (setq lb2_end (polar lb2_start 0 rec_w))
  (setq lb3_start (polar lb2_start (/ pi 2) (/ rec_h 4)))
  (setq lb3_end (polar lb3_start 0 rec_w))
  
  (command "._line" lb1_start lb1_end "")
  (command "._line" lb2_start lb2_end "")
  (command "._line" lb3_start lb3_end "")
    
  (setq center_pt (polar insert_pt 0 (/ rec_w 2)))
  
  (setq ass_pt (polar center_pt (/ pi 2) (* (/ rec_h 8) 7)))
  (setq hh_pt (polar center_pt (/ pi 2) (* (/ rec_h 8) 5)))
  (setq perc_pt (polar center_pt (/ pi 2) (* (/ rec_h 8) 3)))
  (setq facil_pt (polar center_pt (/ pi 2) (* (/ rec_h 8) 1)))
  
  ; sum of the "facil" plus "ass" should be 16
  (setq facil (- 16 ass))
  (setq int_perc (rtos (* (/ facil hh) 100) 2 0))
  
  (command "._text" "j" "mc" ass_pt 0 ass "")
  (command "._text" "j" "mc" hh_pt 0 hh "")
  (command "._text" "j" "mc" perc_pt 0 (strcat int_perc "%") "")
  (command "._text" "j" "mc" facil_pt 0 facil "")
  
  (restore_osnap)
)

(defun save_osnap ()
  (setq osnap_mode (getvar "osmode"))
)

(defun restore_osnap ()
  (setvar "osmode" osnap_mode)
)