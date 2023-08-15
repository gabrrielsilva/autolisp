(defun c:demanda ()
  (save_osnap)
  (setvar "osmode" 0)
  
  (setq start_point (getpoint "\nClique no local desejado: "))
  (command "._rectang" start_point "d" 0.00004 0.000066 start_point)
  
  (setq first_line_break_start (polar start_point (/ pi 2) (/ 0.000066 4))) ; 270°
  (setq first_line_break_end (polar first_line_break_start 0 0.00004))
  (setq second_line_break_start (polar first_line_break_start (/ pi 2) (/ 0.000066 4)))
  (setq second_line_break_end (polar second_line_break_start 0 0.00004))
  (setq third_line_break_start (polar second_line_break_start (/ pi 2) (/ 0.000066 4)))
  (setq third_line_break_end (polar third_line_break_start 0 0.00004))
  
  (command "._line" first_line_break_start first_line_break_end "")
  (command "._line" second_line_break_start second_line_break_end "")
  (command "._line" third_line_break_start third_line_break_end "")
  
  (restore_osnap)
  )

(defun save_osnap ()
  (setq osnap_mode (getvar "osmode"))
  )

(defun restore_osnap ()
  (setvar "osmode" osnap_mode)
  )