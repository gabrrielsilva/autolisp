(defun c:demanda ()
  (setq start_point (getpoint "\nClique no local desejado: "))
  (setq top_right_point (polar start_point 0 0.00004))
  (setq bottom_right_point (polar top_right_point (/ (* 3 pi) 2) 0.000066))
  (setq bottom_left_point (polar bottom_right_point pi 0.00004))
  
  (command "._pline" start_point top_right_point bottom_right_point bottom_left_point start_point "")
  )