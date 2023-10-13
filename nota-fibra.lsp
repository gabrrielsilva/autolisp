(defun draw ()
  (setq insert_pt (getpoint "\nClique no local desejado: "))
  
  (setq csh (strcat "SPXXX001H SPO CSH000" (rtos number 2 0)))
    
  (disable_attdia)
  (command "_.INSERT" "NOME_CABO" insert_pt 23.0909 23.0909 0.0 csh "CABO FO 06F" "" "")
  (enable_attdia)
)

(defun c:FIBRA (/ i s)
  (setq s (itoa (setq number (if number (1+ number) 1))))
  
  (if (setq i (getint (strcat "\nFibra <" s ">: "))) 
    (setq number i)
  )
  
  (draw)
  (princ)
)

(defun disable_attdia ()
  ; Set ATTDIA to 0 to disable the attribute dialog box
  (setvar "ATTDIA" 0)
)

(defun enable_attdia () 
  ; Set ATTDIA to 1 to enable the attribute dialog box
  (setvar "ATTDIA" 1)
)