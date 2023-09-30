(defun bolha () 
  (setq pt (getpoint "\Clica aí inferno!")) 
  (setq dimsc (getvar "dimscale")) 
  (command "._style" "ROMANS" "ROMANS" 0.00001 "1" "0" "N" "N" "N")
  
  (command "._circle" pt (if (>= number 1000) (* 0.000022 dimsc) (* 0.000018 dimsc))) 
  (command "._text" "j" "mc" pt 0 number "") 
) 

(defun c:NUMERAR (/ i pt s dimsc) 
  (setq s (itoa (setq number (if number (1+ number) 1))))
  
  (if (setq i (getint (strcat "\nNumero <" s ">: "))) 
    (setq number i)
  )
  
  (command "layer" "new" "NUMERACAO_POSTES" "color" "103" "NUMERACAO_POSTES" "") 
  (command "layer" "set" "NUMERACAO_POSTES" "") 

  (bolha) 
  (princ) 
)