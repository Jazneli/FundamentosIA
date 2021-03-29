;TOLENTINO PÉREZ JAZMIN YANELI
;Paquete 1, Common Lisp.

(format t "~%           PAQUETE DE EJERCICIOS Num.1 ~%~%")

;1.-Construya una sola expresión LISP para calcular lo que se indica en cada uno de los siguientes incisos:
(format t "   Ejercicio 1: Expresiones LISP ~%~%")

;a.Esta expresión regresa el 5to elemento de una lista
(format t " a) ~S~%" (nth 4 (list '((1 2) 3) '4 '(5 (6)) 'A '(B C) 'D '(E (F G)))))

;b.El número de segundos que tiene el año bisiesto 2014
(format t " b) Segundos: ~S~%" (* 60 60 24 366))

;c.Si el valor numérico asociado a la variable x es diferente de cero y además menor o igual que el valor asociado a la varible y
(defparameter x 5)
(defparameter y 10)
(format t " c) X: ~S Y: ~S X es cero: ~S X <= Y: ~S~%" x y (= x 0) (<= x y))

;d.Una lista con las dos soluciones reales de la ecuación 2x^2 + 7x + 5 = 0
; x1 = ((-b) + raiz(b^2 - 4 * a * c))/2*a 
; x2 = ((-b) - raiz(b^2 - 4 * a * c))/2*a 
(format t " d) ~S~%~%" (list ( / (+ -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)) ( / (- -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2))))

; 2.-Escriba, en notación prefija y evalúe las siguientes expresiones aritméticas:
(format t "   Ejercicio 2: Notación prefija  ~%~%")

;a. 2(4) + (6-8)
(format t " a) ~S~%" (+ (* 2 4) (- 6 8)))

;b (5+(-3+4))/(6+(2/5))
(format t " b) ~S~%" (/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5))))
 
;c raiz 1
(format t " c) ~S~%" (sqrt (/ (+ (* -1 (- -4 (/ 3 8))) 1.4502) (expt -1 (expt (- 3 5) (/ 1 3))))))

;d raiz 2
(format t " d) ~S~%~%" (expt (/ (expt (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17) (/ 1 7)))

; 3.- Indique el resultado de evaluar cada una de las siguientes expresiones:
(format t "   Ejercicio 3: Evaluar expresiones ~%~%")

; a) ( cdar '((one two) three four)))
(format t " a) cdar: ~S~%" (cdar '((one two) three four)))

;b) (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
(format t " b) append: ~S~%" (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven)))

;c) (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
(format t " c) subst: ~S~%" (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin)))

;d) (remove 'sven '(eva sven lisa sven anna))
(format t " d) remove: ~S~%"  (remove 'sven '(eva sven lisa sven anna)))

; e) (butlast '(karl adam nilsson gregg alisson vilma) 3)
(format t " e) butlast: ~S~%" (butlast '(karl adam nilsson gregg alisson vilma) 3))

;f)(nth 2 '(a b c d e))
(format t " f) nth: ~S~%" (nth 2 '(a b c d e)))

;g) (nthcdr 2 '(a b c d e))
(format t " g) ntchdr: ~S~%" (nthcdr 2 '(a b c d e)))

;h) (intersection '(a b c) '(x b z c))
(format t " h) intersection: ~S~%" (intersection '(a b c) '(x b z c)))

;i)(cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8)))))
(format t " i) cdadar: ~S~%~%" (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8))))))

; 4.- Defina una función  Recombina que reciba como argumento una lista de la forma((A.x) (B.y) (C.z)), 
;donde  A, B y C son átomos simbólicos, mientras que  x, y y z  son números.  Como respuesta, la función 
;debe entregar otra lista con la siguiente estructura:( ((x  y) . A) ((y  z) . C) ((z  y  x) . B) )
(format t "   Ejercicio 4: Recombina~%~%")
(defparameter lista (list (cons 'A '1) (cons 'B '2) (cons 'C '3)))
(defun Recombina (lista )
 (list 
   (cons (list (rest (first lista)) (rest (second lista))) (first (first lista)))
   (cons (list (rest (second lista)) (rest (third lista))) (first (third lista)))
   (cons (list (rest (third lista)) (rest (second lista)) (rest (first lista ))) (first (second lista)))))
(format t " Lista Recombina: ~S~%~% "(Recombina lista))

; 5.- Defina  un  predicado RealNoCero? que  reciba  un  argumento N y  responda  si  su argumento es o no un 
;número real diferente de cero.
(format t "   Ejercicio 5: RealNoCero? ~%~%")
(defun RealNoCero? (N)
  (if  (and (numberp N) (not (= N 0)))
      T
      NIL))
(format t " Resultado ~S~% "(RealNoCero? ()))
(format t "Resultado ~S~% "(RealNoCero? -2))
(format t "Resultado ~S~%~% "(RealNoCero? 0))

; 6.- Construya una función Analiza, con argumento X, que responda una lista con los valores de verdad correspondientes a 
;las respuestas a las siguientes preguntas:  ¿es X un  átomo?, ¿es X un número?,   ¿es X una lista? ,   ¿es X una celda 
;de construcción?  y   ¿es X una lista vacía?
(format t "   Ejercicio 6: Analiza ~%~%")
(defun Analiza (X)
  (let ((atomo (atom X))
	(numero (numberp X))
	(lista (listp X))
	(celda (consp X))
	(listaVacia (null X)))
    (list atomo numero lista celda listaVacia )))
(format t " Resultado ~S~% "(Analiza '(A B C)))
(format t "Resultado ~S~% "(Analiza 3))
(format t "Resultado ~S~%~% "(Analiza ()))

; 7.- Defina  una  función Intercala que  reciba  como  argumentos  dos  listas  cualesquiera y,como resultado entregue 
;otra lista en la que se encuentran intercalados los elementos de las listas originales; siempre en el mismo orden: un 
;elemento de la primera lista y otro dela segunda lista.  Si las listas no tienen la misma longitud, todos los elementos 
;restantes de la lista más grande se colocan seguidos en la respuesta
(format t "   Ejercicio 7: Intercala~%~%")
(defun Intercala (lista1 lista2)
  (let ((listaINtercala '()))
    (do ()
	((and (null lista1) (null lista2)) listaINtercala)
      (if (not (null lista1))
	  (progn (setq listaINtercala (append listaINtercala (list (first lista1))))
		 (setq lista1 (rest lista1))))
      (if (not (null lista2))
	  (progn (setq listaINtercala (append listaINtercala (list (first lista2))))
		 (setq lista2 (rest lista2)))))))

(format t " Resultado ~S~% "(Intercala '(A B C) '(E D F)))
(format t "Resultado ~S~% "(Intercala '(1 2 3) '()))
(format t "Resultado ~S~%~% "(Intercala '(1 2 3) '(4 5 6 7)))

; 8.- Programe un predicado  MismoTipo  que reciba como argumento dos listas de la misma longitud y como respuesta   
;devuelva T si  ambas  listas  tienen  elementos  del  mismotipo y en las mismas posiciones,  NIL en  caso contrario.  ;Observe que los elementos no requieren ser iguales, sólo del mismo tipo de datos.
(format t "   Ejercicio 8: MismoTipo ~%~%")
(defun MismoTipo (lista1 lista2)
  (let ((resultado T)
	(longitud (length lista1)))
      (dotimes (n longitud resultado) 
	(when (not (typep (nth n lista1) (type-of (nth n lista2))))
	  (return-from nil)))))
(format t " Resultado ~S~% "(MismoTipo '(8 10 -1) '(8 10 -1)))
(format t "Resultado ~S~% "(MismoTipo '(A 1 B) '(2 C 3)))
(format t "Resultado ~S~%~% "(MismoTipo '(1 A B) '(2 C D)))

; 9.- Defina una función APalíndromo, sensible a mayúsculas y minúsculas,que reciba como argumento una cadena y, como
;respuesta entrega otra cadena que es el palíndromo de la original.  Ejemplo:  APalíndromo("Hola") =  "HolaaloH"
(format t "   Ejercicio 9: APalíndromo ~%~%")
(defun APalindromo (cadena)
  (let ((cadenaInvertida (reverse cadena)))
	(concatenate 'string cadena cadenaInvertida)))
(format t " Resultado ~S~% "(APalindromo "Hola"))
(format t "Resultado ~S~%~% "(APalindromo "123"))

; 10.- Defina un predicado Bisiesto que reciba como entrada un número entero representando un año  y, como respuesta, 
;indique si se trata de un año bisiesto o no.
(format t "   Ejercicio 10: Bisiesto ~%~%")
(defun Bisiesto ( numero )
  (or (and (= (mod numero 4) 0) (not (= (mod numero 100) 0)) ) (= (mod numero 400) 0)))
(format t " Resultado: ~S~% " (Bisiesto 2004))
(format t "Resultado: ~S~%~% " (Bisiesto 2021))