;TOLENTINO PÉREZ JAZMIN YANELI
;Paquete 2, Common Lisp.

(format t "~%           PAQUETE DE EJERCICIOS Num.2 ~%~%")

;1. Defina una función ElemInPos que reciba tres argumentos: elem, lista y pos. La función debe devolver T si elem está en la posición 
;pos de lista, NIL si no lo está.
(format t "   Ejercicio 1: ElemInPos ~%~%")
(defun ElemInPos (elem lista pos)
    (cond ((null lista) NIL)
        ((>= pos (length lista)) NIL)
        ((equal (nth pos lista) elem))
        (T NIL)))
(format t " Resultado: ~S~% "(ElemInPos '3 '( 1 4 B 3 D) '3))
(format t "Resultado: ~S~%~% "(ElemInPos '5 '( 1 4 B 3 D) '7))

;2. Escriba la función Inicio-en que recibe como argumentos una lista y un elemento cualquiera. La función debe entregar como respuesta 
;una copia de la lista original pero comenzando con la primera ocurrencia del elemento dado en la lista original.
(format t "   Ejercicio 2: Inicio-en ~%~%")
(defun Inicio-en (lista elemento)
    (let ((resultado '()))
        (dolist (i lista resultado)
            (if (equal elemento i)
                (setq resultado (append (list i) resultado))
                (setq resultado (append resultado (list i)))))))
(format t " Resultado: ~S~%~% "(Inicio-en '( 1 2 3 4 5) '3))

;3.  Modifique la función del ejercicio anterior para que  se llame Termina-en y entregue como respuesta una copia de la lista original 
;pero que termina en la última ocurrencia del elemento dado.
(format t "   Ejercicio 3: Termina-en ~%~%")
(defun Termina-en (lista elemento)
    (let ((resultado '())
        (inver (reverse lista)))
        (dolist (i inver resultado)
            (if (equal elemento i)
                (setq resultado (append resultado (list i)))
                (setq resultado (append (list i) resultado))))))
(format t " Resultado: ~S~%~% "(Termina-en '( 1 2 3 4 5) '3))

;4. Construta una función Primer-impar que reciba como argumento una lista y como respuesta entregue otra lista conteniendo el primer 
;elemento de la lista original que sea un número impar y la posición (indice) donde se encuentra. 
(format t "   Ejercicio 4: Primer-impar ~%~%")
(defun Primer-impar(lista)
    (let ((size (length lista))
           (resultado '()))
           (dotimes (n size resultado)
                (when (and (numberp (nth n lista)) (= (mod (nth n lista) 2) 1) )
                    (return (list (nth n lista) n))))))
(format t " Resultado: ~S~% " (Primer-impar '(4 2 B '(8 C) 6 A '(A B) 8 2 7)))
(format t "Resultado: ~S~%~% " (Primer-impar '(A '(3 4) 1 2 B 5)))

;5. Modifique la función del inciso anterior para que entregue en la lista de respuesta el último elemento de la lista que sea un 
;número real mayor o igual que cero y el número de veces que dicho elemento se repite en toda la lista.
(format t "   Ejercicio 5: último-elemento ~%~%")
(defun Ultimo-elemento (lista)
  (let ((resultado '()) 
	(conteo 0)
	(elemento)
	(enesimo))
    (do ((n (- (length lista) 1) (- n 1)))
	((<= n 0) resultado)
      (setq enesimo (nth n lista))
      (cond ((and (realp enesimo) (>= enesimo 0) (= conteo 0))
	    (setq elemento enesimo)
	    (setq conteo 1)
	    (setq resultado (list elemento conteo)))
	   ((and (realp enesimo) (> conteo 0) (eql enesimo elemento))
	    (setq conteo (+ conteo 1))
	    (setq resultado (list elemento conteo))))))) 
(format t " Resultado: ~S~%~%" (Ultimo-elemento '(1 0 5 1 3 -5 1 2 -3 -1 0)))

;6. Escriba la función Conteo que recibe como argumentos una lista cualquiera y como respuesta, entregue una celda de construcción cuya 
;primera parte contiene el conteo de elementos numéricos de las lista original y cuya segunda parte contiene el conteo de sublistas 
;contenidas en la lista original.
(format t "   Ejercicio 6: Conteo~%~%")
(defun Conteo (lista)
  (let ((size (length lista))
      (numNumeros 0)
      (numSublistas 0)
  )
  (dotimes (n size numNumeros)
    (when (numberp (nth n lista))
      (setq numNumeros (+ numNumeros 1))
    )
    (when (listp (nth n lista))
      (setq numSublistas (+ numSublistas 1))
    )
  )
  (cons numNumeros numSublistas))
)
(format t " Resultado: ~S ~%~%" (Conteo  '(A B '(3 4) 2 '(B S 4) 8 6 A 8 '( ) 7) ) )

;7. Defina una función Aplana que reciba como argumento una lista con elementos anidados a cualquier nivel de profundidad y, como 
;respuesta, entregue una lista conteniendo los mismos elementos pero todos ellos al nivel principal de profundidad.
(format t "   Ejercicio 7: Aplana~%~%")
(defun aplana (lista)
  (let ((listaAplanada '())
	(pila (list (cons lista (list 0))))
	(tope 0)
	(sublista nil)
	(indice)
	(elemento)
	(longitudSublista))
    (do ()
	((= tope -1) listaAplanada)
      (setq sublista (first (nth tope pila)))
      (setq indice (second (nth tope pila)))
      (setq longitudSublista (length sublista))
      (setq elemento (nth indice sublista))
      (cond ((= indice longitudSublista) 
	     (setq pila (delete (nth tope pila) pila)) 
	     (setq tope (1- tope)))
	    ((and (listp elemento) (> (length elemento) 0))
	     (setf (second (nth tope pila)) (1+ indice))
	     (setq pila (append pila (list (cons elemento (list 0)))))
	     (setq tope (1+ tope)))
	    (T (setf (second (nth tope pila)) (1+ indice))
	       (setq listaAplanada (append listaAplanada (list elemento))))))))
(format t " Resultado: ~S~%~%"(aplana '(A B C (D E) (F (0 -1)) (G) H I (J K (L (M (7 -7)))))))

;8. Escriba la función Diagonal que recibe como argumentos una lista conteniendo m sub-listas de m elementos cada una de ellas y que 
;representa una matriz de m x m elementos. Como respuesta, esta función debe devolver una lista conteniendo los elementos en la 
;diagonal principal de dicha matriz.
(format t "   Ejercicio 8: Diagonal~%~%")
(defun Diagonal (lista)
  (let ((diagonal '())
	(m (length lista))
	(elemento))
    (dolist (sublista lista diagonal)
      (setq elemento (nth (ash m -1) sublista))
      (setq diagonal (append diagonal (list elemento))))))
(defparameter diag (Diagonal  '( (1 2 3) (4 5 6) (7 8 9 10)) ))
(format t " Resultado: ~S ~%~%" (Diagonal  '( (1 2 3) (4 5 6) (7 8 9 10))))

;9. Construta una función que reciba como argumento una lista cualquiera y, como respuesta, entregue una lista, con el mismo número 
;de elementos de primer nivel, pero que contiene un símbolo A si el elemento en la posición correspondiente es un átoma, un símbolo L 
;si el elemento correspondiente es una lista y un símbolo N si el elemento en la posición correspondiente es una lista vacía.
(format t "   Ejercicio 9: Tipos Lista ~%~%")
(defun Tipos(lista)
    (let ((size (length lista)) 
            (result ()))
          (dotimes (n size result)
                (if (atom (nth n lista))
                    (setq result (append result (list 'A)))
                    (if (not (= 0 (length (nth 1 (nth n lista) ))))
                        (setq result (append result (list 'L)))
                        (setq result (append result (list 'N))))))))
(format t " Resultado: ~S~%~%" (Tipos '(A B '(C D '(1 2) 3)  E 4 '() 5 6 B) ))

;10 Defina la función Suma-numérica que recibe como argumento una lista cualquiera(no anidada), y como respuesta entrega la suma de 
;exclusivamente aquellos elementos de la lista que son numéricos.
(format t "   Ejercicio 10: Suma-numérica ~%~%")
(defun Suma-numerica (lista)
    (let ((suma 0))
        (dotimes (n (length lista) suma)
            (if (numberp (nth n lista))
                (setq suma (+ suma (nth n lista)))))))
(format t " Resultado: ~S~%~%" (Suma-numerica '(A B 1 2 3 C 5 D 6 7)))

;11 Escriba una función Filtra-Vocales que reciba como argumento una lista y, como respuesta entregue una copia de la lista argumento 
;en la cual se han removiso las letras vocales.
(format t "   Ejercicio 10: Filtra-Vocales ~%~%")
(defun Filtra-vocales(lista)
    (let ( (listaSinVocales ()))
        (dotimes (n (length lista) listaSinVocales)
            (if (listp (nth n lista))
                (setq listaSinVocales (append listaSinVocales (list (Filtra-vocales (nth n lista))))) ;Usa recursividad en caso de sublistas
                (if (not (or (eql (nth n lista) 'A ) (eql (nth n lista) 'E ) (eql (nth n lista) 'I ) (eql (nth n lista) 'O ) (eql (nth n lista) 'U )))
                    (setq listaSinVocales (append listaSinVocales (list (nth n lista)))))))))
(format t " Resultado: ~S~%~%"  (Filtra-vocales  '(a a f e b (i o (s u h) k j a) 5 A E I O U)))

(defun Filtra-vocales(lista)
    (let ( (listaSinVocales ()))
        (dotimes (n (length lista) listaSinVocales)
                (if (not (or (eql (nth n lista) 'A ) (eql (nth n lista) 'E ) (eql (nth n lista) 'I ) (eql (nth n lista) 'O ) (eql (nth n lista) 'U )))
                    (setq listaSinVocales (append listaSinVocales (list (nth n lista))))))))
(format t " Resultado con aplana: ~S~%~%"  (Filtra-vocales (aplana'(a a f e b (i o (s u h) k j a) 5 A E I O U) )))

;12 Construya una función Filtra-múltiplos que reciba como argumentos una lista y un número entero. Como respuesta debe de entregar 
;una copia de la lista argumento en la cual se han removido todos los multiplos del entero recibido.
(format t "   Ejercicio 12: Filtra-múltiplos ~%~%")

(defun Filtra-multiplos(lista multiplo)
    (let ((listaSinMultiplos ())           )
        (dotimes (n (length lista) listaSinMultiplos)
                (if (numberp (nth n lista))
                    (if (not (= 0 (mod (nth n lista) multiplo)))
                        (setq listaSinMultiplos (append listaSinMultiplos (list (nth n lista))))
                    )
                    (if (listp (nth n lista))
                        (setq listaSinMultiplos (append listaSinMultiplos (list (Filtra-multiplos (nth n lista) multiplo)))) ;Usa recursividad en caso de sublistas
                        (setq listaSinMultiplos (append listaSinMultiplos (list (nth n lista)))))))))
(format t " Resultado: ~S~%~%"(Filtra-multiplos '(1 2 3 (4 5) (6 (7 -1)) (-2 5) -3 -4 ) 2))

(defun Filtra(lista multiplo)
    (let ((listaSinMultiplos ())           )
        (dotimes (n (length lista) listaSinMultiplos)
                (if (numberp (nth n lista))
                    (if (not (= 0 (mod (nth n lista) multiplo)))
                        (setq listaSinMultiplos (append listaSinMultiplos (list (nth n lista))))
                    )))))
(format t " Resultado con aplana: ~S~%~%"(Filtra (aplana '(1 2 3 (4 5) (6 (7 -1)) (-2 5) -3 -4 )) 2))

;13 Defina la función celdas que recibe como argumento una lista y, como respuesta entrega el número de celdas de construcción que
;contiene la representación interna de la lista argumento.
(format t "   Ejercicio 13: Numero-celdas ~%~%")
(defun numero-Celdas(lista)
    (let ((numCeldas 0))
          (dotimes (n (length lista) numCeldas)
                (if (consp (nth n lista))
                    (setq numCeldas (+ numCeldas 1))
                    (if (listp (nth n lista))
                        (setq numCeldas (+ (numero-Celdas (nth n lista))))
                    )))))
(format t " Resultado: ~S~%~%" (numero-celdas '( (cons A  B) (cons a k) (s k (cons k j) k) A b ) )) 

;14 Construya una función Implica con aridad indeterminada, que implemente el operador lógico de la implicación.
(format t "   Ejercicio 14: Implica ~%~%")
(defun implica (&rest args)
  (cond ((< (length args) 2)
	 (print "Necesitas al menos 2 argumentos"))
	 (T
	 (let ((p (first args))
	       (q (second args))
	       (resto (rest (rest args))))
	   (do ((pimplicaq (or (or (and p q) (and (not p) q) (and (not p) (not q))) NIL)
			   (or (or (and p q) (and (not p) q) (and (not p) (not q))) NIL)))
	       ((null resto) pimplicaq)
	     (setq p pimplicaq)
	     (setq q (first resto))
	     (setq resto (rest resto)))))))

(format  t " Resultado: ~S~%~% " (implica nil t ))

;15 Escriba una función Mult que recibe como argumento dos lista, conteniendo sub-listas númericas representando matrices. 
;La función debe regresar la multiplicación de las dos matrices si es que estás son compatibles, en caso de no serlo debe de regresar NIL.
(format t "   Ejercicio 15: Mult ~%~%")
(defun Mult(A B)
    (if (= (length (nth 0 A)) (length B))
        (let ((res1 ())
        (res2 ())
        (res3 0)
        (n (length A))
        (m (length B))
        (p (length (nth 0 B))))
        (dotimes (i n res1)
        (dotimes (j p res2)
            (setq res3 0)
            (dotimes (k m res3)
                (setq res3 (+ res3 (* (nth k (nth i A)) (nth j (nth k B)))))
            )
            (setq res2 (append res2 (list res3)))
        )
        (push res2 res1)
        (setq res2 ())
    )
        (reverse res1))
        NIL
    )
)
(format t " Resultado: ~S ~%~%" (Mult '( (1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9) ) ))
(format t " Resultado: ~S ~%~%" (Mult '( (1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9) (3)) ) )