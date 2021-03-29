;TOLENTINO PÉREZ JAZMIN YANELI
;Paquete 3, Common Lisp.

(format t "~%           PAQUETE DE EJERCICIOS Num.3 ~%~%")
;1 Resuelva de nuevo los 15 ejercicios del paquete anterior pero ahora en forma recursiva.

;1
(format t "   Ejercicio 1: ElemInPos ~%~%")
(defun ElemInPos (elem lista pos)
  (cond ((null lista) NIL)
	((= pos 0)
	 (if (equal (first lista) elem) T NIL))
	(T (ElemInPos elem (rest lista) (1- pos)))))

(format t " Resultado: ~S~% "(ElemInPos '3 '( 1 4 B 3 D) '3))
(format t "Resultado: ~S~%~% "(ElemInPos '5 '( 1 4 B 3 D) '7))

;2
(format t "   Ejercicio 2: Inicio-en ~%~%")
(defun Inicio-en (lista elemento)
  (Inicio-en-aux lista elemento () NIL)) ;lista elem resultado flag

(defun Inicio-en-aux (lista elemento resultado encontrado)
  (cond ((null lista) resultado)
	((and (not encontrado) (equal elemento (first lista)))
	 (setq resultado (append (list elemento) resultado))
	 (Inicio-en-aux (rest lista) elemento resultado T))
	(T (setq resultado (append resultado (list (first lista))))
	(Inicio-en-aux (rest lista) elemento resultado NIL))))

(format t " Resultado: ~S~%~% "(Inicio-en '( 1 2 3 4 5) '3))

;3
(format t "   Ejercicio 3: Termina-en ~%~%")
(defun Termina-en (lista elemento)
  (Termina-en-aux (reverse lista) elemento () nil));lista elem resultado flag

(defun Termina-en-aux (lista elemento resultado encontrado)
  (cond ((null lista) resultado)
	((and (not encontrado) (equal elemento (first lista)))
	 (setq resultado (append resultado (list (first lista))))
	 (Termina-en-aux (rest lista) elemento resultado T))
	(T (setq resultado (append (list (first lista)) resultado))
	 (Termina-en-aux (rest lista) elemento resultado NIL))))

(format t " Resultado: ~S~%~% "(Termina-en '( 1 2 3 4 5) '3))

;4
(format t "   Ejercicio 4: Primer-impar ~%~%")
(defun Primer-impar (lista)
  (Primer-impar-aux lista 0))

(defun Primer-impar-aux (lista contador)
  (cond ((null lista) (list NIL NIL))
	((and (numberp (first lista)) (oddp (first lista)))
	 (list (first lista) contador))
	(T (Primer-impar-aux (rest lista) (1+ contador)))))

(format t " Resultado: ~S~% " (Primer-impar '(4 2 B (8 C) 6 A (A B) 8 2 7)))
(format t "Resultado: ~S~%~% " (Primer-impar '(A (3 4) 1 2 B 5)))

;5
(format t "   Ejercicio 5: último-elemento ~%~%")
(defun Ultimo-elemento (lista)
  (ultimo-elemento-aux (reverse lista) 'NoEncontrado 0))

(defun ultimo-elemento-aux (lista elemento contador)
  (let ((primero (first lista)))
    (cond ((null lista) (list elemento contador))
	      ((= contador 0)
	       (if (and (numberp primero) (>= primero 0))
		   (ultimo-elemento-aux (rest lista) primero 1)
		   (ultimo-elemento-aux (rest lista) elemento 0)))
	      ((and (numberp primero) (= primero elemento))
	       (ultimo-elemento-aux (rest lista) elemento (1+ contador)))
	      (T (ultimo-elemento-aux (rest lista) elemento contador)))))

(format t " Resultado: ~S~%~%" (Ultimo-elemento '(1 0 5 1 3 -5 1 2 -3 -1 0)))

;6
(format t "   Ejercicio 6: Conteo~%~%")
(defun Conteo(lista &optional (B 0) (C 0))
    (if (not (= 0 (length lista)))
        (if (not (numberp (first lista)))
            (if (listp (first lista))
                (Conteo (rest lista) B  (+ C 1) )    
                (Conteo (rest lista) B C))
            (Conteo (rest lista) (+ B 1) C))
        (cons B C)))

(format t " Resultado: ~S ~%~%" (Conteo  '(A B '(3 4) 2 '(B S 4) 8 6 A 8 '( ) 7) ) )

;7
(format t "   Ejercicio 7: Aplana~%~%")
(defun aplana (lista)
    (if (null lista)
        nil
        (if (atom (first lista))
            (cons (first lista) (aplana (rest lista)))
            (append (aplana (first lista)) (aplana (rest lista))))))

(format t " Resultado: ~S~%~%"(aplana '(A B C (D E) (F (0 -1)) (G) H I (J K (L (M (7 -7)))))))

;8
(format t "   Ejercicio 8: Diagonal~%~%")
(defun Diagonal (lista)
  (diagonal-aux lista () (length lista)))

(defun diagonal-aux (lista respuesta m)
  (cond ((null lista) respuesta)
	(T (setq respuesta (append respuesta (list (nth (ash m -1) (first lista)))))
	 (diagonal-aux (rest lista) respuesta m))))

(format t " Resultado: ~S ~%~%" (Diagonal  '( (1 2 3) (4 5 6) (7 8 9 10))))

;9
(format t "   Ejercicio 9: Tipos Lista ~%~%")
(defun Tipos(lista &optional B)
    (if (not (= 0 (length lista)))
        (if (not (atom (first lista)))
            (if (and (listp (first lista))  (= 0 (length (nth 1 (first lista)))))  
                (Tipos (rest lista) (append B (list 'N)))
                (Tipos (rest lista) (append B (list 'L))))
           (Tipos (rest lista) (append B (list 'A)))    )B))

(format t " Resultado: ~S~%~%" (Tipos '(A B '(C D '(1 2) 3)  E 4 '() 5 6 B) ))

;10
(format t "   Ejercicio 10: Suma-numérica ~%~%")
(defun Suma-numerica(lista &optional (B 0))
    (if (not (= 0 (length lista)))
        (if (numberp (first lista))
            (Suma-numerica (rest lista) (+ B (first lista)) )
            (Suma-numerica (rest lista) B )
        )B))

(format t " Resultado: ~S~%~%" (Suma-numerica '(A B 1 2 3 C 5 D 6 7)))

;11
(format t "   Ejercicio 11: Filtra-Vocales ~%~%")
(defun Filtra-Vocales (lista)
  (filtra-vocales-aux lista '()))
  
(defparameter vocales '(A E I O U a e i o u))
(defun filtra-vocales-aux (lista acumulado)
  (cond ((null lista) acumulado)
	((listp (first lista))
	 (setq acumulado (append acumulado (list (filtra-vocales-aux (first lista) '()))))
	 (filtra-vocales-aux (rest lista) acumulado))
	((member (first lista) vocales)
	 (filtra-vocales-aux (rest lista) acumulado))
	(T (setq acumulado (append acumulado (list (first lista))))
	   (filtra-vocales-aux (rest lista) acumulado))))

(format t " Resultado: ~S~%~%"  (Filtra-Vocales  '(a a f e b (i o (s u h) k j a) 5 A E I O U)))

;12
(format t "   Ejercicio 12: Filtra-múltiplos ~%~%")
(defun Filtra-multiplos (lista numero)
  (filtra-multiplos-aux lista numero ()))

(defun filtra-multiplos-aux (lista numero resultado)
  (cond ((null lista) resultado)
	((and (numberp (first lista)) (= (mod (first lista) numero) 0))
	 (filtra-multiplos-aux (rest lista) numero resultado))
	(T (setq resultado (append resultado (list (first lista))))
	   (filtra-multiplos-aux (rest lista) numero resultado))))

(format t " Resultado: ~S~%~%"(Filtra-multiplos '(1 2 3 (4 5) (6 (7 -1)) (-2 5) -3 -4 ) 2))

;13
(format t "   Ejercicio 13: Numero-celdas ~%~%")
(defun numero-celdas (lista) 
    (let ((no_celdas 0)
        (aux NIL))
        (loop for i from 0 to (- (length lista) 1) do
            (setq aux (nth i lista))
            (cond ((consp aux) 
                        (setq no_celdas (+ 1 no_celdas)))
                    ((listp aux)
                        (setq no_celdas (+ no_celdas (numero-celdas aux))))))
        (setq no_celdas no_celdas)))
        
(format t " Resultado: ~S~%~%" (numero-celdas '( (cons A  B) (cons a k) (s k (cons k j) k) A b ) )) 

;14
(format t "   Ejercicio 14: Implica ~%~%")
(defun Implica (&rest args)
  (cond ((< (length args) 2)
	 (print "Necesitas al menos 2 argumentos"))
	(T
	 (implica-aux (first args) (second args) (rest (rest args))))))

(defun Implica-aux (p q resto)
  (let ((pimplicaq (or (or (and p q) (and (not p) q) (and (not p) (not q))) NIL)))
    (cond ((null resto) pimplicaq)
	  (T (Implica-aux pimplicaq (first resto) (rest resto))))))

(format t " Resultado: ~S~%~% " (Implica nil t nil t nil t nil))

;15
(format t "   Ejercicio 15: Mult ~%~%")
(defun Mult (a b)
  (cond ((/= (length (first a)) (length b)) "Las matrices no se pueden operar")
	(T (multiplica-aux a b '()))))

(defun multiplica-renglon (renglonA matrizB)
  (let ((renglon '())
	(suma 0))
    (dotimes (j (length (first matrizB)) renglon)
      (setq suma 0)
      (dotimes (i (length renglonA))
	(setq suma (+ suma (* (nth i renglonA) (nth j (nth i matrizB))))))
      (setq renglon (append renglon (list suma))))))

(defun multiplica-aux (a b resultado)
  (cond ((null a) resultado)
	(T (setq resultado (append resultado (list (multiplica-renglon (first a) b))))
	   (multiplica-aux (rest a) b resultado))))

(format t " Resultado: ~S ~%~%" (Mult '( (1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9) ) ))
(format t " Resultado: ~S ~%~%" (Mult '( (1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9) (3)) ) )

;16. Defina una función recursiva Cambia que reciba como argumento una lista y dos elementos
;elem1, elem2. Como respuesta, la función debe entregar otra lista parecida a la original, pero
;donde todas las ocurrencias de elem1 se substituyeron por elem2.
(format t "   Ejercicio 16: Cambia ~%~%")
(defun Cambia (lista elem1 elem2)
  (cambia-aux lista elem1 elem2 nil))
(defun Cambia-aux (lista elem1 elem2 resultado)
  (cond ((null lista) resultado)
	((equal (first lista) elem1)
	 (Cambia-aux (rest lista) elem1 elem2 (append resultado (list elem2))))
	(T (Cambia-aux (rest lista) elem1 elem2 (append resultado (list (first lista)))))))

(format t " Resultado: ~S~%~%" (Cambia '(A B C D E) 'C 'X))

;17. En el URL http://www.cliki.net/fibonacci se presentan diversas implementaciones para los
;números de Fibonacci. Implemente TODAS las opciones que ahí se presentan y compare su
;desempeño con time para el argumento 50.
(format t "   Ejercicio 17: Fibonacci ~%~%")
(defun fib1 (n)
  (check-type n (integer 0 *))
  (if (< n 2) n
      (+ (fib1 (1- n)) (fib1 (- n 2)))))
;(time (fib1 50))

(defun fib2 (n)
  (check-type n (integer 0 *))
  (labels ((fib2-aux (n f1 f2)
	     (if (zerop n) f1
		 (fib2-aux (1- n) f2 (+ f1 f2)))))
    (fib2-aux n 0 1)))

(defun fib3 (n)
  (check-type n (integer 0 *))
   (loop for f1 = 0 then f2
	 and f2 = 1 then (+ f1 f2)
	 repeat n finally (return f1)))

(defun fib4 (n)
  (check-type n (integer 0 *))
  (do ((i n (1- i))
       (f1 0 f2)
       (f2 1 (+ f1 f2)))
      ((= i 0) f1)))

(defun fib5 (n)
  (check-type n (integer 0 *))
  (labels ((fib5-aux (n k)
	     (if (zerop n) 
		 (funcall k 0 1)
		 (fib5-aux (1- n) (lambda (x y)
				    (funcall k y (+ x y)))))))
    (fib5-aux n #'(lambda (a b) a))))

(defun fib6 (n)
  (labels ((fib6-aux (n)
	     (cond ((= n 0) (values 1 0))
		   (T
		    (multiple-value-bind (val prev-val)
			(fib6-aux (- n 1))
		      (values (+ val prev-val)
			      val))))))
    (nth-value 0 (fib6-aux n))))

(defun fib7 (n)
  (check-type n (integer 0 *))
  (labels ((fib7-aux (a b p q count)
	     (cond ((= count 0) b)
		   ((evenp count)
		    (fib7-aux a
			      b
			      (+ (* p p) (* q q))
			      (+ (* q q) (* 2 p q))
			      (/ count 2)))
		   (t (fib7-aux (+ (* b q) (* a q) (* a p))
				(+ (* b p) (* a q))
				p
				q
				(- count 1))))))
    (fib7-aux 1 0 0 1 n)))

(defun fib8 (n)
  (if (< n 2) n
      (if (oddp n) 
	  (let ((k (/ (1+ n) 2)))
	    (+ (expt (fib8 k) 2) (expt (fib8 (1- k)) 2)))
	  (let* ((k (/ n 2)) (fk (fib8 k)))
	    (* (+ (* 2 (fib8 (1- k))) fk) fk)))))

;18. Defina una función recursiva Mapea que opere exactamente igual que la función mapcar de
;Common Lisp.
(format t "   Ejercicio 18: Mapea ~%~%")
(defun Mapea (funcion arg)
    (let ((mismo_tam T)
            (aux_tam (length (nth 0 arg)))
            (aux_fun (first arg))
            (aux '())
            (res '()))
        (cond ((not (equalp aux_fun NIL))
            (loop for i from 1 to (- (length arg) 1) do
                (setq mismo_tam (and mismo_tam (equalp (length (nth i arg)) aux_tam)))))
                ( (equalp aux_fun NIL)
                    (setq mismo_tam NIL)) )       
        (cond ((equalp mismo_tam T)   
            (setq aux_fun (nth 0 (nth 0 arg)))                                         
            (loop for i from 1 to (- (length arg) 1) do                                             
            (setq aux_fun (funcall funcion aux_fun (nth 0 (nth i arg)) ))  )     
            (loop for i from 0 to (- (length arg) 1) do 
              (setq aux (append aux (list (rest (nth i arg))))))                        
             (setq res (if (>= (length (nth 0 aux)) 1) (append res (list aux_fun) (Mapea funcion aux)) (append res (list aux_fun)))) )  )
        (setq res res)))

(format t " Resultado: ~S ~%~%" (Mapea #'+ (list(list 1 3 3) (list 1 2 3) (list 1 3 3)) ))

;19 Solución ejercicio 9

;20 Defina una función recursiva Elimina que reciba como argumento una lista y un número real
;n. La función debe entregar como resultado una copia de la lista original, en la cual se hayan
;eliminado todos los elementos que no sean numéricos, así como todos aquellos elementos
;numéricos que sean menores o iguales que n
(format t "   Ejercicio 20: Elimina ~%~%")
(defun elimina (lista n)
  (elimina-aux #'(lambda (x &optional (y n)) (or (not (numberp x)) (<= x y))) lista))

(defun elimina-aux (funcion lista)
  (cond ((null lista) nil)
	((funcall funcion (first lista)) 
	 (elimina-aux funcion (rest lista)))
	(T (cons (first lista) (elimina-aux funcion (rest lista))))))

(format t " Resultado ~S~%~% " (elimina '(A B 1 2 3 4 5 C D 6 7 8 9 ) 5))

;21. Defina una función recursiva PegaYCambia que reciba como argumento dos listas lista1
;lista2 y dos elementos elem1, elem2. Como respuesta, la función debe entregar una lista donde
;concatene las dos listas originales, pero substituyendo todas las ocurrencias (en ambas listas) de
;elem1 por elem2.
(format t "   Ejercicio 21: PegaYCambia ~%~%")
(defun PegaYCambia (lista1 lista2 elem1 elem2)
  (combina #'(lambda (x &optional(y elem1)) (equal x y)) (append lista1 lista2) elem2))

(defun combina (funcion lista elem2)
  (cond ((null lista) nil)
	((funcall funcion (first lista))
	 (cons elem2 (combina funcion (rest lista) elem2)))
	(T (cons (first lista) (combina funcion (rest lista) elem2)))))

(format t " Resultado: ~S~%~%"(PegaYCambia '(A B C X ((7 8 9))) '( 7 -2 3 (A B) () X ) 'X 'CAMBIO))

;22. Defina una función QSort que reciba como argumento único una lista e implemente con
;ellos el algoritmo de ordenamiento Quick Sort, ignorando por completo aquellos elementos de
;la lista original que no sean numéricos. La respuesta de la función debe ser una lista con los
;elementos numéricos de la original ordenados de forma ascendente.
(format t "   Ejercicio 22: Quicksort ~%~%")

(defun quicksort (A)
    (when A
        (let ( (pivot (first A))  (A_rest (rest A) )  )
            (let ((left (remove-if-not (lambda (x) (< x pivot)) A_rest))
                    (right (remove-if-not (lambda (x) (>= x pivot)) A_rest)))
                (append (quicksort left) (list pivot) (quicksort right))
            )
        )
    )
)

(format t " Resultado ~S~%~% " (quicksort '(4 9 2 1 6 3 8)))