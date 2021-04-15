;;;**********************************************************************************************************************
;;; Ranas.lisp
;;;     Resuelve el problema de las Ranas y el estanque nocturno.
;;; La representación de los estados, elegida es:
;;;     Una lista con 8 elementos que representan las 6 ranas y el espacio que los separa y un elemento más para indicar que rana salta primero
;;; La estructura de los estados, es:
;;;      ((1 1 1 0 2 2 2 X))
;;;  X = 1, salta primero el lado izquierdo, 0 el lado derecho
;;;En donde el 1 representa a las ranas verdes, 2 a las ranas cafes y el 0 el espacio libre.
;;;         Estado Inicial:    EStado meta:
;;;         (1 1 1 0 2 2 2)  (2 2 2 0 1 1 1)
;;; Las posiciones son [1 2 3 4 3 2 1] --> Orden de los saltos
;;; Las restricciones asociadas al problema son:
;;; 1. Las ranas de color verde no pueden saltar a una roca cuya posicion sea menor a la posicion donde se encuentran.
;;; 2. Las ranas de color cafe no pueden saltar a una roca cuya posicion sea mayor a la posicion donde se encuentran.
;;;
;;; Tolentino Pérez Jazmin Yaneli, Abril 2021.
;;;**********************************************************************************************************************
;; Frontera de búsqueda
(defparameter *open* '())

;; Memoria de intentos previos
(defparameter *memory* '())

;; Definición de los operadores
(defparameter  *ops*  '( 
						 ;;Avanza una piedra, el número indica la posición
						 (:Avanza-p1		(-1 +1 0 0 0 0 0))
						 (:Avanza-p2		(0 -1 +1 0 0 0 0))
						 (:Avanza-p3		(0 0 -1 +1 0 0 0))
						 (:Avanza-p4		(0 0 0 -1 +1 0 0))
						 (:Avanza-p5		(0 0 0 0 -1 +1 0))
						 (:Avanza-p6 		(0 0 0 0 0 -1 +1))
						 ;;Para saltar una roca
						 (:Salta-unop1		(-1 0 +1 0 0 0 0))
						 (:Salta-unop2		(0 -1 0 +1 0 0 0))
						 (:Salta-unop3		(0 0 -1 0 +1 0 0))
						 (:Salta-unop4		(0 0 0 -1 0 +1 0))
						 (:Salta-unop5		(0 0 0 0 -1 0 +1))
						 ;;Para saltar dos rocas
						 (:Salta-dosp1		(-1 0 0 +1 0 0 0))
						 (:Salta-dosp2		(0 -1 0 0 +1 0 0))
						 (:Salta-dosp3		(0 0 -1 0 0 +1 0))
						 (:Salta-dosp4 		(0 0 0 -1 0 0 +1))
						 
						 ))

;; Identificador del último nodo creado
(defparameter *id* -1)

;; Id del ancestro común de todos los descendientes que se generen
(defparameter *current-ancestor* nil)

;; Lista donde se almacenará la solución recuperada de la memoria
(defparameter *solucion* nil)

;;;************************************************************************************************************************
;; Create-node (estado op)
;;      estado: Un estado del problema a resolver
;;    operador: El operador cuta aplicación genero el estado
;;;************************************************************************************************************************
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro "
	  ;;Incremento para que lo primero es procesarse sea la respuesta
      (incf  *id*) 
	  ;;Los nodos procesados son descendientes de *current-ancestor*
      (list  *id*  estado  *current-ancestor*  (first op)) ) 

;;;************************************************************************************************************************
;; INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;************************************************************************************************************************
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo :breath-first)
		          (setq *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *Open*))

(defun  barge-shore (estado)
     (if  (= 1 (eighth (first  estado)))  0  1))

;;;************************************************************************************************************************
;;VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos (El grnajero indica la orilla en la que se encuentra la barca)
;;;************************************************************************************************************************
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado..."  
  (let*  ( (orilla  (barge-shore  estado)) 
  			(uno (posicion (second op) 0 6) )                         
  		   	(cero (posicionN (second op) 0 6)))
  			(if (< uno cero) (return-from valid-operator? nil))		
  			(if (and (= uno 100) (= cero 100))
  				(return-from valid-operator? T)
  				(and (= (nth uno (nth orilla estado)) 0) (> (nth cero (nth orilla estado)) 0) )
  				)	
        )
   	)  

;;;************************************************************************************************************************
;; VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;;************************************************************************************************************************
(defun  valid-state? ( estado  op )
    (let* ((orilla  (flip (barge-shore  estado)))
			(uno (posicion (second op) 0 6) )                         
  		   	(cero (posicionN (second op) 0 6)))
    	(if (< uno cero) (return-from valid-state? nil))	;;VALIDAMOS EL ESTADO EN EL QUE NO ESTMOS 
  			(if (and (= uno 100) (= cero 100))
  				(return-from valid-state? T)
  				(and (= (nth cero (nth orilla estado)) 0) (> (nth uno (nth orilla estado)) 0)))))

;;;************************************************************************************************************************
;; Encuentra la posición
;;;************************************************************************************************************************
(defun posicion (op num paro)
	(cond((= (first op) 1) num)
		((= num paro) 100)
		(T (posicion (rest op) (+ num 1) paro))
		)		
	)

;;;************************************************************************************************************************
;; Encuentra la posición negativa
;;;************************************************************************************************************************
(defun posicionN (op num paro)
	(cond ((= (first op) -1) num)
		((= num paro) 100)
		(T (posicionN (rest op) (+ num 1) paro))
		)
	)

;;;************************************************************************************************************************
;; Encuentra cero
;;;************************************************************************************************************************
(defun encuentracero (estado num paro)
	(cond ((= (first estado) 0) T)
		((= num paro) nil)
		(T (encuentracero (rest estado) (+ num 1) paro))
		)
	)

;;;************************************************************************************************************************
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema
;;;************************************************************************************************************************
(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((orilla1  (first  estado))
	       (orilla2  (second  estado))
           ;;En el caso de que las ranas que salten sean las de la izquierda
	       (elm0   (first orilla1))
   	       (elm1   (second orilla1))
	       (elm2   (third  orilla1))
	       (elm3 	(fourth orilla1))
	       (elm4	(fifth orilla1))
	       (elm5 	(sixth orilla1))
	       (elm6	(seventh orilla1))
	       (elm7 	(eighth orilla1))
	       ;; Saltan ranas del lado derecho
	       (elm20   (first  orilla2))
	       (elm21   (second  orilla2))
	       (elm22   (third   orilla2))
	       (elm23 	(fourth orilla2))
	       (elm24	(fifth orilla2))
	       (elm25	(sixth orilla2))
	       (elm26	(seventh orilla2))
	       (elm27 	(eighth orilla2))
	       (orilla-barca  (barge-shore estado))
		   ;Etiqueta humana 
	       (operador (first op)))     
	 (case operador 
	    (:Avanza-p1  
                (if (= orilla-barca 0)  
	                    (list  (list  elm1 elm0 elm2 elm3 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm23 elm24 elm26 elm25 (flip elm27)))
                        (list  (list  elm0 elm1 elm2 elm3 elm4 elm6 elm5 (flip elm7))   (list  elm21 elm20 elm22 elm23 elm24 elm25 elm26 (flip elm27)))
                        ))
	    (:Avanza-p2 
                (if (= orilla-barca 0)  
	                    (list  (list  elm0 elm2 elm1 elm3 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm23 elm25 elm24 elm26 (flip elm27)))
                        (list  (list  elm0 elm1 elm2 elm3 elm5 elm4 elm6 (flip elm7))   (list  elm20 elm22 elm21 elm23 elm24 elm25 elm26 (flip elm27)))
                        ))
	    (:Avanza-p3  
	            (if (= orilla-barca 0)  
                        (list  (list  elm0 elm1 elm3 elm2 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm24 elm23 elm25 elm26 (flip elm27)))
	                    (list  (list  elm0 elm1 elm2 elm4 elm3 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm23 elm22 elm24 elm25 elm26 (flip elm27)))
                        ))
	    (:Avanza-p4 
	            (if (= orilla-barca 0)  
                        (list  (list  elm0 elm1 elm2 elm4 elm3 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm23 elm22 elm24 elm25 elm26 (flip elm27)))
	                    (list  (list  elm0 elm1 elm3 elm2 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm24 elm23 elm25 elm26 (flip elm27)))
                        ))
	    (:Avanza-p5   
	            (if (= orilla-barca 0)  
                        (list  (list  elm0 elm1 elm2 elm3 elm5 elm4 elm6 (flip elm7))   (list  elm20 elm22 elm21 elm23 elm24 elm25 elm26 (flip elm27)))
	                    (list  (list  elm0 elm2 elm1 elm3 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm23 elm24 elm25 elm26 (flip elm27)))
                        ))
	    (:Avanza-p6    
	            (if (= orilla-barca 0)  
                        (list  (list  elm0 elm1 elm2 elm3 elm4 elm6 elm5 (flip elm7))   (list  elm21 elm20 elm22 elm23 elm24 elm25 elm26 (flip elm27)))
	                    (list  (list  elm1 elm0 elm2 elm3 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm23 elm24 elm25 elm26 (flip elm27)))
                        ))
	    (:Salta-unop1 
                (if (= orilla-barca 0)  
	                    (list  (list  elm2 elm1 elm0 elm3 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm23 elm26 elm25 elm24 (flip elm27)))
	                    (list  (list  elm0 elm1 elm2 elm3 elm6 elm5 elm4 (flip elm7))   (list  elm21 elm21 elm20 elm23 elm24 elm25 elm26 (flip elm27)))
                    ))
	    (:Salta-unop2 
                (if (= orilla-barca 0)  
	                    (list  (list  elm0 elm3 elm2 elm1 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm25 elm24 elm23 elm26 (flip elm27)))
	                    (list  (list  elm0 elm1 elm2 elm5 elm4 elm3 elm6 (flip elm7))   (list  elm20 elm23 elm22 elm21 elm24 elm25 elm26 (flip elm27)))
                    ))
	    (:Salta-unop3  
                (if (= orilla-barca 0)  
	                    (list  (list  elm0 elm1 elm4 elm3 elm2 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm24 elm23 elm22 elm25 elm26 (flip elm27)))
	                    (list  (list  elm0 elm1 elm4 elm3 elm2 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm24 elm23 elm22 elm25 elm26 (flip elm27)))
                    ))
	    (:Salta-unop4  
                (if (= orilla-barca 0)  
	                    (list  (list  elm0 elm1 elm2 elm5 elm4 elm3 elm6 (flip elm7))   (list  elm20 elm23 elm22 elm21 elm24 elm25 elm26 (flip elm27)))
	                    (list  (list  elm0 elm3 elm2 elm1 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm25 elm24 elm23 elm26 (flip elm27)))
                    ))
	    (:Salta-unop5  
                (if (= orilla-barca 0)  
	                    (list  (list  elm0 elm1 elm2 elm3 elm6 elm5 elm4 (flip elm7))   (list  elm22 elm21 elm20 elm23 elm24 elm25 elm26 (flip elm27)))
	                    (list  (list  elm2 elm1 elm0 elm3 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm23 elm26 elm25 elm24 (flip elm27)))
                    ))
	    (:Salta-dosp1  
                (if (= orilla-barca 0)  
	                    (list  (list  elm3 elm1 elm2 elm0 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm26 elm24 elm25 elm23 (flip elm27)))
	                    (list  (list  elm0 elm1 elm2 elm6 elm4 elm5 elm3 (flip elm7))   (list  elm23 elm21 elm22 elm20 elm24 elm25 elm26 (flip elm27)))
                    ))
	    (:Salta-dosp2  
                (if (= orilla-barca 0)  
	                    (list  (list  elm0 elm4 elm2 elm3 elm1 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm25 elm23 elm24 elm22 elm26 (flip elm27)))
	                    (list  (list  elm0 elm1 elm5 elm3 elm4 elm2 elm6 (flip elm7))   (list  elm20 elm24 elm22 elm23 elm21 elm25 elm26 (flip elm27)))
                    ))
	    (:Salta-dosp3  
                (if (= orilla-barca 0)  
	                    (list  (list  elm0 elm1 elm5 elm3 elm4 elm2 elm6 (flip elm7))   (list  elm20 elm24 elm22 elm23 elm21 elm25 elm26 (flip elm27)))
	                    (list  (list  elm0 elm4 elm2 elm3 elm1 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm25 elm23 elm24 elm22 elm26 (flip elm27)))
                    ))
	    (:Salta-dosp4  
                (if (= orilla-barca 0)  
	                    (list  (list  elm0 elm1 elm2 elm6 elm4 elm5 elm3 (flip elm7))   (list  elm23 elm21 elm22 elm20 elm24 elm25 elm26 (flip elm27)))
	                    (list  (list  elm3 elm1 elm2 elm0 elm4 elm5 elm6 (flip elm7))   (list  elm20 elm21 elm22 elm26 elm24 elm25 elm23 (flip elm27)))
                    ))
	    (T "error"))))

;;;***************************************************************
;; EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;************************************************************************************************************************
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let* ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	         (setq  nuevo-estado  (apply-operator  op estado))
		 (when (and (valid-operator?  op  estado) 
			    (valid-state?  nuevo-estado op ) )
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )

;;;************************************************************************************************************************
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;************************************************************************************************************************
(defun  remember-state?  (estado  lista-memoria)
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*"
     (cond ((null  lista-estados-y-ops)  Nil)
	 		 ; Si se recuerda el primer elemento de la lista, filtrarlo.
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*) 
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  

;;;************************************************************************************************************************
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema.
;;       Extract-solution: Recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial.
;;       Display-solution: Despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema
;;;****************************************************************
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
	;Busca un nodo por su id, si lo encuentra regresa el nodo completo
     (labels ((locate-node  (id  lista)   
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        ;Agregar a la solución el nodo actual   
		 (push  current  *solucion*)     
		 (setq  current  (locate-node  (third  current) *memory*)))) 
	     *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solucion con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		  	;A partir de este estado inicial
		   (format t "Inicio en: ~A~%" (second  nodo)) 
	       ;;else
		   	;Imprimir el número de paso, operador y estado							;Se muestra cuando salta el del lado izquierdo
		   (format t "\(~2A\)  aplicando ~15A se llega a ~A~%"  i (fourth  nodo)  (first (second  nodo))))))) 

;;;************************************************************************************************************************
;; RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;************************************************************************************************************************
(defun reset-all () 
"Reinicia todas las variables globales para iniciar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil)
	  (tiempo1 (get-internal-run-time))
	  (tiempo2 0)
	  (tiempoTotal 0)
	  )

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
			        (null *open*))  do
	   (setq  nodo    (get-from-open)   
		     estado  (second  nodo)
		     operador  (third  nodo))
	   		(push  nodo  *memory*)
	   (cond    ((equal  edo-meta  estado)  
		                (format  t  "Exito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))
			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo)))))

      (setq tiempo2 (get-internal-run-time))
      (setq tiempoTotal (/ (- tiempo2 tiempo1) 1000))
      (format t "El tiempo total es: ~6$~%" tiempoTotal)

      )  )
(format t "     Busqueda por el método: depth-first ~%~%")			          
(blind-search '((1 1 1 0 2 2 2 1) (2 2 2 0 1 1 1 0)) '((2 2 2 0 1 1 1 0) (1 1 1 0 2 2 2 1)) :breath-first)

(format t "~%     Busqueda por el método: breath-first ~%~%")
(blind-search '((1 1 1 0 2 2 2 1) (2 2 2 0 1 1 1 0)) '((2 2 2 0 1 1 1 0) (1 1 1 0 2 2 2 1)) :depth-first)