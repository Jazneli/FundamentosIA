;;;**********************************************************************************************************************
;;;  GLOL.lisp
;;;     Resuelve el problema de Granjero, Lobo, Oveja y Legumbre con búsqueda ciega, a lo profundo y a lo ancho.
;;;
;;; La representación de los estados, elegida es:
;;;     Lista con dos sublistas internas, una por cada orilla.
;;;     La estructura de los estados, es:
;;;         ((Lobo Oveja Legumbre Barca) (Lobo Oveja Legumbre Barca))
;;;     Considerando que el granjero, representa tambien la posición de la barca.
;;;     En cada orilla, Lobo (Lo), Oveja (O), Legumbre (Le) y Granjero(G).
;;;             Estado inicial:             Estado Meta:
;;;           Lo O Le G  Lo O Le G       Lo O Le G Lo O Le G
;;;          ((1 1 1 1)  (0 0 0 0))     ((0 0 0 0)  (1 1 1 1))
;;; 
;;; El problema consiste: un granjero desea cruzar el río, acompañado de su lobo, oveja y una caja de Legumbres
;;; Las restricciones del problema son las siguientes:
;;;     1. En la lancha sólo cabe uno de sus elementos, además del granjero como remero.
;;;     2. No deben estar solos, en ninguna orilla, el lobo con la oveja,
;;;       ni la oveja con las legumbres.
;;;
;;; Tolentino Pérez Jazmin Yaneli, Abril 2021.
;;;************************************************************************************************************************

;; Frontera de búsqueda
(defparameter *open* '())

;; Memoria de intentos previos
(defparameter *memory* '())

;; Definición de los operadores
(defparameter *ops* '( (:Pasa-Lobo     (1 0 0))
                       (:Pasa-Oveja    (0 1 0))
                       (:Pasa-Legumbre (0 0 1))
                       (:Pasa-Granjero (0 0 0))
                      )
)

;; Identificador del último nodo creado
(defparameter *id* -1)

;; Id del ancestro común de todos los descendientes que se generen
(defparameter *current-ancestor* nil)

;; Lista donde se almacenará la solución recuperada de la memoria
(defparameter *solucion* nil)

;;Indicadores de desempeño
(defparameter *nodos_creados* 0)
(defparameter *nodos_expandidos* 0)
(defparameter *longitud_maxima_frontera* 0)
(defparameter *tiempo1* 0)
(defparameter *tiempo2* 0)
(defparameter *tiempoTotal* 0)
;;;************************************************************************************************************************
;; Create-node (estado op)
;;      estado: Un estado del problema a resolver
;;    operador: El operador cuta aplicación genero el estado
;;;************************************************************************************************************************
(defun Create-node (estado op)
    "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
    ;;Incremento para que lo primero es procesarse sea la respuesta
    (incf *id*)
    ;;Incremento nodos creados
    (incf *nodos_creados*)
    ;;Los nodos procesados son descendientes de *current-ancestor*
    (list *id* estado (first op) *current-ancestor* )
)

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
         (incf *longitud_maxima_frontera*)
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo)))))))

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*)
)

;;;************************************************************************************************************************
;; BARGE-SHORE (estado)
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;************************************************************************************************************************
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  
  0 - origen  1 - destino"
    ;;El granjero es el indicador de la posición de la barca, debido a que el es el remador.
    (if (= (fourth (first estado)) 1) 0 1)
) 

(defun orilla-sin-granjero (estado)
  (if (= (fourth (first estado)) 1) 1 0))
;;;************************************************************************************************************************
;;VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos (El grnajero indica la orilla en la que se encuentra la barca)
;;;************************************************************************************************************************
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [(<Lobo0><Oveja0><Legumbre0>) (<Lobo1><Oveja1><Legumbre1>)],
     el operador tiene estructura : [<etiqueta-humana> <lista operador con (<num Lobo><num Oveja><num Legumbre>)>]"  
  (let*  ((orilla  (barge-shore  estado))    
  ;;Obtenemos el valor del lobo, oveja y legumbre                     
	    (lobo  (first  (nth  orilla  estado)))   
	    (oveja   (second  (nth  orilla  estado)))
        (legumbre (third (nth orilla estado))))
  ;;Verificar si es posible realizar el movimiento
    (and  (>=  lobo  (first (second op)))        
          (>=  oveja   (second (second op)))
          (>= legumbre (third (second op))))))

;;;************************************************************************************************************************
;; VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;        Es decir, no deben estar solos, en ninguna orilla, el lobo con la oveja, ni la oveja con las legumbres.
;;;************************************************************************************************************************
(defun valid-state? (estado)
    "Predicado. Valida un estado según las restricciones generales del problema
    El estado tiene una estructura [(<Lobo0><Oveja0><Legumbre0>) (<Lobo1><Oveja1><Legumbre1>)]"
    (let* ((orilla (orilla-sin-granjero estado))
      (lobo (first (nth orilla estado)))
      (oveja (second (nth orilla estado)))
      (legumbre (third (nth orilla estado))))
      (and (or (> oveja lobo) (zerop oveja))
           (or (> legumbre oveja) (zerop legumbre)))))

;;;************************************************************************************************************************
;; APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema
;;;************************************************************************************************************************
(defun flip (bit) (boole BOOLE-XOR bit 1))

(defun apply-operator (op estado)
    "Obtiene el descendiente de [estado] al aplicarle [op] SIN VALIDACIONES"
              ;;Asignación de los datos a los elementos
     (let* ((orilla_barca (barge-shore estado))
              (lobo0 (first (first estado)))
              (oveja0 (second (first estado)))
              (legumbre0 (third (first estado)))
              (granjero0 (fourth (first estado)))
              (lobo1 (first (second estado)))
              (oveja1 (second (second estado)))
              (legumbre1 (third (second estado)))
              (granjero1 (fourth (second estado))))
              ;;Etiqueta humana del operador
        (case (first op)
           ;;Operador a aplicar (1 0 0)
          (:Pasa-Lobo 
            ;;Restar elementos de la orilla con la barca y sumarlos a la otra orilla
            (if (= orilla_barca 0)
              (list (list 0 oveja0 legumbre0 (flip granjero0)) (list 1 oveja1 legumbre1 (flip granjero1)))
	            (list (list 1 oveja0 legumbre0 (flip granjero0)) (list 0 oveja1 legumbre1 (flip granjero1)))
            )
          )
          ;;Operador a aplicar (0 1 0)
          (:Pasa-Oveja
            ;;Restar elementos de la orilla con la barca y sumarlos a la otra orilla
            (if (= orilla_barca 0)
	            (list (list lobo0 0 legumbre0 (flip granjero0)) (list lobo1 1 legumbre1 (flip granjero1)))
	            (list (list lobo0 1 legumbre0 (flip granjero0)) (list lobo1 0 legumbre1 (flip granjero1))))
          )
          ;;Operador a aplicar (0 0 1)
          (:Pasa-Legumbre
            ;;Restar elementos de la orilla con la barca y sumarlos a la otra orilla
           (if (= orilla_barca 0)
	            (list (list lobo0 oveja0 0 (flip granjero0)) (list lobo1 oveja1 1 (flip granjero1)))
	            (list (list lobo0 oveja0 1 (flip granjero0)) (list lobo1 oveja1 0 (flip granjero1)))))
          ;;Operador a aplicar (0 0 0)
          (:Pasa-Granjero
            (list (list lobo0 oveja0 legumbre0 (flip granjero0)) (list lobo1 oveja1 legumbre1 (flip granjero1)))))))


;;;************************************************************************************************************************
;; EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;************************************************************************************************************************
(defun expand (estado)
  "Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo orden"
  (let ((descendientes nil)
          (nuevo-estado nil)
    )
    (incf *nodos_expandidos*)
    (dolist (op *ops* descendientes)
      ;; primero se aplica el operador  y  después
      (setq nuevo-estado (apply-operator op estado))
      ; se valida el resultado
      (when (and (valid-operator? op estado)
                  (valid-state? nuevo-estado)
            )
        (setq descendientes (cons (list nuevo-estado op) descendientes))
      )
    )
  )
)

;;;************************************************************************************************************************
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;************************************************************************************************************************
(defun remember-state? (estado lista-memoria)
  "Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura: [(<Lobo0><Oveja0><Legumbre0>) (<Lobo1><Oveja1><Legumbre1>)]
     el nodo tiene estructura : [<id> <estado> <id-ancestro> <operador>]"
  (cond ((null lista-memoria) nil)
      ;;el estado es igual al que se encuentra en el nodo?
      ((equal estado (second (first lista-memoria))) T)
      (T (remember-state? estado (rest lista-memoria)))
  )
)

(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  nil)
          ; Si se recuerda el primer elemento de la lista, filtrarlo.
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  
		       (filter-memories  (rest  lista-estados-y-ops)))
      (T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))
    ) 
)  

;;;************************************************************************************************************************
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema.
;;       Extract-solution: Recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial.
;;       Display-solution: Despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema
;;;************************************************************************************************************************
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
      ;Busca un nodo por su id, si lo encuentra regresa el nodo completo
     (labels ((locate-node  (id  lista)     
		  (cond ((null  lista)  nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current (locate-node  (first  nodo)  *memory*)))
	     (loop while (not (null  current))  do             
      ;Agregar a la solución el nodo actual           
		      (push current  *solucion*)  
     ; Y luego regresa a su antecesor
		      (setq current  (locate-node  (fourth current) *memory*)))) 
	     *solucion*)
) 

(defun display-solution (solucion)
  "Despliega la solución en forma conveniente y numerando los pasos"
    (format t "Solución con ~A pasos:~%~%" (1- (length solucion)))
    (let ((nodo nil))
      (dotimes (i (length solucion))
        (setq nodo (nth i solucion))
        (if (= i 0)
            ;A partir de este estado inicial
          (format t "Inicio en: ~A~%"(second nodo))
          ;else
            ;Imprimir el número de paso, operador y estado
          (format t "\(~2A\) aplicando ~20A se llega a ~A~%" i (third nodo) (second nodo))
        )
      )
    )
)

;;;************************************************************************************************************************
;; RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;************************************************************************************************************************
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  -1)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))

(defun blind-search (edo-inicial edo-meta metodo)
    "Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                              :breath-first - búsqueda en anchura"
    (reset-all)
    (let ((nodo nil)
          (estado nil)
          (sucesores nil)
          (operador nil)
          (meta-encontrada nil)
          )

      (insert-to-open edo-inicial nil metodo)
      (loop until (or meta-encontrada (null *open*)) do
        (setq nodo     (get-from-open))
        (setq estado   (second nodo))
        (setq operador (third nodo))
        (push nodo *memory*)
        (cond ((equal estado edo-meta)
          (format t "Exito. Meta encontrada en ~A intentos ~%" (first nodo))
          (display-solution  (extract-solution  nodo))
          (setq meta-encontrada T))
          (t (setq *current-ancestor* (first nodo))
          (setq sucesores (expand estado))
          (setq sucesores (filter-memories sucesores))
          (loop for element in sucesores do
            (insert-to-open (first element) (second element) metodo)))))
    )
)

;;;************************************************************************************************************************
;;MOSTRAR INDICADORES
;;;************************************************************************************************************************
(defun mostrar-indicadores ()
  (format t "~%Nodos creados: ~A~%" *nodos_creados*)
  (format t "Nodos expandidos: ~A~%" *nodos_expandidos*)
  (format t "Longitud maxima de la frontera de busqueda: ~A~%" *longitud_maxima_frontera*)
  (format t "Longitud de la solucion: ~A operadores~%" (1- (length *solucion*)))
  (format t "Tiempo para encontrar la solucion: ~6$ segundos~%" *tiempoTotal*))

;;;************************************************************************************************************************
(format t "     Busqueda por el método: depth-first ~%~%")
(setq *tiempo1* (get-internal-run-time))
(blind-search '((1 1 1 1) (0 0 0 0)) '((0 0 0 0) (1 1 1 1)) ':depth-first)
(setq *tiempo2* (get-internal-run-time))
(setq *tiempoTotal* (/ (- *tiempo2* *tiempo1*) (get-internal-real-time)))
(mostrar-indicadores)

(format t "~%     Busqueda por el método: breath-first ~%~%")
(setq *tiempo1* (get-internal-run-time))
(blind-search '((1 1 1 1) (0 0 0 0)) '((0 0 0 0) (1 1 1 1)) ':breath-first)
(setq *tiempo2* (get-internal-run-time))
(setq *tiempoTotal* (/ (- *tiempo2* *tiempo1*) (get-internal-real-time)))
(mostrar-indicadores)
