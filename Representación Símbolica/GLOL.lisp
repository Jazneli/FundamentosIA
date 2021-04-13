;;;*******************************************************************************************************************************
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
;;;*******************************************************************************************************************************

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

;;;*******************************************************************************************************************************
;; Create-node (estado op)
;;      estado: Un estado del problema a resolver
;;    operador: El operador cuta aplicación genero el estado
;;;*******************************************************************************************************************************
(defun Create-node (estado op)
    "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
    ;;Incremento para que lo primero es procesarse sea la respuesta
    (incf *id*)
    ;;Los nodos procesados son descendientes de *current-ancestor*
    (list *id* estado *current-ancestor* (first op))
)

;;;*******************************************************************************************************************************
;; INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;*******************************************************************************************************************************
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil))
     )  
)

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*)
)

;;;*******************************************************************************************************************************
;; BARGE-SHORE (estado)
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;*******************************************************************************************************************************
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  
  0 - origen  1 - destino"
    ;;El granjero es el indicador de la posición de la barca, debido a que el es el remador.
     (if  (= 1 (third (first  estado)))  0  1)
) 

;;;*******************************************************************************************************************************
;;VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos (El grnajero indica la orilla en la que se encuentra la barca)
;;;*******************************************************************************************************************************
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
          (>=  oveja   (second (second op))))
          (>= legumbre (third(second op)))
    )
  )
)  

;;;*******************************************************************************************************************************
;; VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;        Es decir, no deben estar solos, en ninguna orilla, el lobo con la oveja, ni la oveja con las legumbres.
;;;*******************************************************************************************************************************
(defun valid-state? (estado)
    "Predicado. Valida un estado según las restricciones generales del problema
    El estado tiene una estructura [(<Lobo0><Oveja0><Legumbre0>) (<Lobo1><Oveja1><Legumbre1>)]"
    (let * ((orilla (flip (barge-shore estado)))
            (lobo (first (nth orilla estado)))
            (oveja (second (nth orilla estado)))
            (legumbre (third (nth orilla estado))))
        (and (or (> legumbre oveja) (zerop legumbre))
             (or (> oveja lobo) (zerop oveja))
        )
    )
)

;;;*******************************************************************************************************************************
;; APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema
;;;*******************************************************************************************************************************
(defun flip (bit) (boole BOOLE-XOR bit 1))

(defun apply-operator (op estado)
    "Obtiene el descendiente de [estado] al aplicarle [op] SIN VALIDACIONES"
              ;;Asignación de los datos a los elementos
     (let * ((orilla1 (first estado))
              (orilla2 (second estado))
              (lobo0 (first orilla1))
              (oveja0 (second orilla1))
              (legumbre0 (third orilla1))
              (granjero0 (fourth orilla1))
              (lobo1 (first orilla2))
              (oveja1 (second orilla2))
              (legumbre1 (third orilla2))
              (granjero1 (fourth orilla2))
              (orilla-barca (barge-shore estado))
              ;;Etiqueta humana del operador
              (operador (first op)))
        (case operador
           ;;Operador a aplicar (1 0 0)
          (:Pasa-Lobo 
            ;;Restar elementos de la orilla con la barca y sumarlos a la otra orilla
            (if (= orilla-barca 0)
              (list (list (- lobo0 1) oveja0 legumbre0 (flip granjero0)) (list (+ lobo1 1) oveja1 legumbre1 (flip granjero1)))
              (list (list (+ lobo0 1) oveja0 legumbre0 (flip granjero0)) (list (- lobo1 1) oveja1 legumbre1 (flip granjero1)))
            )
          )
          ;;Operador a aplicar (0 1 0)
          (:Pasa-Oveja
            ;;Restar elementos de la orilla con la barca y sumarlos a la otra orilla
            (if (= orilla-barca 0)
            (list (list lobo0 (- oveja0 1) legumbre0 (flip granjero0)) (list lobo1 (+ oveja1 1) legumbre1 (flip granjero1)))
            (list (list lobo0 (+ oveja0 1) legumbre0 (flip granjero0)) (list lobo1 (- oveja1 1) legumbre1 (flip granjero1)))
            )
          )
          ;;Operador a aplicar (0 0 1)
          (:Pasa-Legumbre
            ;;Restar elementos de la orilla con la barca y sumarlos a la otra orilla
            (if (= orilla-barca 0)
              (list (list lobo0 oveja0 (- legumbre0 1) (flip granjero0)) (list lobo1 oveja1  (+ legumbre1 1) (flip granjero1)))
              (list (list lobo0 oveja0 (+ legumbre0 1) (flip granjero0)) (list lobo1 oveja1  (- legumbre1 1) (flip granjero1)))
            )
          )
          ;;Operador a aplicar (0 0 0)
          (:Pasa-Granjero
            ;;Restar elementos de la orilla con la barca y sumarlos a la otra orilla
            (if (= orilla-barca 0)
              (list (list lobo oveja0 legumbre0 (flip granjero0)) list(lobo1 oveja1 legumbre1 (flip granjero1)))
              (list (list lobo oveja0 legumbre0 (flip granjero0)) list(lobo1 oveja1 legumbre1 (flip granjero1)))
            )
          )
          (T "error")
        )
      )
)

;;;*******************************************************************************************************************************
;; EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;*******************************************************************************************************************************
(defun expand (estado)
  "Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo orden"
  (let* ((descendientes nil)
          (nuevo-descendiente nil)
    )
    (dolist (op *ops* descendientes)
      (print (second op))
      (setq nuevo-estado (apply-operator op estado))
      (print nuevo-estado)
      (when (and (valid-operator? op estado)
                  (valid-state? nuevo-estado)
            )
            (print (valid-operator? op estado))
            (print (valid-sate? nuevo-estado))
            (setq descendientes (cons (list nuevo-estado op) descendientes))
      )
    )
  )
)

;;;*******************************************************************************************************************************
;; 
;;;*******************************************************************************************************************************