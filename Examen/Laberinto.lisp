;;;**********************************************************************************************************************
;; Laberinto.lisp
;;     Se resuelve el problema de encontrar la salida a un laberinto apartir de un estado inicial.
;;     Con diferentes metodos, en 2 y 3 dimensiones.
;;
;; Tolentino Pérez Jazmin Yaneli, Abril 2021.
;;;**********************************************************************************************************************

;Librería
(load "maze_lib.lisp")

;Algoritmos a agregarse al menú de la página.
(add-algorithm 'A*-Search)
(add-algorithm 'Best-First-Search)
(add-algorithm 'Depth-First-Search)

;; Frontera de búsqueda
(defparameter *open* '())

;; Memoria de intentos previos
(defparameter *memory* '())

;;Estado Meta
(defparameter *edoMeta* nil)

;;Límites en X y Y
(defparameter *limiteEnX* 0)
(defparameter *limiteEnY* 0)

;; Id del ancestro común de todos los descendientes que se generen
(defparameter *current-ancestor* nil)

;; Identificador del último nodo creado
(defparameter *id* 0)

;;Profundidad del ancestro 
(defparameter *profundidad* -1)

;; Definición de los operadores
(defparameter *operadores* '(
                ;;Solo se consideran los movimientos básicos
                (:Mover-Arriba           (-1  0) 0)
                (:Mover-Derecha          ( 0  1) 2)
                (:Mover-Abajo            ( 1  0) 4)
                (:Mover-Izquierda        ( 0 -1) 6)
                ;;Movimientos subiendo el puente
                (:Mover-Arriba-Puente    (-2  0) 0)
                (:Mover-Derecha-Puente   ( 0  2) 2)
                (:Mover-Abajo-Puente     ( 2  0) 4)
                (:Mover-Izquierda-Puente ( 0 -2) 6)
))

;;;************************************************************************************************************************
;; Create-node (estado operador aptitud)
;;  ESTRUCTURA DE UN NODO
;;    (<id> <estado> <aptitud> <ancestro> <operador> <profundidad>)
;;;************************************************************************************************************************
(defun create-node (estado operador aptitud)
    "Se crea un nuevo nodo, recibimos como parametros el estado, operador y la aptitud"
    ;;Incremento para que lo primero es procesarse sea la respuesta
    (incf *id*)
    ;;Los nodos procesados son descendientes de *current-ancestor*
    (list *id* estado aptitud *current_ancestor* (third operador) ( 1+ *profundidad*))
)

;;;************************************************************************************************************************
;; Funciones de Aptitud
;;  Distancia Manhatan f(x) = h(x) 
;;      donde h(x)= |x1 - x0| + |y1 - y0|
;;  Aptitudes
;;      BFS = h(x)
;;      A* = g(x) + h(x) donde h(x) es la profundidad
;;;************************************************************************************************************************
(defun DistanciaManhatan (x0 y0 x1 y1)
    "Regresa la distancia Manhatan entre dos coordenadas"
    (+ (abs (- x1 x0)) (abs (- y1 y0)))
)

(defun insertar-aptitud (estado metodo)
    (cond ((eql metodo :A*) (+ (1+ *profundidad*)
        (DistanciaManhatan (first estado) (second estado) (first *edoMeta*) (second *edoMeta*))))
        ((eql metodo :BFS) (DistanciaManhatan (first estado)
        (second estado) (first *edoMeta*) (second *edoMeta*)))
        (T 1)
    )
)

;;;************************************************************************************************************************
;; INSERT-TO-ORDER 
;;  Insert-to-order: Inserta en la frontera especificamente haciendo uso del algoritmo BFS
;;;************************************************************************************************************************
(defun insert-in-order (nodo aux-open)
 "Insertar el nodo en *operdorEn* segun su valor de aptitud"
    (cond ((null aux-open) (list nodo))
    ;Se comparan las aptitudes que estan en la frontera
       ((<= (third nodo) (third (first aux-open))) (cons nodo aux-open))
       (T (cons (first aux-open) (insert-in-order nodo (rest aux-open))))
    )
)

;;;************************************************************************************************************************
;; ENCUENTRA-ESTADO
;;;************************************************************************************************************************

(defun encuentra-estado (estado aptitud lista)
  (cond ((null lista) (list nil nil))
    ((equal estado (third (first lista)))
        (if (< aptitud (second (first lista))) 
            (list T T) (list T nil)
        )
    )
    (T (encuentra-estado estado aptitud (rest lista)))
  )
)

;;;************************************************************************************************************************
;; ELIMINA-ESTADO
;;;************************************************************************************************************************
(defun elimina-estado (estado lista)
    (cond ((equal estado (third (first lista))) (rest lista))
        (T (cons (first lista) (elimina-estado estado (rest lista))))
    )
)

;;;************************************************************************************************************************
;; INSERT-TO-OPEN E INSERT-A*
;;  Insert-to-operadorEn: Decide el método de inserción que se va a utilizar, ya sea A*, BFS, depth-first y breath-first
;;;************************************************************************************************************************
(defun insert-A* (nodo)
    (let ((res (encuentra-estado (second nodo) (third nodo) *open*)))
        (cond ((and (first res)(second res))
                (setq *open* (elimina-estado (second nodo) *open*) 
                      *open* (insert-in-order nodo *open*))
            )
            ((and (null (first res)) (null (second res)))
                (setq *open* (insert-in-order nodo *open*))
            )
        )
    )
)

(defun insert-to-open (estado operador metodo)
    "Función que determina como se insertaran los nodos de acuerdo al método"
    (let* ((aptitud (insertar-aptitud estado metodo)) 
            (nodo (create-nodo estado operador aptitud))
          )
         (cond ((eql metodo :A*) (insert-A* nodo))
               ((eql metodo :BFS) (setq 
               *open* (insert-in-order nodo *open*)))
               ((eql metodo :depth-first) (push nodo *open*))
         )
    )
)

;;;************************************************************************************************************************
;; GET-FROM-OPEN
;;  Regresa la solución
;;;************************************************************************************************************************
(defun get-from-open ()
    "Recupera el elemento a revisar de frontera de búsqueda *open*"
        (pop *open*)
)

;;;************************************************************************************************************************
;; Funciones que cuentan los puentesconsecutivamente dependiendo de su operador
;;;************************************************************************************************************************
(defun sube(estado avance contador)
    (cond ((< avance 15) contador)
        (T (sube (list (1- (first estado)) (second estado))
        (get-cell-walls (1- (first estado)) (second estado)) (1+ contador)))
    )
)

(defun baja (estado avance contador)
    (cond ((< avance 15) contador)
        (T (baja (list (1+ (first estado)) (second estado))
           (get-cell-walls (1+ (first estado)) (second estado)) (1+ contador)))
    )
)

(defun der (estado avance contador)
    (cond ((< avance 15) contador)}
        (T (der (list (first estado) (1+ (second estado)))
           (get-cell-walls (first estado) (1+ (second estado)) (1+ contador))))
    )
)

(defun izq (estado avance contador)
    (cond ((< avance 15) contador) 
        (T (izq (list (first estado) (1- (second estado)))
        (get-cell-walls (first estado) (1- (second etado))) (1+ contador)))
    )
)

;;;************************************************************************************************************************
;; Cuentan el número de puentes que existen consecutivamente
;;;************************************************************************************************************************
(defun cuenta(eatdo op)
    "Cuenta los puentes existentes"
    (let* ((total 0) (avance 1))
        (cond ((eql (first op) :Mover-Arriba-Puente)
               (setq total (sube estado avance total))
              )
              ((eql (first op) :Mover-Abajo-Puente)
                (setq total (baja estado avance total))
              )
              ((eql (first op) :Mover-Derecha-Puente)
                (setq total (der estado avance total))
              )
              ((eql (first op) :Mover-Izquierda-Puente)
                (setq total (izq estado avance total))
              )
              (T -1)
        )
    )
)

;;;************************************************************************************************************************
;; VALID-OPERAOR [op, estado]
;;     Predicado. Indica si es prosible aplicar el operador [op] a [estado]
;;;************************************************************************************************************************
(defun valid-operator (op estado)
    (let* ((newX (+ (first estado) (first (second op))))
           (newY (+ (second estado) (second (second op))))
            (p nil) (d nil) 
          )
        (cond ((not (and (>= newX 0) (< newX *limiteEnX*)
              (>= newY 0) (< newY *limiteEnY*) )) nil)
              (T (setq p (get-cell-walls (first estado) (second estado)) d (get-cell-walls newX newY))
                (cond 
                    ((eql (first op) :Mover-Arriba)
                        (and (/= (logand d 16) 16) (/= (logand d 17) 17) (/= (logand p 1) 1))
                    )
                    ((eql (first op) :Mover-Arriba-Puente)
                        (setq d (get-cell-walls (1+ newX) newY))
                        (return-from valid-operator (or (= (logand d 16) 16) (= (logand d 17) 17)))
                    )
                    ((eql (first op) :Mover-Derecha)
                        (and (/= (logand d 16) 16) (/= (logand d 17) 17) (/= (logand p 2) 2))
                    )
                    ((eql (first op) :Mover-Derecha-Puente)
                        (setq d (get-cell-walls newX (1- newY)))
                        (or (= (logand d 16) 16) (= (logand d 17) 17))
                    )
                    ((eql (first op) :Mover-Abajo)
                     (and (/= (logand d 16) 16) (/= (logand d 17) 17) (/= (logand p 4) 4))
                    )
                    ((eql (first op) :Mover-Abajo-Puente)
                     (setq d (get-cell-walls (1- newX) newY) )
                     (or (= (logand d 16) 16) (= (logand d 17) 17))
                    )
                    ((eql  (first op) :Mover-Izquierda)
                     (and (/= (logand d 16) 16) (/= (logand d 17) 17) (/= (logand p 8) 8))
                    )
                    ((eql  (first op) :Mover-Izquierda-Puente)
                     (setq d (get-cell-walls newX (1+ newY)))
                     (or (= (logand d 16) 16) (= (logand d 17) 17))
                    )
                    ((eql  (first op) :Mover-Arriba-derecha)
                     (cond ((= (+ (logand p 2) (logand d 8)) 10) Nil)
                           ((= (+ (logand p 1) (logand d 4)) 5) Nil)
                           ((= (logand p 3) 3) Nil)
                           ((= (logand d 12) 12) Nil)
                           ((or (= (logand d 16) 16) (= (logand d 17) 17)) Nil)
                           ((or (= (get-cell-walls (- (first estado) 1) (second estado)) 17) (= (get-cell-walls (- (first estado) 1) (second estado)) 16)) Nil)
                           ((or (= (get-cell-walls (first estado) (+ (second estado) 1)) 17) (= (get-cell-walls (first estado) (+ (second estado) 1)) 16)) Nil)
                           (T T)
                     )
                    )
                    ((eql  (first op) :Mover-Abajo-derecha)
                     (cond ((= (+ (logand p 2) (logand d 8)) 10) Nil)
                           ((= (+ (logand p 4) (logand d 1) ) 5) Nil)
                           ((= (logand p 6) 6) Nil)
                           ((= (logand d 9) 9) Nil)
                           ((or (= (logand d 16) 16) (= (logand d 17) 17)) Nil)
                           ((or (= (get-cell-walls (+ (first estado) 1) (second estado)) 17) (= (get-cell-walls (+ (first estado) 1) (second estado)) 16)) Nil)
                           ((or (= (get-cell-walls (first estado) (+ (second estado) 1)) 17) (= (get-cell-walls (first estado) (+ (second estado) 1)) 16)) Nil)
                           (T T)
                      )
                    )
                    ((eql  (first op) :Mover-Abajo-izquierda)
                     (cond ((= (+ (logand p 4) (logand d 1)) 5) Nil)
                           ((= (+ (logand p 8) (logand d 2)) 10) Nil)
                           ((= (logand p 12) 12) Nil)
                        ((= (logand d 3) 3) Nil)
                        ((or (= (logand d 16) 16) (= (logand d 17) 17)) Nil)
                        ((or (= (get-cell-walls (+ (first estado) 1) (second estado)) 17) (= (get-cell-walls (+ (first estado) 1) (second estado)) 16)) Nil)
                        ((or (= (get-cell-walls (first estado) (- (second estado) 1)) 17) (= (get-cell-walls (first estado) (- (second estado) 1)) 16)) Nil)
                        (T T)
                     )
                    )
                    ((eql  (first op) :Mover-Arriba-izquierda)
                     (cond ((= (+ (logand p 8) (logand d 2)) 10) Nil)
                          ((= (+ (logand p 1) (logand d 4)) 5) Nil)
                          ((= (logand p 9) 9) Nil)
                          ((= (logand d 6) 6) Nil)
                          ((or (= (logand d 16) 16) (= (logand d 17) 17)) Nil)
                          ((or (= (get-cell-walls (- (first estado) 1) (second estado)) 17) (= (get-cell-walls (- (first estado) 1) (second estado)) 16)) Nil)
                          ((or (= (get-cell-walls (first estado) (- (second estado) 1)) 17) (= (get-cell-walls (first estado) (- (second estado) 1)) 16)) Nil)
                          (T T)
                     )
                    )
                    (T  Nil)
                )
            )
        )
    )
)

;;;************************************************************************************************************************
;; APPLY-OPERATOR (op, estado)
;;;************************************************************************************************************************
(defun apply-operator (op estado repetir)
    "Obtiene el descendiente de (estado) al aplicarle (op) con validaciones en los estados"
    (let ((nuevo nil))
         (cond ((= repetir -1)      
        (setq nuevo (list (+ (first estado) (first (second op))) (+ (second estado) (second (second op)))))
          )
        ((/= repetir -1) 
           (cond 
              ((eql (first op) :Mover-Arriba-Puente) 
                  (setq nuevo (list (+ (first estado) (* repetir -1)) (+ (second estado) (second (second op)))))
                )
              ((eql (first op) :Mover-Abajo-Puente) 
                  (setq nuevo (list (+ (first estado) repetir) (+ (second estado) (second (second op)))))
                )
              ((eql (first op) :Mover-Derecha-Puente) 
                  (setq nuevo (list (+ (first estado) (first (second op))) (+ (second estado) repetir)))
                )
              ((eql (first op) :Mover-Izquierda-Puente)
                  (setq nuevo (list (+ (first estado) (first (second op))) (+  (second estado) (* repetir -1))))
                )
            )
          )
      ) 
    )
)

;;;************************************************************************************************************************{
;; EXPAND(estado)
;;  Construye y regresa una lista con todos los descendientes válidos de (estado)
;;;************************************************************************************************************************
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
    (let* ((descendientes  nil)
        (nuevo-estado  nil) (repetir 0) (primedos nil)(segundos nil))
        (dolist  (op  *ops*  descendientes) 
          (when (valid-operator  op  estado)
            (format t "~a ~a ~%"(valid-operator  op  estado) (first op))
            (setq repetir (cuenta estado op))
            (setq primedos (list (first op) (second op)))
            (setq segundos (list (first op) (second op) (list repetir (third op))))
            (setq  nuevo-estado  (apply-operator  op estado repetir))
              (cond ((= repetir -1) (setq  descendientes  (cons  (list nuevo-estado op) descendientes)) )
                (T (setq  descendientes  (cons  (list nuevo-estado  segundos)  descendientes)) )
              )
          )
        )
    )
)

;;;************************************************************************************************************************
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;************************************************************************************************************************
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos"
    (cond ((null  lista-memoria)  Nil)
          ;;¿El estado es igual al que se encuentra en el nod?
          ((equalp  estado  (second (first  lista-memoria)))  T)  
          (T  (remember-state?  estado  (rest  lista-memoria)))
    )  
)

(defun  filter-memories (lista-estados-y-ops metodo) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory* la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
    (cond ((null  lista-estados-y-ops)  Nil)
       ;;Si se recuerda en la memoria, filtrarlo...
      ((or (remember-state? (first (first  lista-estados-y-ops)) *memory*)  
        ;;Si se encuentra en la frontera de busqueda, filtrarlo...
        (if (eql metodo :BFS) (remember-state? (first (first  lista-estados-y-ops)) *open*) nil)) 
         (filter-memories  (rest  lista-estados-y-ops) metodo)
      )
          ;;De lo contrario, incluirlo en la respuesta
        (T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops) metodo)))
    ) 
) 

;;;************************************************************************************************************************
;; Función APLANA, para aplanar la solución de la lista
;;;************************************************************************************************************************
(defun aplana(lista)
  (cond ((null lista) lista)
    ((atom (first lista)) (cons (first lista) (aplana (rest lista))))
    (T (append (aplana (first lista)) (aplana (rest lista))))
  )
)

;;;************************************************************************************************************************
;;  EXTRACT-SOLUTION  
;;  Extrae la solucion de la memoria
;;;************************************************************************************************************************
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)
         (cond ((null  lista)  Nil)
           ((eql  id  (first (first  lista))) (first  lista))
           (T (locate-node  id (rest  lista)))
         )
        ))
        (let ((current  (locate-node  (first  nodo)  *memory*)))
            (loop  while  (not (null  current))  do                        
                (push  (nth 4 current)  *solution*)     
                (setq  current  (locate-node  (fourth  current) *memory*))
            )
        )
        (pop *solution*)
     )
)

;;;************************************************************************************************************************
;;  RESET-ALL
;;      Reinicia todas las variables globales para iniciar una nueva búsqueda.
;;;************************************************************************************************************************
(defun reset-all () 
    (setq  *open*  nil)
    (setq  *memory*  nil)
    (setq  *id*  0)
    (setq  *current-ancestor*  nil)
    (setq  *solution*  nil)
    (setq  *edoMeta* nil) 
    (setq *profundidad* -1)
    (setq *limiteEnX* (get-maze-rows)) 
    (setq *limiteEnY* (get-maze-cols))
)

;;;************************************************************************************************************************
;; CREALISTA
;;  Crea una lista dependiendo del numero de puentes consecutivos que existieron
;;;************************************************************************************************************************
(defun crealista(repetir num)
  "Crea una lista con n numeros repetidos"
  (let ((lista nil))
    (dotimes (i repetir lista)
      (setq lista (cons num lista))
    )
  )
)

;;;************************************************************************************************************************
;; RECORRE
;;  Recorre la solucion y si se encuentra una lista la transforma 
;;;************************************************************************************************************************
(defun recorre (lista)
  (cond ((null lista) lista)
      ((listp (first lista)) (cons (crealista (first (first lista)) (second (first lista))) (recorre (rest lista))))
      ((atom (first lista)) (cons (first lista) (recorre (rest lista))))
  )
)

;;;************************************************************************************************************************
;; LABERINTO 
;;;************************************************************************************************************************
(defun Maze-Search (edo-inicial  edo-meta  metodo)
"Realiza una busqueda para un laberinto los métodos posibles para evaluar la aptitud son: :A*, :BFS, depth-first, breath-first"
  (reset-all)
  (let ((nodo nil)
    (estado nil)
    (sucesores  '())
    (operador  nil)
    (meta-encontrada  nil))
      (setq *edoMeta* edo-meta )
      (insert-to-open  edo-inicial nil metodo)
      (loop until (or  meta-encontrada (null *open*))  do
        (setq  nodo (get-from-open)   
          estado  (second  nodo)
          operador  (nth 4 nodo))
        (cond ((not (remember-state? estado *memory*)) 
          (push  nodo  *memory*)
          (cond ((equal  edo-meta  estado)
                    (extract-solution  nodo)
                      (setq  meta-encontrada  T))
                (T 
                  (setq  *current-ancestor*  (first  nodo)) 
                  (setq  *profundidad*  (nth 5 nodo))
                  (setq  sucesores  (expand estado))
                  (format t "sucesores ~a~%" sucesores)
                  (setq  sucesores  (filter-memories  sucesores metodo))
                  (format t "sucesores filtrados~a~%" sucesores)
                  (loop for  element  in  sucesores  do
                    (insert-to-open  (first element)  (second element)  metodo)
                  )
                )
          )
        ))  
      )
    )

  (setq *solution* (recorre *solution*))
  ;(print *solution*)
  (setq *solution* (aplana *solution*))
  ;(print *solution*)
)

(defun A*-Search ()
  ;;Obtenemos la posición inicial del arreglo guardado en la variable *start*
  ;;Obtenemos la posición final del arreglo guardado en la variable *goal*
  ;;Para finalizar agregamos el metodo que deseamos utilizar
  (Maze-Search  (list (aref *start* 0) (aref *start* 1))  
          (list (aref *goal* 0) (aref *goal* 1))   ':A*)
)

(defun Best-First-Search()
  ;;Obtenemos la posición inicial del arreglo guardado en la variable *start*
  ;;Obtenemos la posición final del arreglo guardado en la variable *goal*
  ;;Para finalizar agregamos el metodo que deseamos utilizar
  (Maze-Search  (list (aref *start* 0) (aref *start* 1))  
          (list (aref *goal* 0) (aref *goal* 1))   ':BFS)
)

(defun Depth-First-Search()
  ;;Obtenemos la posición inicial del arreglo guardado en la variable *start*
  ;;Obtenemos la posición final del arreglo guardado en la variable *goal*
  ;;Para finalizar agregamos el metodo que deseamos utilizar
  (Maze-Search  (list (aref *start* 0) (aref *start* 1))  
          (list (aref *goal* 0) (aref *goal* 1))   ':depth-first)
)

(start-maze)