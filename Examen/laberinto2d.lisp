;;;**********************************************************************************************************************
;; laberinto2d.lisp
;;     Se resuelve el problema de encontrar la salida a un laberinto apartir de un estado inicial.
;;     Con diferentes metodos en 2 dimensiones.
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
(defparameter *open* nil)

;Solución
(defparameter *solution* nil)

;;Memoria de intentos previos
(defparameter *memory* nil)

;; Id del ancestro común de todos los descendientes que se generen
(defparameter *current-ancestor* nil)

;; Identificador del último nodo creado
(defparameter *id* -1)

;;Límites en X y Y
(defparameter *xMeta* nil)
(defparameter *yMeta* nil)

(defvar *operadores* '((:Mover-Arriba    		0)
					   (:Mover-Arriba-Derecha   1)
					   (:Mover-Derecha     		2)
					   (:Mover-Abajo-Derecha    3)
					   (:Mover-Abajo            4)
					   (:Mover-Abajo-Izquierda  5)
					   (:Mover-Izquierda        6)
					   (:Mover-Arriba-Izquierda 7)))

;;;************************************************************************************************************************
;; Crear-nodo (estado operador aptitud)
;;;************************************************************************************************************************
(defun crear-nodo (estado operador aptitud)
;;Incremento para que lo primero es procesarse sea la respuesta
  (incf *id*)
;;Los nodos procesados son descendientes de *current-ancestor*
  (list *id* estado operador *current-ancestor* aptitud))

;;;************************************************************************************************************************
;;; Agregar-a-open: Agrega un nodo a la frontera de busqueda
;;;************************************************************************************************************************
(defun agregar-a-open (estado operador metodo &optional (aptitud))
  (let ((nodo (crear-nodo estado operador aptitud)))
    (cond ((eql metodo :Depth-First-Search)
	   (push nodo *open*))
	  ((eql metodo :sin-orden)
	   (push nodo *open*)))))

;;;************************************************************************************************************************
;;; Sacar-a-open: Sacar un nodo a la frontera de busqueda
;;;************************************************************************************************************************
(defun sacar-de-open ()
  (pop *open*))

;;;************************************************************************************************************************
;; Operador-valido? [op, estado]
;;     Predicado. Indica si es posible aplicar el operador [op] a [estado]
;;;************************************************************************************************************************
(defun operador-valido? (operador estado)
  (let* ((fila (aref estado 0))
	 (columna (aref estado 1))
	 (celda_actual (get-cell-walls fila columna))
	 (celda_siguiente nil)
	 (tamanio_laberinto (array-dimensions (get-maze-data))))
    (case (first operador)
      (:Mover-Arriba
       (cond ((>= (1- fila) 0)
	      (setq celda_siguiente (get-cell-walls (1- fila) columna))
	      (and (evenp celda_actual) (not (logbitp 2 celda_siguiente))))
	     (T nil))) 
      (:Mover-Abajo
       (cond ((< (1+ fila) (nth 0 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls (1+ fila) columna))
	      (and (not (logbitp 2 celda_actual)) (evenp celda_siguiente)))
	     (T nil)))
      (:Mover-Derecha 
       (cond ((< (1+ columna) (nth 1 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls fila (1+ columna)))
	      (and (not (logbitp 1 celda_actual)) (<= celda_siguiente 7)))
	     (T nil)))
      (:Mover-Izquierda 
       (cond ((>= (1- columna) 0)
	      (setq celda_siguiente (get-cell-walls fila (1- columna)))
	      (and (<= celda_actual 7) (not (logbitp 1 celda_siguiente))))
	     (T nil)))
      (:Mover-Arriba-Izquierda 
       (cond ((and (>= (1- fila) 0) (>= (1- columna) 0))
	      (setq celda_siguiente (get-cell-walls (1- fila) (1- columna)))
	      (and (not (eql (logbitp 0 celda_actual) (logbitp 2 celda_siguiente)))
		   (not (eql (logbitp 3 celda_actual) (logbitp 1 celda_siguiente)))
		   (not (eql (logbitp 0 celda_actual) (logbitp 3 celda_actual)))
		   (not (eql (logbitp 1 celda_siguiente) (logbitp 2 celda_siguiente)))))
	     (T nil)))
      (:Mover-Arriba-Derecha 
       (cond ((and (>= (1- fila) 0) (< (1+ columna) (nth 1 tamanio_laberinto)))
	      (setq celda_siguiente (get-cell-walls (1- fila) (1+ columna)))
	      (and (not (eql (logbitp 0 celda_actual) (logbitp 2 celda_siguiente)))
		   (not (eql (logbitp 1 celda_actual) (logbitp 3 celda_siguiente)))
		   (not (eql (logbitp 0 celda_actual) (logbitp 1 celda_actual)))
		   (not (eql (logbitp 2 celda_siguiente) (logbitp 3 celda_siguiente)))))
	     (T nil)))
      (:Mover-Abajo-Izquierda 
       (cond ((and (< (1+ fila) (nth 0 tamanio_laberinto)) (>= (1- columna) 0))
	      (setq celda_siguiente (get-cell-walls (1+ fila) (1- columna)))
	      (and (not (eql (logbitp 2 celda_actual) (logbitp 0 celda_siguiente)))
		   (not (eql (logbitp 3 celda_actual) (logbitp 1 celda_siguiente)))
		   (not (eql (logbitp 2 celda_actual) (logbitp 3 celda_actual)))
		   (not (eql (logbitp 0 celda_siguiente) (logbitp 1 celda_siguiente)))))
	     (T nil)))
      (:Mover-Abajo-Derecha 
       (cond ((and (< (1+ fila) (nth 0 tamanio_laberinto)) (< (1+ columna) (nth 1 tamanio_laberinto)))
	      (setq celda_siguiente (get-cell-walls (1+ fila) (1+ columna)))
	      (and (not (eql (logbitp 2 celda_actual) (logbitp 0 celda_siguiente)))
		   (not (eql (logbitp 1 celda_actual) (logbitp 3 celda_siguiente)))
		   (not (eql (logbitp 1 celda_actual) (logbitp 2 celda_actual)))
		   (not (eql (logbitp 0 celda_siguiente) (logbitp 3 celda_siguiente)))))
	     (T nil)))
      (T nil))))

;;;************************************************************************************************************************
;; Aplicar-operador (op, estado)
;;;************************************************************************************************************************
(defun aplicar-operador (operador estado)
  (let* ((fila (aref estado 0))
	 (columna (aref estado 1)))
    (case (first operador)
      (:Mover-Arriba
       (make-array '(2) :initial-contents (list (1- fila) columna)))
      (:Mover-Arriba-Derecha 
	(make-array '(2) :initial-contents (list (1- fila) (1+ columna))))
      (:Mover-Derecha
       (make-array '(2) :initial-contents (list fila (1+ columna))))
      (:Mover-Abajo-Derecha 
	(make-array '(2) :initial-contents (list (1+ fila) (1+ columna))))
      (:Mover-Abajo
	(make-array '(2) :initial-contents (list (1+ fila) columna)))
      (:Mover-Abajo-Izquierda 
	(make-array '(2) :initial-contents (list (1+ fila) (1- columna))))
      (:Mover-Izquierda
	(make-array '(2) :initial-contents (list fila (1- columna))))
      (:Mover-Arriba-Izquierda 
	(make-array '(2) :initial-contents (list (1- fila) (1- columna))))
      (T nil))))

;;;************************************************************************************************************************{
;; Expandir-estado (estado)
;;  Construye y regresa una lista con todos los descendientes válidos de (estado)
;;;************************************************************************************************************************
(defun expandir-estado ( estado )
  (let ((descendientes nil)
	(nuevo_estado nil))
    (dolist (operador *operadores* descendientes)
      (setq nuevo_estado (aplicar-operador operador estado))
      (when (and (operador-valido? operador estado))
	(setq descendientes (cons (list nuevo_estado operador) descendientes))))))

;;;************************************************************************************************************************
;;  Estado-recordado?  y  Filtrar-estado-recordados
;;        Permiten administrar la memoria de intentos previos
;;	Estado-recordado? : Verifica si un [estado] ya se encuentra en la lista de [memory]
;;	Filtrar-estado-recordados : Filtra los estados, al descartar aquellos que ya se encuentran en la lista de memory.
;;;************************************************************************************************************************
(defun estado-recordado? ( estado memory )
  (cond ((null memory) nil)
	((equalp estado (second (first memory))) T)
	(T (estado-recordado? estado (rest memory)))))

(defun filtrar-estados-recordados (estados-y-operadores)
  (cond ((null estados-y-operadores) nil)
	((estado-recordado? (first (first estados-y-operadores)) *memory*)
	 (filtrar-estados-recordados (rest estados-y-operadores)))
	(T 
	 (cons (first estados-y-operadores) (filtrar-estados-recordados (rest estados-y-operadores))))))

;;;************************************************************************************************************************{
;; Extraer-solution (estado) :Extrae la solution del problema a partir del nodo con el estado meta
;; rastreando los nodos ancestros.
;;;************************************************************************************************************************
(defun extraer-solution (nodo)
  (labels ((localizar-nodo (id lista)
	     (cond ((null lista) nil)
		   ((eql id (first (first lista))) (first lista))
		   (T (localizar-nodo id (rest lista))))))
    (let ((nodo_actual (localizar-nodo  (first  nodo)  *memory*)))
      (loop  while  (not (null  nodo_actual))  do
	     (push nodo_actual *solution*)
	     (setq nodo_actual (localizar-nodo (fourth nodo_actual) *memory*))))
    *solution*))

;;;************************************************************************************************************************
;;  Crear-lista-movimientos
;;      Crea una lista almacenando los movimientos
;;;************************************************************************************************************************
(defun crear-lista-movimientos (lista_nodos)
  (let ((nodo nil)
	(solution nil))
    (dotimes (i (length lista_nodos) solution)
      (setq nodo (nth i lista_nodos))
      (if (/= i 0)
	  (setq solution (append solution (list (second (third nodo)))))))))

;;;************************************************************************************************************************
;;  Inicializar
;;      Reinicia todas las variables globales para iniciar una nueva búsqueda.
;;;************************************************************************************************************************
(defun inicializar ()
  (setq *open* nil)
  (setq *memory* nil)
  (setq *id* 0)
  (setq *current-ancestor* nil)
  (setq *solution* nil)
  (setq *xMeta* nil)
  (setq *yMeta* nil))

;;;************************************************************************************************************************
;; Método de búsqueda Depth-First-Search
;;;************************************************************************************************************************
(defun Depth-First-Search ()
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil)
	(solution nil))
    (agregar-a-open *start* nil ':Depth-First-Search)
    (loop until (or meta_encontrada (null *open*)) do
	  (setq nodo (sacar-de-open))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memory*)
	  (cond ((equalp estado *goal*) 
		 (setq solution (crear-lista-movimientos (extraer-solution nodo)))
		 (setq *solution* solution)
		 (setq meta_encontrada t))
		(T (setq *current-ancestor* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados-recordados sucesores))
		   (loop for sucesor in sucesores do
			 (agregar-a-open (first sucesor) (second sucesor) ':Depth-First-Search)))))))

;;;************************************************************************************************************************
;; Sacar-nodo-con-mejor-evaluacon
;;  Retorna el nodo con mejor evaluacion, ya sea aptitud para el algoritmo best-first-search o costo real para el algoritmo a-*
;;		Best-First-Search : La mejor aptitud, cuyo valor es el nodo con el estado más proximo al estado meta.
;;		A*-Search : Menor costo real, cuyo valor es el estado más proximo al estado meta
;;				    y con un menor número de operaciones necesarias para llegar a el.
;;;************************************************************************************************************************
(defun sacar-nodo-con-mejor-evaluacion ()
  (let ((menor nil)
	(nodo nil))
    (dotimes (i (length *open*) menor)
      (setq nodo (nth i *open*))
      (cond ((= i 0)
	     (setq menor nodo))
	    ((< (nth 4 nodo) (nth 4 menor))
	     (setq menor nodo))))))

;;;************************************************************************************************************************
;; Estado-en-open? : Busca el [estado] en la [frontera], en caso de encontrarlo regresa el nodo que
;;;************************************************************************************************************************
(defun estado-en-open? ( estado open )
  (cond ((null open) nil)
	((equalp estado (second (first open))) 
	 (first open))
	(T
	 (estado-en-open? estado (rest open)))))

;;;************************************************************************************************************************
;; Filtro-Best-First-Search : Filtra los estados, al descartar aquellos que ya se encuentran en la frontera de busqueda o en la lista de memoria
;;;************************************************************************************************************************
(defun filtro-Best-First-Search ( estados-y-operadores )
  (cond ((null estados-y-operadores) nil)
	((or (estado-recordado? (first (first estados-y-operadores)) *memory*) 
	     (not (null (estado-en-open? (first (first estados-y-operadores)) *open*))))
	 (filtro-Best-First-Search (rest estados-y-operadores)))
	(T
	 (cons (first estados-y-operadores) (filtro-Best-First-Search (rest estados-y-operadores))))))

;;;************************************************************************************************************************
;;	Calcular-aptitud
;;  Distancia Manhatan f(x) = h(x) 
;;      donde h(x)= |x1 - x0| + |y1 - y0|	
;;;************************************************************************************************************************
(defun calcular-aptitud (estado)
  (let ((xi (aref estado 0))
	(yi (aref estado 1)))
	"Regresa la distancia Manhatan entre dos coordenadas"
    (+ (abs (- xi *xMeta*)) (abs (- yi *yMeta*)))))

;;;************************************************************************************************************************
;; Método de búsqueda Best-First-Search
;;;************************************************************************************************************************
(defun Best-First-Search()
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(aptitud nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil)
	(solution nil))
    (setq *xMeta* (aref *goal* 0))
    (setq *yMeta* (aref *goal* 1))
    (agregar-a-open *start* nil ':sin-orden 0)
    (loop until (or meta_encontrada (null *open*)) do
	  (setq nodo (sacar-nodo-con-mejor-evaluacion))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (setq *open* (delete nodo *open*))
	  (push nodo *memory*)
	  (cond ((equalp estado *goal*) 
		 (setq solution (crear-lista-movimientos (extraer-solution nodo)))
		 (setq *solution* solution)
		 (print (length solution))
		 (setq meta_encontrada t))
		(T (setq *current-ancestor* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtro-Best-First-Search sucesores))
		   (loop for sucesor in sucesores do
			 (setq aptitud (calcular-aptitud (first sucesor)))
			 (agregar-a-open (first sucesor) (second sucesor) ':sin-orden aptitud)))))))


;;;************************************************************************************************************************
;; Pasa-filtro-A* (estado costo_real)
;;;************************************************************************************************************************
(defun pasa-filtro-A*? ( estado costo_real )
  (let* ((nodo (estado-en-open? estado *open*)))
    (cond ((null nodo) t) 
	  ((< costo_real (fifth nodo))
	   (setq *open* (delete nodo *open*))
	   t)
	  (T nil))))

;;;************************************************************************************************************************
;; Método de búsqueda A*
;;;************************************************************************************************************************
(defun A*-Search()
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil)
	(solution nil)
	(costo_real nil))
    (setq *xMeta* (aref *goal* 0))
    (setq *yMeta* (aref *goal* 1))
    (agregar-a-open *start* nil ':sin-orden 0)
    (loop until (or meta_encontrada (null *open*)) do
	  (setq nodo (sacar-nodo-con-mejor-evaluacion))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (setq *open* (delete nodo *open*))
	  (push nodo *memory*)
	  (cond ((equalp estado *goal*) 
		 (setq solution (crear-lista-movimientos (extraer-solution nodo)))
		 (setq *solution* solution)
		 (print (length solution))
		 (setq meta_encontrada t))
		(T (setq *current-ancestor* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados-recordados sucesores))
		   (loop for sucesor in sucesores do
			 (setq costo_real (+ *current-ancestor* (calcular-aptitud (first sucesor))))
			 (if (pasa-filtro-A*? (first sucesor) costo_real)
			     (agregar-a-open (first sucesor) (second sucesor) ':sin-orden costo_real))))))))
				 
(start-maze)