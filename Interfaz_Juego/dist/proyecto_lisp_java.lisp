;Funcion que se encarga de inicializar las variables globales que usaremos en el juego
(defun inicia-juego(turno dificultad)
	(setq 
		;Guardara la combinacion actual de fichas en el tablero
		*Tablero* '(0 0 0 0 0 0
					0 0 0 0 0 0
					0 0 0 0 0 0
					0 0 0 0 0 0
					0 0 0 0 0 0
					0 0 0 0 0 0)

		*Pesos* '(3 4 5 5 4 3
				  4 6 9 9 6 4
				  5 7 11 11 7 5
				  5 7 11 11 7 5
				  4 6 9 9 6 4
				  3 4 5 5 4 3)

		;Define si es el turno del jugador o maquina
		*Turno* turno

		*FichaHumano* 1
		*FichaMaquina* -1
		*FichaVacia* 0

		;Dificultad: 1 Facil 2 Intermedio 3 Dificil
		*Dificultad* dificultad

		;Guardara la altura de cada columna respectivamente
		*Alturas* '(0 0 0 0 0 0)
		*Estado* 0
		*Jugadas* 0
	)
	;(jugar)
)

;Funcion que muestra el tablero en
;La linea de comandos de lisp
;Se utiliza para el testing previo a la interfaz en java
(defun mostrar-tablero ()
	(format t "~D | ~D | ~D | ~D | ~D | ~D" (nth 30 *Tablero*) (nth 31 *Tablero*) (nth 32 *Tablero*) (nth 33 *Tablero*) (nth 34 *Tablero*) (nth 35 *Tablero*))
	(terpri)
	(format t "----------------------------------")
	(terpri)
	(format t "~D | ~D | ~D | ~D | ~D | ~D" (nth 24 *Tablero*) (nth 25 *Tablero*) (nth 26 *Tablero*) (nth 27 *Tablero*) (nth 28 *Tablero*) (nth 29 *Tablero*))
	(terpri)
	(format t "----------------------------------")
	(terpri)
	(format t "~D | ~D | ~D | ~D | ~D | ~D" (nth 18 *Tablero*) (nth 19 *Tablero*) (nth 20 *Tablero*) (nth 21 *Tablero*) (nth 22 *Tablero*) (nth 23 *Tablero*))
	(terpri)
	(format t "----------------------------------")
	(terpri)
	(format t "~D | ~D | ~D | ~D | ~D | ~D" (nth 12 *Tablero*) (nth 13 *Tablero*) (nth 14 *Tablero*) (nth 15 *Tablero*) (nth 16 *Tablero*) (nth 17 *Tablero*))
	(terpri)
	(format t "----------------------------------")
	(terpri)
	(format t "~D | ~D | ~D | ~D | ~D | ~D" (nth 6 *Tablero*) (nth 7 *Tablero*) (nth 8 *Tablero*) (nth 9 *Tablero*) (nth 10 *Tablero*) (nth 11 *Tablero*))
	(terpri)
	(format t "----------------------------------")
	(terpri)
	(format t "~D | ~D | ~D | ~D | ~D | ~D" (nth 0 *Tablero*) (nth 1 *Tablero*) (nth 2 *Tablero*) (nth 3 *Tablero*) (nth 4 *Tablero*) (nth 5 *Tablero*))
	(terpri)
)

;Funcion que devuelve el tipo de ficha que hay en cierta posicion
(defun ficha(pos tablero)
	(let 
		((ficha-en-pos (nth pos tablero)))
		(cond
			((equal ficha-en-pos *FichaVacia*) 0)
			((equal ficha-en-pos *FichaMaquina*) -1)
			((equal ficha-en-pos *FichaHumano*) 1)
		)
	)
)

;Verifica si es posible colocar una ficha en cierta columna
(defun es-posible (alturas columna)
	(< (nth columna alturas) 6)
)

;Posicion real de la ficha en el tablero
(defun posicion-real (alturas columna) 
	(+ columna (* 6 (nth columna alturas)))
)

;Funcion que actualiza el tablero con la ficha colocada
;Devuelve el tablero y las alturas actualizadas
(defun editar-tablero (tablero alturas columna turno)
	(setq
		tAux tablero
		aAux alturas
		lAux '()
		i 0
	)
	(if (es-posible alturas columna)
		(progn
			(setq posreal (posicion-real alturas columna))
			(dolist (x tAux)
				(if (or (< i posreal) (> i posreal))
					(setq lAux (append lAux (list x)))
				)
				(if (= i posreal)
					(progn
						(setq lAux (append lAux (list turno)))
						(setq aAux
							(append 
								(butlast aAux (- 6 columna))
								(list (+ 1 (nth columna aAux)))
								(nthcdr (+ 1 columna) aAux)
							)
						)
					)
				)
				(setq i (+ 1 i))
			)
		)
	)
	(list lAux aAux)
)

;Determina la diagonal donde buscar empezando de la izquierda
(defun hallar-diagonalesI (ultMov)
	(setq  
		diagIzq'()
		band 0
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 12)
					(= ultMov 19)
					(= ultMov 26)
					(= ultMov 33)
				)
				(progn
					(setq diagIzq (list 12 19 26 33))
					(setq band 1)
				)
			)
		)
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 6)
					(= ultMov 13)
					(= ultMov 20)
					(= ultMov 27)
					(= ultMov 34)
				)
				(progn
					(setq diagIzq (list 6 13 20 27 34))
					(setq band 1)
				)
			)
		)
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 0)
					(= ultMov 7)
					(= ultMov 14)
					(= ultMov 21)
					(= ultMov 28)
					(= ultMov 35)
				)
				(progn
					(setq diagIzq (list 0 7 14 21 28 35))
					(setq band 1)
				)
			)
		)
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 1)
					(= ultMov 8)
					(= ultMov 15)
					(= ultMov 22)
					(= ultMov 29)
				)
				(progn
					(setq diagIzq (list 1 8 15 22 29))
					(setq band 1)
				)
			)
		)
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 2)
					(= ultMov 9)
					(= ultMov 16)
					(= ultMov 23)
				)
				(progn
					(setq diagIzq (list 2 9 16 23))
					(setq band 1)
				)
			)
		)
	)
	diagIzq
)

;Determina la diagonal donde buscar empezando de la derecha
(defun hallar-diagonalesD (ultMov)
	(setq  
		diagDer'()
		band 0
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 17)
					(= ultMov 22)
					(= ultMov 27)
					(= ultMov 32)
				)
				(progn
					(setq diagDer (list 17 22 27 32))
					(setq band 1)
				)
			)
		)
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 11)
					(= ultMov 16)
					(= ultMov 21)
					(= ultMov 26)
					(= ultMov 31)
				)
				(progn
					(setq diagDer (list 11 16 21 26 31))
					(setq band 1)
				)
			)
		)
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 5)
					(= ultMov 10)
					(= ultMov 15)
					(= ultMov 20)
					(= ultMov 25)
					(= ultMov 30)
				)
				(progn
					(setq diagDer (list 5 10 15 20 25 30))
					(setq band 1)
				)
			)
		)
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 4)
					(= ultMov 9)
					(= ultMov 14)
					(= ultMov 19)
					(= ultMov 24)
				)
				(progn
					(setq diagDer (list 4 9 14 19 24))
					(setq band 1)
				)
			)
		)
	)

	(if (= 0 band)
		(progn
			(if 
				(or
					(= ultMov 3)
					(= ultMov 8)
					(= ultMov 13)
					(= ultMov 18)
				)
				(progn
					(setq diagDer (list 3 8 13 18))
					(setq band 1)
				)
			)
		)
	)
	diagDer
)

;Verificar si ya existe un ganador
(defun es-ganador (tablero alturas columna turno)
	(setq ultMov (- (posicion-real alturas columna) 6))
	(setq 
		iniVertical ultMov
		iniHorizontal ultMov
		flag 0
	)

	(if (< ultMov 0)
		(return-from es-ganador flag)
	)

	;Verificamos la vertical
	(if (= flag 0)
		(progn
			(loop while (> iniVertical 5) do
				(setq iniVertical (- iniVertical 6))
			)

			(setq listaVertical 
				(list
					(list iniVertical (+ 6 iniVertical) (+ 12 iniVertical) (+ 18 iniVertical))
					(list (+ 6 iniVertical) (+ 12 iniVertical) (+ 18 iniVertical) (+ 24 iniVertical))
					(list (+ 12 iniVertical) (+ 18 iniVertical) (+ 24 iniVertical) (+ 30 iniVertical))
				)
			)

			(dolist (l listaVertical)
				(if 
					(and 
						(=
							(nth (nth 0 l) tablero)
							(nth (nth 1 l) tablero)
							(nth (nth 2 l) tablero)
							(nth (nth 3 l) tablero)
						)
						(=
							(nth (nth 0 l) tablero)
							turno
						)
					)
					(setq flag 1)
				)
			)
		)
	)

	;Verificamos la horizontal
	(if (= flag 0)
		(progn
			(loop while (not (= 0 (mod iniHorizontal 6))) do
				 (setq iniHorizontal (- iniHorizontal 1))
			)

			(setq listaHorizontal
				(list
					(list iniHorizontal (+ 1 iniHorizontal) (+ 2 iniHorizontal) (+ 3 iniHorizontal))
					(list (+ 1 iniHorizontal) (+ 2 iniHorizontal) (+ 3 iniHorizontal) (+ 4 iniHorizontal))
					(list (+ 2 iniHorizontal) (+ 3 iniHorizontal) (+ 4 iniHorizontal) (+ 5 iniHorizontal))
				)
			)

			(dolist (l listaHorizontal)
				(if 
					(and 
						(=
							(nth (nth 0 l) tablero)
							(nth (nth 1 l) tablero)
							(nth (nth 2 l) tablero)
							(nth (nth 3 l) tablero)
						)
						(=
							(nth (nth 0 l) tablero)
							turno
						)
					)
					(setq flag 1)
				)
			)
		)
	)

	;Verificamos la diagonal izquierda
	(setq listaDiagI (hallar-diagonalesI ultMov))

	(if (= 0 flag)
		(progn
			(if (= 4 (length listaDiagI))
				(progn
					(if 
						(and
							(=
								(nth (nth 0 listaDiagI) tablero)
								(nth (nth 1 listaDiagI) tablero)
								(nth (nth 2 listaDiagI) tablero)
								(nth (nth 3 listaDiagI) tablero)
							)
							(=
								(nth (nth 0 listaDiagI) tablero)
								turno
							)
						)
						(setq flag 1)
					)
				)
			)
		)
	)

	(if (= 0 flag)
		(progn
			(if (= 5 (length listaDiagI))
				(progn
					(if 
						(or 
							(and 
								(=
									(nth (nth 0 listaDiagI) tablero)
									(nth (nth 1 listaDiagI) tablero)
									(nth (nth 2 listaDiagI) tablero)
									(nth (nth 3 listaDiagI) tablero)
								)
								(=
									(nth (nth 0 listaDiagI) tablero)
									turno
								)
							)
							(and 
								(=
									(nth (nth 1 listaDiagI) tablero)
									(nth (nth 2 listaDiagI) tablero)
									(nth (nth 3 listaDiagI) tablero)
									(nth (nth 4 listaDiagI) tablero)
								)
								(=
									(nth (nth 1 listaDiagI) tablero)
									turno
								)
							)
						)
						(setq flag 1)
					)
				)
			)
		)
	)

	(if (= 0 flag)
		(progn
			(if (= 6 (length listaDiagI))
				(progn
					(if 
						(or
							(and 
										(=
											(nth (nth 0 listaDiagI) tablero)
											(nth (nth 1 listaDiagI) tablero)
											(nth (nth 2 listaDiagI) tablero)
											(nth (nth 3 listaDiagI) tablero)
										)
										(=
											(nth (nth 0 listaDiagI) tablero)
											turno
										)
							)
							(and 
										(=
											(nth (nth 1 listaDiagI) tablero)
											(nth (nth 2 listaDiagI) tablero)
											(nth (nth 3 listaDiagI) tablero)
											(nth (nth 4 listaDiagI) tablero)
										)
										(=
											(nth (nth 1 listaDiagI) tablero)
											turno
										)
							)
							(and 
										(=
											(nth (nth 2 listaDiagI) tablero)
											(nth (nth 3 listaDiagI) tablero)
											(nth (nth 4 listaDiagI) tablero)
											(nth (nth 5 listaDiagI) tablero)
										)
										(=
											(nth (nth 2 listaDiagI) tablero)
											turno
										)
							)
						)
						(setq flag 1)
					)
				)
			)
		)
	)


	;Verificamos la diagonal derecha
	(setq listaDiagD (hallar-diagonalesD ultMov))

	(if (= 0 flag)
		(progn
			(if (= 4 (length listaDiagD))
				(progn
					(if 
						(and
							(=
								(nth (nth 0 listaDiagD) tablero)
								(nth (nth 1 listaDiagD) tablero)
								(nth (nth 2 listaDiagD) tablero)
								(nth (nth 3 listaDiagD) tablero)
							)
							(=
								(nth (nth 0 listaDiagD) tablero)
								turno
							)
						)
						(setq flag 1)
					)
				)
			)
		)
	)

	(if (= 0 flag)
		(progn
			(if (= 5 (length listaDiagD))
				(progn
					(if 
						(or 
							(and 
								(=
									(nth (nth 0 listaDiagD) tablero)
									(nth (nth 1 listaDiagD) tablero)
									(nth (nth 2 listaDiagD) tablero)
									(nth (nth 3 listaDiagD) tablero)
								)
								(=
									(nth (nth 0 listaDiagD) tablero)
									turno
								)
							)
							(and 
								(=
									(nth (nth 1 listaDiagD) tablero)
									(nth (nth 2 listaDiagD) tablero)
									(nth (nth 3 listaDiagD) tablero)
									(nth (nth 4 listaDiagD) tablero)
								)
								(=
									(nth (nth 1 listaDiagD) tablero)
									turno
								)
							)
						)
						(setq flag 1)
					)
				)
			)
		)
	)

	(if (= 0 flag)
		(progn
			(if (= 6 (length listaDiagD))
				(progn
					(if 
						(or
							(and 
										(=
											(nth (nth 0 listaDiagD) tablero)
											(nth (nth 1 listaDiagD) tablero)
											(nth (nth 2 listaDiagD) tablero)
											(nth (nth 3 listaDiagD) tablero)
										)
										(=
											(nth (nth 0 listaDiagD) tablero)
											turno
										)
							)
							(and 
										(=
											(nth (nth 1 listaDiagD) tablero)
											(nth (nth 2 listaDiagD) tablero)
											(nth (nth 3 listaDiagD) tablero)
											(nth (nth 4 listaDiagD) tablero)
										)
										(=
											(nth (nth 1 listaDiagD) tablero)
											turno
										)
							)
							(and 
										(=
											(nth (nth 2 listaDiagD) tablero)
											(nth (nth 3 listaDiagD) tablero)
											(nth (nth 4 listaDiagD) tablero)
											(nth (nth 5 listaDiagD) tablero)
										)
										(=
											(nth (nth 2 listaDiagD) tablero)
											turno
										)
							)
						)
						(setq flag 1)
					)
				)
			)
		)
	)
	flag
)

;Verifica si el tablero se encuentra lleno, es decir, no es posible hacer mas movimientos
(defun esta-lleno (alturas)
	(setq flagl 0)
	;Verifica si todas las columnas tienen altura 6
	(if
		(and
			(= 6 (nth 0 alturas))
			(= 6 (nth 1 alturas))
			(= 6 (nth 2 alturas))
			(= 6 (nth 3 alturas))
			(= 6 (nth 4 alturas))
			(= 6 (nth 5 alturas))
		)
		(setq flagl 1)
	)
	flagl
)

;Verifica si un nodo no posee mas posibles movimientos
(defun es-terminal (nodo)
	(setq tab (nth 0 nodo))
	(setq alt (nth 1 nodo))
	(or (= 1 (esta-lleno alt)) (= 1 (es-ganador tab alt (nth 3 nodo) (* -1 (nth 2 nodo)))))
)

;Halla los sucesores de un nodo en el arbol minimax
(defun sucesores (nodo)
	(setq lSucesores '())
	(setq tableroAux (nth 0 nodo))
	(setq alturasAux (nth 1 nodo))
	(setq sucAux '())
	(dolist (x (list 0 1 2 3 4 5))
		(if (es-posible alturasAux x)
			(progn
				(setq actualizacion (editar-tablero tableroAux alturasAux x (nth 2 nodo)))
				(setq sucAux
					(list
						(nth 0 actualizacion) ;tablero
						(nth 1 actualizacion) ;alturas
						(* -1 (nth 2 nodo))
						x
						(+ 1 (nth 4 nodo))
					)
				)
				(setq lSucesores (append lSucesores (list sucAux)))
			)
		)
	)
	lSucesores
)

;Funcion de evaluacion = 30*f1 + 0.05*f2
;Donde f1 = nro maximo de fichas colineales de MAX (Maquina) para hacer 4 en raya - 
; nro maximo de fichas colineales de MIN (Humano) para hacer 4 en raya
;f2 = suma de los pesos de los casilleros con fichas de MAX (Maquina)
(defun feval (tablero)
	(setq nromax -1)
	(setq nro_adv_max -1)
	(setq peso 0) 
	(setq i 0)


	;obtenemos los pesos de las fichas
	(dolist (x tablero)
		(if (= -1 x)
			(setq peso (+ peso (nth i *Pesos*)))
		)
		(setq i (+ 1 i))
	)


	(setq 
		v01 (list 0 6 12 18)
		v02 (list 6 12 18 24)
		v03 (list 12 18 24 30)
		v11 (list 1 7 13 19)
		v12 (list 7 13 19 25)
		v13 (list 13 19 25 31)
		v21 (list 2 8 14 20)
		v22 (list 8 14 20 26)
		v23 (list 14 20 26 32)
		v31 (list 3 9 15 21)
		v32 (list 9 15 21 27)
		v33 (list 15 21 27 33)
		v41 (list 4 10 16 22)
		v42 (list 10 16 22 28)
		v43 (list 16 22 28 34)
		v51 (list 5 11 17 23)
		v52 (list 11 17 23 29)
		v53 (list 17 23 29 35)
		h01 (list 0 1 2 3)
		h02 (list 1 2 3 4)
		h03 (list 2 3 4 5)
		h11 (list 6 7 8 9)
		h12 (list 7 8 9 10)
		h13 (list 8 9 10 11)
		h21 (list 12 13 14 15)
		h22 (list 13 14 15 16)
		h23 (list 14 15 16 17)
		h31 (list 18 19 20 21)
		h32 (list 19 20 21 22)
		h33 (list 20 21 22 23)
		h41 (list 24 25 26 27)
		h42 (list 25 26 27 28)
		h43 (list 26 27 28 29)
		h51 (list 30 31 32 33)
		h52 (list 31 32 33 34)
		h53 (list 32 33 34 35)
		di11 (list 12 19 26 33)
		di21 (list 6 13 20 27)
		di22 (list 13 20 27 34)
		di31 (list 0 7 14 21)
		di32 (list 7 14 21 28)
		di33 (list 14 21 28 35)
		di41 (list 1 8 15 22)
		di42 (list 8 15 22 29)
		di51 (list 2 9 16 23)
		dd11 (list 17 22 27 32)
		dd21 (list 11 16 21 26)
		dd22 (list 16 21 26 31)
		dd31 (list 5 10 15 20)
		dd32 (list 10 15 20 25)
		dd33 (list 15 20 25 30)
		dd41 (list 4 9 14 19)
		dd42 (list 9 14 19 24)
		dd51 (list 3 8 13 18)
	)

	(setq listaEval 
		(list 
			v01 v02 v03 v11 v12 v13 v21 v22 v23 v31 v32 v33 v41 v42 v43 v51 v52 v53
			h01 h02 h03 h11 h12 h13 h21 h22 h23 h31 h32 h33 h41 h42 h43 h51 h52 h53
			di11 di21 di22 di31 di32 di33 di41 di42 di51 dd11 dd21 dd22 dd31 dd32 dd33 dd41 dd42 dd51
		)
	)

	(dolist (l listaEval)
		(progn
			(setq naux 0)
			(dolist (x l)
				(progn
					(if (= (nth x tablero) -1)
						(setq naux (+ 1 naux))
						(if (= (nth x tablero) 1)
							(setq naux -50)
						)
					)
				)
			)
			(if (> naux nromax)
				(setq nromax naux)
			)
		)
	)

	(dolist (l listaEval)
		(progn
			(setq nauxadv 0)
			(dolist (x l)
				(progn
					(if (= (nth x tablero) 1)
						(setq nauxadv (+ 1 nauxadv))
						(if (= (nth x tablero) -1)
							(setq nauxadv -50)
						)
					)
				)
			)
			(if (> nauxadv nro_adv_max)
				(setq nro_adv_max nauxadv)
			)
		)
	)

	(if (= nro_adv_max 4)
		-50
		(+ (* 30 (- nromax nro_adv_max)) (* 0.05 peso))
	)
)

;Llama a la funcion de evaluacion
(defun llama_feval (nodo)
	(list (nth 3 nodo) (feval (nth 0 nodo)))
)

;Devuelve el movimiento optimo
(defun eva_minimax (nodo)
	(let ((maximo -100) (minimo 100) (actual '()) (aux '())) 
		(if (es-terminal nodo)
			(llama_feval nodo)
			(progn
				(if (>= (nth 4 nodo) 3)
					(llama_feval nodo)
					(progn
						(cond 
							;Si estamos en un nivel MAX,
							;Debemos maximizar
							((= -1 (nth 2 nodo))
								(dolist (x (sucesores nodo))
									(setq aux (eva_minimax x))
									(if (>= (nth 1 aux) maximo)
										(progn
											(setq actual (list (nth 3 x) (nth 1 aux) (nth 4 x)))
											(setq maximo (nth 1 aux))
										)
									)
								)
							)

							;Si estamos en un nivel MIN
							;Debemos minimizar
							((= 1 (nth 2 nodo))
								(dolist (x (sucesores nodo))
									(setq aux (eva_minimax x))
									(if (< (nth 1 aux) minimo)
										(progn
											(setq actual (list (nth 3 x) (nth 1 aux) (nth 4 x)))
											(setq minimo (nth 1 aux))
										)
									)
								)
							)
						)
						actual
					)
				)
			)
		)
	)
)

;Inicia el algoritmo minimax
;Nodo del arbol = (tablero alturas turno ultColumna_Tomada nivel_profundidad)
(defun algoritmo-minimax (tablero alturas)
	(setq nodoi (list tablero alturas -1 0 0))
	(setq mov (eva_minimax nodoi))
	;(terpri)
	;(format t "Movimiento: ~S Valor feval: ~S Nivel: ~S" (nth 0 mov) (nth 1 mov) (nth 2 mov))
	;(terpri)
	(nth 0 mov)
)

;Funcion que retorna un movimiento escogido aleatoriamente
(defun mov-aleatorio (tablero alturas)
	(setq nodoa (list tablero alturas -1 0 0))
	(setq lSuc (sucesores nodoa))
	(setq mova (random (length lSuc)))
	(nth 3 (nth mova lSuc))
)

;Movimiento para el nivel intermedio
(defun mov-intermedio(tablero alturas)
	(setq nodoint (list tablero alturas -1 0 0))
	(setq lSuc (sucesores nodoint))
	(setq mejor (feval (nth 0 (nth 0 lSuc))))
	(setq posint 0)
	(setq int 0)
	(dolist (x lSuc)
		(progn
			(if (> (feval (nth 0 x)) mejor)
				(progn
					(setq mejor (feval (nth 0 x)))
					(setq posint int)
				)
			)
			(setq int (+ 1 int))
		)
	)
	(nth 3 (nth posint lSuc))
)
;Eliminar los comentarios si
;se desea testear en la linea de comandos de lisp
;(defun juega-humano-lisp ()
;	(print "JUEGA HUMANO")
;	(print "Ingrese columna donde quiere colocar la ficha [0..5]")
;	(setq movh (read))
;	(loop while (not (es-posible *Alturas* movh)) do
;		(print "Ingrese columna donde quiere colocar la ficha [0..5]")
;		(setq movh (read))
;	)
;	(setq actH (editar-tablero *Tablero* *Alturas* movh *FichaHumano*))
;	(setq *Tablero* (nth 0 actH))
;	(setq *Alturas* (nth 1 actH))
;	(mostrar-tablero)
;	(if (= 1 (es-ganador *Tablero* *Alturas* movh *FichaHumano*))
;		(progn
;			(print "GANADOR: HUMANO")
;			(setq *Estado* 1)
;		)
;		(setq *Turno* -1)
;	)
;)

;Eliminar los comentarios si
;se desea testear en la linea de comandos de lisp
;(defun juega-maquina-lisp ()
;	(print "JUEGA MAQUINA")
;	(terpri)
;	(setq movm (algoritmo-minimax *Tablero* *Alturas*))
;	(setq actM (editar-tablero *Tablero* *Alturas* movm *FichaMaquina*))
;	(setq *Tablero* (nth 0 actM))
;	(setq *Alturas* (nth 1 actM))
;	(mostrar-tablero)
;	(if (= 1 (es-ganador *Tablero* *Alturas* movm *FichaMaquina*))
;		(progn
;			(print "GANADOR: MAQUINA")
;			(setq *Estado* -1)
;		)
;		(setq *Turno* 1)
;	)
;)

;Eliminar los comentarios si
;se desea testear en la linea de comandos de lisp
;(defun jugar()
;	(mostrar-tablero)
;	(setq jugadas 0)
;	(loop while (and (= 0 *Estado*) (< jugadas 36)) do
;		(if (= 1 *Turno*)
;			(juega-humano-lisp)
;			(juega-maquina-lisp)
;		)
;		(setq jugadas (+ 1 jugadas))
;	)
;	(if (= 0 *Estado*)
;		(print "EMPATE")
;	)
;	(print "FIN DE JUEGO")
;)

;Actualiza el tablero con el movimiento hecho por el humano
(defun juega-humano (columna)
	(setq movh columna)
	(setq actH (editar-tablero *Tablero* *Alturas* movh *FichaHumano*))
	(setq *Tablero* (nth 0 actH))
	(setq *Alturas* (nth 1 actH))
	(setq *Jugadas* (+ 1 *Jugadas*))
	(if (= 1 (es-ganador *Tablero* *Alturas* movh *FichaHumano*))
		(setq *Estado* 1)
		(setq *Turno* -1)
	)
)

;Retorna el movimiento de la maquina 
;y ademas actualiza el tablero con dicho movimiento
(defun juega-maquina ()
	(if (= *Dificultad* 1)
		(setq movm (mov-aleatorio *Tablero* *Alturas*))
		(if (= *Dificultad* 2)
			(setq movm (mov-intermedio *Tablero* *Alturas*))
			(setq movm (algoritmo-minimax *Tablero* *Alturas*))
		)
	)
	(setq actM (editar-tablero *Tablero* *Alturas* movm *FichaMaquina*))
	(setq *Tablero* (nth 0 actM))
	(setq *Alturas* (nth 1 actM))
	(setq *Jugadas* (+ 1 *Jugadas*))
	(if (= 1 (es-ganador *Tablero* *Alturas* movm *FichaMaquina*))
		(setq *Estado* -1)
		(setq *Turno* 1)
	)
	(- (posicion-real *Alturas* movm) 6)
)

;Retorna el estado actual
(defun getEstado ()
	*Estado*
)