% Tarea final, problemas solucionados con prolog.
%
% Tolentino Pérez Jazmin Yaneli, Junio 2021.
% --------------------------------------------

% *******************************************************************************
% PROBLEMA 1
% Defina un predicado suma_anteriores(Lista,Respuesta)
% Que sea verdadero cuando Respuesta es una lista en la cual cada elemento es la suma de 
% todos los elementos anteriores en la lista original , hasta su posición.
% *******************************************************************************
% En caso de que sea una lista vacía se devuelve una lista vacía.
    suma_anteriores([], []).
% La lista almenos tiene un elemento 
    suma_anteriores([], _, []).
    suma_anteriores([X|T], [X|ST]) :- suma_anteriores(T, X, ST).
% La lista tiene mas de un elemento 
    suma_anteriores([X|T], S, [S1|ST]) :- S1 is X + S, suma_anteriores(T, S1, ST).

% *******************************************************************************
% PROBLEMA 2
% Considerando el siguiente árbol genealógico.
%         a             Expresando únicamente con el predicado padre(Padre,Hijo)
%      b     c          Defina los sigientes predicados: 
%    d   e     f            hermano(X,Y)    nieto(X,Y)
%      g h i  j k           primo(X,Y)      descendiente(X,Y)
% *******************************************************************************
% Construcción del árbol.
% -------------------------------
% 1er generación.
    padre(a,b).
    padre(a,c).

% 2da generación
    padre(b,d).
    padre(b,e).
    padre(c,f).

% 3er generación
    padre(e, g).
    padre(e, h).
    padre(e, i).
    padre(f, j).
    padre(f, k).

% Definición de predicados.
% -------------------------------

% Y es hermano de X, si Z es el padre de ambos y X y Y son diferentes.
    hermano(X,Y):- padre(Z,X),
                padre(Z,Y),
                X\==Y.

% Y es nieto de X, si X es el padre de Z y Z es el padre de Y.
    nieto(X,Y):- padre(X,Z),
                padre(Z,Y).

% Y es primo de X, si Z es padre de X y W es padre de Y y Z y W son hermanos.
    primo(X,Y):- padre(Z,X),
                padre(W,Y),
                hermano(Z,W).

% Y es descendiente de X, si X es padre de Y.
    descendiente(X,Y):- padre(X,Y).

% Y es descendiente de X, si X es padre de Z y Y es descendiente de Z.
    descendiente:- padre(X,Z),
                descendiente(Z,Y).

% *******************************************************************************
% PROBLEMA 3
% Suponga que conjuntos de objetos se representan con listas prolog. Defina los siguientes predicados.
%   elemento_de(Elemento, Conjunto)         union_de(Conjunto1,Conjunto2,Respuesta)
%   cardinalidad_de(Conjunto, Número)       interseccion_de(Conjunto1,Conjunto2,Respuesta)
%   subconjunto_de(Conjunto1,Conjunto2)     diferencia_de(Conjunto1,Conjunto2,Respuesta)
% *******************************************************************************

% elemento_de(Elemento, Conjunto)
% ------------------------------------------
% Verifica si el Objeto pertenece al Conjunto.
    elemento_de(Elemento,Conjunto):- member(Elemento,Conjunto).


%   cardinalidad_de(Conjunto, Número)
% ------------------------------------------
% Calcula la cardinal de un conjunto.
% Cardinalidad de un conjunto vacío = 0.
    cardinalidad_de([],0).

% Cardinalidad de un Conjunto de al menos un elemento.
    cardinalidad_de([X|Y],N):- cardinalidad_de(Y,N1),
                               N is N1 + 1.


% subconjunto_de(Conjunto1,Conjunto2)
% ------------------------------------------
% Conjunto1 vacío, entonces es subconjunto de Conjunto2.
    subconjunto_de([],_).

% Sí la cabeza de Conjunto1 es igual que la del Conjunto2 se verifica que las colas sean iguales también.
    subconjunto_de([C1|T1],[C2|T2]):- subconjunto_de(T1,T2).

% Sí las cabezas de los conjuntos son distintas, la cabeza de Conjunto1 debe pertenecer al resto de Conjunto2
% Se reordena el Conjunto2, colocando la cabeza al final del conjunto y el resto al inicio.
% Se hace la llamada recursiva con el Conjunto1 sin modificar y el nuevo Conjunto2.
    subconjunto_de([C1|T1],[C2|T2]):- member(C1,T2),
                                      append(T2,[C2],NuevoConjunto2),
                                      subconjunto_de([C1|T1],NuevoConjunto2).


% union_de(Conjunto1,Conjunto2,Respuesta)
% ------------------------------------------
    union_de(Conjunto1,[],Conjunto1).
    union_de([],Conjunto2,Conjunto2).
    union_de(Conjunto1,Conjunto2,Union):- union_de_aux(Conjunto1,Conjunto2,[],Union).
    union_de_aux([],C2,_,C2).
    union_de_aux([H|T],C2,Agregados,[H|Union]):- not(member(H,C2)),
 	                                             not(member(H,Agregados)),
                                                 !,
                                                 union_de_aux(T,C2,[H|Agregados],Union).
    union_de_aux([_|T],C2,Agregados,Union):- union_de_aux(T,C2,Agregados,Union).


% interseccion_de(Conjunto1,Conjunto2,Respuesta)
% ------------------------------------------
    interseccion_de(Conjunto1, Conjunto2, Interseccion):- interseccion_de_aux(Conjunto1,Conjunto2,[],Interseccion).

% La interseccion de un conjunto vacio y cualquier conjunto, es el vacio.
    interseccion_de_aux([],_,_,[]).

% Cuando el Conjunto1 no es vacio, se verifica si el elemento en la cabeza de este conjunto también 
% pertenece a Conjunto2, si pertenece, se agrega a la interseccion.
    interseccion_de_aux([X1|T1],C2,Agregados,[X1|Interseccion]):-
                                                        member(X1, C2),
                                                        !,
                                                        not(member(X1, Agregados)),
                                                        interseccion_de_aux(T1,C2,[X1|Agregados],Interseccion).

% Sí el elemento X1 no pertenece a C2, no se agrega a la intersección.
    interseccion_de_aux([_|T1],C2,Agregados,Interseccion):- interseccion_de_aux(T1,C2,Agregados,Interseccion).


% diferencia_de(Conjunto1,Conjunto2,Respuesta)
% ------------------------------------------
% Conjunto de todos los elem que están en A pero no en B.
    diferencia_de([],_,[]):-!.
    diferencia_de(A,[],A):-!.
    diferencia_de(A,B,Diferencia):-diferencia_de_aux(A,B,[],Diferencia).

% Si ya no hay mas elementos en el conjunto A, se devuelve un conjunto vacio [].
diferencia_de_aux([],_,_,[]).

% Si quedan elementos en A, se agregan al resultado siempre y cuando no hayan sido agregados anteriormente y no pertenezcan a B.
    diferencia_de_aux([H|T],B,Agregados,[H|Diferencia]):-
                                                        not(member(H,B)),
                                                        not(member(H,Agregados)),
                                                        diferencia_de_aux(T,B,[H|Agregados],Diferencia).

    diferencia_de_aux([_|T],B,Agregados,Diferencia):- diferencia_de_aux(T,B,Agregados, Diferencia).


% *******************************************************************************
% PROBLEMA 4
%  Usando la base de conocimiento de la red Metro de la CdMx, programe los siguientes predicados para servicio
%  al usuario (use entrada y salida a discreción).
%       rutas (Origen, Destino , Rutas).        ruta_mas_corta (Origen, Destino , Rutacorta).
%  Asumiendo que el tiempo para transitar una estación es siempre de 5 min por cada línea a la que pertenezca
%  la estación.
%               ruta_mas_rápida (Origen, Destino , Rutarapida, tiempo).
% *******************************************************************************
%--------------------
% Color de cada linea
%--------------------
    color(linea_1, rosa).
    color(linea_2, azul_marino).
    color(linea_3, verde_olivo).
    color(linea_4, azul_cielo).
    color(linea_5, amarillo).
    color(linea_6, rojo).
    color(linea_7, naranja).
    color(linea_8, verde_bandera).
    color(linea_9, cafe).
    color(linea_A, morado).
    color(linea_B, gris_verdoso).
    color(linea_12,dorado).

%----------
% Trayectos 
%----------
trayecto(linea_1,  observatorio,   pantitlan).
trayecto(linea_2,  cuatro_caminos, tasquenia).
trayecto(linea_3,  indios_verdes,  universidad).
trayecto(linea_4,  martin_carrera, santa_anita).
trayecto(linea_5,  pantitlan,	   politecnico).
trayecto(linea_6,  el_rosario,	   martin_carrera ).
trayecto(linea_7,  el_rosario,     barranca_del_muerto).
trayecto(linea_8,  garibaldi,      constitucion_de_1917).
trayecto(linea_9,  pantitlan,      tacubaya).
trayecto(linea_A,  pantitlan,      la_paz).
trayecto(linea_B,  ciudad_azteca,  buenavista).
trayecto(linea_12, tlahuac,        mixcoac).

%------------------------
% Secuencia de estaciones 
%------------------------

%----------------------------------
% Linea 1: Observatorio - Pantitlan
%----------------------------------
sigue(observatorio,	tacubaya,	 linea_1).
sigue(tacubaya,	    juanacatlan, linea_1).
sigue(juanacatlan,  chapultepec, linea_1).
sigue(chapultepec,  sevilla,	 linea_1).
sigue(sevilla,      insurgentes, linea_1).
sigue(insurgentes,  cuauhtemoc,	 linea_1).
sigue(cuauhtemoc,   balderas,    linea_1).
sigue(balderas,	    salto_del_agua,	linea_1).
sigue(salto_del_agua,isabel_la_catolica, linea_1).
sigue(isabel_la_catolica,pino_suarez, linea_1).
sigue(pino_suarez,	merced,		 linea_1).
sigue(merced,		candelaria,	 linea_1).
sigue(candelaria,	san_lorenzo, linea_1).
sigue(san_lorenzo,	moctezuma,	 linea_1).
sigue(moctezuma,	balbuena,	 linea_1).
sigue(balbuena,		boulevard_puertoaereo,	linea_1).
sigue(boulevard_puertoaereo, gomez_farias,	linea_1).
sigue(gomez_farias,	zaragoza,	 linea_1).
sigue(zaragoza,		pantitlan,	 linea_1).

%------------------------------------
% Linea 2: Cuatro caminos - Tasquenia
%------------------------------------
sigue(cuatro_caminos,	panteones,	 linea_2).
sigue(panteones,	tacuba,		 linea_2).
sigue(tacuba,		ciutlahuac,	 linea_2).
sigue(ciutlahuac,	popotla,	 linea_2).
sigue(popotla,		colegio_militar, linea_2).
sigue(colegio_militar,	normal,		 linea_2).
sigue(normal,		san_cosme,	 linea_2).
sigue(san_cosme,	revolucion,	 linea_2).
sigue(revolucion,	hidalgo,	 linea_2).
sigue(hidalgo,		bellas_artes,	 linea_2).
sigue(bellas_artes,	allende,	 linea_2).
sigue(allende,		zocalo,		 linea_2).
sigue(zocalo,		pino_suarez,	 linea_2).
sigue(pino_suarez,      san_antonio_abad,linea_2).
sigue(san_antonio_abad, chabacano,	 linea_2).
sigue(chabacano,        viaducto,	 linea_2).
sigue(viaducto,         xola,		 linea_2).
sigue(xola,             villa_de_cortes, linea_2).
sigue(villa_de_cortes,  nativitas,	 linea_2).
sigue(nativitas,        portales,	 linea_2).
sigue(portales,		ermita,		 linea_2).
sigue(ermita,		general_anaya,	 linea_2).
sigue(general_anaya,	tasquenia,	 linea_2).

%-----------------------------------------------
% Linea 3: Indios Verdes - Universidad 
%-----------------------------------------------
sigue(indios_verdes,	deportivo_18_de_marzo,	linea_3).
sigue(deportivo_18_de_marzo,	potrero,	linea_3).
sigue(potrero,		la_raza, 		linea_3).
sigue(la_raza,		tlatelolco,	linea_3).
sigue(tlatelolco,	guerrero,	linea_3).
sigue(guerrero,		hidalgo,	linea_3).
sigue(hidalgo,		juarez,		linea_3).
sigue(juarez,		balderas,	linea_3).
sigue(balderas,		ninios_heroes,	linea_3).
sigue(ninios_heroes,	hospital_general,	linea_3).
sigue(hospital_general,	centro_medico,	linea_3).
sigue(centro_medico,	etiopia,	linea_3).
sigue(etiopia,		eugenia,	linea_3).
sigue(eugenia,		division_del_norte,	linea_3).
sigue(division_del_norte,	zapata,	linea_3).
sigue(zapata,	coyoacan,	linea_3).
sigue(coyoacan,	viveros,	linea_3).
sigue(viveros,	miguel_angel_de_quevedo,	linea_3).
sigue(miguel_angel_de_quevedo,	copilco,	linea_3).
sigue(copilco,	universidad,	linea_3).

%--------------------------------------
% Linea 4: Martin Carrera - Santa Anita
%--------------------------------------
sigue(martin_carrera,	talisman,	linea_4).
sigue(talisman,		bondojito,	linea_4).
sigue(bondojito,	consulado,	linea_4).
sigue(consulado,	canal_del_norte,linea_4).
sigue(canal_del_norte,	morelos,	linea_4).
sigue(morelos,		candelaria,	linea_4).
sigue(candelaria,	fray_servando,	linea_4).
sigue(fray_servando,	jamaica,	linea_4).
sigue(jamaica,		santa_anita,	linea_4).

%---------------------------------
% Linea 5: Politecnico - Pantitlan 
%---------------------------------
sigue(pantitlan,	hangares,	linea_5).
sigue(hangares,		terminal_aerea,	linea_5).
sigue(terminal_aerea,	oceania,	linea_5).
sigue(oceania,		aragon,		linea_5).
sigue(aragon,		eduardo_molina,	linea_5).
sigue(eduardo_molina,	consulado,	linea_5).
sigue(consulado,	valle_gomez,	linea_5).
sigue(valle_gomez,	misterios,	linea_5).
sigue(misterios,	la_raza,	linea_5).
sigue(la_raza,		autobuses_del_norte,		linea_5).
sigue(autobuses_del_norte,	instituto_del_petroleo,	linea_5).
sigue(instituto_del_petroleo,	politecnico,		linea_5).

%-------------------------------------
% Linea 6: El Rosario - Martin Carrera
%-------------------------------------
sigue(el_rosario,	tezozomoc,	linea_6).
sigue(tezozomoc,	uam_azcapotzalco,	linea_6).
sigue(uam_azcapotzalco,	ferreria,	linea_6).
sigue(ferreria,		norte_45,	linea_6).
sigue(norte_45,		vallejo,	linea_6).
sigue(vallejo,		instituto_del_petroleo,	linea_6).
sigue(instituto_del_petroleo,	lindavista,	linea_6).
sigue(lindavista,	deportivo_18_de_marzo,	linea_6).
sigue(deportivo_18_de_marzo, la_villa,	linea_6).
sigue(la_villa,		martin_carrera,	linea_6).

%------------------------------------------
% Linea 7: El Rosario - Barranca del Muerto
%------------------------------------------
sigue(el_rosario,	aquiles_serdan,	linea_7).
sigue(aquiles_serdan,	camarones,	linea_7).
sigue(camarones,	refineria,	linea_7).
sigue(refineria,	tacuba,		linea_7).
sigue(tacuba,		san_joaquin,	linea_7).
sigue(san_joaquin,	polanco,	linea_7).
sigue(polanco,		auditorio,	linea_7).
sigue(auditorio,	constituyentes,	linea_7).
sigue(constituyentes,	tacubaya,	linea_7).
sigue(tacubaya,		san_pedro_de_los_pinos,	linea_7).
sigue(san_pedro_de_los_pinos, san_antonio,	linea_7).
sigue(san_antonio,	mixcoac,	linea_7).
sigue(mixcoac,		barranca_del_muerto,	linea_7).

%------------------------------------------
% Linea 8: Garibaldi - Constitucion de 1917
%------------------------------------------
sigue(garibaldi,	bellas_artes,	linea_8).
sigue(bellas_artes,	san_juan_de_letran,	linea_8).
sigue(san_juan_de_letran,	salto_del_agua,	linea_8).
sigue(salto_del_agua,	doctores,	linea_8).
sigue(doctores,		obrera,		linea_8).
sigue(obrera,		chabacano,	linea_8).
sigue(chabacano,	la_viga,	linea_8).
sigue(la_viga,		santa_anita,	linea_8).
sigue(santa_anita,	coyuca,		linea_8).
sigue(coyuca,		iztacalco,	linea_8).
sigue(iztacalco,	apatlaco,	linea_8).
sigue(apatlaco,		aculco,		linea_8).
sigue(aculco,		escuadron_201,	linea_8).
sigue(escuadron_201,	atlalilco,	linea_8).
sigue(atlalilco,	iztapalapa,	linea_8).
sigue(iztapalapa,	cerro_de_la_estrella,	linea_8).
sigue(cerro_de_la_estrella,	uam_1,	linea_8).
sigue(uam_1,		constitucion_de_1917,	linea_8).

%------------------------------
% Linea 9: Pantitlan - Tacubaya 
%------------------------------
sigue(tacubaya,         patriotismo,      linea_9).
sigue(patriotismo,      chilpancingo,     linea_9).
sigue(chilpancingo,     centro_medico,    linea_9).
sigue(centro_medico,    lazaro_cardenas,  linea_9).
sigue(lazaro_cardenas,  chabacano,        linea_9).
sigue(chabacano,        jamaica,          linea_9).
sigue(jamaica,          mixiuhca,         linea_9).
sigue(mixiuhca,         velodromo,        linea_9).
sigue(velodromo,        ciudad_deportiva, linea_9).
sigue(ciudad_deportiva, puebla,           linea_9).
sigue(puebla,           pantitlan,        linea_9).

%----------------------------------
% Linea A: Pantitlan - La Paz
%----------------------------------
sigue(pantitlan,	agricola_oriental,	linea_a).
sigue(agricola_oriental,	canal_de_san_juan,	linea_a).
sigue(canal_de_san_juan,	tepalcates,	linea_a).
sigue(tepalcates,	guelatao,	linea_a).
sigue(guelatao,		penion_viejo,	linea_a).
sigue(penion_viejo,	acatitla,	linea_a).
sigue(acatitla,	santa_marta,	linea_a).
sigue(santa_marta,	los_reyes,	linea_a).
sigue(los_reyes,	la_paz,	linea_a).

%-------------------------------------
% Linea B: Ciudad Azteca - Buena Vista
%-------------------------------------
sigue(ciudad_azteca,	plaza_aragon,	linea_b).
sigue(plaza_aragon,	olimpica,	linea_b).
sigue(olimpica,		ecatepec,	linea_b).
sigue(ecatepec,		muzquiz,	linea_b).
sigue(muzquiz,		rio_de_los_remedios,	linea_b).
sigue(rio_de_los_remedios,	impulsora,	linea_b).
sigue(impulsora,	nezahualcoyotl,	linea_b).
sigue(nezahualcoyotl,	villa_de_aragon,	linea_b).
sigue(villa_de_aragon,	bosque_de_aragon,	linea_b).
sigue(bosque_de_aragon,	deportivo_oceania,	linea_b).
sigue(deportivo_oceania,	oceania,	linea_b).
sigue(oceania,		romero_rubio,	linea_b).
sigue(romero_rubio,	ricardo_flores_magon,	linea_b).
sigue(ricardo_flores_magon,	san_lazaro,	linea_b).
sigue(san_lazaro, 	morelos, 	linea_b).
sigue(morelos,		tepito,		linea_b).
sigue(tepito, 		lagunilla,	linea_b).
sigue(lagunilla,	garibaldi,	linea_b).
sigue(garibaldi, 	guerrero,	linea_b).
sigue(guerrero, 	buenavista,	linea_b).

%----------------------------
% Linea 12: Tlahuac - Mixcoac
%----------------------------
sigue(mixcoac,	insurgentes_sur,	linea_12).
sigue(insurgentes_sur,	hospital_20_de_noviembre,	linea_12).
sigue(hospital_20_de_noviembre,	zapata,			linea_12).
sigue(zapata,		parque_de_los_venados,		linea_12).
sigue(parque_de_los_venados,	eje_central,	linea_12).
sigue(eje_central,	ermita,		linea_12).
sigue(ermita,	mexicaltzingo,	linea_12).
sigue(mexicaltzingo,	atlalilco,		linea_12).
sigue(atlalilco,	culhuacan,	linea_12).
sigue(culhuacan,	san_andres_tomatlan, linea_12).
sigue(san_andres_tomatlan,	lomas_estrella,	linea_12).
sigue(lomas_estrella,	calle_11,	linea_12).
sigue(calle_11,	periferico_oriente,	linea_12).
sigue(periferico_oriente,	tezonco,	linea_12).
sigue(tezonco,	olivos,	linea_12).
sigue(olivos,	nopalera,	linea_12).
sigue(nopalera,	zapotitlan,	linea_12).
sigue(zapotitlan,	tlaltenco,	linea_12).
sigue(tlaltenco,	tlahuac,	linea_12).

% -------------------------------
% Predicado: pertenece_a(X,Linea)
% -------------------------------
pertenece_a(X,L):- sigue(X,_,L);
                   sigue(_,X,L).

%----------------------------
% Predicado: conecta(X, Y, L)
%----------------------------
conecta(X,Y,L):- sigue(X,Y,L);
                 sigue(Y,X,L). 

%----------------------
% Predicado: cerca(X,Y)
%----------------------
cerca(X,Y):-conecta(X,Y,_).
cerca(X,Y):- conecta(X,Z,_),
             conecta(Z,Y,_).

%------------------------
% Predicado: alcanza(X,Y)
%------------------------
alcanza(X,Y):-conecta(X,Y,_).
alcanza(X,Y):-conecta(X,Z,_),
              conecta(Z,Y,_).
alcanza(X,Y):-conecta(X,Z1,_),
              conecta(Z1,Z2,_),
              conecta(Z2,Y,_).

%---------------------------
% Predicado: alcanzable(X,Y)
%---------------------------
alcanzable(X,Y):-conecta(X,Y,_).
alcanzable(X,Y):-conecta(X,Z,_),
                 alcanzable(Z,Y).

% ---------------------
% Predicado: costo(E,C)
% ---------------------
costo_estacion(E,C):-
    setof(Linea,pertenece_a(E,Linea),Lineas),
    length(Lineas,CantidadLineas), C is 5 * CantidadLineas.
    
% --------------------------
% Predicado: costo_ruta(R,C)
% --------------------------
% Si la ruta no tiene estaciones, el costo es 0
costo_ruta([],0).

% Si la ruta tiene al menos una estación, el costo es el costo de esa estación + el costo del resto de la ruta.
costo_ruta([H|T],Costo):- costo_ruta(T,CostoRestante),
                          costo_estacion(H, CostoEstacion),
                          Costo is CostoEstacion + CostoRestante.
    
% -----------------------------------------------
% Predicado: ruta_mas_corta(Origen,Destino,Ruta).
% -----------------------------------------------
    ruta_mas_corta(O,D,Ruta):-
        writeln('\nConsidera lo siguiente al escribir el nombre de estaciones: \n'),
        writeln('\t1. Escribir en minusculas.'),
        writeln('\t2. Sustituir los espacios por "_".'),
        writeln('\t3. Omitir los acentos.'),
        writeln('\t4. Si contienen ñ, sustituyela por "ni".\n'),
        writeln('Escribe la estación origen:    ')
        read(O),
        writeln('Escribe la estación destino: '),
        read(D),
        ruta_mas_corta_f(O,D,Ruta,Costo),
        format('~nEl costo de la ruta es: ~a~n',[Costo]),
        write('\nLa ruta óptima es: '),
        writeln( Ruta ),
        writeln('').

% Este predicado unifica en la variable Ruta, la ruta óptipa
% entre la estación [Origen] y la estación [Destino]. Unifica tambiém
% en la variable [Costo] el costo de esa ruta optima.
    ruta_mas_corta_f(Origen, Destino, RutaOptima, Costo):-
                                                            setof(_,rutas_f(Origen,Destino,Rutas),_),
                                                            ruta_menor_costo(Rutas, RutaOptima, Costo).

% Si ya solo hay una ruta, es aquella cuyo costo es el mejor.
    ruta_menor_costo([Ruta|[]], Ruta, Costo):- costo_ruta(Ruta, Costo).

% Si al menos hay 2 rutas y R1 es la de menor costo
    ruta_menor_costo([R1,R2|T], Ruta, Costo ):- costo_ruta(R1,CR1),
                                                costo_ruta(R2,CR2),
                                                CR1 =< CR2,
                                                !,
                                                ruta_menor_costo([R1|T], Ruta, Costo).

% Si al menos hay 2 rutas y R2 es la de menor costo
    ruta_menor_costo([R1,R2|T], Ruta, Costo ):- costo_ruta(R1,CR1),
                                                costo_ruta(R2,CR2),
                                                CR1 >= CR2,
                                                ruta_menor_costo([R2|T], Ruta, Costo).

%---------------------
% Predicado: rutas(X,Y)
%---------------------
    rutas(O,D,Rutas):-
        writeln('\nConsidera lo siguiente al escribir el nombre de estaciones: \n'),
        writeln('\t1. Escribir en minusculas.'),
        writeln('\t2. Sustituir los espacios por "_".'),
        writeln('\t3. Omitir los acentos.'),
        writeln('\t4. Si contienen ñ, sustituyela por "ni".\n'),
        writeln('Escribe la estacion de origen: '),
        read(O),
        writeln('Escribe la estación destino: '),
        read(D),
        rutas_f(O,D,Rutas), 
        writeln(''),
    imprime_rutas(Rutas,1).

    imprime_rutas([],_).

    imprime_rutas([H|T], Ruta ):-   format('Ruta ~a: ',[Ruta]),
                                    writeln(H),
                                    writeln(''),
                                    Rsig is Ruta + 1,
                                    imprime_rutas(T, Rsig).

    rutas_f(Origen,Destino,Rutas):- setof(Ruta,limit(10,ruta(Origen,Destino,Ruta)),Rutas).
    
% Si el origen es el mismo que el destino, se devuelve una lista conteniendo
% al origen, o tambien podría devolverse una lista vacia.
    ruta(Origen,Origen,[Origen]):-!.

% Si el origen es diferente al destino, se buscan las rutas por las que 
% se puede llegar al destino partiendo del origen.
    ruta(Origen, Destino, Ruta):- setof(Linea,pertenece_a(Destino,Linea),LineasDestino),
                                  ruta_aux(Origen,Destino,LineasDestino,[Origen],Ruta).

% Primero se verifica si ya se ha llegado a la estacion destino, 
% se devuelve una lista con la estacion destino.
    ruta_aux(O,O,_,_,[O]).

% Si no se ha llegado a la estacion destino
% Se verifica si la linea de la estacion intermedia, entre O y D pertenece a las LineasDestino, 
% para continuar el recorrido unicamente en esa linea.
    ruta_aux(O, D, LineasDestino, Recorridas, [O|Resto]):-
        conecta(O,I,_),					% Se extrae la estacion intermedia
        not(member(I,Recorridas)),		% Se verifica que no haya sido recorrida
        setof(Linea,pertenece_a(I,Linea),LineasEstacionIntermedia), % Se derivan las lineas a las que pertenece esa linea
        intersection(LineasEstacionIntermedia,LineasDestino,Interseccion), % Se intersectan las lineas a las que pertenece la linea intermedia y las LineasDestino
        length(Interseccion,Longitud),
        Longitud > 0, % Si la longitud de la interseccion es > 0, significa que la linea de la 
                    % Estacion intermedia ya corresponde con al menos una linea de la estacion
                    % destino, por lo que bastará con seguir esa linea hasta llegar a la estacion destino.
        seguir_lineas(I,D,Interseccion,[I|Recorridas],Resto),
        !.
        
% Si la linea de la estacion intermedia, entre O y D no pertenece a las LineasDestino, entonces, se continua
% buscando en todas las estaciones que conecten con O y que no hayan sido recorridas.
    ruta_aux(O,D,LineasDestino,Recorridas,[O|Resto]):-
        conecta(O,I,_),
        not(member(I,Recorridas)),
        ruta_aux(I,D,LineasDestino,[I|Recorridas],Resto).

% Si unicamente hay una linea interseccion, unicamente se busca en esa linea.
    seguir_lineas(O,D,[LineaInterseccion|[]],Recorridas,Ruta):- seguir_linea(O,D,LineaInterseccion,Recorridas,Ruta).

% Si hay mas de una linea por la cual se puede llegar al destino, se siguen todas,
    seguir_lineas(O,D,[LineaInterseccion|Resto],Recorridas,Ruta):- 
            seguir_linea(O,D,LineaInterseccion,Recorridas,Ruta),
            !,
            seguir_lineas(O,D,Resto,Recorridas,Ruta).
    
% Si al seguir una linea, la estacion origen y destino son las mismas, se devuelve
% una lista conteniendo a la estacion destino.
    seguir_linea(A,A,_,_,[A]).

% Si aun no se llega a la estacion deseada, se continua unicamente por esa linea,
    seguir_linea(A,B,Linea,Recorridas,[A|Resto]):-  conecta(A,C,Linea),
                                                    not(member(C,Recorridas)),
                                                    seguir_linea(C,B,Linea,[C|Recorridas],Resto).