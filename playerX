// Agent playerMinMax in project Conecta4.mas2j



/* Initial beliefs and rules */
ficha(player1,1).
ficha(player2,2).


//Devuelve una lista con las posiciones ocupadas por el contrario.
fichasContrarias(C) :- .my_name(Me) & ficha(You,F) & You \== Me & .findall([X,Y],tablero(X,Y,F),C).

//Devuelve una lista con las posiciones ocupadas por mÌ
fichasAliadas(A) :-.my_name(Me) & ficha(Me,F) & .findall([X,Y],tablero(X,Y,F),A).

//Comprueba que el tablero est· vacÌo
comprobarVacio :- .findall([X,Y],tablero(X,Y,0),V) & .length(V,64).

/* Funcion de utilidad para el jugador min, nos da un valor negativo indicando
el grado en que le favorece mover a la casilla marcada por X,Y */

asignarValorMin([X,Y],[V]) :- 
	(hace4Raya(X,Y) & V = -100) |
	(hago4Raya(X,Y) & V = -95) |
	(hace3RayaUtil(X,Y) & estrategia(jugarAGanar) & not hago4Raya(X,Y-1) & V = -90) |
	(hace3RayaUtil(X,Y) & estrategia(jugarAPerder) & V = 90) |
	(hago3RayaUtil(X,Y) & estrategia(jugarAGanar) & not hago4Raya(X,Y-1) & V = -85) |
	(hago3RayaUtil(X,Y) & estrategia(jugarAPerder)& V = -85) |
	(hace3Raya(X,Y) & estrategia(jugarAGanar) & not hago4Raya(X,Y-1) & V = -80) |
	(hace3Raya(X,Y) & estrategia(jugarAPerder) & V = -80) |
	(hago3raya(X,Y) & estrategia(jugarAGanar) & not hago4Raya(X,Y-1) & V = -78) |
	(hago3raya(X,Y) & estrategia(jugarAPerder) & V = -78) |
	(hace2RayaUtil(X,Y) & estrategia(jugarAGanar) & not hago4Raya(X,Y-1) & V = -75) |
	(hace2RayaUtil(X,Y) & estrategia(jugarAPerder)& V = -75) |
	(hace2Raya(X,Y) &  estrategia(jugarAGanar) & not hago4Raya(X,Y-1) & V = -70) |
	(hace2Raya(X,Y) &  estrategia(jugarAPerder) & V = -70) |
	(hago2RayaUtil(X,Y) &  estrategia(jugarAGanar) & not hago4Raya(X,Y-1) & V = -65) |
	(hago2RayaUtil(X,Y) &  estrategia(jugarAPerder) & V = -65) |
	(not hago4Raya(X,Y-1) & estrategia(jugarAGanar) & V = -50)|
	V = -25.

/* Funcion de utilidad para el jugador max, nos da un valor positivo indicando
el grado en que le favorece mover a la casilla marcada por X,Y */

asignarValor([X,Y],[V]) :- 
	(hago4Raya(X,Y) & V = 100) |
	(hace4Raya(X,Y) & V = 95) |
	(hago3RayaUtil(X,Y) & estrategia(jugarAGanar) & not hace4Raya(X,Y-1) & V = 90) |
	(hago3RayaUtil(X,Y) & estrategia(jugarAPerder) & V = 90) |
	(hace3RayaUtil(X,Y) & estrategia(jugarAGanar) & not hace4Raya(X,Y-1) & V = 85) |
	(hace3RayaUtil(X,Y) & estrategia(jugarAPerder)& V = 85) |
	(hago3Raya(X,Y) & estrategia(jugarAGanar) & not hace4Raya(X,Y-1) & V = 80) |
	(hago3Raya(X,Y) & estrategia(jugarAPerder) & V = 80) |
	(hace3raya(X,Y) & estrategia(jugarAGanar) & not hace4Raya(X,Y-1) & V = 78) |
	(hace3raya(X,Y) & estrategia(jugarAPerder) & V = 78) |
	(hago2RayaUtil(X,Y) & estrategia(jugarAGanar) & not hace4Raya(X,Y-1) & V = 75) |
	(hago2RayaUtil(X,Y) & estrategia(jugarAPerder)& V = 75) |
	(hago2Raya(X,Y) &  estrategia(jugarAGanar) & not hace4Raya(X,Y-1) & V = 70) |
	(hago2Raya(X,Y) &  estrategia(jugarAPerder) & V = 70) |
	(hace2RayaUtil(X,Y) &  estrategia(jugarAGanar) & not hace4Raya(X,Y-1) & V = 65) |
	(hace2RayaUtil(X,Y) &  estrategia(jugarAPerder) & V = 65) |
	(not hace4Raya(X,Y-1) & estrategia(jugarAGanar) & V = 50)|
	V = 25.


/*Elige una estrategia en función del modo de juego*/
		
elegirEstrategia(X,Y,jugarAGanar) :-
	estrategiaGanar(X,Y).
elegirEstrategia(X,Y,jugarAPerder) :- 
	estrategiaPerder(X,Y).

/*El primer movimiento se realiza por defecto, en la casilla  3,7 si el agente empieza o en la 3,6 si
 empieza el rival */
 
estrategiaGanar(3,7):- comprobarVacio. //Pone la ficha en el (3,7)
estrategiaGanar(X,Y-1):- fichasContrarias(C) & fichasAliadas(A) & .length(C,1)
	& .length(A,0) & .my_name(Me) & ficha(You,F) & You \== Me & tablero(X,Y,F).//Empezando de segundos la pone encima
estrategiaGanar(X,Y):- minMax(X,Y,N,2). //Elegimos el movimiento en base al algoritmo empleado

/*Las estrategias que se usan cuando el modo de juego es Jugar a Perder. El primer movimieno será en una esquina,
y se elegirá el movimiento que reporte el menor valor posible al jugador dado por la función de utilidad.*/

estrategiaPerder(0,7,_,_):- comprobarVacio. //Pone la ficha en el (0,7)
estrategiaPerder(X,Y):- opciones(O) & .print(O) & calcularValor(O,V) & .print(V) & .min(V,Min)  & .nth(S,V,Min) & .nth(S,O,[X,Y]). //Selecciona el movimiento mejor(Max para ganar, min para perder)


calcularValor([],[]).
calcularValor([[X,Y]|Tl],[V|Rest]) :- asignarValorBase([X,Y],V) & calcularValor(Tl,Rest).



//Comprueba si al poner en X,Y hacemos o hace el contrario 4 en raya	
hago4Raya(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer4RayaHor(X,Y,F)| hacer4RayaVer(X,Y,F) | hacer4RayaDiag1(X,Y,F) | hacer4RayaDiag2(X,Y,F)).
hace4Raya(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me & (hacer4RayaHor(X,Y,F)| hacer4RayaVer(X,Y,F) | hacer4RayaDiag1(X,Y,F) | hacer4RayaDiag2(X,Y,F)).

//Comprueba si al poner en X,Y hacemos o hace el contrario 3 en raya con espacio a los lados
hago3RayaUtil(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer3RayaHorUtil(X,Y,F)| hacer3RayaVerUtil(X,Y,F) | hacer3RayaDiag1Util(X,Y,F) | hacer3RayaDiag2Util(X,Y,F)).
hace3RayaUtil(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me & (hacer3RayaHorUtil(X,Y,F)| hacer3RayaVerUtil(X,Y,F) | hacer3RayaDiag1Util(X,Y,F) | hacer3RayaDiag2Util(X,Y,F)).

//Comprueba si al poner en X,Y hacemos o hace el contrario 3 en raya con al menos un espacio a los lados
hago3Raya(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer3RayaHor(X,Y,F)| hacer3RayaVer(X,Y,F) | hacer3RayaDiag1(X,Y,F) | hacer3RayaDiag2(X,Y,F)).
hace3Raya(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me & (hacer3RayaHor(X,Y,F)| hacer3RayaVer(X,Y,F) | hacer3RayaDiag1(X,Y,F) | hacer3RayaDiag2(X,Y,F)).

//Comprueba si al poner en X,Y hacemos o hace el contrario 2 en raya con espacio a los lados
hago2RayaUtil(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer2RayaHorUtil(X,Y,F)| hacer2RayaVerUtil(X,Y,F) | hacer2RayaDiag1Util(X,Y,F) | hacer2RayaDiag2Util(X,Y,F)).
hace2RayaUtil(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me  & (hacer2RayaHorUtil(X,Y,F)| hacer2RayaVerUtil(X,Y,F) | hacer2RayaDiag1Util(X,Y,F) | hacer2RayaDiag2Util(X,Y,F)).

//Comprueba si al poner en X,Y hacemos o hace el contrario 2 en raya 
hago2Raya(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer2RayaHor(X,Y,F)| hacer2RayaVer(X,Y,F) | hacer2RayaDiag1(X,Y,F) | hacer2RayaDiag2(X,Y,F)).
hace2Raya(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me  & (hacer2RayaHor(X,Y,F)| hacer2RayaVer(X,Y,F) | hacer2RayaDiag1(X,Y,F) | hacer2RayaDiag2(X,Y,F)).

//Dada una posiciÛn a poner ficha, comprueba si hace cuatro en raya en cada direcciÛn
hacer4RayaHor(X,Y,F) :- 
	(tableroAux(X-1,Y,F) & tableroAux(X-2,Y,F) & tableroAux(X-3,Y,F)) |
	(tableroAux(X+1,Y,F) & tableroAux(X+2,Y,F) & tableroAux(X+3,Y,F)) |
	(tableroAux(X-1,Y,F) & tableroAux(X-2,Y,F) & tableroAux(X+1,Y,F)) |
	(tableroAux(X+1,Y,F) & tableroAux(X+2,Y,F) & tableroAux(X-1,Y,F)).
	
hacer4RayaVer(X,Y,F) :- 
	(tableroAux(X,Y-1,F) & tableroAux(X,Y-2,F) & tableroAux(X,Y-3,F)) |
	(tableroAux(X,Y+1,F) & tableroAux(X,Y+2,F) & tableroAux(X,Y+3,F)) |
	(tableroAux(X,Y-1,F) & tableroAux(X,Y-2,F) & tableroAux(X,Y+1,F)) |
	(tableroAux(X,Y+1,F) & tableroAux(X,Y+2,F) & tableroAux(X,Y-1,F)).
	
hacer4RayaDiag1(X,Y,F) :- 
	(tableroAux(X-1,Y-1,F) & tableroAux(X-2,Y-2,F) & tableroAux(X-3,Y-3,F)) |
	(tableroAux(X+1,Y+1,F) & tableroAux(X+2,Y+2,F) & tableroAux(X+3,Y+3,F)) |
	(tableroAux(X-1,Y-1,F) & tableroAux(X-2,Y-2,F) & tableroAux(X+1,Y+1,F)) |
	(tableroAux(X+1,+1,F) & tableroAux(X+2,Y+2,F) & tableroAux(X-1,Y-1,F)).
	
hacer4RayaDiag2(X,Y,F) :- 
	(tableroAux(X-1,Y+1,F) & tableroAux(X-2,Y+2,F) & tableroAux(X-3,Y+3,F)) |
	(tableroAux(X+1,Y-1,F) & tableroAux(X+2,Y-2,F) & tableroAux(X+3,Y-3,F)) |
	(tableroAux(X-1,Y+1,F) & tableroAux(X-2,Y+2,F) & tableroAux(X+1,Y-1,F)) |
	(tableroAux(X+1,Y-1,F) & tableroAux(X+2,Y-2,F) & tableroAux(X-1,Y+1,F)).

//Dada una posiciÛn a poner ficha, comprueba si hace tres en raya en cada direcciÛn con espacio a los lados	
hacer3RayaHorUtil(X,Y,F) :-
	(tableroAux(X-1,Y,F) & tableroAux(X-2,Y,F) & tableroAux(X-3,Y,0) & tableroAux(X+1,Y,0)) |
	(tableroAux(X+1,Y,F) & tableroAux(X+2,Y,F) & tableroAux(X+3,Y,0) & tableroAux(X-1,Y,0)) |
	(tableroAux(X-1,Y,F) & tableroAux(X+1,Y,F) & tableroAux(X-2,Y,0) & tableroAux(X+2,Y,0)).
	
hacer3RayaVerUtil(X,Y,F) :-
	(tableroAux(X,Y-1,F) & tableroAux(X,Y-2,F) & tableroAux(X,Y-3,0) & tableroAux(X,Y+1,0)) |
	(tableroAux(X,Y+1,F) & tableroAux(X,Y+2,F) & tableroAux(X,Y+3,0) & tableroAux(X,Y-1,0)) |
	(tableroAux(X,Y-1,F) & tableroAux(X,Y+1,F) & tableroAux(X,Y-2,0) & tableroAux(X,Y+2,0)).
	
hacer3RayaDiag1Util(X,Y,F) :-
	(tableroAux(X-1,Y-1,F) & tableroAux(X-2,Y-2,F) & tableroAux(X-3,Y-3,0) & tableroAux(X+1,Y+1,0)) |
	(tableroAux(X+1,Y+1,F) & tableroAux(X+2,Y+2,F) & tableroAux(X+3,Y+3,0) & tableroAux(X-1,Y-1,0)) |
	(tableroAux(X-1,Y-1,F) & tableroAux(X+1,Y+1,F) & tableroAux(X-2,Y-2,0) & tableroAux(X+2,Y+2,0)).
	
hacer3RayaDiag2Util(X,Y,F) :-
	(tableroAux(X-1,Y+1,F) & tableroAux(X-2,Y+2,F) & tableroAux(X-3,Y+3,0) & tableroAux(X+1,Y-1,0)) |
	(tableroAux(X+1,Y-1,F) & tableroAux(X+2,Y-2,F) & tableroAux(X+3,Y-3,0) & tableroAux(X-1,Y+1,0)) |
	(tableroAux(X-1,Y+1,F) & tableroAux(X+1,Y-1,F) & tableroAux(X-2,Y+2,0) & tableroAux(X+2,Y-2,0)).

//Dada una posiciÛn a poner ficha, comprueba si hace 3 en raya en cada direcciÛn con al menos un espacio a los lados	
hacer3RayaHor(X,Y,F) :-
	(tableroAux(X-1,Y,F) & tableroAux(X-2,Y,F) & (tableroAux(X-3,Y,0) | tableroAux(X+1,Y,0))) |
	(tableroAux(X+1,Y,F) & tableroAux(X+2,Y,F) & (tableroAux(X+3,Y,0) | tableroAux(X-1,Y,0))) |
	(tableroAux(X-1,Y,F) & tableroAux(X+1,Y,F) & (tableroAux(X-2,Y,0) | tableroAux(X+2,Y,0))).
	
hacer3RayaVer(X,Y,F) :-
	(tableroAux(X,Y-1,F) & tableroAux(X,Y-2,F) & (tableroAux(X,Y-3,0) | tableroAux(X,Y+1,0))) |
	(tableroAux(X,Y+1,F) & tableroAux(X,Y+2,F) & (tableroAux(X,Y+3,0) | tableroAux(X,Y-1,0))) |
	(tableroAux(X,Y-1,F) & tableroAux(X,Y+1,F) & (tableroAux(X,Y-2,0) | tableroAux(X,Y+2,0))).
	
hacer3RayaDiag1(X,Y,F) :-
	(tableroAux(X-1,Y-1,F) & tableroAux(X-2,Y-2,F) & (tableroAux(X-3,Y-3,0) | tableroAux(X+1,Y+1,0))) |
	(tableroAux(X+1,Y+1,F) & tableroAux(X+2,Y+2,F) & (tableroAux(X+3,Y+3,0) | tableroAux(X-1,Y-1,0))) |
	(tableroAux(X-1,Y-1,F) & tableroAux(X+1,Y+1,F) & (tableroAux(X-2,Y-2,0) | tableroAux(X+2,Y+2,0))).
	
hacer3RayaDiag2(X,Y,F) :-
	(tableroAux(X-1,Y+1,F) & tableroAux(X-2,Y+2,F) & (tableroAux(X-3,Y+3,0) | tableroAux(X+1,Y-1,0))) |
	(tableroAux(X+1,Y-1,F) & tableroAux(X+2,Y-2,F) & (tableroAux(X+3,Y-3,0) | tableroAux(X-1,Y+1,0))) |
	(tableroAux(X-1,Y+1,F) & tableroAux(X+1,Y-1,F) & (tableroAux(X-2,Y+2,0) | tableroAux(X+2,Y-2,0))).

//Dada una posiciÛn a poner ficha, comprueba si hace 2 en raya en cada direcciÛn con espacio a los lados	
hacer2RayaHorUtil(X,Y,F) :-
	(tableroAux(X-1,Y,F) & tableroAux(X-2,Y,0) & tableroAux(X+1,Y,0)) |
	(tableroAux(X+1,Y,F) & tableroAux(X+2,Y,0) & tableroAux(X-1,Y,0)) .
	
hacer2RayaVerUtil(X,Y,F) :-
	(tableroAux(X,Y-1,F) & tableroAux(X,Y-2,0) & tableroAux(X,Y+1,0)) |
	(tableroAux(X,Y+1,F) & tableroAux(X,Y+2,0) & tableroAux(X,Y-1,0)) .
	
hacer2RayaDiag1Util(X,Y,F) :-
	(tableroAux(X-1,Y-1,F) & tableroAux(X-2,Y-2,0) & tableroAux(X+1,Y+1,0)) |
	(tableroAux(X+1,Y+1,F) & tableroAux(X+2,Y+2,0) & tableroAux(X-1,Y-1,0)) .
	
hacer2RayaDiag2Util(X,Y,F) :-
	(tableroAux(X-1,Y+1,F) & tableroAux(X-2,Y+2,0) & tableroAux(X+1,Y-1,0)) |
	(tableroAux(X+1,Y-1,F) & tableroAux(X+2,Y-2,0) & tableroAux(X-1,Y+1,0)) .
	
//Dada una posiciÛn a poner ficha, comprueba si hace 2 en raya en cada direcciÛn con al menos un espacio a los lados
hacer2RayaHor(X,Y,F) :-
	(tableroAux(X-1,Y,F) & (tableroAux(X-2,Y,0) | tableroAux(X+1,Y,0))) |
	(tableroAux(X+1,Y,F) & (tableroAux(X+2,Y,0) | tableroAux(X-1,Y,0))) .
	
hacer2RayaVerUtil(X,Y,F) :-
	(tableroAux(X,Y-1,F) & (tableroAux(X,Y-2,0) | tableroAux(X,Y+1,0))) |
	(tableroAux(X,Y+1,F) & (tableroAux(X,Y+2,0) | tableroAux(X,Y-1,0))) .
	
hacer2RayaDiag1Util(X,Y,F) :-
	(tableroAux(X-1,Y-1,F) & (tableroAux(X-2,Y-2,0) | tableroAux(X+1,Y+1,0))) |
	(tableroAux(X+1,Y+1,F) & (tableroAux(X+2,Y+2,0) | tableroAux(X-1,Y-1,0))) .
	
hacer2RayaDiag2Util(X,Y,F) :-
	(tableroAux(X-1,Y+1,F) & (tableroAux(X-2,Y+2,0) | tableroAux(X+1,Y-1,0))) |
	(tableroAux(X+1,Y-1,F) & (tableroAux(X+2,Y-2,0) | tableroAux(X-1,Y+1,0))) .
	
	
opciones(O) :- calcularOpciones(0,7,O).
calcularOpciones(A,A,[[X,Y]]) :- opcion(A,[X,Y]).
calcularOpciones(A,A,[]) :- opcion(A,[]).
calcularOpciones(A,B,[[X,Y]|Tl]) :- opcion(A,[X,Y]) & A <= B & calcularOpciones(A+1,B,Tl). 
calcularOpciones(A,B,Tl) :- opcion(A,[]) & A <= B & calcularOpciones(A+1,B,Tl). 

//Para una columna dada, devuelve la posición donde va a caer la ficha
opcion(A,[]).
opcion(A,[A,B]) :- .findall(Y,tablero(A,Y,0),Vacias) & .max(Vacias,B).
//Devuelve posiciones donde puede poner ficha, calculando cada una para cada columna, encima de la última ficha
opcionesAux(O) :- calcularOpcionesAux(0,8,O).
calcularOpcionesaux(A,A,[[X,Y]]) :- opcionAux(A,[X,Y]).
calcularOpcionesAux(A,A,[]) :- opcionAux(A,[]).
calcularOpcionesAux(A,B,[[X,Y]|Tl]) :- opcionAux(A,[X,Y]) & A <= B & calcularOpcionesAux(A+1,B,Tl). 
calcularOpcionesAux(A,B,Tl) :- opcionAux(A,[]) & A <= B & calcularOpcionesAux(A+1,B,Tl). 

//Para una columna dada, devuelve la posición donde va a caer la ficha en base al estado del tablero auxiliar creado
//para anticipar los movimientos posibles
opcionAux(A,[]).
opcionAux(A,[A,B]) :- .findall(Y,tableroAux(A,Y,0),Vacias) & .max(Vacias,B).
							
//Se actualiza el tablero auxiliar con la ficha del jugador al que le tocaría mover durante la predicción de las jugadas
// en el algoritmo implementado
actualizaTableroAuxMax(X,Y) :- .my_name(Me) & ficha(Me,F) & .abolish(tableroAux(X,Y,0)) & .asserta(tableroAux(X,Y,F)).		
actualizaTableroAuxMin(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me & .abolish(tableroAux(X,Y,0)) & .asserta(tableroAux(X,Y,F)).								

//Vuelve el tablero auxiliar al estado anterior a jugar la ficha en la posicion (X,Y).
revertirTableroAux(X,Y) :- .abolish(tableroAux(X,Y,F)) & F \== 0 & .asserta(tableroAux(X,Y,0)).


//Calcular jugadas futuras

/* Clausula que inicia el algoritmo propuesto, como en el algoritmo minimax se expanden las ramas con todos los posibles movimientos hasta la profundidad
marcada, al llegar a la profundidad señalada, calcula el mayor valor de cada nodo, o el menor, dependiendo si le toca jugar a min o a max, y suma dicho valor
con el obtenido por la funcion de utilidad en el nodo padre recursivamente hasta llegar al más superior. */

minMax(X,Y,N,D):-  opcionesAux(O)& .print("Opciones: ",O) &  empiezaMax(X,Y,O,N,D) &.print("La evaluacion ES: ",N).

/* Para el primer turno queremos que Rx y Ry unifiquen con los valores del nodo que hayan obtenido la mayor evaluacion posible,
para el resto de los casos en la recursividad solo nos interesan el valor de dicha evaluacion en cada Rama. 

Asignamos el valor y lo sumamos con el obtenido en la recursividad de la rama min para el caso de colocar esa ficha, para el resto de nodos se
realiza la misma operacion, y finalmente se comporan dos a dos, eligiendo el que tiene la mayor evaluación y sus coordenadas asociadas.


*/


empiezaMax(X,Y,[[X,Y]],[N],Profundidad):-  Profundidad >1 & asignarValor([X,Y],Nmax) & .member(NmaxAp,Nmax) & actualizaTableroAuxMax(X,Y) 
							& opcionesAux(O)  & min(O,Nmin,Profundidad-1) & .member(NminAp,Nmin) & N=NmaxAp+NminAp & revertirTableroAux(X,Y).	
empiezaMax(Rx,Ry,[[X,Y]|Cdr],[N],Profundidad):-   Profundidad >1 &   Cdr \== [] & asignarValor([X,Y],Nmax) & .member(NmaxAp,Nmax) &actualizaTableroAuxMax(X,Y) 
							& opcionesAux(O) & min(O,Nmin,Profundidad-1) & .member(NminAp,Nmin) & N2Ap= NmaxAp+NminAp
							 & N2= [N2Ap] & revertirTableroAux(X,Y) & empiezaMax(X3,Y3,Cdr,N3,Profundidad) 
							& .concat(N2,N3,Nv) & .max(Nv,N) & ((N2 \== N3 &  .member(N2v,N2) & .max(Nv,N2v)  & Rx=X & Ry =Y)
							| (  .member(N3v,N3) &.max(Nv,N3v)  & Rx=X3 & Ry =Y3)).							

/* estas clausulas funcionan de forma analoga a las descritas arriba, llaman recursivamente la clausula min para simular el turno del oponente
colocada cada una de las opciones actuales disponibles en el tablero auxiliar. Se consideran nodos terminales los que se encuentran a la profundidad determinada
como parametro en la llamada a la clausula del algoritmo, de estos nodos se calcula su valor con la funcion de utilidad correspondiente, y el resultado se 
suma a la evaluacion del nodo padre.*/


max([[X,Y]],N,1):-  asignarValor([X,Y],N).
max([[X,Y]],[N],Profundidad):-  Profundidad >1 & asignarValor([X,Y],Nmax) & .member(NmaxAp,Nmax) & actualizaTableroAuxMax(X,Y) 
							& opcionesAux(O)  & min(O,Nmin,Profundidad-1) & .member(NminAp,Nmin) 
							& N=NmaxAp+NminAp& revertirTableroAux(X,Y).						
max([[X,Y]|Cdr],[N],1):-    Cdr \== [] & asignarValor([X,Y],N1) & max(Cdr,N2,1) 
									&  .concat(N1,N2,Nv) 
									& .max(Nv,N) .

max([[X,Y]|Cdr],[N],Profundidad):-   Profundidad >1 &   Cdr \== [] & asignarValor([X,Y],Nmax) & .member(NmaxAp,Nmax) &actualizaTableroAuxMax(X,Y) 
							& opcionesAux(O) 
							& min(O,Nmin,Profundidad-1) & .member(NminAp,Nmin) & N2Ap= NmaxAp+NminAp
							 & N2= [N2Ap] 
							& revertirTableroAux(X,Y) & max(Cdr,N3,Profundidad) .							



min([[X,Y]],N,1):- asignarValorMin([X,Y],N) .
min([[X,Y]],[N],Profundidad):-  Profundidad >1 &  asignarValorMin([X,Y],Nmin) & .member(NminAp,Nmin) & actualizaTableroAuxMin(X,Y) 
							& opcionesAux(O) & max(O,Nmax,Profundidad-1) & .member(NmaxAp,Nmax) & N=NminAp+NmaxAp & revertirTableroAux(X,Y).
min([[X,Y]|Cdr],[N],1):-  Cdr \== []  & asignarValorMin([X,Y],N1)  & min(Cdr,N2,1)
									& .concat(N1,N2,Nv) & .min(Nv,N) .
min([[X,Y]|Cdr],[N],Profundidad):- Profundidad >1 & Cdr \== [] & asignarValorMin([X,Y],Nmin) & .member(NminAp,Nmin) & actualizaTableroAuxMin(X,Y) 
							& opcionesAux(O) & max(O,Nmax,Profundidad-1) & .member(NmaxAp,Nmax) & N2Ap=NminAp+NmaxAp & revertirTableroAux(X,Y) 
							& min(Cdr,N3,Profundidad) & N2= [N2Ap]
							& .concat(N2,N3,Nv) & .min(Nv,N) .
						

//actualizarListaTablero(X,Y,Tablero) :- 							
							
							
/* Initial goals */



!start.



/* Plans */

+!start <- !jugar.

+!jugar : .my_name(Me) & turno(Me)[source(percept)] & estrategia(Est)[source(percept)]  <- .wait(1000);
		.findall(tableroAux(X,Y,Z),tablero(X,Y,Z),L);
		for(.member(T,L)){ //Añadimos a las creencias del agente un tablero auxiliar que sirve de base para realizar la prediccion de las jugadas
			.asserta(T);
		
			};
     ?elegirEstrategia(X,Y,Est); 
	 put(X);
	 .abolish(tableroAux(_,_,_));
	 !start.
+!jugar <- .wait(1); !start.

