// Agent player1 in project conecta4.mas2j

/* Initial beliefs and rules */

//Fichas correspondientes a player1 y player2
ficha(player1,1).
ficha(player2,2).

//ElecciÛn de estrategia
elegirEstrategia(X,Y,jugarAGanar) :- estrategiaGanar(X,Y).	
elegirEstrategia(X,Y,jugarAPerder) :- estrategiaPerder(X,Y).

//Comprueba que el tablero est· vacÌo
comprobarVacio :- .findall([X,Y],tablero(X,Y,0),V) & .length(V,64).

//Estrategia para perder
estrategiaPerder(0,7):- comprobarVacio. //Pone la ficha en el (0,7) si empezamos
estrategiaPerder(X,Y):- opciones(O) & calcularValor(O,V) & .min(V,Min)  & .nth(S,V,Min) & .nth(S,O,[X,Y]). //Selecciona el movimiento mejor, en este caso min para perder

//Estrategia para ganar
estrategiaGanar(3,7):- comprobarVacio. //Pone la ficha en el (3,7) si empezamos
estrategiaGanar(X,Y-1):- fichasContrarias(C) & fichasAliadas(A) & .length(C,1) & .length(A,0) & .my_name(Me) & ficha(You,F) & You \== Me & tablero(X,Y,F).//Empezando de segundos la pone encima
estrategiaGanar(X,Y):- opciones(O) & calcularValor(O,V) & .print(V) & .max(V,Max) & .nth(S,V,Max) & .nth(S,O,[X,Y]) . //Selecciona el movimiento mejor, en este caso max para perder

//Para cada posiciÛn donde es posible poner ficha, asigna un valor en funciÛn de que estado se consiga con ese movimiento
calcularValor([],[]).
calcularValor([[X,Y]|Tl],[V|Rest]) :- asignarValor([X,Y],V) & calcularValor(Tl,Rest).

//Los valores se asignan de mayor a menor, el mayor es preferible para ganar, para perder el menor
//Hay que comprobar que al colocar ficha en un sitio, si se quiere ganar, no le permita arriba hacer 4 en raya al otro
asignarValor([X,Y],V) :- 
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
	V = 25.//Caso base,calquiera que se pueda
	//El comprobar que no haga 4 en raya el otro, sÛlo es necesario al ganar

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
	(tablero(X-1,Y,F) & tablero(X-2,Y,F) & tablero(X-3,Y,F)) |
	(tablero(X+1,Y,F) & tablero(X+2,Y,F) & tablero(X+3,Y,F)) |
	(tablero(X-1,Y,F) & tablero(X-2,Y,F) & tablero(X+1,Y,F)) |
	(tablero(X+1,Y,F) & tablero(X+2,Y,F) & tablero(X-1,Y,F)).
	
hacer4RayaVer(X,Y,F) :- 
	(tablero(X,Y-1,F) & tablero(X,Y-2,F) & tablero(X,Y-3,F)) |
	(tablero(X,Y+1,F) & tablero(X,Y+2,F) & tablero(X,Y+3,F)) |
	(tablero(X,Y-1,F) & tablero(X,Y-2,F) & tablero(X,Y+1,F)) |
	(tablero(X,Y+1,F) & tablero(X,Y+2,F) & tablero(X,Y-1,F)).
	
hacer4RayaDiag1(X,Y,F) :- 
	(tablero(X-1,Y-1,F) & tablero(X-2,Y-2,F) & tablero(X-3,Y-3,F)) |
	(tablero(X+1,Y+1,F) & tablero(X+2,Y+2,F) & tablero(X+3,Y+3,F)) |
	(tablero(X-1,Y-1,F) & tablero(X-2,Y-2,F) & tablero(X+1,Y+1,F)) |
	(tablero(X+1,+1,F) & tablero(X+2,Y+2,F) & tablero(X-1,Y-1,F)).
	
hacer4RayaDiag2(X,Y,F) :- 
	(tablero(X-1,Y+1,F) & tablero(X-2,Y+2,F) & tablero(X-3,Y+3,F)) |
	(tablero(X+1,Y-1,F) & tablero(X+2,Y-2,F) & tablero(X+3,Y-3,F)) |
	(tablero(X-1,Y+1,F) & tablero(X-2,Y+2,F) & tablero(X+1,Y-1,F)) |
	(tablero(X+1,Y-1,F) & tablero(X+2,Y-2,F) & tablero(X-1,Y+1,F)).

//Dada una posiciÛn a poner ficha, comprueba si hace tres en raya en cada direcciÛn con espacio a los lados	
hacer3RayaHorUtil(X,Y,F) :-
	(tablero(X-1,Y,F) & tablero(X-2,Y,F) & tablero(X-3,Y,0) & tablero(X+1,Y,0)) |
	(tablero(X+1,Y,F) & tablero(X+2,Y,F) & tablero(X+3,Y,0) & tablero(X-1,Y,0)) |
	(tablero(X-1,Y,F) & tablero(X+1,Y,F) & tablero(X-2,Y,0) & tablero(X+2,Y,0)).
	
hacer3RayaVerUtil(X,Y,F) :-
	(tablero(X,Y-1,F) & tablero(X,Y-2,F) & tablero(X,Y-3,0) & tablero(X,Y+1,0)) |
	(tablero(X,Y+1,F) & tablero(X,Y+2,F) & tablero(X,Y+3,0) & tablero(X,Y-1,0)) |
	(tablero(X,Y-1,F) & tablero(X,Y+1,F) & tablero(X,Y-2,0) & tablero(X,Y+2,0)).
	
hacer3RayaDiag1Util(X,Y,F) :-
	(tablero(X-1,Y-1,F) & tablero(X-2,Y-2,F) & tablero(X-3,Y-3,0) & tablero(X+1,Y+1,0)) |
	(tablero(X+1,Y+1,F) & tablero(X+2,Y+2,F) & tablero(X+3,Y+3,0) & tablero(X-1,Y-1,0)) |
	(tablero(X-1,Y-1,F) & tablero(X+1,Y+1,F) & tablero(X-2,Y-2,0) & tablero(X+2,Y+2,0)).
	
hacer3RayaDiag2Util(X,Y,F) :-
	(tablero(X-1,Y+1,F) & tablero(X-2,Y+2,F) & tablero(X-3,Y+3,0) & tablero(X+1,Y-1,0)) |
	(tablero(X+1,Y-1,F) & tablero(X+2,Y-2,F) & tablero(X+3,Y-3,0) & tablero(X-1,Y+1,0)) |
	(tablero(X-1,Y+1,F) & tablero(X+1,Y-1,F) & tablero(X-2,Y+2,0) & tablero(X+2,Y-2,0)).

//Dada una posiciÛn a poner ficha, comprueba si hace 3 en raya en cada direcciÛn con al menos un espacio a los lados	
hacer3RayaHor(X,Y,F) :-
	(tablero(X-1,Y,F) & tablero(X-2,Y,F) & (tablero(X-3,Y,0) | tablero(X+1,Y,0))) |
	(tablero(X+1,Y,F) & tablero(X+2,Y,F) & (tablero(X+3,Y,0) | tablero(X-1,Y,0))) |
	(tablero(X-1,Y,F) & tablero(X+1,Y,F) & (tablero(X-2,Y,0) | tablero(X+2,Y,0))).
	
hacer3RayaVer(X,Y,F) :-
	(tablero(X,Y-1,F) & tablero(X,Y-2,F) & (tablero(X,Y-3,0) | tablero(X,Y+1,0))) |
	(tablero(X,Y+1,F) & tablero(X,Y+2,F) & (tablero(X,Y+3,0) | tablero(X,Y-1,0))) |
	(tablero(X,Y-1,F) & tablero(X,Y+1,F) & (tablero(X,Y-2,0) | tablero(X,Y+2,0))).
	
hacer3RayaDiag1(X,Y,F) :-
	(tablero(X-1,Y-1,F) & tablero(X-2,Y-2,F) & (tablero(X-3,Y-3,0) | tablero(X+1,Y+1,0))) |
	(tablero(X+1,Y+1,F) & tablero(X+2,Y+2,F) & (tablero(X+3,Y+3,0) | tablero(X-1,Y-1,0))) |
	(tablero(X-1,Y-1,F) & tablero(X+1,Y+1,F) & (tablero(X-2,Y-2,0) | tablero(X+2,Y+2,0))).
	
hacer3RayaDiag2(X,Y,F) :-
	(tablero(X-1,Y+1,F) & tablero(X-2,Y+2,F) & (tablero(X-3,Y+3,0) | tablero(X+1,Y-1,0))) |
	(tablero(X+1,Y-1,F) & tablero(X+2,Y-2,F) & (tablero(X+3,Y-3,0) | tablero(X-1,Y+1,0))) |
	(tablero(X-1,Y+1,F) & tablero(X+1,Y-1,F) & (tablero(X-2,Y+2,0) | tablero(X+2,Y-2,0))).

//Dada una posiciÛn a poner ficha, comprueba si hace 2 en raya en cada direcciÛn con espacio a los lados	
hacer2RayaHorUtil(X,Y,F) :-
	(tablero(X-1,Y,F) & tablero(X-2,Y,0) & tablero(X+1,Y,0)) |
	(tablero(X+1,Y,F) & tablero(X+2,Y,0) & tablero(X-1,Y,0)) .
	
hacer2RayaVerUtil(X,Y,F) :-
	(tablero(X,Y-1,F) & tablero(X,Y-2,0) & tablero(X,Y+1,0)) |
	(tablero(X,Y+1,F) & tablero(X,Y+2,0) & tablero(X,Y-1,0)) .
	
hacer2RayaDiag1Util(X,Y,F) :-
	(tablero(X-1,Y-1,F) & tablero(X-2,Y-2,0) & tablero(X+1,Y+1,0)) |
	(tablero(X+1,Y+1,F) & tablero(X+2,Y+2,0) & tablero(X-1,Y-1,0)) .
	
hacer2RayaDiag2Util(X,Y,F) :-
	(tablero(X-1,Y+1,F) & tablero(X-2,Y+2,0) & tablero(X+1,Y-1,0)) |
	(tablero(X+1,Y-1,F) & tablero(X+2,Y-2,0) & tablero(X-1,Y+1,0)) .
	
//Dada una posiciÛn a poner ficha, comprueba si hace 2 en raya en cada direcciÛn con al menos un espacio a los lados
hacer2RayaHor(X,Y,F) :-
	(tablero(X-1,Y,F) & (tablero(X-2,Y,0) | tablero(X+1,Y,0))) |
	(tablero(X+1,Y,F) & (tablero(X+2,Y,0) | tablero(X-1,Y,0))) .
	
hacer2RayaVerUtil(X,Y,F) :-
	(tablero(X,Y-1,F) & (tablero(X,Y-2,0) | tablero(X,Y+1,0))) |
	(tablero(X,Y+1,F) & (tablero(X,Y+2,0) | tablero(X,Y-1,0))) .
	
hacer2RayaDiag1Util(X,Y,F) :-
	(tablero(X-1,Y-1,F) & (tablero(X-2,Y-2,0) | tablero(X+1,Y+1,0))) |
	(tablero(X+1,Y+1,F) & (tablero(X+2,Y+2,0) | tablero(X-1,Y-1,0))) .
	
hacer2RayaDiag2Util(X,Y,F) :-
	(tablero(X-1,Y+1,F) & (tablero(X-2,Y+2,0) | tablero(X+1,Y-1,0))) |
	(tablero(X+1,Y-1,F) & (tablero(X+2,Y-2,0) | tablero(X-1,Y+1,0))) .

//Devuelve posiciones donde puede poner ficha, calculando cada una para cada columna, encima de la ˙ltima ficha
opciones(O) :- calcularOpciones(0,7,O).
calcularOpciones(A,A,[[X,Y]]) :- opcion(A,[X,Y]).
calcularOpciones(A,A,[]) :- opcion(A,[]).
calcularOpciones(A,B,[[X,Y]|Tl]) :- opcion(A,[X,Y]) & A <= B & calcularOpciones(A+1,B,Tl). 
calcularOpciones(A,B,Tl) :- opcion(A,[]) & A <= B & calcularOpciones(A+1,B,Tl). 

//Para una columna dada, devuelve la posiciÛn donde va a caer la ficha
opcion(A,[]).
opcion(A,[A,B]) :- .findall(Y,tablero(A,Y,0),Vacias) & .max(Vacias,B).

//Devuelve una lista con las posiciones ocupadas por el contrario.
fichasContrarias(C) :- .my_name(Me) & ficha(You,F) & You \== Me & .findall([X,Y],tablero(X,Y,F),C).

//Devuelve una lista con las posiciones ocupadas por mÌ
fichasAliadas(A) :-.my_name(Me) & ficha(Me,F) & .findall([X,Y],tablero(X,Y,F),A).


/* Initial goals */

!start.

/* Plans */  

+!start : comprobarVacio & .abolish(ultima(_,_)) & .abolish(otras(_)) & .abolish(ultimaOtro(_)) & .asserta(ultima(X,Y))  & .asserta(otras([])) & .asserta(ultimaOtro(O)) <- 
	.wait(10); !jugar.
+!start <- !jugar.

+!jugar : .my_name(Me) & turno(Me)[source(percept)] & estrategia(Est)[source(percept)] & elegirEstrategia(X,Y,Est)   <- .wait(1000);
	put(X);
	!start.
+!jugar <- .wait(1); !start.

