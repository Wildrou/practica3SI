// Agent player1 in project conecta4.mas2j



/* Initial beliefs and rules */


//Fichas correspondientes a player1 y player2
ficha(player1,1).
ficha(player2,2).

elegirEstrategia(X,Y,jugarAGanar) :-
	estrategiaGanar(X,Y).
	
elegirEstrategia(X,Y,jugarAPerder) :- estrategiaPerder(X,Y).

//Comprueba que el tablero está vacío
comprobarVacio :- .findall([X,Y],tablero(X,Y,0),V) & .length(V,64).

//Estrategia para perder si empezamos de primeros.
estrategiaPerder(0,7):- comprobarVacio. //Pone la ficha en el (0,7)
estrategiaPerder(X,Y):- opciones(O) & .print(O) & calcularValor(O,V) & .print(V) & .min(V,Min)  & .nth(S,V,Min) & .nth(S,O,[X,Y]). //Selecciona el movimiento mejor(Max para ganar, min para perder)

estrategiaGanar(3,7):- comprobarVacio. //Pone la ficha en el (3,7)
estrategiaGanar(X,Y):- opciones(O) & calcularValor(O,V) & .print(V) & .max(V,Max) & .nth(S,V,Max) & .nth(S,O,[X,Y]) .


calcularValor([],[]).
calcularValor([[X,Y]|Tl],[V|Rest]) :- asignarValor([X,Y],V) & calcularValor(Tl,Rest).

asignarValor([X,Y],V) :- 
	(hago4Raya(X,Y) & V = 100) |
	(hace4Raya(X,Y) & V = 95) |
	(hago3RayaUtil(X,Y) & not hace4Raya(X,Y-1) & V = 90) |
	(hace3RayaUtil(X,Y) & not hace4Raya(X,Y-1) & V = 85) |
	(hago3Raya(X,Y) & not hace4Raya(X,Y-1) & V = 80) |
	(hago2RayaUtil(X,Y) & not hace4Raya(X,Y-1) & V = 75) |
	(hago2Raya(X,Y) & not hace4Raya(X,Y-1) & V = 70) |
	(hace2RayaUtil(X,Y) & not hace4Raya(X,Y-1) & V = 65) |
	V = 50.//Caso base, todavía hay que meter más comprobaciones

//Comprueba si al poner en X,Y hacemos o hace el contrario 4 en raya	
hago4Raya(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer4RayaHor(X,Y,F)| hacer4RayaVer(X,Y,F) | hacer4RayaDiag1(X,Y,F) | hacer4RayaDiag2(X,Y,F)).
hace4Raya(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me & (hacer4RayaHor(X,Y,F)| hacer4RayaVer(X,Y,F) | hacer4RayaDiag1(X,Y,F) | hacer4RayaDiag2(X,Y,F)).

//Comprueba si al poner en X,Y hacemos o hace el contrario 3 en raya que se pueda convertir en 4
hago3RayaUtil(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer3RayaHorUtil(X,Y,F)| hacer3RayaVerUtil(X,Y,F) | hacer3RayaDiag1Util(X,Y,F) | hacer3RayaDiag2Util(X,Y,F)).
hace3RayaUtil(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me & (hacer3RayaHorUtil(X,Y,F)| hacer3RayaVerUtil(X,Y,F) | hacer3RayaDiag1Util(X,Y,F) | hacer3RayaDiag2Util(X,Y,F)).

hago3Raya(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer3RayaHor(X,Y,F)| hacer3RayaVer(X,Y,F) | hacer3RayaDiag1(X,Y,F) | hacer3RayaDiag2(X,Y,F)).

hago2RayaUtil(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer2RayaHorUtil(X,Y,F)| hacer2RayaVerUtil(X,Y,F) | hacer2RayaDiag1Util(X,Y,F) | hacer2RayaDiag2Util(X,Y,F)).
hace2RayaUtil(X,Y) :- .my_name(Me) & ficha(You,F) & You \== Me  & (hacer2RayaHorUtil(X,Y,F)| hacer2RayaVerUtil(X,Y,F) | hacer2RayaDiag1Util(X,Y,F) | hacer2RayaDiag2Util(X,Y,F)).


hago2Raya(X,Y) :- .my_name(Me) & ficha(Me,F) & (hacer2RayaHor(X,Y,F)| hacer2RayaVer(X,Y,F) | hacer2RayaDiag1(X,Y,F) | hacer2RayaDiag2(X,Y,F)).

//Dada una posición a poner ficha, comprueba si hace cuatro en raya en cada dirección
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

//Comprueba que donde se ponga la ficha hace 2 en raya
hago2raya(X,Y) :- contiguas(X,Y,C) & alMenosUnaMia(C).

//Comprueba que conla ficha a poner hace dos en raya que pueda llegar a ser un 4 en raya
hago2rayaUtil(X,Y):- hago2rayaUtilVer(X,Y) | hago2rayaUtilHor(X,Y) | hago2rayaUtilDiag1(X,Y) | hago2rayaUtilDiag2(X,Y).

hago2rayaUtilHor(X,Y) :- .my_name(Me) & ficha(Me,F) & tablero(X-1,Y,F) & ((tablero(X-2,Y,0) & tablero(X+1,Y,0)) | (tablero(X-2,Y,0) & tablero(X-3,Y,0)) | (tablero(X+1,Y,0) & tablero(X+2,Y,0))).
hago2rayaUtilHor(X,Y) :- .my_name(Me) & ficha(Me,F) & tablero(X+1,Y,F) & ((tablero(X-1,Y,0) & tablero(X+2,Y,0)) | (tablero(X-1,Y,0) & tablero(X-2,Y,0)) | (tablero(X+2,Y,0) & tablero(X+3,Y,0))).

//Comprueba que de la lista que se le pasa al menos una ficha es nuestra
alMenosUnaMia([[X,Y]|Tl]) :- .my_name(Me) & ficha(Me,F) & tablero(X,Y,F).
alMenosUnaMia([_|Tl]) :- alMenosUnaMia(Tl).

libre([],[]).
libre([[X,Y]|Tl],[X,Y]):- contiguas(X,Y,C) & vacias(C).
libre([_|Tl],Cdr):- libre(Tl,Cdr).

vacias([]).
vacias([[X,Y]|Tl]) :- tablero(X,Y,0) & vacias(Tl).


//Posiciones contiguas a una dada
contiguas(X,Y,[[X+1,Y],[X+1,Y+1],[X,Y+1]]) :- X = 0 & Y = 0.
contiguas(X,Y,[[X,Y-1],[X+1,Y-1],[X+1,Y]]) :- X = 0 & Y = 7.
contiguas(X,Y,[[X-1,Y],[X-1,Y+1],[X,Y+1]]) :- X = 7 & Y = 0.
contiguas(X,Y,[[X-1,Y],[X-1,Y-1],[X,Y-1]]) :- X = 7 & Y = 7.
contiguas(X,Y,[[X,Y-1],[X+1,Y-1],[X+1,Y],[X+1,Y+1],[X,Y+1]]) :- X = 0 & Y > 0 & Y < 7.
contiguas(X,Y,[[X-1,Y],[X-1,Y+1],[X,Y+1],[X+1,Y+1],[X+1,Y]]) :- Y = 0 & X > 0 & X < 7.
contiguas(X,Y,[[X,Y-1],[X-1,Y-1],[X-1,Y],[X-1,Y+1],[X,Y+1]]) :- X = 7 & Y > 0 & Y < 7.
contiguas(X,Y,[[X-1,Y],[X-1,Y-1],[X,Y-1],[X+1,Y-1],[X+1,Y]]) :- Y = 7 & X > 0 & X < 7.
contiguas(X,Y,[[X-1,Y-1],[X,Y-1],[X+1,Y-1],[X+1,Y],[X+1,Y+1],[X,Y+1],[X-1,Y+1],[X-1,Y]]) :- X > 0 & X < 7 & Y > 0 & Y < 7.

//Devuelve posiciones donde puede poner ficha, calculando cada una para cada columna, encima de la última ficha
opciones(O) :- calcularOpciones(0,7,O).
calcularOpciones(A,A,[[X,Y]]) :- opcion(A,[X,Y]).
calcularOpciones(A,A,[]) :- opcion(A,[]).
calcularOpciones(A,B,[[X,Y]|Tl]) :- opcion(A,[X,Y]) & A <= B & calcularOpciones(A+1,B,Tl). 
calcularOpciones(A,B,Tl) :- opcion(A,[]) & A <= B & calcularOpciones(A+1,B,Tl). 

//Para una columna dada, devuelve la posición donde va a caer la ficha
opcion(A,[]).
opcion(A,[A,B]) :- .findall(Y,tablero(A,Y,0),Vacias) & .max(Vacias,B).

//Devuelve una lista con las posiciones ocupadas por el contrario.
fichasContrarias(C) :- .my_name(Me) & ficha(You,F) & You \== Me & .findall([X,Y],tablero(X,Y,F),C).

//Devuelve una lista con las posiciones ocupadas por mí
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
