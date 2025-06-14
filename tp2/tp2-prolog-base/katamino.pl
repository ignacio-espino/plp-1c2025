:- use_module(piezas).


% Ejercicio 1

% sublista(+Descartar, +Tomar, +L, -R)
sublista(Descartar, Tomar, L, R) :- append(L1, L2, L), 
									length(L1, Descartar), 
									append(R, _, L2), 
									length(R, Tomar).

% La idea es dividir a la lista L en dos, 
% la lista de los elementos que se descartan y el resto,
% la parte del "resto" incluye a los elementos que se quieren tomar 
% y un "resto extra" que no nos importa, por eso ni lo nombramos y usamos _.


% Ejercicio 2

% Predicado que genera una fila F con N columnas, con N>0
% fila(+N, -F)
fila(1, [_]).
fila(N, [_|F]) :- N>1, N1 is N-1, fila(N1,F).

% Predicado que arma una matriz T de N filas x M columnas
% tableroGenerico(+N,+M,-T)
tableroGenerico(0,_,[]).
tableroGenerico(N,M,[F|T]) :- N>0, fila(M,F), N1 is N-1, tableroGenerico(N1,M,T).


% Tablero vacío de K>0 columnas y 5 filas.
% tablero(+K, -T)
tablero(K,T) :- tableroGenerico(5, K, T).


% Ejercicio 3
%! tamaño(+T,-F,-C)

tamaño([F1|T], F, C) :- length(T, A), F is A+1, length(F1, C). 

% Ejercicio 4
%! coordenadas(+T, -IJ)
% I indica fila, J indica columna
coordenadas(T,(I,J)) :- tamaño(T, F, C), between(1, F, I), between(1, C, J).

% Ejercicio 5

%! kPiezas(+K, -PS)
kPiezas(K, PS) :- nombrePiezas(L), tomar(K, L, PS).

%! tomar(+N, +Universo, -Seleccion)
tomar(0, _, []).
tomar(N, [H|T], [H|PS]) :- Nant is N-1, hayAlMenos(Nant, T), tomar(Nant, T, PS). 
tomar(N, [_|T], PS) :- hayAlMenos(N, T), tomar(N, T, PS). 

% hayAlMenos(+N, +L)
hayAlMenos(N, L) :- length(L, LLen), LLen >= N.

% Ejercicio 6
%! seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)

seccionTablero(T, ALTO, ANCHO, (I,J), ST) :- Iant is I-1, sublista(Iant, ALTO, T, R1), Jant is J-1, cortarFilas(R1, ANCHO, Jant, ST).

cortarFilas([], _, _, []).
cortarFilas([Fila1|RestoTablero], ANCHO, J, [Fila1Res|RestoRes]) :- sublista(J, ANCHO, Fila1 , Fila1Res), cortarFilas(RestoTablero, ANCHO, J, RestoRes). 

% Ejercicio 7

%! ubicarPieza(+Tablero, +Identificador)
ubicarPieza(T, Id) :- pieza(Id, Pieza), tamaño(Pieza, AltoPieza, AnchoPieza), 
                    tamaño(T, AltoTab, AnchoTab), between(1, AltoTab, I), between(1, AnchoTab, J),
                    seccionTablero(T, AltoPieza, AnchoPieza, (I,J), Pieza).

% Ejercicio 8
%! ubicarPiezas(+Tablero, +Poda, +Identificadores)
ubicarPiezas(T, Poda, Ids) :- poda(Poda, T), maplist(ubicarPieza(T), Ids).

poda(sinPoda, _).

