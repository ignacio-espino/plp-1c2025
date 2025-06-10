:- use_module(piezas).


% Ejercicio 1


%! long(+L, ?N)
long([], 0).
long([_|T], N) :- long(T, M), N is 1 + M.

append([],L,L).
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).

% sublista(+Descartar, +Tomar, +L, -R)
sublista(Descartar, Tomar, L, R) :- append(L1, L2, L), 
									long(L1, Descartar), 
									append(R, _, L2), 
									long(R, Tomar).

% La idea es dividir a la lista L en dos, 
% la lista de los elementos que se descartan y el resto,
% la parte del "resto" incluye a los elementos que se quieren tomar 
% y un "resto extra" que no nos importa, por eso ni lo nombramos y usamos _.


% Ejercicio 2


Escribir un predicado tablero(+K, -T) que genera un tablero vac´ıo de K > 0 columnas. El tablero ser´a una
matriz de 5 × K representada como lista de filas. Cada casilla del tablero debe ser una variable no instanciada
distinta.
?- tablero(3, T)
T = [[_,_,_],[_,_,_],[_,_,_],[_,_,_],[_,_,_]].

% Tablero vacío de K>0 columnas y 5 filas.
% tablero(+K, -T)
tablero(1, T) :- [[_],[_],[_],[_],[_]]
tablero(K, T) :- long(T,5), append(L1, L2, T)


% Idea: iterar sobre K??? usando tablero(K-1, T) y agregando a cada fila una columna extra??








