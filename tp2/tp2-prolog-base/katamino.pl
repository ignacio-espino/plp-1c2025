:- use_module(piezas).

% Completar ...

% -- Ejercicio 1 --

% sublista(+Descartar, +Tomar, +L, -R)

sublista(D, T, Lista, R) :- append(A, B, Lista), length(A, D), append(R, _, B), length(R, T).

length_k(K, L) :- length(L, K).

% tablero(+K, -T)
tablero(K, T) :- length(T, 5), maplist(length_k(K), T).

% tamano(+M, -F, -C)
tamano(M, F, C) :- length(M, F), maplist(length_k(C), M).

% coordenadas(+T, -IJ)

% coordenadas(+T, -IJ)
coordenadas(T, (I,J)) :- tamano(T, F, C), between(1, F, I), between(1, C, J).