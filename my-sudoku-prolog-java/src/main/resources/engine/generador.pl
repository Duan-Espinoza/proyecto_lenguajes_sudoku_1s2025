:- use_module(library(clpfd)).
:- use_module(library(random)).

% ==========================================================
% Predicado: generar_tablero_completo(-Tablero)
%
% Descripción:
%   Genera una solución completa válida de Sudoku. Crea una
%   matriz 9x9 con valores del 1 al 9, cumpliendo todas las
%   reglas del juego: no repetir valores en filas, columnas 
%   ni bloques 3x3.
%
% Entradas:
%   Ninguna (es un generador)
%
% Salidas:
%   - Tablero: lista de 9 listas, cada una con 9 enteros del 1 al 9
%
% Restricciones:
%   - Todas las filas, columnas y subcuadrantes 3x3 contienen números únicos
%   - Es una solución completamente válida de Sudoku
% ==========================================================
generar_tablero_completo(Tablero) :-
    length(Tablero, 9),
    maplist(same_length(Tablero), Tablero),
    append(Tablero, Vars),
    Vars ins 1..9,
    maplist(all_distinct, Tablero),
    transpose(Tablero, Columnas),
    maplist(all_distinct, Columnas),
    Tablero = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    bloques(R1, R2, R3),
    bloques(R4, R5, R6),
    bloques(R7, R8, R9),
    label(Vars).

% ==========================================================
% Predicado: bloques(+Fila1, +Fila2, +Fila3)
%
% Descripción:
%   Aplica la restricción de que cada bloque 3x3 del tablero
%   debe contener números únicos. Recibe 3 filas del tablero
%   y agrupa elementos por bloques horizontales.
%
% Entradas:
%   - Fila1, Fila2, Fila3: 3 listas de 9 elementos
%
% Salidas:
%   - true si cada subbloque 3x3 contiene elementos distintos
%
% Restricciones:
%   - Cada grupo de 3x3 elementos debe ser all_distinct
% ==========================================================
bloques([], [], []).
bloques([A,B,C|R1], [D,E,F|R2], [G,H,I|R3]) :-
    all_distinct([A,B,C,D,E,F,G,H,I]),
    bloques(R1, R2, R3).

% ==========================================================
% Predicado: generar_pistas(+TableroCompleto, -TableroConPistas, +NumPistas)
%
% Descripción:
%   Dado un tablero completo válido, selecciona aleatoriamente
%   NumPistas celdas para conservar y vacía el resto. Retorna
%   un tablero parcialmente lleno como punto de partida del juego.
%
% Entradas:
%   - TableroCompleto: tablero 9x9 lleno con solución válida
%   - NumPistas: número entero entre 17 y 25
%
% Salidas:
%   - TableroConPistas: tablero 9x9 con NumPistas celdas fijas, el resto vacías
%
% Restricciones:
%   - Se seleccionan aleatoriamente NumPistas posiciones únicas
% ==========================================================
generar_pistas(TableroCompleto, TableroConPistas, NumPistas) :-
    flatten(TableroCompleto, Lista),
    length(Lista, 81),
    generar_indices_pistas(81, NumPistas, Indices),
    construir_tablero_pistas(Lista, Indices, ListaConPistas),
    dividir_tablero(ListaConPistas, TableroConPistas).

% ==========================================================
% Predicado: generar_indices_pistas(+Max, +N, -Indices)
%
% Descripción:
%   Genera una lista de N índices únicos entre 0 y Max-1, que
%   representan las posiciones que se conservarán como pistas.
%
% Entradas:
%   - Max: valor máximo (81 para un tablero 9x9)
%   - N: cantidad de índices a generar
%
% Salidas:
%   - Indices: lista de N índices únicos en el rango 0..Max-1
%
% Restricciones:
%   - No se repiten índices
% ==========================================================
generar_indices_pistas(Max, N, Indices) :-
    Max1 is Max - 1,
    numlist(0, Max1, Lista),
    random_permutation(Lista, Mezclada),
    length(Indices, N),
    append(Indices, _, Mezclada).

% ==========================================================
% Predicado: construir_tablero_pistas(+Lista, +Indices, -Resultado)
%
% Descripción:
%   Recibe una lista plana de 81 valores y una lista de índices
%   que se deben conservar. Retorna una lista donde solo esas
%   posiciones tienen valor y el resto son variables vacías (_).
%
% Entradas:
%   - Lista: tablero completo como lista plana de 81 números
%   - Indices: índices de pistas que se deben conservar
%
% Salidas:
%   - Resultado: lista plana con pistas fijas y las demás como variables (_)
%
% Restricciones:
%   - Las posiciones no indicadas quedan como celdas vacías
% ==========================================================
construir_tablero_pistas(_, [], []).
construir_tablero_pistas(Original, Indices, Resultado) :-
    construir_tablero_pistas(Original, 0, Indices, Resultado).

construir_tablero_pistas([], _, _, []).
construir_tablero_pistas([H|T], Index, Indices, [R|RT]) :-
    (member(Index, Indices) -> R = H ; R = _),
    NextIndex is Index + 1,
    construir_tablero_pistas(T, NextIndex, Indices, RT).

% ==========================================================
% Predicado: dividir_tablero(+Lista, -Tablero)
%
% Descripción:
%   Convierte una lista plana de 81 elementos en una matriz 9x9
%   dividiéndola en sublistas de 9 elementos.
%
% Entradas:
%   - Lista: lista plana de 81 elementos
%
% Salidas:
%   - Tablero: lista de 9 listas de 9 elementos (matriz 9x9)
%
% Restricciones:
%   - Lista debe tener longitud múltiplo de 9
% ==========================================================
dividir_tablero([], []).
dividir_tablero(Lista, [Fila|Rest]) :-
    length(Fila, 9),
    append(Fila, Resto, Lista),
    dividir_tablero(Resto, Rest).

% ==========================================================
% Predicado: nuevo_juego(-Tablero)
%
% Descripción:
%   Genera un tablero de Sudoku parcialmente lleno válido para jugar,
%   con entre 17 y 25 pistas aleatorias. Se asegura que haya al menos
%   2 pistas por fila y columna para evitar soluciones triviales.
%
% Entradas:
%   Ninguna
%
% Salidas:
%   - Tablero: matriz 9x9 parcialmente llena con 17–25 pistas
%
% Restricciones:
%   - El tablero debe ser generado 100% aleatoriamente
%   - Debe cumplir las reglas del Sudoku
%   - Debe tener al menos 2 pistas por fila y columna para evitar trivialidad
% ==========================================================
nuevo_juego(Tablero) :-
    repeat,
        generar_tablero_completo(Completo),
        random_between(17, 25, Pistas),
        generar_pistas(Completo, Tablero, Pistas),
        tablero_valido(Tablero),
    !.

% ==========================================================
% Predicado: tablero_valido(+Tablero)
%
% Descripción:
%   Verifica que el tablero generado tenga al menos 2 pistas
%   por cada fila y por cada columna. Esto asegura que el juego
%   no sea trivial ni tenga áreas completamente vacías.
%
% Entradas:
%   - Tablero: matriz 9x9 parcialmente llena
%
% Salidas:
%   true si al menos 2 pistas por fila y columna
%
% Restricciones:
%   - Ninguna fila ni columna debe estar completamente vacía o con solo 1 pista
% ==========================================================
tablero_valido(Tablero) :-
    transpose(Tablero, Columnas),
    forall(member(Fila, Tablero), (cuenta_pistas(Fila, N1), N1 >= 2)),
    forall(member(Col, Columnas), (cuenta_pistas(Col, N2), N2 >= 2)).

% ==========================================================
% Predicado: cuenta_pistas(+Lista, -Cantidad)
%
% Descripción:
%   Cuenta cuántas celdas de una lista (fila o columna) no están vacías.
%
% Entradas:
%   - Lista: fila o columna del tablero
%
% Salidas:
%   - Cantidad: número de celdas no vacías
%
% Restricciones:
%   - Considera como pista cualquier número no variable
% ==========================================================
cuenta_pistas(Lista, Cantidad) :-
    include(nonvar, Lista, Pistas),
    length(Pistas, Cantidad).