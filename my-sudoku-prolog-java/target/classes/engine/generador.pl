/*
*   Archivo: generador.pl
*   Generador de tableros de Sudoku en Prolog.
*   Contiene predicados para crear un tablero completo, generar
*   pistas y crear un nuevo juego con restricciones específicas.
*/

:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(system)).  

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
    label_aleatorio(Vars).

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
    semilla_aleatoria,
    repeat,
        generar_tablero_completo(Completo),
        random_between(17, 25, Pistas),
        generar_pistas(Completo, Tablero, Pistas),
        tablero_valido(Tablero),
        no_cadenas_triviales(Tablero),
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

% ==========================================================
% Predicado: valido_sudoku(+Tablero)
%
% Descripción:
%   Verifica que un tablero (incompleto o completo) cumpla con
%   las reglas del Sudoku: no repite números en filas, columnas
%   ni bloques 3x3 (ignorando celdas vacías).
% ==========================================================
valido_sudoku(Tablero) :-
    maplist(valida_lista, Tablero),
    transpose(Tablero, Columnas),
    maplist(valida_lista, Columnas),
    Tablero = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    bloques_validos(R1, R2, R3),
    bloques_validos(R4, R5, R6),
    bloques_validos(R7, R8, R9).

valida_lista(Lista) :-
    exclude(var, Lista, Numeros),
    all_distinct(Numeros).

bloques_validos([], [], []).
bloques_validos([A,B,C|R1], [D,E,F|R2], [G,H,I|R3]) :-
    valida_lista([A,B,C,D,E,F,G,H,I]),
    bloques_validos(R1, R2, R3).

% ==========================================================
% Predicado: no_cadenas_triviales(+Tablero)
%
% Descripción:
%   Verifica que no existan "cadenas triviales" en el tablero,
%   es decir, filas o columnas que contengan solo una pista
%   o que tengan pistas aisladas (sin continuidad).
%
% Entradas:
%   - Tablero: matriz 9x9 parcialmente llena
%
% Salidas:
%   true si no hay cadenas triviales en filas y columnas
%
% Restricciones:
%   - No debe haber filas o columnas con solo 1 pista
%   - No debe haber pistas aisladas que no formen parte de una cadena continua
% ==========================================================

no_cadenas_triviales(Tablero) :-
    forall(member(Fila, Tablero), \+ cadena_trivial(Fila)),
    transpose(Tablero, Columnas),
    forall(member(Col, Columnas), \+ cadena_trivial(Col)).

% ==========================================================
% Predicado: cadena_trivial(+Lista)
%
% Descripción:
%   Verifica si una lista (fila o columna) tiene una cadena
%   trivial, es decir, si contiene solo una pista o si las pistas
%   están aisladas (no forman una secuencia continua).
%
% Entradas:
%   - Lista: fila o columna del tablero
%
% Salidas:
%   true si la lista tiene una cadena trivial
%
% Restricciones:
%   - Una cadena trivial es aquella con solo 1 pista o con pistas
%     que no están en posiciones consecutivas
% ==========================================================

cadena_trivial(Lista) :-
    findall(Index, (nth0(Index, Lista, Val), nonvar(Val)), Indices),
    sort(Indices, Ordenadas),
    incluye_aislada(Ordenadas).


% ==========================================================
% Predicado: incluye_aislada(+Lista)
%
% Descripción:
%   Verifica si una lista de índices contiene pistas aisladas,
%   es decir, si hay al menos un par de índices que no están
%   consecutivos. Si hay un índice aislado, significa que no
% forman una cadena continua.
%
% Entradas:
%   - Lista: lista de índices ordenados donde hay pistas
%
% Salidas:
%   true si hay al menos un índice aislado
%
% Restricciones:
%   - Una lista con un solo elemento no puede tener pistas aisladas
%   - Una lista con dos elementos puede ser aislada si no son consecutivos

incluye_aislada([_]). % Solo 1 pista → trivial
incluye_aislada([A,B|Resto]) :-
    (B - A > 1 -> true ; incluye_aislada([B|Resto])).


% ==========================================================
% Predicado: semilla_aleatoria
%
% Descripción:
%   Inicializa la semilla aleatoria basada en el tiempo actual.
%   Esto asegura que cada vez que se ejecute el generador, los
%   tableros generados sean diferentes y aleatorios.
%
% Entradas:
%   Ninguna
%
% Salidas:
%   - Establece la semilla aleatoria para el generador de números
%   - Utiliza el tiempo actual para crear una semilla única
%
% Restricciones:
%   - Asegura que cada ejecución del generador produzca resultados
%     diferentes y no repetidos
% ==========================================================

semilla_aleatoria :-
    get_time(T),
    Seed is round(T),
    set_random(seed(Seed)).

% ==========================================================
% Predicado: label_aleatorio(+Vars)
%
% Descripción:
%   Etiqueta las variables del tablero de Sudoku de forma aleatoria
%   utilizando el algoritmo de búsqueda de primer cambio (ffc).
%   Esto asegura que las variables se asignen de manera eficiente
%   y se evite la generación de soluciones repetitivas.
%
% Entradas:
%   - Vars: lista de variables del tablero que deben ser etiquetadas
%
% Salidas:
%   - Asigna valores a las variables de forma aleatoria
%
% Restricciones:
%   - Utiliza el algoritmo de etiquetado ffc para asignar valores
%   - Asegura que las variables se llenen de manera eficiente
%   - Utiliza random_permutation para mezclar las variables antes de etiquetar
%   - Asegura que las variables se llenen de manera eficiente
% ==========================================================

label_aleatorio(Vars) :-
    random_permutation(Vars, VarsRandom),
    labeling([ffc], VarsRandom).