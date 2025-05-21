% Acá se encargará mediante este archivo o la combinación de varios
% de la generación de una matriz, la cual esta será la que represente el juego en
% su totalidad, esta tendrá sus validaciones respectivas

%% --- REPRESENTACIÓN DE LA MATRIZ DE SUDOKU ---
% La matriz se representa como una lista plana de 81 elementos.
% Un '0' indica una celda vacía.

% Ejemplo de un Sudoku inicial (para resolver)
% La matriz que proporcionaste anteriormente:
sudoku_ejemplo_inicial([
    5, 3, 0, 0, 7, 0, 0, 0, 0,
    6, 0, 0, 1, 9, 5, 0, 0, 0,
    0, 9, 8, 0, 0, 0, 0, 6, 0,
    8, 0, 0, 0, 6, 0, 0, 0, 3,
    4, 0, 0, 8, 0, 3, 0, 0, 1,
    7, 0, 0, 0, 2, 0, 0, 0, 6,
    0, 6, 0, 0, 0, 0, 2, 8, 0,
    0, 0, 0, 4, 1, 9, 0, 0, 5,
    0, 0, 0, 0, 8, 0, 0, 7, 9
]).

% --- PREDICADOS AUXILIARES ---

% Obtiene un elemento de una lista en una posición específica (base 0)
% element_at(+List, +Index, -Element)
element_at([H|_], 0, H).
element_at([_|T], I, E) :-
    I > 0,
    I1 is I - 1,
    element_at(T, I1, E).

% Predicado para obtener una fila específica de la matriz
% get_row(+Board, +RowIndex, -RowList)
get_row(Board, R, Row) :-
    StartIdx is R * 9,
    length(Row, 9),
    get_n_elements(Board, StartIdx, 9, Row).

% Predicado para obtener una columna específica de la matriz
% get_col(+Board, +ColIndex, -ColList)
get_col(Board, C, Col) :-
    findall(Val, (
        between(0, 8, R),
        Idx is R * 9 + C,
        element_at(Board, Idx, Val)
    ), Col).

% Predicado para obtener un bloque 3x3 específico
% get_block(+Board, +BlockIndex, -BlockList)
% BlockIndex va de 0 a 8
get_block(Board, BlockIdx, Block) :-
    BlockRowStart is (BlockIdx // 3) * 3,
    BlockColStart is (BlockIdx mod 3) * 3,
    findall(Val, (
        between(0, 2, R_offset),
        between(0, 2, C_offset),
        R is BlockRowStart + R_offset,
        C is BlockColStart + C_offset,
        Idx is R * 9 + C,
        element_at(Board, Idx, Val)
    ), Block).

% Predicado auxiliar para extraer N elementos de una lista desde un índice dado
% get_n_elements(+List, +StartIndex, +Count, -SubList)
get_n_elements(List, Start, Count, SubList) :-
    nth0(Start, List, _, Rest),
    take_n(Rest, Count, SubList).

% Predicado auxiliar para tomar los primeros N elementos de una lista
% take_n(+List, +N, -TakenList)
take_n(List, 0, []) :- !.
take_n([H|T], N, [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_n(T, N1, Rest).

% Predicado para verificar que una lista (fila, columna o bloque) contiene números válidos
% (1-9) y no tiene duplicados, ignorando los ceros (espacios vacíos).
% unique_and_valid_values(+List)
unique_and_valid_values(List) :-
    % Filtra los ceros y asegura que los números restantes estén entre 1 y 9.
    include(\=(0), List, NonZeroList),
    maplist(is_valid_digit, NonZeroList),
    % Verifica que no haya duplicados en la lista de no ceros.
    list_to_set(NonZeroList, Set),
    length(NonZeroList, L1),
    length(Set, L2),
    L1 == L2.

% Verifica que un número es un dígito válido de Sudoku (1-9)
is_valid_digit(X) :- integer(X), X >= 1, X =< 9.

% --- LÓGICA DE LA MATRIZ ESTÉ BIEN (para una matriz parcial o completa) ---

% sudoku_well_formed(+Board)
% Verifica que las reglas de Sudoku se cumplen para filas, columnas y bloques 3x3.
% Ignora los ceros, por lo que es útil para validar el estado de un Sudoku mientras se resuelve.
sudoku_well_formed(Board) :-
    % 1. Validar Filas
    forall(between(0, 8, R), (
        get_row(Board, R, Row),
        unique_and_valid_values(Row)
    )),
    % 2. Validar Columnas
    forall(between(0, 8, C), (
        get_col(Board, C, Col),
        unique_and_valid_values(Col)
    )),
    % 3. Validar Bloques 3x3
    forall(between(0, 8, B), (
        get_block(Board, B, Block),
        unique_and_valid_values(Block)
    )).

% --- FUNCIÓN QUE VALIDA LA MATRIZ GENERADA (SOLUCIÓN COMPLETA) ---

% validate_sudoku_solution(+Board)
% Valida una matriz de Sudoku *completamente llena* para asegurar que es una solución válida.
% Todos los elementos deben ser números del 1 al 9 y cumplir las reglas.
validate_sudoku_solution(Board) :-
    % 1. Asegurarse de que todos los elementos son dígitos válidos (1-9)
    maplist(is_valid_digit, Board),
    % 2. Aplicar la lógica de "bien formada" que ya verifica duplicados en filas, columnas y bloques.
    sudoku_well_formed(Board).

% --- PREDICADO PARA "GENERAR" UNA MATRIZ DE SUDOKU ---
% En este contexto, "generar" significa obtener una plantilla inicial o una matriz con ceros.
% No genera un Sudoku resuelto desde cero, sino una plantilla para empezar a resolver.

% generate_sudoku_template(-SudokuBoard)
% Provee una plantilla de Sudoku predefinida para ser resuelta.
% Puedes tener múltiples hechos `sudoku_template` o cargarlos de un archivo.
generate_sudoku_template(Board) :-
    sudoku_ejemplo_inicial(Board).

% Puedes añadir más plantillas si lo deseas:
% sudoku_ejemplo_inicial_2([...]).
% generate_sudoku_template(Board) :-
%    sudoku_ejemplo_inicial_2(Board).




sudoku([
    5, 3, 0,  % Fila 1, columna 3 vacía
    0, 7, 0,  % Fila 1, columna 4 y 6 vacías
    0, 0, 0,  % Fila 1, columna 7, 8 y 9 vacías

    6, 0, 0,  % Fila 2, columna 2 y 3 vacías
    1, 9, 5,
    0, 0, 0,  % Fila 2, columna 7, 8 y 9 vacías

    0, 9, 8,  % Fila 3, columna 1 vacía
    0, 0, 0,  % Fila 3, columna 4, 5 y 6 vacías
    0, 6, 0,  % Fila 3, columna 7 y 9 vacías

    % --- Separador de bloques ---

    8, 0, 0,  % Fila 4, columna 2 y 3 vacías
    0, 6, 0,  % Fila 4, columna 4 y 6 vacías
    0, 0, 3,  % Fila 4, columna 7 y 8 vacías

    4, 0, 0,  % Fila 5, columna 2 y 3 vacías
    8, 0, 3,  % Fila 5, columna 5 vacía
    0, 0, 1,  % Fila 5, columna 7 y 8 vacías

    7, 0, 0,  % Fila 6, columna 2 y 3 vacías
    0, 2, 0,  % Fila 6, columna 4 y 6 vacías
    0, 0, 6,  % Fila 6, columna 7 y 8 vacías

    % --- Separador de bloques ---

    0, 6, 0,  % Fila 7, columna 1 y 3 vacías
    0, 0, 0,  % Fila 7, columna 4, 5 y 6 vacías
    2, 8, 0,  % Fila 7, columna 9 vacía

    0, 0, 0,  % Fila 8, columna 1, 2 y 3 vacías
    4, 1, 9,
    0, 0, 5,  % Fila 8, columna 7 y 8 vacías

    0, 0, 0,  % Fila 9, columna 1, 2 y 3 vacías
    0, 8, 0,  % Fila 9, columna 4 y 6 vacías
    0, 7, 9   % Fila 9, columna 7 y 8 vacías
]).