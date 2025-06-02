/*
 * Archivo: my-sudoku-prolog-java/src/main/resources/puzzles/juego_sudoku.pl
 * Lógica de juego para Sudoku en Prolog.
 *
 * El generador de tableros se encuentra en:
 *   my-sudoku-prolog-java/src/main/resources/engine/generador.pl
 *
 * Contiene validaciones de movimiento, manejo de vidas, sugerencias,
 * estadísticas y comandos para interacción con el juego.
 *
 * Uso:
 *   1. Cargar módulos en SWI-Prolog:
 *      ?- ["../engine/generador.pl"].
 *      ?- [juego_sudoku].
 *   2. Iniciar partida: ?- iniciar_juego.
 *   3. Insertar número: ?- accion_insertar(Fila, Columna, Valor).
 *   4. Verificar estado: ?- accion_verificar.
 *   5. Solicitar sugerencia: ?- accion_sugerir(Fila, Columna).
 *   6. Mostrar solución: ?- ver_solucion.
 *   7. Reiniciar: ?- reiniciar.
 */

:- use_module(library(clpfd)).
:- use_module(library(random)).
:- dynamic tablero_inicial/1, tablero_actual/1.
:- dynamic vidas/1, sugerencias/1.
:- dynamic estadisticas/5.

% ------------------------------------------
% iniciar_juego
%   Reinicia el juego: genera nuevo puzzle, restablece vidas,
%   sugerencias y estadísticas.
% ------------------------------------------
iniciar_juego :-
    % Generar tablero completo y pistas
    nuevo_juego(Puzzle),
    retractall(tablero_inicial(_)),
    retractall(tablero_actual(_)),
    assert(tablero_inicial(Puzzle)),
    assert(tablero_actual(Puzzle)),
    % Iniciar vidas y sugerencias
    retractall(vidas(_)), assert(vidas(3)),
    retractall(sugerencias(_)), assert(sugerencias(5)),
    % Iniciar estadísticas: celdas, verificaciones, errores, sugerencias, estado final
    retractall(estadisticas(_,_,_,_,_)),
    length(Puzzle, 9), flatten(Puzzle, Flat), length(Flat, Total),
    assert(estadisticas(Total, 0, 0, 0, pending)),
    format("~n*** ¡Nuevo juego iniciado! Tienes 3 vidas y 5 sugerencias. ***~n"),
    mostrar_tablero.

% ------------------------------------------
% mostrar_tablero
%   Imprime el tablero actual con formato.
% ------------------------------------------
mostrar_tablero :- tablero_actual(T), mostrar_tablero(T).
mostrar_tablero(T) :-
    nl, forall(nth1(I, T, Fila), (
        forall(nth1(J, Fila, V), (
            (var(V) -> write('_ ') ; format('~w ', [V]))
        )), nl,
        (I mod 3 =:= 0 -> writeln('-------------') ; true)
    )).

% ------------------------------------------
% obtener(Tablero, Fila, Columna, Valor)
%   Extrae Valor de Tablero[Fila][Columna].
% ------------------------------------------
obtener(Tablero, F, C, V) :-
    nth1(F, Tablero, Row), nth1(C, Row, V).

% ------------------------------------------
% actualizar(Tablero, Fila, Columna, Valor, NuevoTablero)
%   Genera NuevoTablero con Valor en la posición indicada.
% ------------------------------------------
actualizar(Tablero, F, C, V, Nuevo) :-
    nth1(F, Tablero, Row, RestRows),
    nth1(C, Row, _, RestCols),
    nth1(C, NewRow, V, RestCols),
    nth1(F, Nuevo, NewRow, RestRows).

% ------------------------------------------
% validar_movimiento(+F, +C, +V, -Resultado)
%   Verifica reglas Sudoku y actualiza vidas y estadísticas.
% ------------------------------------------
validar_movimiento(F, C, V, success) :-
    tablero_inicial(Init), obtener(Init, F, C, Cell), var(Cell),
    tablero_actual(Act),
    maplist(all_distinct, Act),
    transpose(Act, Cols), maplist(all_distinct, Cols),
    bloques_unicos(Act),
    actualizar(Act, F, C, V, NewAct),
    retract(tablero_actual(_)), assert(tablero_actual(NewAct)),
    retract(estadisticas(Celdas, Ver, Err, Sug, Pen)),
    Ver1 is Ver+1, assert(estadisticas(Celdas, Ver1, Err, Sug, Pen)).
validar_movimiento(_, _, _, failure) :-
    retract(vidas(N)), N1 is N-1, assert(vidas(N1)),
    retract(estadisticas(Celdas, Ver, Err, Sug, Pen)),
    Err1 is Err+1, assert(estadisticas(Celdas, Ver, Err1, Sug, Pen)).

% Verificación de bloques 3x3
bloques_unicos(Tab) :-
    Tab = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    bloques(R1,R2,R3), bloques(R4,R5,R6), bloques(R7,R8,R9).

% Carga del generador externo
:- use_module('../engine/generador').

% ------------------------------------------
% accion_insertar(+F, +C, +V)
%   Inserta un valor y notifica éxito o fallo.
% ------------------------------------------
accion_insertar(F, C, V) :-
    ( validar_movimiento(F,C,V, success) ->
        writeln('Movimiento válido.'), mostrar_tablero
    ; writeln('Movimiento inválido. Perdiste una vida.'),
      vidas(R), format('Vidas restantes: ~w~n', [R]),
      (R =< 0 -> writeln('Game Over. Se agotaron tus vidas.'), fail ; true)
    ).

% ------------------------------------------
% accion_verificar
%   Muestra errores y celdas vacías o mensaje de finalización.
% ------------------------------------------
accion_verificar :-
    tablero_actual(T), flatten(T, Flat), include(var, Flat, Vac), length(Vac, Nvac),
    estadisticas(_, Ver, Err, _, _),
    ( Err > 0 -> format('Hay ~w errores y ~w celdas vacías.~n', [Err, Nvac])
    ; ( Nvac > 0 -> format('No hay errores, pero ~w celdas vacías.~n', [Nvac])
               ; writeln('¡Juego finalizado exitosamente! Felicidades!') ) ).

% ------------------------------------------
% accion_sugerir(+F, +C)
%   Ofrece sugerencia y reduce contador.
% ------------------------------------------
accion_sugerir(F, C) :-
    sugerencias(S), S > 0,
    tablero_inicial(Init), engine:generar_tablero_completo(Sol),
    obtener(Sol, F, C, V), accion_insertar(F,C,V),
    retract(sugerencias(S)), S1 is S-1, assert(sugerencias(S1)),
    format('Sugerencias restantes: ~w~n', [S1]).

% ------------------------------------------
% ver_solucion
%   Muestra la solución completa.
% ------------------------------------------
ver_solucion :-
    engine:generar_tablero_completo(Sol), mostrar_tablero(Sol).

% ------------------------------------------
% reiniciar
%   Restaura el tablero al estado inicial.
% ------------------------------------------
reiniciar :-
    tablero_inicial(Init), retract(tablero_actual(_)), assert(tablero_actual(Init)),
    writeln('Tablero reiniciado al estado inicial.'), mostrar_tablero.

/*
 * Prueba de funcionamiento:
 * 1) Cargar los módulos: 
 *    ?- ["my-sudoku-prolog-java/src/main/resources/engine/generador.pl"].
 *    ?- ["my-sudoku-prolog-java/src/main/resources/puzzles/juego_sudoku.pl"].
 * 2) Iniciar juego: ?- iniciar_juego.
 * 3) Insertar:         ?- accion_insertar(1,1,5).
 * 4) Insertar inválido:?- accion_insertar(1,2,5).
 * 5) Verificar:        ?- accion_verificar.
 * 6) Sugerencia:       ?- accion_sugerir(2,3).
 * 7) Solución:         ?- ver_solucion.
 * 8) Reiniciar:        ?- reiniciar.
 */

% Asegurar que tablero_actual siempre tenga un valor
:- dynamic tablero_actual/1.
tablero_actual([]).

% Para depuración
mostrar_tablero_actual :-
    tablero_actual(T),
    write('Tablero actual en Prolog:'), nl,
    mostrar_tablero(T).