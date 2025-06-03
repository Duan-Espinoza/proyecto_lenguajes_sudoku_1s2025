/*
 * Archivo: my-sudoku-prolog-java/src/main/resources/puzzles/juego_sudoku.pl
 * Lógica de juego para Sudoku en Prolog.
 */

:- use_module(library(clpfd)).
:- use_module(library(random)).

:- dynamic tablero_inicial/1, tablero_actual/1.
:- dynamic vidas/1, sugerencias/1.
:- dynamic estadisticas/5.
:- dynamic solucion_actual/1. 
:- use_module('../engine/generador').

iniciar_juego :-
    engine:generar_tablero_completo(Solucion),
    retractall(solucion_actual(_)),
    assert(solucion_actual(Solucion)),
    random_between(17, 25, NumPistas),
    engine:generar_pistas(Solucion, Puzzle, NumPistas),
    retractall(tablero_inicial(_)), 
    retractall(tablero_actual(_)),
    assert(tablero_inicial(Puzzle)),
    assert(tablero_actual(Puzzle)),
    retractall(vidas(_)), assert(vidas(3)),
    retractall(sugerencias(_)), assert(sugerencias(5)),
    retractall(estadisticas(_,_,_,_,_)),
    length(Puzzle, 9), flatten(Puzzle, Flat), length(Flat, Total),
    assert(estadisticas(Total, 0, 0, 0, pending)),
    writeln('---------- juego_sudoku.pl ----------'),
    format('Juego iniciado. Tablero generado con ~w pistas.~n', [NumPistas]),
    writeln('Tablero inicial:'),
    mostrar_tablero,
    writeln('--------------------------------------').

mostrar_tablero :- tablero_actual(T), mostrar_tablero(T).
mostrar_tablero(T) :-
    forall(member(Fila, T), (
        maplist(valor_para_imprimir, Fila, ValoresStr),
        atomic_list_concat(ValoresStr, ',', Linea),
        writeln(Linea)
    )).

valor_para_imprimir(V, '0') :- var(V), !.
valor_para_imprimir(V, S) :- number(V), number_string(V, S).

obtener(Tablero, F, C, V) :-
    nth1(F, Tablero, Row), nth1(C, Row, V).

actualizar(Tablero, Fila, Columna, Valor, NuevoTablero) :-
    ( Valor < 1 ; Valor > 9 ->
        format('Error: valor ~w fuera de rango (1..9)~n', [Valor]), fail
    ; length(Tablero, N),
        (Fila < 1 ; Fila > N ; Columna < 1 ; Columna > N) ->
        format('Error: posición (~w,~w) fuera del tablero~n', [Fila,Columna]), fail
    ; obtener(Tablero, Fila, Columna, Celda),
        nonvar(Celda) ->
        format('Error: la celda (~w,~w) ya tiene valor fijo ~w~n', [Fila,Columna,Celda]), fail
    ; actualizar_tablero(Tablero, Fila, Columna, Valor, NuevoTablero)
    ).

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1, I1 is I - 1, replace(T, I1, X, R).

actualizar_tablero(Tablero, Fila, Columna, Valor, NuevoTablero) :-
    nth1(Fila, Tablero, Row),
    nth1(Columna, Row, _, Restantes),
    nth1(Columna, NuevaFila, Valor, Restantes),
    replace(Tablero, Fila, NuevaFila, NuevoTablero).

% Si algún argumento está sin instanciar, fallar con mensaje
validar_movimiento(F, C, V, failure) :-
    ( var(F) ; var(C) ; var(V) ), !,
    writeln('(PL) Error: uno de los argumentos no está instanciado.'),
    fail.

% Movimiento válido
validar_movimiento(F, C, V, Resultado) :-
    integer(F), integer(C), integer(V),
    tablero_actual(Tab),
    obtener(Tab, F, C, Celda),
    ( nonvar(Celda) ->
        writeln('(PL) Celda ya tiene un valor, movimiento inválido'),
        actualizar_estadisticas_error,
        Resultado = failure
    ;
        ( actualizar(Tab, F, C, V, NuevoTab) ->
            ( valido_sudoku(NuevoTab) ->
                retractall(tablero_actual(_)),
                assertz(tablero_actual(NuevoTab)),
                writeln('(PL) Movimiento realizado exitosamente'),
                Resultado = success
            ;
                writeln('(PL) Movimiento invalido'),
                actualizar_estadisticas_error,
                Resultado = failure
            )
        ;
            writeln('(PL) Error al actualizar el tablero'),
            actualizar_estadisticas_error,
            Resultado = failure
        )
    ).

actualizar_estadisticas_error :-
    vidas(N),
    N1 is max(0, N - 1),
    retractall(vidas(_)),
    assert(vidas(N1)),
    estadisticas(Celdas, Ver, Err, Sug, Pen),
    Err1 is Err + 1,
    retractall(estadisticas(_,_,_,_,_)),
    assert(estadisticas(Celdas, Ver, Err1, Sug, Pen)),
    %Imprimir vidas restantes
    format('(PL) Vidas restantes: ~w~n', [N1]). 

accion_insertar(Fila, Col, Valor) :-
    validar_movimiento(Fila, Col, Valor, Resultado),
    ( Resultado = success ->
        estadisticas(Celdas, Ver, Err, Sug, Estado),
        Ver1 is Ver + 1,
        retractall(estadisticas(_,_,_,_,_)),
        assert(estadisticas(Celdas, Ver1, Err, Sug, Estado)),
        writeln('(PL) Movimiento insertado correctamente.')
    ;
        writeln('(PL) Movimiento inválido o error al insertar.'),
        fail
    ).


accion_verificar :-
    tablero_actual(T), flatten(T, Flat), include(var, Flat, Vac), length(Vac, Nvac),
    estadisticas( _, Ver, Err, _, _),
    ( Err > 0 -> format('Hay ~w errores y ~w celdas vacías.~n', [Err, Nvac])
    ; ( Nvac > 0 -> format('No hay errores, pero ~w celdas vacías.~n', [Nvac])
            ; writeln('¡Juego finalizado exitosamente! Felicidades!') ) ).

accion_sugerir(F, C) :-
    sugerencias(S), S > 0,
    solucion_actual(Sol),
    obtener(Sol, F, C, V),
    accion_insertar(F, C, V),
    S1 is S - 1,
    retractall(sugerencias(_)),
    assert(sugerencias(S1)),
    format('Sugerencias restantes: ~w~n', [S1]).

ver_solucion :-
    solucion_actual(Sol), % Mostrar solución actual
    retractall(tablero_actual(_)), % Limpiar tablero actual
    assertz(tablero_actual(Sol)),% Actualizar tablero actual con la solución
    writeln('Tablero actualizado con la solución:'),
    mostrar_tablero.

reiniciar :-
    tablero_inicial(Init),
    retractall(tablero_actual(_)),
    assert(tablero_actual(Init)),
    retractall(vidas(_)), assert(vidas(3)),
    retractall(sugerencias(_)), assert(sugerencias(5)),
    flatten(Init, Flat), length(Flat, Total),
    retractall(estadisticas(_,_,_,_,_)),
    assert(estadisticas(Total, 0, 0, 0, pending)),
    writeln('(PL) Tablero reiniciado al estado inicial.'),
    mostrar_tablero.

accion_estado :-
    vidas(V), sugerencias(S),
    estadisticas(C, Ver, Err, Sug, Estado),
    format('(PL) Vidas: ~w, Sugerencias: ~w~n', [V, S]),
    format('(PL) Estadísticas - Celdas: ~w, Verificadas: ~w, Errores: ~w, Sugerencias usadas: ~w, Estado: ~w~n',
           [C, Ver, Err, Sug, Estado]).

mostrar_tablero_actual :-
    tablero_actual(T),
    write('(PL) Tablero actual en Prolog:'), nl,
    mostrar_tablero(T).