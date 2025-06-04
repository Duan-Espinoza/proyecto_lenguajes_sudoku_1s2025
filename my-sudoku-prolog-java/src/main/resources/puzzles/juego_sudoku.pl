/*
 * juego_sudoku.pl
 * ----------------
 * Este archivo contiene la lógica principal del juego de Sudoku implementada en Prolog.
 * Proporciona predicados para inicializar el juego, mostrar el tablero, validar y realizar movimientos,
 * gestionar vidas y sugerencias, así como verificar el estado del juego y mostrar la solución.
 *
 * Principales predicados exportados:
 * - iniciar_juego/0: Inicializa un nuevo juego de Sudoku, generando un tablero y configurando las variables de estado.
 * - mostrar_tablero/0, mostrar_tablero/1: Muestra el tablero actual o uno dado en formato legible.
 * - accion_insertar/3: Inserta un valor en una celda específica, validando el movimiento y actualizando estadísticas.
 * - accion_verificar/0: Verifica el estado actual del tablero, indicando errores y celdas vacías.
 * - accion_sugerir/2: Inserta el valor correcto en una celda como sugerencia, decrementando el contador de sugerencias.
 * - ver_solucion/0: Muestra la solución completa del tablero actual.
 * - reiniciar/0: Reinicia el tablero al estado inicial, restaurando vidas y sugerencias.
 * - accion_estado/0: Muestra el estado actual del juego, incluyendo vidas, sugerencias y estadísticas.
 * - mostrar_tablero_actual/0: Muestra el tablero actual en consola.
 *
 * Variables dinámicas utilizadas:
 * - tablero_inicial/1: Guarda el estado inicial del tablero.
 * - tablero_actual/1: Guarda el estado actual del tablero.
 * - vidas/1: Número de vidas restantes.
 * - sugerencias/1: Número de sugerencias restantes.
 * - estadisticas/5: Estadísticas del juego (celdas, verificadas, errores, sugerencias usadas, estado).
 * - solucion_actual/1: Solución completa del tablero actual.
 *
 * Dependencias:
 * - Utiliza la biblioteca CLP(FD) para restricciones de dominio finito.
 * - Requiere el módulo externo '../engine/generador' para la generación de tableros y pistas.
 *
 * Notas:
 * - El tablero se representa como una lista de listas (matriz 9x9).
 * - Los movimientos inválidos decrementan vidas y actualizan estadísticas.
 * - El juego termina exitosamente cuando no hay errores ni celdas vacías.
 */

:- use_module(library(clpfd)).
:- use_module(library(random)).

:- dynamic tablero_inicial/1, tablero_actual/1.
:- dynamic vidas/1, sugerencias/1.
:- dynamic estadisticas/5.
:- dynamic solucion_actual/1. 
:- use_module('../engine/generador').


/*
* Nombre: iniciar_juego/0
* Descripción: Inicializa un nuevo juego de Sudoku generando un tablero completo,
*  creando un tablero inicial con pistas y configurando las variables de estado.
* Parámetros: No recibe parámetros.
* Salida: No retorna ningún valor.
* Restricciones:
*   - Debe haber un predicado Prolog "generar_tablero_completo" definido.
*   - Debe haber un predicado Prolog "generar_pistas" definido.
* Objetivo:
*   - Preparar el juego de Sudoku para que el jugador pueda comenzar a jugar.
*   - Genera un tablero completo y un tablero inicial con pistas.
*   - Inicializa las vidas y sugerencias del jugador.
*/

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

/**
 * Nombre: mostrar_tablero/0
 * Descripción: Muestra el tablero actual en un formato legible.
 * Salida: No retorna ningún valor.
 * Restricciones: Debe haber un tablero actual definido.
 * Objetivo: Permitir al jugador ver el estado actual del tablero.
 */
mostrar_tablero :- tablero_actual(T), mostrar_tablero(T).
mostrar_tablero(T) :-
    forall(member(Fila, T), (
        maplist(valor_para_imprimir, Fila, ValoresStr),
        atomic_list_concat(ValoresStr, ',', Linea),
        writeln(Linea)
    )).

/**
 * Nombre: valor_para_imprimir/2
 * Descripción: Convierte un valor de celda a una cadena para imprimir.
 * Parámetros:
 *   - V: Valor de la celda (puede ser un número o una variable).
 *   - S: Cadena resultante para imprimir.
 * Salida: No retorna ningún valor, pero imprime el valor en formato adecuado.
 * Restricciones: Si V es una variable, se imprime '0'.
 * Objetivo: Formatear los valores del tablero para su visualización.
 */

valor_para_imprimir(V, '0') :- var(V), !.
valor_para_imprimir(V, S) :- number(V), number_string(V, S).

/**
 * Nombre: obtener/4
 * Descripción: Obtiene el valor de una celda específica en el tablero.
 * Parámetros:
 *   - Tablero: El tablero actual (lista de listas).
 *   - F: Fila de la celda (1-9).
 *   - C: Columna de la celda (1-9).
 *   - V: Valor de la celda (será un número o una variable).
 * Salida: No retorna ningún valor, pero unifica V con el valor de la celda.
 * Restricciones: F y C deben estar dentro del rango del tablero.
 * Objetivo: Permitir acceder a los valores del tablero para validaciones y actualizaciones.
 */
obtener(Tablero, F, C, V) :-
    nth1(F, Tablero, Row), nth1(C, Row, V).

/**
 * Nombre: actualizar/4
 * Descripción: Actualiza el valor de una celda específica en el tablero.
 * Parámetros:
 *   - Tablero: El tablero actual (lista de listas).
 *   - Fila: Fila de la celda a actualizar (1-9).
 *   - Columna: Columna de la celda a actualizar (1-9).
 *   - Valor: Nuevo valor a insertar en la celda (1-9).
 *   - NuevoTablero: Tablero resultante después de la actualización.
 * Salida: Unifica NuevoTablero con el tablero actualizado.
 * Restricciones:
 *   - Valor debe estar entre 1 y 9.
 *   - La celda no debe tener un valor fijo ya asignado.
 * Objetivo: Permitir al jugador modificar el tablero durante el juego.
 */

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

/**
 * Nombre: replace/4
 * Descripción: Reemplaza un elemento en una lista en una posición específica.
 * Parámetros:
 *   - Lista: Lista original.
 *   - Pos: Posición del elemento a reemplazar (1-).
 *   - X: Nuevo valor a insertar en la posición.
 *   - R: Lista resultante con el elemento reemplazado.
 * Salida: Unifica R con la lista modificada.
 * Restricciones: Pos debe ser un entero positivo.
 * Objetivo: Facilitar la actualización de filas en el tablero.
 */

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1, I1 is I - 1, replace(T, I1, X, R).

/**
 * Nombre: actualizar_tablero/5
 * Descripción: Actualiza una celda específica en el tablero con un nuevo valor.
 * Parámetros:
 *   - Tablero: El tablero actual (lista de listas).
 *   - Fila: Fila de la celda a actualizar (1-9).
 *   - Columna: Columna de la celda a actualizar (1-9).
 *   - Valor: Nuevo valor a insertar en la celda (1-9).
 *   - NuevoTablero: Tablero resultante después de la actualización.
 * Salida: Unifica NuevoTablero con el tablero actualizado.
 * Restricciones: Fila y Columna deben estar dentro del rango del tablero.
 * Objetivo: Permitir al jugador modificar el tablero durante el juego.
 */
actualizar_tablero(Tablero, Fila, Columna, Valor, NuevoTablero) :-
    nth1(Fila, Tablero, Row),
    nth1(Columna, Row, _, Restantes),
    nth1(Columna, NuevaFila, Valor, Restantes),
    replace(Tablero, Fila, NuevaFila, NuevoTablero).

/*
    * Nombre: validar_movimiento/4
    * Descripción: Valida un movimiento en el tablero de Sudoku.
    * Parámetros:
    *   - F: Fila de la celda (1-9).
    *   - C: Columna de la celda (1-9).
    *   - V: Valor a insertar en la celda (1-9).
    *   - Resultado: Resultado de la validación (success o failure).
    * Salida: Unifica Resultado con success si el movimiento es válido, o failure si no lo es.
    * Restricciones:
    *   - F, C y V deben ser enteros.
    *   - La celda no debe tener un valor ya asignado.
    * Objetivo: Validar y realizar un movimiento en el tablero, actualizando estadísticas y vidas.
    */

validar_movimiento(F, C, V, failure) :-
    ( var(F) ; var(C) ; var(V) ), !,
    writeln('(PL) Error: uno de los argumentos no está instanciado.'),
    fail.

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

/**
 * Nombre: valido_sudoku/1
 * Descripción: Verifica si un tablero de Sudoku es válido.
 * Parámetros:
 *   - Tablero: El tablero a verificar (lista de listas).
 * Salida: Unifica con true si el tablero es válido, o false si no lo es.
 * Restricciones: El tablero debe ser una lista de listas de enteros entre 1 y 9 o variables.
 * Objetivo: Validar la integridad del tablero después de un movimiento.
 */

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

/**
 * Nombre: accion_insertar/3
 * Descripción: Inserta un valor en una celda específica del tablero, validando el movimiento.
 * Parámetros:
 *   - Fila: Fila de la celda (1-9).
 *   - Col: Columna de la celda (1-9).
 *   - Valor: Valor a insertar en la celda (1-9).
 * Salida: No retorna ningún valor, pero actualiza el tablero y las estadísticas.
 * Restricciones: Fila, Col y Valor deben ser enteros.
 * Objetivo: Permitir al jugador realizar movimientos en el tablero.
 */

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

/**
 * Nombre: accion_verificar/0
 * Descripción: Verifica el estado actual del tablero, mostrando errores y celdas vacías.
 * Salida: No retorna ningún valor, pero imprime el estado del tablero.
 * Restricciones: Debe haber un tablero actual definido.
 * Objetivo: Permitir al jugador verificar el progreso del juego y detectar errores.
 */

accion_verificar :-
    tablero_actual(T), flatten(T, Flat), include(var, Flat, Vac), length(Vac, Nvac),
    estadisticas( _, Ver, Err, _, _),
    ( Err > 0 -> format('Hay ~w errores y ~w celdas vacías.~n', [Err, Nvac])
    ; ( Nvac > 0 -> format('No hay errores, pero ~w celdas vacías.~n', [Nvac])
            ; writeln('¡Juego finalizado exitosamente! Felicidades!') ) ).

/**
 * Nombre: accion_sugerir/2
 * Descripción: Inserta un valor sugerido en una celda específica, decrementando el contador de sugerencias.
 * Parámetros:
 *   - F: Fila de la celda (1-9).
 *   - C: Columna de la celda (1-9).
 * Salida: No retorna ningún valor, pero actualiza el tablero y las sugerencias restantes.
 * Restricciones: Debe haber sugerencias disponibles.
 * Objetivo: Proporcionar ayuda al jugador sugiriendo un valor para una celda vacía.
 */

accion_sugerir(F, C) :-
    sugerencias(S), S > 0,
    solucion_actual(Sol),
    obtener(Sol, F, C, V),
    accion_insertar(F, C, V),
    S1 is S - 1,
    retractall(sugerencias(_)),
    assert(sugerencias(S1)),
    format('Sugerencias restantes: ~w~n', [S1]).

/**
 * Nombre: ver_solucion/0
 * Descripción: Muestra la solución completa del tablero actual.
 * Salida: No retorna ningún valor, pero imprime la solución en consola.
 * Restricciones: Debe haber una solución definida.
 * Objetivo: Permitir al jugador ver la solución completa del Sudoku.
 */

ver_solucion :-
    solucion_actual(Sol), % Mostrar solución actual
    retractall(tablero_actual(_)), % Limpiar tablero actual
    assertz(tablero_actual(Sol)),% Actualizar tablero actual con la solución
    writeln('Tablero actualizado con la solución:'),
    mostrar_tablero.

/**
 * Nombre: reiniciar/0
 * Descripción: Reinicia el juego al estado inicial, restaurando el tablero, vidas y sugerencias.
 * Salida: No retorna ningún valor, pero imprime un mensaje de reinicio y muestra el tablero.
 * Restricciones: Debe haber un tablero inicial definido.
 * Objetivo: Permitir al jugador reiniciar el juego desde el principio.
 */

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

/**
 * Nombre: accion_estado/0
 * Descripción: Muestra el estado actual del juego, incluyendo vidas, sugerencias y estadísticas.
 * Salida: No retorna ningún valor, pero imprime el estado en consola.
 * Restricciones: Debe haber un tablero actual definido.
 * Objetivo: Permitir al jugador ver el progreso del juego y las estadísticas actuales.
 */

accion_estado :-
    vidas(V), sugerencias(S),
    estadisticas(C, Ver, Err, Sug, Estado),
    format('(PL) Vidas: ~w, Sugerencias: ~w~n', [V, S]),
    format('(PL) Estadísticas - Celdas: ~w, Verificadas: ~w, Errores: ~w, Sugerencias usadas: ~w, Estado: ~w~n',
           [C, Ver, Err, Sug, Estado]).

/**
 * Nombre: mostrar_tablero_actual/0
 * Descripción: Muestra el tablero actual en Prolog.
 * Salida: No retorna ningún valor, pero imprime el tablero en consola.
 * Restricciones: Debe haber un tablero actual definido.
 * Objetivo: Permitir al jugador ver el estado actual del tablero en formato Prolog.
 */

mostrar_tablero_actual :-
    tablero_actual(T),
    write('(PL) Tablero actual en Prolog:'), nl,
    mostrar_tablero(T).