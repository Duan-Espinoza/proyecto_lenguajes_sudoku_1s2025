    package com.miempresa.sudoku.controller;

    import com.miempresa.sudoku.model.SudokuModel;
    import com.miempresa.sudoku.model.GameStats;
    import java.util.Map;
    import org.jpl7.*;
    import java.io.UnsupportedEncodingException;
    import java.net.URLDecoder;
    import java.nio.charset.StandardCharsets;

    /**
     * Controlador principal para la integración entre Java y Prolog en el juego de Sudoku.
     * 
     * Esta clase se encarga de gestionar la comunicación con el motor Prolog utilizando JPL,
     * permitiendo cargar archivos Prolog, iniciar nuevos juegos, insertar números, dar sugerencias,
     * mostrar la solución, reiniciar el juego y obtener información relevante como el número de vidas.
     * 
     * Funcionalidades principales:
     * 
     *   Carga y consulta de archivos Prolog necesarios para la lógica del Sudoku.
     *   Inicialización de un nuevo juego y obtención del tablero inicial desde Prolog.
     *   Conversión de términos Prolog a estructuras de datos Java (tableros 2D).
     *   Inserción de números en el tablero, validando movimientos a través de Prolog.
     *   Gestión de sugerencias y vidas, sincronizando el estado entre Java y Prolog.
     *   Obtención y visualización de la solución del Sudoku desde Prolog.
     *   Reinicio del juego, restaurando el tablero y los contadores de vidas/sugerencias.
     * 
     * Dependencias:
     *   JPL (Java-Prolog bridge) para la comunicación con Prolog.
     *   Clases auxiliares: {@link SudokuModel} y {@link GameStats} para el manejo del estado del juego.
     * 
     * Uso típico:
     * 
     *     SudokuModel model = new SudokuModel();
     *     GameStats stats = new GameStats();
     *     PrologController controller = new PrologController(model, stats);
     *     controller.nuevoJuego();
     *     controller.insertarNumero(1, 1, 5);
     * 
     * 
     * Nota: Esta clase asume que los archivos Prolog están correctamente ubicados en el classpath
     * y que las reglas Prolog implementan los predicados esperados (consultar, iniciar_juego, etc.).
     * 
     * @author Geovanni González Auguilar, Duan Espinoza Olivares
     * @version 1.0
     */
    public class PrologController {
        private final SudokuModel model;
        private final GameStats stats;

        /**
         * Nombre: PrologController
         * Descripción: Constructor que inicializa el controlador de Prolog, carga los archivos necesarios
         * y comienza un nuevo juego.
         * Parámetros:
         *   model - Instancia del modelo de Sudoku que contiene el estado del juego.   
         *  stats - Instancia de GameStats que mantiene las estadísticas del juego.
         * 
         * Salida: No retorna ningún valor.
         * Restricciones:
         *   - Los archivos Prolog deben estar ubicados en el classpath del proyecto.
         *  - Los predicados Prolog deben estar correctamente definidos y accesibles.
         * * Excepciones:
         *   - UnsupportedEncodingException si hay un problema al decodificar las rutas de los archivos.
         *  - JPLException si hay un error al cargar los archivos Prolog o ejecutar consultas.
         * Objetivo:
         *   - Inicializar el controlador de Prolog, cargar los archivos necesarios y preparar el juego.
         * 
         */
        public PrologController(SudokuModel model, GameStats stats) {
            this.model = model;
            this.stats = stats;
            cargarArchivosProlog();
            nuevoJuego();
        }
        
        /**
         * Nombre: cargarArchivosProlog
         * Descripción: Carga los archivos Prolog necesarios para el funcionamiento del juego de Sudoku.
         * Parámetros: No recibe parámetros.
         * Salida: No retorna ningún valor.
         * Restricciones:
         *   - Los archivos Prolog deben estar ubicados en el classpath del proyecto.
         *   - Las rutas deben ser accesibles y correctamente decodificadas.
         * Excepciones:
         *   - UnsupportedEncodingException si hay un problema al decodificar las rutas de los archivos.
         *   - JPLException si hay un error al cargar los archivos Prolog o ejecutar consultas.
         * Objetivo:
         *   - Preparar el entorno Prolog para que el controlador pueda interactuar con él.
         */
        private void cargarArchivosProlog() {
            try {
                // Decodificar rutas para manejar espacios
                String generadorPath = URLDecoder.decode(
                    getClass().getResource("/engine/generador.pl").getPath(),
                    StandardCharsets.UTF_8.name()
                );
                
                String juegoPath = URLDecoder.decode(
                    getClass().getResource("/puzzles/juego_sudoku.pl").getPath(),
                    StandardCharsets.UTF_8.name()
                );
                System.out.println("--------------PrologController.cargarArchivosProlog()-----------------");  
                // Imprimir rutas para depuración
                System.out.println("Ruta generador: " + generadorPath);
                System.out.println("Ruta juego: " + juegoPath);
                
                // Normalizar rutas para Windows
                generadorPath = generadorPath.replaceFirst("^/(.:/)", "$1");
                juegoPath = juegoPath.replaceFirst("^/(.:/)", "$1");
                
                // Cargar archivos Prolog
                new Query("consult", new Term[]{new Atom(generadorPath)}).hasSolution();
                new Query("consult", new Term[]{new Atom(juegoPath)}).hasSolution();
                System.out.println("Archivos Prolog cargados correctamente.");

            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            } catch (JPLException e) {
                System.err.println("Error al cargar archivos Prolog: " + e.getMessage());
                e.printStackTrace();
            }
            System.out.println("---------------------------------------------------------------------------");
        }

        /**
         * Nombre: normalizarRutaWindows
         * Descripción: Normaliza la ruta de un archivo para sistemas Windows, asegurando que se manejen correctamente
         * los espacios y las barras invertidas.
         * Parámetros:
         *   path - Ruta del archivo a normalizar.
         * Salida: Retorna la ruta normalizada como una cadena de texto.
         * Restricciones: No aplica.
         * Excepciones: No lanza excepciones.
         * Objetivo: Facilitar la carga de archivos Prolog en sistemas Windows.
         */
        private String normalizarRutaWindows(String path) {
            if (System.getProperty("os.name").toLowerCase().contains("win")) {
                return path.replaceFirst("^/(.:/)", "$1");
            }
            return path;
        }

        /**
         * Nombre: nuevoJuego
         * Descripción: Inicia un nuevo juego de Sudoku, consultando Prolog para obtener el tablero inicial.
         * Parámetros: No recibe parámetros.
         * Salida: No retorna ningún valor.
         * Restricciones:
         *   - Debe haber un predicado Prolog "iniciar_juego" definido.
         *   - El predicado "tablero_actual" debe devolver un término que represente el tablero.
         * Excepciones:
         *   - JPLException si hay un error al ejecutar las consultas Prolog.
         * Objetivo:
         *   - Preparar el tablero inicial del juego y actualizar el modelo con el estado actual.
         */

        public void nuevoJuego() {
        try {
            // Ejecutar nuevo juego en Prolog
            new Query("iniciar_juego").hasSolution();

            // Obtener tablero actual
            Query q = new Query("tablero_actual(T)");
            if (q.hasSolution()) {
                Map<String, Term> sol = q.oneSolution();
                int[][] tablero = convertirTermATablero(sol.get("T"));  // Convertir el término a un tablero 2D
                model.setTableroActual(tablero);
                model.setTableroInicial(tablero);  // Guardar el tablero inicial
                stats.nuevoJuego();  // Reiniciar estadísticas del juego

                System.out.println("(Funcion nuevoJuego) Tablero actual:");
                for (int i = 0; i < tablero.length; i++) {
                    for (int j = 0; j < tablero[i].length; j++) {
                        System.out.print(tablero[i][j] + " ");
                    }
                    System.out.println();
                }
            } else {
                System.err.println("No se encontró solución para tablero_actual");
                model.setTableroActual(new int[9][9]);

            }        
        } catch (JPLException e) {
            System.err.println("Error al iniciar juego: " + e.getMessage());
        }
    }

        /**
         * Nombre: termToInt
         * Descripción: Convierte un término Prolog a un entero, manejando casos de celdas vacías o variables.
         * Parámetros:
         *   t - Término Prolog a convertir.
         * Salida: Retorna el valor entero del término, o 0 si es una celda vacía o variable.
         * Restricciones: El término debe ser un entero o una celda vacía/variable.
         * Excepciones: No lanza excepciones.
         * Objetivo: Facilitar la conversión de términos Prolog a enteros para el tablero de Sudoku.
         */
        
        private int termToInt(Term t) {
        if (t.isInteger()) {
            return t.intValue();
        } else {
            return 0; // Celda vacía o variable
        }
        }

        /**
         * Nombre: convertirTermATablero
         * Descripción: Convierte un término Prolog que representa un tablero de Sudoku a una matriz 2D de enteros.
         * Parámetros:
         *   term - Término Prolog que representa el tablero.
         * Salida: Retorna una matriz 2D de enteros representando el tablero de Sudoku.
         * Restricciones:
         *   - El término debe ser una lista de listas (9x9).
         * Excepciones:
         *   - RuntimeException si el término no es una lista o si alguna fila no es una lista.
         * Objetivo: Facilitar la conversión de términos Prolog a estructuras de datos Java para el tablero.
         */
        private int[][] convertirTermATablero(Term term) {
            int[][] tablero = new int[9][9];
            if (!term.isList()) {
                throw new RuntimeException("Término no es una lista: " + term);
            }
            Term[] filas = term.toTermArray();
            for (int i = 0; i < 9; i++) {
                Term fila = filas[i];
                if (!fila.isList()) {
                    throw new RuntimeException("Fila no es una lista: " + fila);
                }
                Term[] cols = fila.toTermArray();
                for (int j = 0; j < 9; j++) {
                    tablero[i][j] = termToInt(cols[j]);
                }
            }
            return tablero;
        }

        /**
         * Nombre: insertarNumero
         * Descripción: Inserta un número en una posición específica del tablero de Sudoku, validando el movimiento
         * a través de Prolog.
         * Parámetros:
         *   fila - Fila donde se desea insertar el número (1-9).
         *   col - Columna donde se desea insertar el número (1-9).
         *   valor - Valor a insertar (1-9).
         * Salida: Retorna true si el movimiento es válido, false en caso contrario.
         * Restricciones:
         *   - Las filas y columnas deben estar en el rango de 1 a 9.
         *   - El valor debe ser un entero entre 1 y 9.
         * Excepciones:
         *   - JPLException si hay un error al ejecutar la consulta Prolog.
         * Objetivo:
         *   - Validar e insertar un número en el tablero, actualizando el modelo y las estadísticas del juego.
         */
        public boolean insertarNumero(int fila, int col, int valor) {
            try {
                boolean resultado = new Query(
                    "accion_insertar", 
                    new Term[]{
                        new org.jpl7.Integer(fila), 
                        new org.jpl7.Integer(col), 
                        new org.jpl7.Integer(valor)
                    }
                ).hasSolution();

                if (resultado) {
                    System.out.println(" (JAVA) Movimiento correcto: " + valor + " en (" + fila + ", " + col + ")");
                    model.getTableroActual()[fila - 1][col - 1] = valor; 
                    model.setTableroActual(model.getTableroActual());
                    stats.registrarIngresoCelda();  // Registrar ingreso de celda
                    stats.registrarVerificacion(true);  // Registrar verificación correcta

                } else {
                    System.out.println("(JAVA) Movimiento incorrecto: " + valor + " en (" + fila + ", " + col + ")");
                    int vidasRestantes = obtenerVidasDesdeProlog();
                    model.setVidas(vidasRestantes);
                    stats.registrarIngresoCelda();  // Registrar ingreso de celda
                    stats.registrarVerificacion(false);  // Registrar error de verificación
                
                    System.out.println("(JAVA) Vidas restantes: " + vidasRestantes);
                }

                return resultado;

            } catch (Exception e) {
                System.err.println("(JAVA) Error al insertar número: " + e.getMessage());
                return false;
            }
        }

        /**
         * Nombre: darSugerencia
         * Descripción: Solicita una sugerencia para una celda específica del tablero, consultando Prolog.
         * Parámetros:
         *   fila - Fila de la celda donde se desea obtener una sugerencia (1-9).
         *   col - Columna de la celda donde se desea obtener una sugerencia (1-9).
         * Salida: No retorna ningún valor, pero actualiza el modelo y las estadísticas del juego.
         * Restricciones:
         *   - Las filas y columnas deben estar en el rango de 1 a 9.
         * Excepciones:
         *   - JPLException si hay un error al ejecutar la consulta Prolog.
         * Objetivo:
         *   - Proporcionar una sugerencia al jugador y actualizar las estadísticas del juego.
         */
        
        public void darSugerencia(int fila, int col) {
            new Query(
                "accion_sugerir", 
                new Term[]{
                    new org.jpl7.Integer(fila), 
                    new org.jpl7.Integer(col)
                }
            ).hasSolution();
            stats.registrarSugerencia();  // Registrar la sugerencia en las estadísticas
            model.setSugerencias(model.getSugerencias() - 1);
        }

        /**
         * Nombre: mostrarSolucion
         * Descripción: Muestra la solución del Sudoku consultando Prolog y actualiza el modelo con el tablero de solución.
         * Parámetros: No recibe parámetros.
         * Salida: No retorna ningún valor, pero actualiza el modelo con la solución del Sudoku.
         * Restricciones:
         *   - Debe existir un predicado Prolog "ver_solucion" que devuelva la solución.
         * Excepciones:
         *   - JPLException si hay un error al ejecutar la consulta Prolog.
         * Objetivo:
         *   - Obtener y mostrar la solución del Sudoku al jugador.
         */
        public void mostrarSolucion() {
            Query q = new Query("ver_solucion");
            if (!q.hasSolution()) {
                System.err.println("No se pudo encontrar una solución.");
                return;
            }

            Query qTab = new Query("tablero_actual(T)");
            if (!qTab.hasSolution()) {
                System.err.println("No se pudo obtener el tablero de solución.");
                return;
            }

            Map<String, Term> sol = qTab.oneSolution();
            Term t = sol.get("T");

            if (t == null) {
                System.err.println("El término T es null.");
                return;
            }

            int[][] solucion = convertirTermATablero(t);
            if (solucion == null) {
                System.err.println("Error al convertir el término T a tablero.");
                return;
            }
                                    
            model.setSolucion(solucion);
            stats.setTipoFinalizacionSolucion();
        }

        /**
         * Nombre: getModel
         * Descripción: Obtiene el modelo de Sudoku asociado al controlador.
         * Parámetros: No recibe parámetros.
         * Salida: Retorna la instancia del modelo de Sudoku.
         * Restricciones: No aplica.
         * Excepciones: No lanza excepciones.
         * Objetivo: Proporcionar acceso al modelo de Sudoku para otras operaciones o consultas.
         */
        public SudokuModel getModel() {
            return model;
        }

        public int obtenerVidasDesdeProlog() {
            Query q = new Query("vidas(V)");
            if (q.hasSolution()) {
                Term t = q.oneSolution().get("V");
                return t.intValue();
            }
            return model.getVidas();  // fallback si no hay solución
        }
        
        /**
         * Nombre: reiniciarSudoku
         * Descripción: Reinicia el juego de Sudoku, restaurando el tablero inicial y los contadores de vidas y sugerencias.
         * Parámetros: No recibe parámetros.
         * Salida: No retorna ningún valor, pero actualiza el modelo con el tablero inicial y los contadores reiniciados.
         * Restricciones:
         *   - Debe existir un predicado Prolog "reiniciar" que restablezca el estado del juego.
         *   - El predicado "tablero_inicial" debe devolver un término que represente el tablero inicial.
         * Excepciones:
         *   - JPLException si hay un error al ejecutar las consultas Prolog.
         * Objetivo:
         *   - Permitir al jugador reiniciar el juego a su estado inicial.
         */
        public void reiniciarSudoku() {
            try {
                // Ejecutar reinicio en Prolog
                new Query("reiniciar").hasSolution();

                // Obtener tablero inicial
                Query q = new Query("tablero_inicial(T)");
                if (q.hasSolution()) {
                    Map<String, Term> sol = q.oneSolution();
                    int[][] tableroInicial = convertirTermATablero(sol.get("T"));
                    model.setTableroActual(tableroInicial);
                    model.setTableroInicial(tableroInicial);  // Guardar el tablero inicial
                    model.setVidas(3);  // Reiniciar vidas
                    model.setSugerencias(5);  // Reiniciar sugerencias

                    System.out.println("(Funcion reiniciarSudoku) Tablero reiniciado:");
                    for (int i = 0; i < tableroInicial.length; i++) {
                        for (int j = 0; j < tableroInicial[i].length; j++) {
                            System.out.print(tableroInicial[i][j] + " ");
                        }
                        System.out.println();
                    }
                } else {
                    System.err.println("No se encontró solución para tablero_inicial");
                }
            } catch (JPLException e) {
                System.err.println("Error al reiniciar Sudoku: " + e.getMessage());
            }
        }
        /**
         * Nombre: getStats
         * Descripción: Obtiene las estadísticas del juego de Sudoku.
         * Parámetros: No recibe parámetros.
         * Salida: Retorna la instancia de GameStats que contiene las estadísticas del juego.
         * Restricciones: No aplica.
         * Excepciones: No lanza excepciones.
         * Objetivo: Proporcionar acceso a las estadísticas del juego para su visualización o análisis.
         */
        public GameStats getStats() {
            return stats;
        }
    }

