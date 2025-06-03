    package com.miempresa.sudoku.controller;

    import com.miempresa.sudoku.model.SudokuModel;
    import com.miempresa.sudoku.model.GameStats;
    import java.util.Map;
    import org.jpl7.*;
    import java.io.UnsupportedEncodingException;
    import java.net.URLDecoder;
    import java.nio.charset.StandardCharsets;

    public class PrologController {
        private final SudokuModel model;
        private final GameStats stats;


        public PrologController(SudokuModel model, GameStats stats) {
            this.model = model;
            this.stats = stats;
            cargarArchivosProlog();
            nuevoJuego();
        }
        
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
        
        private String normalizarRutaWindows(String path) {
            if (System.getProperty("os.name").toLowerCase().contains("win")) {
                return path.replaceFirst("^/(.:/)", "$1");
            }
            return path;
        }

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

        private int termToInt(Term t) {
        if (t.isInteger()) {
            return t.intValue();
        } else {
            return 0; // Celda vacía o variable
        }
        }

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
                } else {
                    System.out.println("(JAVA) Movimiento incorrecto: " + valor + " en (" + fila + ", " + col + ")");
                    int vidasRestantes = obtenerVidasDesdeProlog();
                    model.setVidas(vidasRestantes);
                    System.out.println("(JAVA) Vidas restantes: " + vidasRestantes);
                }

                return resultado;

            } catch (Exception e) {
                System.err.println("(JAVA) Error al insertar número: " + e.getMessage());
                return false;
            }
        }


        public void darSugerencia(int fila, int col) {
            new Query(
                "accion_sugerir", 
                new Term[]{
                    new org.jpl7.Integer(fila), 
                    new org.jpl7.Integer(col)
                }
            ).hasSolution();
            stats.incrementarSugerencias();
            model.setSugerencias(model.getSugerencias() - 1);
        }
        
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
        }

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
    }

