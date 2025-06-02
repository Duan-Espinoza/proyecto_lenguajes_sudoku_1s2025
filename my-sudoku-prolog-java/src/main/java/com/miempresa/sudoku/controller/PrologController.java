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
            
            // Imprimir rutas para depuración
            System.out.println("Ruta generador: " + generadorPath);
            System.out.println("Ruta juego: " + juegoPath);
            
            // Normalizar rutas para Windows
            generadorPath = generadorPath.replaceFirst("^/(.:/)", "$1");
            juegoPath = juegoPath.replaceFirst("^/(.:/)", "$1");
            
            new Query("consult", new Term[]{new Atom(generadorPath)}).hasSolution();
            new Query("consult", new Term[]{new Atom(juegoPath)}).hasSolution();
            new Query("iniciar_juego").hasSolution();
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
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
                int[][] tablero = convertirTermATablero(sol.get("T"));
                model.setTableroActual(tablero);
                model.setTableroInicial(tablero); // Guardar como inicial
            } else {
                System.err.println("No se encontró solución para tablero_actual");
                model.setTableroActual(new int[9][9]);
            }
            
            model.setVidas(3);
            model.setSugerencias(5);
            stats.setEstado("abandono");
        } catch (Exception e) {
            e.printStackTrace();
            // Tablero de respaldo en caso de error
            int[][] tableroRespaldo = {
                {0,0,0,0,0,0,0,0,0},
                {0,0,0,0,0,0,0,0,0},
                {0,0,0,0,0,0,0,0,0},
                {0,0,0,0,0,0,0,0,0},
                {0,0,0,0,0,0,0,0,0},
                {0,0,0,0,0,0,0,0,0},
                {0,0,0,0,0,0,0,0,0},
                {0,0,0,0,0,0,0,0,0},
                {0,0,0,0,0,0,0,0,0}
            };
            model.setTableroActual(tableroRespaldo);
            model.setTableroInicial(tableroRespaldo);
        }
    }

    private int[][] convertirTermATablero(Term term) {
        int[][] tablero = new int[9][9];
        
        // Verificar que es una lista
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
                try {
                    tablero[i][j] = cols[j].intValue();
                } catch (JPLException e) {
                    tablero[i][j] = 0; // Celda vacía
                }
            }
        }
        return tablero;
    }

    public boolean insertarNumero(int fila, int col, int valor) {
        Query q = new Query(
            "accion_insertar", 
            new Term[]{
                new org.jpl7.Integer(fila), 
                new org.jpl7.Integer(col), 
                new org.jpl7.Integer(valor)
            }
        );
        
        if (q.hasSolution()) {
            stats.incrementarVerificaciones();
            return true;
        } else {
            stats.incrementarErrores();
            model.setVidas(model.getVidas() - 1);
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
        q.hasSolution();
    
        Query qTab = new Query("tablero_actual(T)");
        Map<String, Term> sol = qTab.oneSolution();
        model.setSolucion(convertirTermATablero(sol.get("T")));
    }
    
    
}