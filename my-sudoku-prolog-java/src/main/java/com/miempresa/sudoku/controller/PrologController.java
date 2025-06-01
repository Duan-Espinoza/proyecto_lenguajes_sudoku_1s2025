package com.miempresa.sudoku.controller;

import com.miempresa.sudoku.model.SudokuModel;
import com.miempresa.sudoku.model.GameStats;
import java.util.Map;
import org.jpl7.*;

public class PrologController {
    private final SudokuModel model;
    private final GameStats stats;

    public PrologController(SudokuModel model, GameStats stats) {
        this.model = model;
        this.stats = stats;
        cargarArchivosProlog();
    }

    private void cargarArchivosProlog() {
        String generadorPath = getClass().getResource("/engine/generador.pl").getPath();
        String juegoPath = getClass().getResource("/puzzles/juego_sudoku.pl").getPath();
        
        generadorPath = normalizarRutaWindows(generadorPath);
        juegoPath = normalizarRutaWindows(juegoPath);
        
        new Query("consult", new Term[]{new Atom(generadorPath)}).hasSolution();
        new Query("consult", new Term[]{new Atom(juegoPath)}).hasSolution();
        new Query("iniciar_juego").hasSolution();
    }
    
    private String normalizarRutaWindows(String path) {
        if (System.getProperty("os.name").toLowerCase().contains("win")) {
            return path.replaceFirst("^/(.:/)", "$1");
        }
        return path;
    }

    public void nuevoJuego() {
        Query q = new Query("tablero_actual(T)");
        Map<String, Term> sol = q.oneSolution();
        model.setTableroActual(convertirTermATablero(sol.get("T")));
        model.setVidas(3);
        model.setSugerencias(5);
        stats.setEstado("abandono");
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