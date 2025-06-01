package com.miempresa.sudoku.model;

public class JuegoSudoku {
    // Aquí puedes definir estructuras para guardar estado del juego
    // por ejemplo, una matriz local en Java si deseas visualizarlo
    private Integer[][] tablero;

    public JuegoSudoku() {
        tablero = new Integer[9][9]; // vacío inicialmente
    }

    public Integer[][] getTablero() {
        return tablero;
    }

    public void setCelda(int fila, int col, Integer valor) {
        tablero[fila][col] = valor;
    }
}
