/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.miempresa.sudoku.model;

/**
 *
 * @author Usuario
 */
// src/main/java/com/miempresa/sudoku/model/SudokuModel.java


public class SudokuModel {
    // Inicializar los arreglos para evitar null
    private int[][] tableroInicial = new int[9][9];
    private int[][] tableroActual = new int[9][9];
    private int vidas = 3;
    private int sugerencias = 5;
    private int[][] solucion = new int[9][9];

    // Getters y Setters
    public int[][] getTableroInicial() { return tableroInicial; }
    public void setTableroInicial(int[][] tableroInicial) { 
        this.tableroInicial = tableroInicial; 
    }
    // Métodos para obtener y establecer el número de vidas restantes
    public int getVidas() { return vidas; }
    public void setVidas(int vidas) { this.vidas = vidas; }

    // Métodos para obtener y establecer el número de sugerencias restantes
    public int getSugerencias() { return sugerencias; }
    public void setSugerencias(int sugerencias) { this.sugerencias = sugerencias; }

    // Métodos para obtener y establecer el tablero actual del Sudoku
    public int[][] getTableroActual() { return tableroActual; }
    public void setTableroActual(int[][] tableroActual) { 
        this.tableroActual = tableroActual; 
    }

    // Métodos para obtener y establecer la solución del Sudoku
    public int[][] getSolucion() { return solucion; }
    public void setSolucion(int[][] solucion) { 
        this.solucion = solucion; 
    }


}
