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
    private int[][] tableroInicial;
    private int[][] tableroActual;
    private int vidas = 3;
    private int sugerencias = 5;
    private int[][] solucion; // Nuevo campo para almacenar la solución

    // Getters y Setters
    public int[][] getTableroInicial() { return tableroInicial; }
    public void setTableroInicial(int[][] tableroInicial) { 
        this.tableroInicial = tableroInicial; 
    }
    // ... otros getters/setters



    public int getVidas() { return vidas; }
    public void setVidas(int vidas) { this.vidas = vidas; }
    public int getSugerencias() { return sugerencias; }
    public void setSugerencias(int sugerencias) { this.sugerencias = sugerencias; }

    public int[][] getTableroActual() { return tableroActual; }
    public void setTableroActual(int[][] tableroActual) { 
        this.tableroActual = tableroActual; 
    }
    
    public int[][] getSolucion() { return solucion; }
    public void setSolucion(int[][] solucion) { 
        this.solucion = solucion; 
    }
}
