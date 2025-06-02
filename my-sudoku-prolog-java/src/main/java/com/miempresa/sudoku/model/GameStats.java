/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.miempresa.sudoku.model;

/**
 *
 * @author Usuario
 */
// src/main/java/com/miempresa/sudoku/model/GameStats.java


public class GameStats {
    private int celdasTotales = 81;
    private int verificaciones;
    private int errores;
    private int sugerenciasUsadas;
    private String estado = "pending";

    // Getters y Setters
    public void setEstado(String estado) { this.estado = estado; }
    public void incrementarVerificaciones() { verificaciones++; }
    public void incrementarErrores() { errores++; }
    public void incrementarSugerencias() { sugerenciasUsadas++; }
    
    @Override
    public String toString() {
        return String.format(
            "Celdas: %d\nVerificaciones: %d\nErrores: %d\nSugerencias: %d\nEstado: %s",
            celdasTotales, verificaciones, errores, sugerenciasUsadas, estado
        );
    }

    
}
