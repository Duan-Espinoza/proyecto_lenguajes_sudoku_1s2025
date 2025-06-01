/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/GUIForms/JFrame.java to edit this template
 */
package com.miempresa.sudoku.view;

import com.miempresa.sudoku.controller.PrologController;
import java.awt.Font;
import javax.swing.JTextField;

public class SudokuGUI extends javax.swing.JFrame {

    private PrologController controller;

    public SudokuGUI() {
        initComponents();
        initCustomComponents();
    }

    private void initCustomComponents() {
        controller = new PrologController();
        controller.iniciarJuego();

        // Configurar campos de texto del tablero
        for (int i = 0; i < 81; i++) {
            JTextField field = new JTextField();
            field.setHorizontalAlignment(JTextField.CENTER);
            field.setFont(new Font("SansSerif", Font.BOLD, 20));
            panelTablero.add(field);
        }

        // Configurar acciones de los botones
        btnVerificar.addActionListener(e -> controller.verificar());
        btnSugerencia.addActionListener(e -> controller.sugerir(1, 1));
        btnSolucion.addActionListener(e -> controller.verSolucion());
        btnReiniciar.addActionListener(e -> controller.reiniciar());
    }

    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        panelTablero = new javax.swing.JPanel();
        panelBotones = new javax.swing.JPanel();
        btnVerificar = new javax.swing.JButton();
        btnSugerencia = new javax.swing.JButton();
        btnSolucion = new javax.swing.JButton();
        btnReiniciar = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Sudoku Prolog");
        getContentPane().setLayout(new java.awt.BorderLayout());

        panelTablero.setLayout(new java.awt.GridLayout(9, 9));
        getContentPane().add(panelTablero, java.awt.BorderLayout.CENTER);

        panelBotones.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER));

        btnVerificar.setText("Verificar");
        panelBotones.add(btnVerificar);

        btnSugerencia.setText("Sugerencia");
        panelBotones.add(btnSugerencia);

        btnSolucion.setText("Ver solución");
        panelBotones.add(btnSolucion);

        btnReiniciar.setText("Reiniciar");
        panelBotones.add(btnReiniciar);

        getContentPane().add(panelBotones, java.awt.BorderLayout.SOUTH);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    public static void main(String args[]) {
        try {
            javax.swing.UIManager.setLookAndFeel(javax.swing.UIManager.getSystemLookAndFeelClassName());
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(SudokuGUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }

        java.awt.EventQueue.invokeLater(() -> {
            new SudokuGUI().setVisible(true);
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnReiniciar;
    private javax.swing.JButton btnSolucion;
    private javax.swing.JButton btnSugerencia;
    private javax.swing.JButton btnVerificar;
    private javax.swing.JPanel panelBotones;
    private javax.swing.JPanel panelTablero;
    // End of variables declaration//GEN-END:variables
}