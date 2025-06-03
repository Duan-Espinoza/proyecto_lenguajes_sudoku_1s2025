package com.miempresa.sudoku.view;

import com.formdev.flatlaf.FlatLightLaf;
import com.miempresa.sudoku.controller.PrologController;
import com.miempresa.sudoku.model.SudokuModel;
import com.miempresa.sudoku.model.GameStats;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Arrays;

public class SudokuGUI extends JFrame {
    private final SudokuModel model = new SudokuModel();
    private final GameStats stats = new GameStats();
    private final PrologController controller;
    private JTextField[][] celdas = new JTextField[9][9];
    private JLabel lblVidas, lblSugerencias;
    private int filaSeleccionada = -1;
    private int colSeleccionada = -1;

    public SudokuGUI() {
        try {
            UIManager.setLookAndFeel(new FlatLightLaf());
        } catch (Exception e) {
            System.err.println("Error cargando FlatLaf");
        }

        controller = new PrologController(model, stats);
        try {
            initUI();
            actualizarTablero();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void actualizarEstado() {
        lblVidas.setText("Vidas: " + model.getVidas());
        lblSugerencias.setText("Sugerencias: " + model.getSugerencias());

        if (model.getVidas() <= 0) {
            JOptionPane.showMessageDialog(this, "¡Juego terminado! No tienes más vidas.");
            controller.nuevoJuego();
            actualizarTablero();
        }
    }

    private void initUI() {
        setTitle("Sudoku Prolog-Java");
        setLayout(new BorderLayout());

        // Panel de estado
        JPanel panelEstado = new JPanel();
        panelEstado.setBackground(new Color(245, 245, 245));
        lblVidas = new JLabel("Vidas: " + model.getVidas());
        lblVidas.setFont(new Font("Segoe UI", Font.BOLD, 16));
        lblVidas.setForeground(new Color(60, 60, 60));
        lblSugerencias = new JLabel("Sugerencias: " + model.getSugerencias());
        lblSugerencias.setFont(new Font("Segoe UI", Font.BOLD, 16));
        lblSugerencias.setForeground(new Color(60, 60, 60));
        panelEstado.add(lblVidas);
        panelEstado.add(lblSugerencias);

        // Panel del tablero
        JPanel tableroPanel = new JPanel(new GridLayout(9, 9));
        Color colorBloque = new Color(230, 240, 255);
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                JTextField celda = new JTextField();
                celda.setHorizontalAlignment(JTextField.CENTER);
                celda.setFont(new Font("Segoe UI", Font.PLAIN, 22));
                celda.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
                celda.setForeground(Color.BLACK);
                celda.setBackground((i / 3 + j / 3) % 2 == 0 ? colorBloque : Color.WHITE);

                final int fila = i, col = j;
                celda.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent e) {
                        filaSeleccionada = fila;
                        colSeleccionada = col;
                        resaltarCeldaSeleccionada();
                    }
                });

                celda.addActionListener(e -> {
                    try {
                        int valor = Integer.parseInt(celda.getText().trim());
                        if (valor < 1 || valor > 9) {
                            JOptionPane.showMessageDialog(this, "Solo números del 1 al 9");
                            return;
                        }

                        boolean exito = controller.insertarNumero(fila + 1, col + 1, valor);
                        if (exito) {
                            celda.setForeground(Color.BLACK);
                            celda.setBackground((fila / 3 + col / 3) % 2 == 0 ? colorBloque : Color.WHITE);
                        } else {
                            celda.setForeground(Color.RED);
                            celda.setBackground(Color.PINK);
                        }

                    } catch (NumberFormatException ex) {
                        JOptionPane.showMessageDialog(this, "Entrada inválida");
                    } finally {
                        actualizarEstado();
                        if (model.getVidas() <= 0) {
                            JOptionPane.showMessageDialog(this, "¡Juego terminado!");
                            controller.nuevoJuego();
                            actualizarTablero();
                            actualizarEstado();
                        }
                    }
                });

                celdas[i][j] = celda;
                tableroPanel.add(celda);
            }
        }

        // Panel de botones
        JPanel buttonPanel = new JPanel();
        String[] botones = {"Nuevo Juego", "Reiniciar", "Verificar", "Sugerencia", "Solución"};
        for (String texto : botones) {
            JButton btn = new JButton(texto);
            btn.setBackground(new Color(70, 130, 180));
            btn.setForeground(Color.WHITE);
            btn.setFocusPainted(false);
            btn.setFont(new Font("Segoe UI", Font.BOLD, 14));
            btn.setBorder(BorderFactory.createEmptyBorder(10, 20, 10, 20));
            btn.addActionListener(crearManejadorBoton(texto));
            buttonPanel.add(btn);
        }

        add(panelEstado, BorderLayout.NORTH);
        add(tableroPanel, BorderLayout.CENTER);
        add(buttonPanel, BorderLayout.SOUTH);

        setSize(650, 650);
        setLocationRelativeTo(null);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    private void resaltarCeldaSeleccionada() {
        Color colorBloque = new Color(230, 240, 255);
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                celdas[i][j].setBackground((i / 3 + j / 3) % 2 == 0 ? colorBloque : Color.WHITE);
            }
        }
        if (filaSeleccionada >= 0 && colSeleccionada >= 0) {
            celdas[filaSeleccionada][colSeleccionada].setBackground(new Color(255, 255, 160)); // amarillo suave
        }
    }

    private ActionListener crearManejadorBoton(String texto) {
        return e -> {
            switch (texto) {
                case "Nuevo Juego":
                    controller.nuevoJuego();
                    filaSeleccionada = -1;
                    colSeleccionada = -1;
                    actualizarTablero();
                    actualizarEstado();
                    break;
                case "Reiniciar":
                    controller.reiniciarSudoku();
                    filaSeleccionada = -1;
                    colSeleccionada = -1;
                    actualizarTablero();
                    actualizarEstado();
                    break;
                case "Verificar":
                    JOptionPane.showMessageDialog(this, stats.toString());
                    break;
                case "Sugerencia":
                    if (model.getSugerencias() > 0) {
                        if (filaSeleccionada >= 0 && colSeleccionada >= 0) {
                            controller.darSugerencia(filaSeleccionada + 1, colSeleccionada + 1);
                            actualizarTablero();
                            actualizarEstado();
                        } else {
                            JOptionPane.showMessageDialog(this, "Seleccione una celda primero");
                        }
                    } else {
                        JOptionPane.showMessageDialog(this, "Sin sugerencias disponibles");
                    }
                    break;
                case "Solución":
                    controller.mostrarSolucion();
                    int[][] solucion = model.getSolucion();
                    if (solucion == null) {
                        JOptionPane.showMessageDialog(this, "No se pudo obtener la solución.");
                        return;
                    }
                    for (int i = 0; i < 9; i++) {
                        for (int j = 0; j < 9; j++) {
                            celdas[i][j].setText(String.valueOf(solucion[i][j]));
                            celdas[i][j].setForeground(model.getTableroInicial()[i][j] == 0 ? Color.GREEN : Color.BLUE);
                            celdas[i][j].setEditable(false);
                        }
                    }
                    stats.setEstado("autosolución");
                    break;
            }
        };
    }

    private void actualizarTablero() {
        int[][] tablero = model.getTableroActual();
        if (tablero == null) {
            tablero = new int[9][9];
        }
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                if (tablero[i][j] != 0) {
                    celdas[i][j].setText(String.valueOf(tablero[i][j]));
                    celdas[i][j].setEditable(false);
                    celdas[i][j].setForeground(Color.BLUE);
                } else {
                    celdas[i][j].setText("");
                    celdas[i][j].setEditable(true);
                    celdas[i][j].setForeground(Color.BLACK);
                }
            }
        }
        resaltarCeldaSeleccionada();
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> new SudokuGUI().setVisible(true));
    }
}
