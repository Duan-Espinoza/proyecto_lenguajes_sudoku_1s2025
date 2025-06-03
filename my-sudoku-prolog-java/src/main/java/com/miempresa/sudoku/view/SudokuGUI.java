package com.miempresa.sudoku.view;

import com.miempresa.sudoku.controller.PrologController;
import com.miempresa.sudoku.model.SudokuModel;
import com.miempresa.sudoku.model.GameStats;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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
        controller = new PrologController(model, stats);
        initUI();
        actualizarTablero();
    }

        private void actualizarEstado() {
        lblVidas.setText("Vidas: " + model.getVidas());
        if (model.getVidas() <= 0) {
            JOptionPane.showMessageDialog(this, "¡Juego terminado! No tienes más vidas.");
            controller.nuevoJuego();  // Reiniciar el juego
        }
        lblSugerencias.setText("Sugerencias: " + model.getSugerencias());
    }

    private void initUI() {
        setTitle("Sudoku Prolog-Java");
        setLayout(new BorderLayout());

        // Panel de estado
        JPanel panelEstado = new JPanel();
        lblVidas = new JLabel("Vidas: " + model.getVidas());
        lblVidas.setFont(new Font("SansSerif", Font.BOLD, 16));
        lblSugerencias = new JLabel("Sugerencias: " + model.getSugerencias());
        lblSugerencias.setFont(new Font("SansSerif", Font.BOLD, 16));
        panelEstado.add(lblVidas);
        panelEstado.add(lblSugerencias);

        // Panel del tablero
        JPanel tableroPanel = new JPanel(new GridLayout(9, 9));
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                celdas[i][j] = new JTextField();
                celdas[i][j].setHorizontalAlignment(JTextField.CENTER);
                celdas[i][j].setFont(new Font("SansSerif", Font.BOLD, 20));

                // Resaltar bloques 3x3
                if ((i / 3 + j / 3) % 2 == 0) {
                    celdas[i][j].setBackground(new Color(240, 240, 240));
                }

                final int fila = i, col = j;

                // Listener para selección de celda
                celdas[i][j].addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent e) {
                        filaSeleccionada = fila;
                        colSeleccionada = col;
                        resaltarCeldaSeleccionada();
                    }
                });

                // Listener para entrada de datos
                celdas[i][j].addActionListener(e -> {
                    try {
                        int valor = Integer.parseInt(celdas[fila][col].getText());  // Obtener valor ingresado
                        // Validar que el valor esté entre 1 y 9
                        if (valor >= 1 && valor <= 9) {
                            if (controller.insertarNumero(fila + 1, col + 1, valor)) {
                                celdas[fila][col].setForeground(Color.BLACK);
                            } else {
                                celdas[fila][col].setForeground(Color.RED);  
                            }

                            actualizarEstado();
                        } else {
                            JOptionPane.showMessageDialog(this, "Solo números del 1 al 9");
                        }
                    } catch (NumberFormatException ex) {
                        JOptionPane.showMessageDialog(this, "Entrada inválida");
                    }
                });

                tableroPanel.add(celdas[i][j]);
            }
        }

        // Panel de botones
        JPanel buttonPanel = new JPanel();
        String[] botones = {"Nuevo Juego", "Reiniciar", "Verificar", "Sugerencia", "Solución"};
        for (String texto : botones) {
            JButton btn = new JButton(texto);
            btn.addActionListener(crearManejadorBoton(texto));
            buttonPanel.add(btn);
        }

        add(panelEstado, BorderLayout.NORTH);
        add(tableroPanel, BorderLayout.CENTER);
        add(buttonPanel, BorderLayout.SOUTH);

        setSize(600, 600);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    private void resaltarCeldaSeleccionada() {
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                celdas[i][j].setBackground(
                    (i / 3 + j / 3) % 2 == 0 ?
                    new Color(240, 240, 240) :
                    Color.WHITE
                );
            }
        }
        if (filaSeleccionada >= 0 && colSeleccionada >= 0) {
            celdas[filaSeleccionada][colSeleccionada].setBackground(Color.YELLOW);
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
                    // Restaurar tablero inicial
                    model.setTableroActual(model.getTableroInicial());
                    filaSeleccionada = -1;
                    colSeleccionada = -1;
                    actualizarTablero();
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
                            celdas[i][j].setForeground(
                                model.getTableroInicial()[i][j] == 0 ?
                                Color.GREEN : Color.BLUE
                            );
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
            System.err.println("Advertencia: Tablero es null, usando respaldo");
            tablero = new int[9][9];
        }

        System.out.println("Actualizando tablero:");
        for (int i = 0; i < 9; i++) {
            System.out.println(Arrays.toString(tablero[i]));
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
