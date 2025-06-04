package com.miempresa.sudoku.view;

import com.formdev.flatlaf.FlatLightLaf;
import com.miempresa.sudoku.controller.PrologController;
import com.miempresa.sudoku.model.SudokuModel;
import com.miempresa.sudoku.model.GameStats;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Arrays;

/**
 * SudokuGUI es la interfaz gráfica principal para la aplicación de Sudoku.
 * 
 * Esta clase extiende {@link JFrame} y proporciona una GUI completa para jugar Sudoku,
 * incluyendo una cuadrícula de 9x9, indicadores de estado para vidas y sugerencias,
 * y botones de control para acciones como nuevo juego, reinicio, verificación,
 * solicitud de sugerencias y mostrar la solución.
 *
 * 
 *   Integra un backend basado en Prolog mediante {@link PrologController} para la lógica del juego.
 *   Muestra y actualiza el tablero de Sudoku usando una cuadrícula de componentes {@link JTextField}.
 *   Gestiona y muestra estadísticas del jugador como vidas restantes y sugerencias disponibles.
 *   Maneja interacciones del usuario, incluyendo selección de celdas, ingreso de números y acciones de botones.
 *   Proporciona retroalimentación visual para movimientos correctos e incorrectos, y resalta la celda seleccionada.
 *   Permite mostrar la solución y reinicia automáticamente el juego cuando se agotan las vidas.
 *
 * Uso:
 *     public static void main(String[] args) {
 *         SwingUtilities.invokeLater(() -> new SudokuGUI().setVisible(true));
 *     }
 *
 * @author Duan Espinoza Olivares, Geovanni Gonzalez Aguilar
 * @version 1.0
 */
public class SudokuGUI extends JFrame {
    private final SudokuModel model = new SudokuModel();
    private final GameStats stats = new GameStats();
    private final PrologController controller;
    private JTextField[][] celdas = new JTextField[9][9];
    private JLabel lblVidas, lblSugerencias;
    private int filaSeleccionada = -1;
    private int colSeleccionada = -1;


    /**
     * Nombre: SudokuGUI
     * Descripción: Constructor de la clase SudokuGUI que inicializa la interfaz gráfica,
     * configura el Look and Feel de FlatLaf y establece el controlador PrologController.
     * Entrada: Ninguna.
     * Salida: Crea una instancia de SudokuGUI con la interfaz gráfica lista para ser mostrada.
     * Restricciones: Debe ser llamado en el hilo de eventos de Swing.
     * Excepciones: Puede lanzar excepciones si hay problemas al cargar el Look and Feel o al inicializar la UI.
     */

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

    /**
     * Nombre: actualizarEstado
     * Descripción: Actualiza las etiquetas de estado que muestran las vidas y sugerencias restantes.
     * También verifica si el jugador ha perdido todas sus vidas y reinicia el juego si es necesario.
     * Entrada: Ninguna.
     * Salida: Actualiza las etiquetas de la interfaz gráfica y muestra un mensaje de fin de juego si es necesario.
     * Restricciones: Debe ser llamado después de cada acción del usuario que modifique el estado del juego.
     */
    private void actualizarEstado() {
        lblVidas.setText("Vidas: " + model.getVidas());
        lblSugerencias.setText("Sugerencias: " + model.getSugerencias());   

        if (model.getVidas() <= 0) {
            JOptionPane.showMessageDialog(this, "¡Juego terminado! No tienes más vidas.");
            //Mensaje con opcion de reiniciar el juego o iniciar uno nuevo
            int opcion = JOptionPane.showOptionDialog(this, "¿Deseas iniciar un nuevo juego o reiniciar el actual?",
                    "Juego Terminado", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE,
                    null, new String[]{"Nuevo Juego", "Reiniciar"}, "Nuevo Juego");
            if (opcion == JOptionPane.YES_OPTION) {
                controller.nuevoJuego();
                filaSeleccionada = -1;
                colSeleccionada = -1;
                actualizarTablero();
                actualizarEstado();
            } else if (opcion == JOptionPane.NO_OPTION) {
                controller.reiniciarSudoku();
                stats.reiniciarJuegoActual();
                filaSeleccionada = -1;
                colSeleccionada = -1;
                actualizarTablero();
                actualizarEstado();
            }
        }
    }

    /**
     * Nombre: initUI
     * Descripción: Inicializa la interfaz gráfica de usuario.
     * Entrada: Ninguna.
     * Salida: Crea y organiza los componentes de la interfaz gráfica.
     * Restricciones: Debe ser llamado en el hilo de eventos de Swing.
     */
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

    /**
     * Nombre: resaltarCeldaSeleccionada
     * Descripción: Resalta la celda seleccionada en el tablero de Sudoku.
     * Entrada: Ninguna.
     * Salida: Cambia el color de fondo de la celda seleccionada a un color amarillo suave.
     * Restricciones: Debe ser llamado después de que se seleccione una celda.
     */
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

    /**
     * Nombre: crearManejadorBoton
     * Descripción: Crea un ActionListener para manejar los eventos de los botones.
     * Entrada: Un String que representa el texto del botón.
     * Salida: Un ActionListener que maneja la acción del botón correspondiente.
     * Restricciones: Debe ser llamado al crear cada botón en la interfaz gráfica.
     */

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
                    stats.reiniciarJuegoActual();
                    filaSeleccionada = -1;
                    colSeleccionada = -1;
                    actualizarTablero();
                    actualizarEstado();
                    break;

                case "Verificar":
                    String mensaje = controller.resumenEstadisticas();
                    JOptionPane.showMessageDialog(this, mensaje, "Estadísticas", JOptionPane.INFORMATION_MESSAGE);
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
                
                    break;
            }
        };
    }
    
    /**
     * Nombre: actualizarTablero
     * Descripción: Actualiza el tablero de Sudoku en la interfaz gráfica.
     * Entrada: Ninguna.
     * Salida: Refresca los componentes de la interfaz gráfica para mostrar el estado actual del tablero.
     * Restricciones: Debe ser llamado después de cada acción del usuario que modifique el estado del juego.
     */
    
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

    /**
     * Nombre: main
     * Descripción: Método principal que inicia la aplicación de Sudoku.
     * Entrada: Argumentos de línea de comandos (no utilizados).
     * Salida: Inicia la interfaz gráfica de Sudoku.
     * Restricciones: Debe ser llamado en el hilo de eventos de Swing para asegurar que la GUI se construya correctamente.
     */
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> new SudokuGUI().setVisible(true));
    }
}
