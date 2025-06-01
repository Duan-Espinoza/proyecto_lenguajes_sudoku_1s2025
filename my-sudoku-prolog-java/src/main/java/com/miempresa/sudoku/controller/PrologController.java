package com.miempresa.sudoku.controller;

import org.jpl7.*;
import java.io.InputStream;
import java.nio.file.*;

public class PrologController {

    static {
        // Configuración específica para Windows
        System.setProperty("swi.home.dir", "C:\\Program Files\\swipl");
        System.setProperty("jpl.library.path", "C:\\Program Files\\swipl\\bin\\jpl.dll");
        
        try {
            // Inicializar JPL explícitamente
            JPL.init();
            System.out.println("JPL inicializado correctamente");
        } catch (UnsatisfiedLinkError e) {
            System.err.println("Error crítico al cargar JPL:");
            e.printStackTrace();
            System.exit(1);
        }
    }

    public PrologController() {
        inicializarProlog();
    }

    private void inicializarProlog() {
        // Cargar archivos desde recursos usando ClassLoader
        cargarArchivoProlog("engine/generador.pl");
        cargarArchivoProlog("puzzles/juego_sudoku.pl");
    }

    private void cargarArchivoProlog(String resourcePath) {
        try {
            // Obtener recurso como InputStream
            InputStream inputStream = getClass().getClassLoader().getResourceAsStream(resourcePath);
            if (inputStream == null) {
                System.err.println("Archivo no encontrado: " + resourcePath);
                return;
            }
            
            // Crear archivo temporal
            Path tempFile = Files.createTempFile("prolog_", ".pl");
            Files.copy(inputStream, tempFile, StandardCopyOption.REPLACE_EXISTING);
            String filePath = tempFile.toAbsolutePath().toString();
            
            // Consultar en Prolog
            String consultQuery = "consult('" + filePath.replace("\\", "/") + "')";
            Query query = new Query(consultQuery);
            
            if (query.hasSolution()) {
                System.out.println("Archivo cargado: " + resourcePath);
            } else {
                System.err.println("Error al cargar: " + resourcePath);
            }
            
            // Cerrar recursos
            inputStream.close();
        } catch (Exception e) {
            System.err.println("Excepción al cargar " + resourcePath + ": " + e.getMessage());
            e.printStackTrace();
        }
    }

    public void iniciarJuego() {
        ejecutarConsulta("iniciar_juego");
    }

    public void insertar(int fila, int col, int valor) {
        ejecutarConsulta(String.format("accion_insertar(%d, %d, %d)", fila, col, valor));
    }

    public void verificar() {
        ejecutarConsulta("accion_verificar");
    }

    public void sugerir(int fila, int col) {
        ejecutarConsulta(String.format("accion_sugerir(%d, %d)", fila, col));
    }

    public void verSolucion() {
        ejecutarConsulta("ver_solucion");
    }

    public void reiniciar() {
        ejecutarConsulta("reiniciar");
    }
    
    private void ejecutarConsulta(String consulta) {
        try {
            Query q = new Query(consulta);
            if (!q.hasSolution()) {
                System.err.println("La consulta falló: " + consulta);
            }
        } catch (Exception e) {
            System.err.println("Error en consulta '" + consulta + "': " + e.getMessage());
        }
    }
}