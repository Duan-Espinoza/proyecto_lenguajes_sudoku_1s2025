package com.miempresa.sudoku.model;

import java.util.ArrayList;
import java.util.List;

public class GameStats {

    public enum TipoFinalizacion {
        EXITOSA, ABANDONO, AUTOSOLUCION
    }

    // Representa un juego individual
    public static class RegistroJuego {
        int celdasIngresadas;
        int verificaciones;
        int erroresVerificacion;
        int sugerenciasUsadas;
        TipoFinalizacion tipoFinalizacion;

        // Constructor por defecto
        // Inicializa los contadores a cero y tipoFinalizacion a null
        public RegistroJuego() {
            celdasIngresadas = 0;
            verificaciones = 0;
            erroresVerificacion = 0;
            sugerenciasUsadas = 0;
            tipoFinalizacion = null;
        }
        @Override
        public String toString() {
            return "RegistroJuego{" +
                    "celdasIngresadas=" + celdasIngresadas +
                    ", verificaciones=" + verificaciones +
                    ", erroresVerificacion=" + erroresVerificacion +
                    ", sugerenciasUsadas=" + sugerenciasUsadas +
                    ", tipoFinalizacion=" + tipoFinalizacion +
                    '}';
        }
    }

    private final List<RegistroJuego> juegos;
    private RegistroJuego juegoActual;

    public GameStats() {
        juegos = new ArrayList<>();
        juegoActual = new RegistroJuego();
    }

    // Se llama al iniciar un nuevo juego
    public void nuevoJuego() {
        if (juegoActual != null && juegoActual.tipoFinalizacion == null) {
            juegoActual.tipoFinalizacion = TipoFinalizacion.ABANDONO;
            juegos.add(juegoActual);
        }
        juegoActual = new RegistroJuego();
    }

    public void registrarIngresoCelda() {
        juegoActual.celdasIngresadas++;
    }

    public void registrarVerificacion(boolean fueCorrecta) {
        juegoActual.verificaciones++;
        if (!fueCorrecta) {
            juegoActual.erroresVerificacion++;
        }
    }

    public void registrarSugerencia() {
        juegoActual.sugerenciasUsadas++;
    }

    public void finalizarJuego(TipoFinalizacion tipo) {
        juegoActual.tipoFinalizacion = tipo;
        juegos.add(juegoActual);
        juegoActual = new RegistroJuego(); // preparado por si se comienza otro juego
    }

    public List<RegistroJuego> obtenerEstadisticas() {
        return new ArrayList<>(juegos); // para evitar modificación externa
    }

    public String obtenerResumenEstadisticas() {
        StringBuilder sb = new StringBuilder("Resumen de juegos de la sesión:\n");
        int i = 1;
        for (RegistroJuego r : juegos) {
            sb.append("Juego ").append(i++).append(":\n")
              .append("  Celdas ingresadas: ").append(r.celdasIngresadas).append("\n")
              .append("  Verificaciones: ").append(r.verificaciones).append("\n")
              .append("  Errores de verificación: ").append(r.erroresVerificacion).append("\n")
              .append("  Sugerencias usadas: ").append(r.sugerenciasUsadas).append("\n")
              .append("  Tipo finalización: ").append(r.tipoFinalizacion).append("\n\n");
        }
        return sb.toString();
    }

    // Metodo para establecer el tipo de finalización del juego actual como AUTOSOLUCION
    public void setTipoFinalizacionSolucion() {
        if (juegoActual != null) {
            juegoActual.tipoFinalizacion = TipoFinalizacion.AUTOSOLUCION;
        }
    }

    //Metodo para establecer el tipo de finalización del juego actual como EXITOSA
    public void setTipoFinalizacionExitosa() {
        if (juegoActual != null) {
            juegoActual.tipoFinalizacion = TipoFinalizacion.EXITOSA;
        }
    }

    // Método para establecer el tipo de finalización del juego actual como ABANDONO
    public void setTipoFinalizacionAbandono() {
        if (juegoActual != null) {
            juegoActual.tipoFinalizacion = TipoFinalizacion.ABANDONO;
        }
    }

    @Override
    public String toString() {
        return "GameStats{" +
                "celdasIngresadas=" + juegoActual.celdasIngresadas +
                ", verificaciones=" + juegoActual.verificaciones +
                ", erroresVerificacion=" + juegoActual.erroresVerificacion +    
                ", sugerenciasUsadas=" + juegoActual.sugerenciasUsadas +
                ", tipoFinalizacion=" + juegoActual.tipoFinalizacion +
                ", juegos=" + juegos.size() +
                '}';
    }
}

