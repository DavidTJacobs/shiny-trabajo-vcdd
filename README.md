# NFL Big Data Bowl — Shiny Dashboard

Dashboard interactivo desarrollado con R Shiny para el análisis de datos de pases del NFL Big Data Bowl 2021. La aplicación permite explorar el rendimiento ofensivo y defensivo de los 32 equipos de la NFL a lo largo de las primeras 8 semanas de la temporada 2021, con análisis por partido, por equipo, por quarterback y por situación táctica.

---

## 1. Descripción del proyecto y objetivos

El objetivo del proyecto es construir una herramienta de visualización interactiva que permita a analistas y aficionados explorar los datos de jugadas de pase de la NFL de manera intuitiva y visual.

Los objetivos específicos son:

- Visualizar el resultado de cada partido con marcador por cuartos y comparativas ofensivas y defensivas entre los dos equipos
- Analizar el rendimiento de cada equipo a lo largo de la temporada comparando sus métricas con la media de la liga semana a semana
- Evaluar el rendimiento individual de los quarterbacks mediante un ranking con rating NFL oficial
- Identificar patrones tácticos analizando qué formaciones ofensivas funcionan mejor contra cada tipo de cobertura defensiva

---

## 2. Estructura del dashboard y funcionalidades implementadas

El dashboard está organizado en 4 pestañas, cada una con su propio sidebar de filtros:

**Games**
Análisis de un partido concreto seleccionado por el usuario. Incluye marcador visual con puntos por cuarto, tarjetas KPI con resumen de cada equipo (jugadas, conversiones en 3ª y 4ª oportunidad, penalizaciones) y comparativas ofensiva y defensiva mediante barras apiladas proporcionales.

**Team**
Análisis de un equipo seleccionado a lo largo de toda la temporada. Incluye un gráfico de tendencia semanal para 12 métricas seleccionables (la misma métrica controla simultáneamente el gráfico y la tabla de ranking de liga), barras comparativas del equipo contra sus rivales agrupadas por categoría, y una tabla de ranking de los 32 equipos con el equipo seleccionado destacado en azul.

**QB**
Matriz comparativa de quarterbacks filtrable por mínimo de intentos. Incluye un scatter plot de QB Rating vs Completion % con el tamaño del punto proporcional a los intentos, y una tabla detallada con rating NFL oficial, estadísticas de pase e intercepciones.

**Tactics**
Dashboard táctico filtrable por formación ofensiva, personal, cobertura defensiva, cuarto, down y yards to go. Incluye KPIs reactivos, un boxplot de yards por jugada según formación, un heatmap de completion % por combinación formación × cobertura (verde = exitoso, rojo = fallido), y tres tablas de análisis (matriz formación-cobertura, resumen por down y rendimiento por cobertura).

---

## 3. Descripción de los datos y su origen

Los datos provienen del **NFL Big Data Bowl 2023**, disponibles públicamente en [Kaggle](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023/data).

| Archivo | Descripción |
|---|---|
| `plays.csv` | Una fila por jugada de pase: formación, cobertura, resultado, penalizaciones |
| `games.csv` | Metadata de cada partido: equipos, fecha, semana, temporada |
| `players.csv` | Información de jugadores: posición, nombre, datos físicos |
| `pffScoutingData.csv` | Datos de scouting PFF por jugador y jugada: hurries, hits, sacks, rol |

- **Temporada:** 2021
- **Semanas:** 1 a 8
- **Partidos:** 122
- **Equipos:** 32 (liga completa)
- **Jugadas totales:** ~8.500 jugadas de pase

---

## 4. Enlace a versión desplegada

No conseguido

---

## 5. Fundamentos de visualización de datos aplicados

**Jerarquía visual en KPI cards**
Los valores numéricos clave se presentan en negrita y con tipografía grande, con el título de la métrica en texto pequeño y mayúsculas sobre ellos. El ojo va directamente al número sin necesidad de leer la etiqueta previamente.

**Comparación directa con barras apiladas proporcionales**
En lugar de gráficos de barras separados, las comparativas entre equipos usan una barra dividida donde el ancho de cada lado es proporcional al valor relativo. Permite leer de un vistazo quién domina cada métrica sin necesidad de comparar alturas entre dos elementos separados.

**Heatmap con escala anclada**
El heatmap de formación × cobertura utiliza una escala fija de 0 a 100% (no relativa a los datos del filtro activo). Esto evita que un 54% parezca excelente simplemente porque el resto de las celdas son peores — el color siempre comunica el mismo valor absoluto independientemente del contexto.

**Boxplot para comparar distribuciones**
Se eligió boxplot sobre violin para la distribución de yards por formación porque los volúmenes entre formaciones son muy desiguales (SHOTGUN tiene ~5.500 jugadas, JUMBO ~30). Con muestras pequeñas el KDE del violin genera formas artificiales. El boxplot muestra la mediana, los cuartiles y los valores atípicos con precisión numérica, más útil para un contexto de toma de decisiones deportivas.

**Consistencia de paleta**
Toda la app usa la paleta oficial NFL: azul `#013369` y rojo `#c94c5a`. Los gráficos Plotly respetan esta paleta en lugar de usar los colores por defecto de la librería.

**Control unificado**
El selector de métrica en la pestaña Team controla simultáneamente el gráfico de tendencia semanal y la tabla de ranking de liga. Un único input, dos outputs sincronizados — reduce la carga cognitiva del usuario.

---

## 6. Conclusiones y posibles mejoras futuras

**Conclusiones**

- SHOTGUN es la formación dominante en la liga (>64% de las jugadas) y también la que más yards explosivos genera, aunque con alta varianza
- Las coberturas de zona (Cover-2, Cover-3) tienden a generar completion rates más altas que las de hombre a hombre, pero con menos yards por jugada
- Existe una correlación clara entre QB Rating y Completion %, con los quarterbacks de alto volumen (>150 intentos) concentrados en la zona 60-70% / 85-110 rating
- El time of possession varía significativamente entre equipos a lo largo de la temporada, con algunas franquicias mostrando tendencias claras de control del balón

**Posibles mejoras futuras**

- Incorporar datos de más temporadas para análisis de tendencias a largo plazo
- Incluir visualizaciones de campo (field plots) con la posición exacta de cada jugada
- Añadir un modelo predictivo de éxito de jugada basado en situación táctica
- Implementar comparación directa entre dos equipos seleccionables en la pestaña Team
