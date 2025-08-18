library(shiny)
library(dplyr)
data <- read.csv("dataset.csv", sep = ";", stringsAsFactors = FALSE)

data <- data %>%
  rename(
    Escalafon = escalafon_desc,
    Dedicacion = dedicacion_desc,
    Categoria = categoria_desc,
    Concepto = concepto_desc,
    `Salario bruto` = importe_liq
  )

data$Escalafon <- gsub("Superior", "Autoridades", data$Escalafon)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f0f2f5;
        color: #333;
      }
      .title {
        color: #0056b3;
        text-align: center;
        margin-bottom: 30px;
        font-weight: bold;
      }
      .well {
        background-color: #ffffff;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        padding: 20px;
        margin-bottom: 20px;
      }
      .form-group.shiny-input-container label {
        font-weight: 600;
        color: #555;
        margin-bottom: 8px;
        display: block;
      }
      .selectize-input, .selectize-dropdown-content {
        border-radius: 5px;
        border: 1px solid #ced4da;
        padding: 8px 12px;
        background-color: #fff;
      }
      .selectize-input {
        box-shadow: inset 0 1px 2px rgba(0,0,0,.075);
      }
      .output-box {
        background-color: #e9ecef;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-top: 30px;
        font-size: 1.1em;
        line-height: 1.6;
        color: #212529;
      }
      .output-label {
        font-weight: bold;
        color: #0056b3;
        margin-bottom: 5px;
        display: block;
      }
      hr {
        border-top: 1px solid #ddd;
        margin-top: 25px;
        margin-bottom: 25px;
      }
      .total-line {
        font-weight: bold;
        color: #d9534f; /* Un color distinto para el total */
        margin-top: 10px;
        padding-top: 10px;
        border-top: 1px dashed #ccc;
      }
    "))
  ),

  titlePanel(div(class = "title", "Escalas Salariales del Personal de la UNC")),

  sidebarLayout(
    sidebarPanel(
      class = "well",
      selectInput(
        inputId = "escalafon",
        label = "Seleccione el Escalafón:",
        choices = c("Seleccione..." = "", unique(na.omit(data$Escalafon))),
        selected = ""
      ),
      hr(),

      uiOutput("categoria_ui"),
      hr(),

      # Utilizar conditionalPanel para mostrar/ocultar el filtro de Dedicación
      conditionalPanel(
        condition = "input.escalafon != 'No Docente' && input.escalafon != '' && input.categoria != ''", # "No Docente"
        uiOutput("dedicacion_ui")
      ),
      # Mensaje para Dedicación si es No Docente
      conditionalPanel(
        condition = "input.escalafon == 'No Docente'", # "No Docente"
        helpText("La Dedicación no aplica para Escalafón No Docente.")
      ),
      # Mensaje para Dedicación si falta Escalafón o Categoría
      conditionalPanel(
        condition = "input.escalafon != 'No Docente' && (input.escalafon == '' || input.categoria == '')", # "No Docente"
        helpText("Por favor, seleccione Escalafón y Categoría primero.")
      ),
      hr(),

      # Nuevo filtro de Antigüedad, reactivo al escalafón
      uiOutput("antiguedad_ui")
    ),

    mainPanel(
      div(class = "output-box",
          h4(class = "output-label", "Información del Salario:"),
          htmlOutput("salario_concepto")
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {

  # Renderizar el combobox de Categoría dinámicamente
  output$categoria_ui <- renderUI({
    req(input$escalafon)

    if (input$escalafon == "") {
      helpText("Por favor, seleccione un Escalafón primero.")
    } else {
      # Filtrar las opciones de "Categoría" según "Escalafón"
      categorias_disponibles <- data %>%
        filter(Escalafon == input$escalafon) %>%
        pull(Categoria) %>%
        unique() %>%
        na.omit()

      # Orden específico para el Escalafón "Docente"
      if (input$escalafon == "Docente") {
        orden_docente <- c("PROFESOR TITULAR", "PROFESOR ASOCIADO", "PROFESOR ADJUNTO", "PROFESOR ASISTENTE", "PROFESOR AYUDANTE A")
        # Asegurarse de que solo las categorías presentes en los datos se incluyan y en el orden deseado
        categorias_disponibles <- factor(categorias_disponibles, levels = orden_docente[orden_docente %in% categorias_disponibles])
        categorias_disponibles <- as.character(sort(categorias_disponibles)) # Convertir a character y ordenar según el factor
      } else {
        categorias_disponibles <- sort(categorias_disponibles) # Orden alfabético por defecto
      }

      selectInput(
        inputId = "categoria",
        label = "Seleccione la Categoría:",
        choices = c("Seleccione..." = "", categorias_disponibles),
        selected = ""
      )
    }
  })

  # Renderizar el combobox de Dedicación dinámicamente
  output$dedicacion_ui <- renderUI({
    req(input$escalafon, input$categoria)

    # Filtrar las opciones de "Dedicación" según "Escalafón" y "Categoría"
    dedicaciones_disponibles <- data %>%
      filter(Escalafon == input$escalafon,
             Categoria == input$categoria) %>%
      pull(Dedicacion) %>%
      unique() %>%
      na.omit() %>% # Eliminar NA de las opciones
      sort() # Ordenar alfabéticamente

    selectInput(
      inputId = "dedicacion",
      label = "Seleccione la Dedicación:",
      choices = c("Seleccione..." = "", dedicaciones_disponibles),
      selected = ""
    )
  })

  # Renderizar el filtro de Antigüedad dinámicamente
  output$antiguedad_ui <- renderUI({
    req(input$escalafon)

    if (input$escalafon == "") {
      helpText("Por favor, seleccione un Escalafón primero para definir la Antigüedad.")
    } else if (input$escalafon == "Docente") {
      selectInput(
        inputId = "antiguedad",
        label = "Seleccione la Antigüedad (años):",
        choices = c(
          "Seleccione..." = "",
          "0 a 4 años",
          "5 a 6 años",
          "7 a 9 años",
          "10 a 11 años",
          "12 a 14 años",
          "15 a 16 años",
          "17 a 19 años",
          "20 a 21 años",
          "22 a 23 años",
          "+24 años"
        ),
        selected = ""
      )
    } else if (input$escalafon == "No Docente") {
      sliderInput(
        inputId = "antiguedad",
        label = "Seleccione la Antigüedad (años):",
        min = 0,
        max = 24,
        value = 0,
        step = 1
      )
    } else if (input$escalafon == "Autoridades") {
      selectInput(
        inputId = "antiguedad",
        label = "Seleccione la Antigüedad (años):",
        choices = c(
          "Seleccione..." = "",
          "0 a 19 años",
          "20 a 21 años",
          "22 a 23 años",
          "+24 años"
        ),
        selected = ""
      )
    } else {
      helpText("Seleccione un Escalafón para ver las opciones de Antigüedad.")
    }
  })


  # Mostrar el "Salario bruto" y "Concepto" según las selecciones finales
  output$salario_concepto <- renderUI({
    # Requiere que Escalafón y Categoría estén seleccionados
    req(input$escalafon, input$categoria, input$antiguedad)

    if (input$escalafon == "" || input$categoria == "" || input$antiguedad == "") {
      return(HTML("<p>Por favor, complete todas las selecciones para ver la información.</p>"))
    }

    # Filtrar el dataset con las selecciones iniciales (Escalafon y Categoria)
    filtered_data_base <- data %>%
      filter(Escalafon == input$escalafon,
             Categoria == input$categoria)

    # Si el Escalafón NO es "No Docente", aplicar filtro de Dedicación
    if (input$escalafon != "No Docente") {
      req(input$dedicacion) # Dedicación es requerida si no es No Docente
      if (input$dedicacion == "") {
        return(HTML("<p>Por favor, complete todas las selecciones para ver la información.</p>"))
      }
      filtered_data <- filtered_data_base %>%
        filter(Dedicacion == input$dedicacion)
    } else {
      # Si es No Docente, filtered_data es igual a filtered_data_base (sin filtro de Dedicación)
      filtered_data <- filtered_data_base
    }

    # Procesar los datos filtrados
    if (nrow(filtered_data) > 0) {
      # Extraer el salario básico del concepto "Basico"
      basico_row <- filtered_data %>% filter(Concepto == "Basico")
      basico_amount <- ifelse(nrow(basico_row) > 0, basico_row$`Salario bruto`[1], 0) # Tomar el primer básico si hay varios, o 0 si no se encuentra

      adicional_antiguedad_amount <- 0
      adicional_antiguedad_concepto <- "Adicional Antigüedad"

      # Calcular Adicional Antigüedad según el Escalafón
      if (input$escalafon == "Docente") {
        porcentaje <- switch(input$antiguedad,
                             "0 a 4 años" = 0.20,
                             "5 a 6 años" = 0.30,
                             "7 a 9 años" = 0.40,
                             "10 a 11 años" = 0.50,
                             "12 a 14 años" = 0.60,
                             "15 a 16 años" = 0.70,
                             "17 a 19 años" = 0.80,
                             "20 a 21 años" = 1.00,
                             "22 a 23 años" = 1.10,
                             "+24 años" = 1.20,
                             0) # Default a 0 si no coincide
        adicional_antiguedad_amount <- basico_amount * porcentaje
      } else if (input$escalafon == "No Docente") {
        # Cálculo acumulativo para No Docente
        years_nodocente <- as.numeric(input$antiguedad)
        if (years_nodocente > 0) {
          adicional_antiguedad_amount <- basico_amount * ((1 + 0.02)^years_nodocente - 1)
        } else {
          adicional_antiguedad_amount <- 0
        }
      } else if (input$escalafon == "Autoridades") {
        porcentaje <- switch(input$antiguedad,
                             "0 a 19 años" = 0.80,
                             "20 a 21 años" = 1.00,
                             "22 a 23 años" = 1.10,
                             "+24 años" = 1.20,
                             0) # Default a 0 si no coincide
        adicional_antiguedad_amount <- basico_amount * porcentaje
      }

      # Obtener los conceptos distintos y sus salarios para la visualización individual
      resultado_distinct <- filtered_data %>%
        distinct(`Salario bruto`, Concepto)

      # Crear la fila de antigüedad como un tibble
      antiguedad_df_row <- tibble(
        `Salario bruto` = adicional_antiguedad_amount,
        Concepto = adicional_antiguedad_concepto
      )

      # Usar bind_rows para combinar, que es más robusto con data frames vacíos
      all_concepts <- bind_rows(resultado_distinct, antiguedad_df_row)

      # Crear una lista de párrafos para cada concepto, formateando como "Concepto: $Valor"
      output_paragraphs <- apply(all_concepts, 1, function(row) {
        salario_formateado_individual <- format(as.numeric(row["Salario bruto"]), big.mark = ".", decimal.mark = ",")
        paste0("<p><strong>", row["Concepto"], ":</strong> $", salario_formateado_individual, "</p>")
      })

      final_html <- paste(output_paragraphs, collapse = "")

      # Calcular el total del salario bruto final (suma de todos los conceptos, incluyendo antigüedad)
      # Este total se calculará siempre para todos los casos
      total_salario_final <- sum(all_concepts$`Salario bruto`, na.rm = TRUE)
      total_salario_final_formateado <- format(total_salario_final, big.mark = ".", decimal.mark = ",")

      # Agregar la línea de "Salario bruto total" al final en todos los casos
      final_html <- paste0(final_html, "<p class='total-line'><strong>Salario bruto total:</strong> $", total_salario_final_formateado, "</p>")

      HTML(final_html)
    } else {
      HTML("<p>No se encontraron datos para la combinación seleccionada.</p>")
    }
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)





