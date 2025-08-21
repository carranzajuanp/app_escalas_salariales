if (!require(shiny)) install.packages("shiny")
if (!require(dplyr)) install.packages("dplyr")

library(shiny)
library(dplyr)
library(tibble)

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

no_docente_basics_data <- data %>%
  filter(Escalafon == "No Docente", Concepto == "Basico") %>%
  select(Categoria, `Salario bruto`) %>%
  distinct()

no_docente_basic_salaries_map <- setNames(
  no_docente_basics_data$`Salario bruto`,
  no_docente_basics_data$Categoria
)

no_docente_category_order_by_basic_asc <- no_docente_basics_data %>%
  mutate(category_num = as.numeric(gsub("Categ (\\d+) Esc\\. 366/06", "\\1", Categoria))) %>%
  arrange(desc(category_num)) %>%
  pull(Categoria)

get_basic_salary_for_category <- function(category_name, basic_salaries_map) {
  basic_salaries_map[category_name] %||% 0
}

get_superior_category_basic_salary <- function(current_category_name, all_basic_salaries_map, category_order_list_asc) {
  current_index_in_order <- which(category_order_list_asc == current_category_name)
  
  if (length(current_index_in_order) == 0 || current_index_in_order == length(category_order_list_asc)) {
    return(0)
  } else {
    superior_category_name <- category_order_list_asc[current_index_in_order + 1]
    return(get_basic_salary_for_category(superior_category_name, all_basic_salaries_map))
  }
}

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
        font-size: 1.2em;
      }
      hr {
        border-top: 1px solid #ccc;
        margin-top: 25px;
        margin-bottom: 25px;
      }
      .total-line {
        font-weight: bold;
        background-color: #676B99;
        color: #FFFFFF;
        margin-top: 10px;
        padding: 10px;
        border-radius: 5px;
        font-size: 1.2em;
      }
      .net-salary-line {
        font-weight: bold;
        background-color: #676B99;
        color: #FFFFFF;
        margin-top: 10px;
        padding: 10px;
        border-radius: 5px;
        font-size: 1.2em;
      }
      .space-separator {
        height: 15px;
        margin: 5px 0;
      }
      .section-title {
        font-weight: bold;
        color: #0056b3;
        margin-top: 20px;
        margin-bottom: 10px;
        font-size: 1.2em;
      }
      .section-divider-blue {
        border-top: 2px solid #0056b3;
        margin: 20px 0;
      }
      .disclaimer-text {
        font-size: 1.1em;
        color: #6c757d;
        margin-top: 20px;
        padding: 10px;
        border: 1px solid #e0e0e0;
        border-radius: 5px;
        background-color: #f8f9fa;
        line-height: 1.4;
      }
      .update-info-top {
        text-align: left;
        font-size: 1.1em;
        color: #6c757d;
        margin-bottom: 15px;
        font-weight: bold;
      }
    "))
  ),
  
  titlePanel(div(class = "title", "Simulador Salarial UNC")),
  
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
      
      uiOutput("dedicacion_agrupamiento_ui"),
      hr(),
      
      uiOutput("antiguedad_ui"),
      
      uiOutput("permanencia_categoria_ui"),
      hr(),
      
      uiOutput("formacion_academica_ui")
    ),
    
    mainPanel(
      div(class = "update-info-top", textOutput("info_actualizacion")),
      div(class = "output-box",
          h4(class = "output-label", "Haberes:"),
          htmlOutput("salario_concepto")
      ),
      div(class = "disclaimer-text",
          p( tags$strong("Importante:"),"Las simulaciones realizadas no incluyen las retenciones aplicadas por ", tags$strong("Impuesto a las Ganancias"), ", dado que dependen de las condiciones particulares de cada trabajador. Además, por practicidad se excluyen del cálculo algunos conceptos que no llegan a representar un 1% de los haberes ni de las retenciones. Además, se aclara que los importes aquí expresados pueden variar según casos particulares en donde se abonen garantías salariales, capacitaciones o algunos conceptos excepcionales.")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$categoria_ui <- renderUI({
    req(input$escalafon)
    
    if (input$escalafon == "") {
      helpText("Por favor, seleccione un Escalafón primero.")
    } else {
      categorias_disponibles <- data %>%
        filter(Escalafon == input$escalafon) %>%
        pull(Categoria) %>%
        unique() %>%
        na.omit()
      
      if (input$escalafon == "Docente") {
        orden_docente <- c("PROFESOR TITULAR", "PROFESOR ASOCIADO", "PROFESOR ADJUNTO", "PROFESOR ASISTENTE", "PROFESOR AYUDANTE A")
        categorias_disponibles <- factor(categorias_disponibles, levels = orden_docente[orden_docente %in% categorias_disponibles])
        categorias_disponibles <- as.character(sort(categorias_disponibles))
      } else {
        categorias_disponibles <- sort(categorias_disponibles)
      }
      
      selectInput(
        inputId = "categoria",
        label = "Seleccione la Categoría:",
        choices = c("Seleccione..." = "", categorias_disponibles),
        selected = ""
      )
    }
  })
  
  output$dedicacion_agrupamiento_ui <- renderUI({
    req(input$escalafon)
    
    if (input$escalafon == "No Docente") {
      req(input$categoria)
      selectInput(
        inputId = "agrupamiento",
        label = "Seleccione el Agrupamiento:",
        choices = c(
          "Seleccione..." = "",
          "Administrativo",
          "Técnico-Profesional",
          "Mantenimiento, Producción y Servicios Generales",
          "Asistencial"
        ),
        selected = ""
      )
    } else {
      req(input$categoria)
      
      if (input$escalafon == "" || input$categoria == "") {
        helpText("Por favor, seleccione Escalafón y Categoría primero.")
      } else {
        dedicaciones_disponibles <- data %>%
          filter(Escalafon == input$escalafon,
                 Categoria == input$categoria) %>%
          pull(Dedicacion) %>%
          unique() %>%
          na.omit() %>%
          sort()
        
        selectInput(
          inputId = "dedicacion",
          label = "Seleccione la Dedicación:",
          choices = c("Seleccione..." = "", dedicaciones_disponibles),
          selected = ""
        )
      }
    }
  })
  
  
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
  
  output$permanencia_categoria_ui <- renderUI({
    req(input$escalafon)
    if (input$escalafon == "No Docente") {
      current_antiguedad_val <- as.numeric(input$antiguedad)
      
      max_permanencia_years <- if (is.na(current_antiguedad_val) || length(current_antiguedad_val) == 0) {
        24
      } else {
        current_antiguedad_val
      }
      
      current_permanencia_value <- input$permanencia_categoria %||% 0
      adjusted_permanencia_value <- min(current_permanencia_value, max_permanencia_years)
      
      sliderInput(
        inputId = "permanencia_categoria",
        label = "Años de permanencia en la Categoría:",
        min = 0,
        max = max_permanencia_years,
        value = adjusted_permanencia_value,
        step = 1
      )
    } else {
      NULL
    }
  })
  
  output$formacion_academica_ui <- renderUI({
    req(input$escalafon)
    
    if (input$escalafon == "") {
      helpText("Seleccione un Escalafón para ver las opciones de Formación Académica.")
    } else if (input$escalafon == "Docente" || input$escalafon == "Autoridades") {
      checkboxGroupInput(
        inputId = "formacion_academica",
        label = "Seleccione Formación Académica:",
        choices = c("Doctorado", "Maestría", "Especialización")
      )
    } else if (input$escalafon == "No Docente") {
      checkboxGroupInput(
        inputId = "formacion_academica",
        label = "Seleccione Formación Académica:",
        choices = c("Posgrado", "Grado", "Tecnicatura en Gestión Universitaria", "Secundario")
      )
    } else {
      helpText("Seleccione un Escalafón para ver las opciones de Formación Académica.")
    }
  })
  
  output$salario_concepto <- renderUI({
    req(input$escalafon, input$categoria, input$antiguedad)
    
    if (input$escalafon == "" || input$categoria == "" || input$antiguedad == "") {
      return(HTML("<p>Por favor, complete todas las selecciones para ver la información.</p>"))
    }
    
    filtered_data_base <- data %>%
      filter(Escalafon == input$escalafon,
             Categoria == input$categoria)
    
    filtered_data <- filtered_data_base
    
    if (input$escalafon != "No Docente") {
      if (is.null(input$dedicacion) || input$dedicacion == "") {
        return(HTML("<p>Por favor, seleccione una <strong>Dedicación</strong> para ver los haberes.</p>"))
      }
      filtered_data <- filtered_data_base %>%
        filter(Dedicacion == input$dedicacion)
    } else {
      filtered_data <- filtered_data_base %>%
        filter(Concepto == "Basico")
    }
    
    if (nrow(filtered_data) > 0) {
      all_positive_concepts_df <- tibble(`Salario bruto` = numeric(), Concepto = character())
      
      basico_row <- filtered_data %>% filter(Concepto == "Basico")
      basico_amount <- ifelse(nrow(basico_row) > 0, basico_row$`Salario bruto`[1], 0)
      
      basico_categ7_nodocente <- data %>%
        filter(Escalafon == "No Docente", Categoria == "Categ 7 Esc. 366/06", Concepto == "Basico") %>%
        pull(`Salario bruto`)
      basico_categ7_nodocente_amount <- ifelse(length(basico_categ7_nodocente) > 0, basico_categ7_nodocente[1], 0)
      
      
      if (basico_amount > 0) {
        all_positive_concepts_df <- bind_rows(all_positive_concepts_df, tibble(`Salario bruto` = basico_amount, Concepto = "Basico"))
      }
      
      adicional_antiguedad_amount <- 0
      adicional_antiguedad_concepto <- "Adicional Antigüedad"
      
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
                             0)
        adicional_antiguedad_amount <- basico_amount * porcentaje
      } else if (input$escalafon == "No Docente") {
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
                             0)
        adicional_antiguedad_amount <- basico_amount * porcentaje
      }
      
      if (adicional_antiguedad_amount > 0) {
        all_positive_concepts_df <- bind_rows(all_positive_concepts_df, tibble(`Salario bruto` = adicional_antiguedad_amount, Concepto = adicional_antiguedad_concepto))
      }
      
      if (!is.null(input$formacion_academica) && length(input$formacion_academica) > 0) {
        adicional_formacion_academica_amount <- 0
        selected_formacion_level <- ""
        
        if (input$escalafon == "Docente" || input$escalafon == "Autoridades") {
          priority_order <- c("Doctorado", "Maestría", "Especialización")
          
          for (level in priority_order) {
            if (level %in% input$formacion_academica) {
              selected_formacion_level <- level
              break
            }
          }
          
          if (selected_formacion_level != "") {
            porcentaje_formacion <- switch(selected_formacion_level,
                                           "Doctorado" = 0.18,
                                           "Maestría" = 0.08,
                                           "Especialización" = 0.05,
                                           0)
            adicional_formacion_academica_amount <- basico_amount * porcentaje_formacion
          }
          
        } else if (input$escalafon == "No Docente") {
          priority_order <- c("Posgrado", "Grado", "Tecnicatura en Gestión Universitaria", "Secundario")
          
          for (level in priority_order) {
            if (level %in% input$formacion_academica) {
              selected_formacion_level <- level
              break
            }
          }
          
          if (selected_formacion_level != "") {
            porcentaje_formacion <- switch(selected_formacion_level,
                                           "Posgrado" = 0.30,
                                           "Grado" = 0.25,
                                           "Tecnicatura en Gestión Universitaria" = 0.20,
                                           "Secundario" = 0.175,
                                           0)
            
            if (selected_formacion_level == "Secundario") {
              adicional_formacion_academica_amount <- basico_categ7_nodocente_amount * porcentaje_formacion
            } else {
              adicional_formacion_academica_amount <- basico_amount * porcentaje_formacion
            }
          }
        }
        
        if (adicional_formacion_academica_amount > 0) {
          all_positive_concepts_df <- bind_rows(all_positive_concepts_df, tibble(`Salario bruto` = adicional_formacion_academica_amount, Concepto = "Adicional Formacion Academica"))
        }
      }
      
      if (input$escalafon == "No Docente") {
        req(input$permanencia_categoria)
        permanencia_years <- as.numeric(input$permanencia_categoria)
        adicional_permanencia_amount <- 0
        
        current_antiguedad_val <- as.numeric(input$antiguedad)
        if (!is.na(current_antiguedad_val) && permanencia_years > current_antiguedad_val) {
          adicional_permanencia_amount <- 0
        } else {
          porcentaje_permanencia <- 0
          if (permanencia_years >= 2 && permanencia_years < 4) {
            porcentaje_permanencia <- 0.10
          } else if (permanencia_years >= 4 && permanencia_years < 6) {
            porcentaje_permanencia <- 0.25
          } else if (permanencia_years >= 6 && permanencia_years < 8) {
            porcentaje_permanencia <- 0.45
          } else if (permanencia_years >= 8) {
            porcentaje_permanencia <- 0.70
          } else {
            porcentaje_permanencia <- 0
          }
          
          base_for_permanence_calc <- 0
          if (input$categoria == "Categ 1 Esc. 366/06") {
            base_for_permanence_calc <- basico_amount * 0.37
          } else {
            current_category_selected <- input$categoria
            superior_basic_amount <- get_superior_category_basic_salary(current_category_selected, no_docente_basic_salaries_map, no_docente_category_order_by_basic_asc)
            base_for_permanence_calc <- superior_basic_amount - basico_amount
            
            if (base_for_permanence_calc < 0) {
              base_for_permanence_calc <- 0
            }
          }
          
          adicional_permanencia_amount <- base_for_permanence_calc * porcentaje_permanencia
        }
        
        if (adicional_permanencia_amount > 0) {
          all_positive_concepts_df <- bind_rows(all_positive_concepts_df, tibble(`Salario bruto` = adicional_permanencia_amount, Concepto = "Adicional por Permanencia en Categoría"))
        }
      }
      
      if (input$escalafon == "No Docente") {
        req(input$agrupamiento)
        if (input$agrupamiento == "Asistencial") {
          adicional_tarea_asistencial_amount <- basico_amount * 0.12
          suplemento_riesgo_amount <- basico_amount * 0.10
          
          if (adicional_tarea_asistencial_amount > 0) {
            all_positive_concepts_df <- bind_rows(all_positive_concepts_df, tibble(`Salario bruto` = adicional_tarea_asistencial_amount, Concepto = "Adicional por Tarea Asistencial"))
          }
          if (suplemento_riesgo_amount > 0) {
            all_positive_concepts_df <- bind_rows(all_positive_concepts_df, tibble(`Salario bruto` = suplemento_riesgo_amount, Concepto = "Suplemento por Riesgo"))
          }
        }
      }
      
      
      excluded_concepts <- c("Basico", "Adicional Antiguedad", "Adicional Formacion Academica",
                             "Adicional por Permanencia en Categoría", "Adicional por Tarea Asistencial", "Suplemento por Riesgo")
      
      other_concepts_df <- filtered_data %>%
        filter(!Concepto %in% excluded_concepts) %>%
        distinct(`Salario bruto`, Concepto)
      
      all_positive_concepts_df <- bind_rows(all_positive_concepts_df, other_concepts_df)
      
      current_html_output <- ""
      salario_bruto_calculado <- 0
      
      desired_order_concepts <- c(
        "Basico",
        "Adicional Antiguedad",
        "Adicional Formacion Academica",
        "Adicional por Permanencia en Categoría",
        "Adicional por Tarea Asistencial",
        "Suplemento por Riesgo"
      )
      
      for (concept_name in desired_order_concepts) {
        if (concept_name %in% all_positive_concepts_df$Concepto) {
          monto <- all_positive_concepts_df %>% filter(Concepto == concept_name) %>% pull(`Salario bruto`)
          salario_bruto_calculado <- salario_bruto_calculado + monto
          monto_formateado <- format(monto, big.mark = ".", decimal.mark = ",")
          current_html_output <- paste0(current_html_output, "<p><strong>", concept_name, ":</strong> $", monto_formateado, "</p>")
        }
      }
      
      remaining_concepts <- all_positive_concepts_df %>%
        filter(!Concepto %in% desired_order_concepts)
      
      if (nrow(remaining_concepts) > 0) {
        remaining_concepts_ordered <- remaining_concepts %>%
          arrange(Concepto)
        for (i in 1:nrow(remaining_concepts_ordered)) {
          monto <- remaining_concepts_ordered$`Salario bruto`[i]
          concepto_name <- remaining_concepts_ordered$Concepto[i]
          salario_bruto_calculado <- salario_bruto_calculado + monto
          monto_formateado <- format(monto, big.mark = ".", decimal.mark = ",")
          current_html_output <- paste0(current_html_output, "<p><strong>", concepto_name, ":</strong> $", monto_formateado, "</p>")
        }
      }
      
      salario_bruto_formateado <- format(salario_bruto_calculado, big.mark = ".", decimal.mark = ",")
      current_html_output <- paste0(current_html_output, "<p class='total-line'><strong>Salario Bruto:</strong> $", salario_bruto_formateado, "</p>")
      
      salario_neto_calculado <- salario_bruto_calculado
      
      retenciones_specs <- NULL
      
      if (input$escalafon == "Docente") {
        retenciones_specs <- list(
          list(concepto = "Fondo Adicional Univ.", porcentaje = 0.02),
          list(concepto = "Jubilación (régimen especial)", porcentaje = 0.11),
          list(concepto = "Caja complementaria", porcentaje = 0.045),
          list(concepto = "Ley 19032", porcentaje = 0.03),
          list(concepto = "Obra social (DASPU)", porcentaje = 0.03)
        )
      } else if (input$escalafon == "No Docente" || input$escalafon == "Autoridades") {
        retenciones_specs <- list(
          list(concepto = "Fondo Adicional Univ.", porcentaje = 0.02),
          list(concepto = "Jubilación", porcentaje = 0.11),
          list(concepto = "Caja complementaria", porcentaje = 0.045),
          list(concepto = "Ley 19032", porcentaje = 0.03),
          list(concepto = "Obra social (DASPU)", porcentaje = 0.03)
        )
      }
      
      
      if (!is.null(retenciones_specs)) {
        current_html_output <- paste0(current_html_output, "<div class='section-divider-blue'></div>")
        current_html_output <- paste0(current_html_output, "<p class='section-title'>Retenciones:</p>")
        
        for (retencion in retenciones_specs) {
          monto_retencion <- salario_bruto_calculado * retencion$porcentaje
          salario_neto_calculado <- salario_neto_calculado - monto_retencion
          monto_formateado <- format(monto_retencion, big.mark = ".", decimal.mark = ",")
          current_html_output <- paste0(current_html_output, "<p><strong>", retencion$concepto, ":</strong> $", monto_formateado, "</p>")
        }
      }
      
      salario_neto_formateado <- format(salario_neto_calculado, big.mark = ".", decimal.mark = ",")
      current_html_output <- paste0(current_html_output, "<p class='net-salary-line'><strong>Salario Neto (de bolsillo):</strong> $", salario_neto_formateado, "</p>")
      
      HTML(current_html_output)
      
    } else {
      HTML("<p>No se encontraron datos para la combinación seleccionada.</p>")
    }
  })
  
  output$info_actualizacion <- renderText({
    first_periodo_id <- data$periodo_id[1]
    
    if (!is.null(first_periodo_id) && !is.na(first_periodo_id) && nchar(first_periodo_id) == 6) {
      year <- substr(first_periodo_id, 1, 4)
      month_num <- as.integer(substr(first_periodo_id, 5, 6))
      
      month_names <- c(
        "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
      )
      month_name <- month_names[month_num]
      
      paste0("Información actualizada a ", month_name, " de ", year, ".")
    } else {
      "Información de fecha no disponible."
    }
  })
}

shinyApp(ui, server)

