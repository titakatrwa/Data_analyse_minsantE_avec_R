suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(plotly)
  library(DT)
  library(scales)
})

if (!file.exists("outputs/clean_health_data.csv")) {
  source("01_nettoyage_analyse.R", local = TRUE)
}

health <- readr::read_csv(
  "outputs/clean_health_data.csv",
  show_col_types = FALSE,
  col_types = cols(consultation_date = col_date(), consultation_month = col_date())
)

ui <- page_sidebar(
  title = div(
    class = "brand-title",
    tags$img(src = "logo-sante-sud-1.svg", class = "brand-logo"),
    div("Santé Cameroun", tags$small("Pilotage des consultations"))
  ),
  theme = bs_theme(
    version = 5,
    bg = "#F5F8FA",
    fg = "#17324D",
    primary = "#007C91",
    secondary = "#E16B4B",
    base_font = font_google("Source Sans 3"),
    heading_font = font_google("Source Sans 3")
  ),
  tags$head(tags$style(HTML("
    .brand-title {display:flex; align-items:center; gap:.8rem;}
    .brand-logo {height:42px; max-width:130px;}
    .brand-title small {display:block; color:#64748b; font-size:.78rem;}
    .card {border:0; box-shadow:0 5px 20px rgba(20,59,93,.08);}
    .metric-value {font-size:2rem; font-weight:750; color:#143B5D;}
    .metric-label {color:#64748b; font-size:.9rem;}
  "))),
  sidebar = sidebar(
    width = 285,
    dateRangeInput(
      "dates",
      "Période",
      start = min(health$consultation_date, na.rm = TRUE),
      end = max(health$consultation_date, na.rm = TRUE),
      min = min(health$consultation_date, na.rm = TRUE),
      max = max(health$consultation_date, na.rm = TRUE)
    ),
    selectInput(
      "regions",
      "Région",
      choices = sort(unique(health$region)),
      multiple = TRUE
    ),
    selectInput(
      "diagnoses",
      "Diagnostic",
      choices = sort(unique(health$diagnosis)),
      multiple = TRUE
    ),
    checkboxInput("valid_dates", "Exclure les dates invalides", TRUE),
    hr(),
    downloadButton("download", "Télécharger la sélection", class = "btn-primary w-100")
  ),
  layout_columns(
    value_box(
      title = "Consultations",
      value = textOutput("n_consultations", inline = TRUE),
      showcase = icon("user-doctor"),
      theme = "primary"
    ),
    value_box(
      title = "Ruptures de stock",
      value = textOutput("stockout_rate", inline = TRUE),
      showcase = icon("pills"),
      theme = "warning"
    ),
    value_box(
      title = "Non assurés",
      value = textOutput("uninsured_rate", inline = TRUE),
      showcase = icon("shield"),
      theme = "danger"
    ),
    value_box(
      title = "Coût médian",
      value = textOutput("median_cost", inline = TRUE),
      showcase = icon("coins"),
      theme = "success"
    ),
    col_widths = c(3, 3, 3, 3)
  ),
  navset_card_tab(
    nav_panel(
      "Vue d’ensemble",
      layout_columns(
        card(
          card_header("Consultations par mois"),
          plotlyOutput("monthly_plot", height = 340)
        ),
        card(
          card_header("Diagnostics principaux"),
          plotlyOutput("diagnosis_plot", height = 340)
        ),
        col_widths = c(7, 5)
      ),
      card(
        card_header("Ruptures de médicaments par région"),
        plotlyOutput("stockout_plot", height = 380)
      )
    ),
    nav_panel(
      "Données",
      card(
        card_header("Consultations filtrées"),
        DTOutput("table")
      )
    ),
    nav_panel(
      "Qualité",
      card(
        card_header("Audit de qualité"),
        DTOutput("audit_table")
      )
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    d <- health
    if (isTRUE(input$valid_dates)) d <- d |> filter(!is.na(consultation_date))
    if (length(input$dates) == 2) {
      d <- d |> filter(
        is.na(consultation_date) |
          between(consultation_date, input$dates[1], input$dates[2])
      )
    }
    if (length(input$regions)) d <- d |> filter(region %in% input$regions)
    if (length(input$diagnoses)) d <- d |> filter(diagnosis %in% input$diagnoses)
    d
  })

  output$n_consultations <- renderText(comma(nrow(filtered()), big.mark = " "))
  output$stockout_rate <- renderText(percent(mean(filtered()$medication_stockout), accuracy = 0.1))
  output$uninsured_rate <- renderText({
    known <- filtered() |> filter(insurance_status %in% c("Insured", "Uninsured"))
    if (nrow(known) == 0) return("—")
    percent(mean(known$insurance_status == "Uninsured"), accuracy = 0.1)
  })
  output$median_cost <- renderText({
    value <- median(filtered()$treatment_cost, na.rm = TRUE)
    if (is.nan(value)) "—" else number(value, accuracy = 0.01)
  })

  output$monthly_plot <- renderPlotly({
    p <- filtered() |>
      filter(!is.na(consultation_month)) |>
      count(consultation_month) |>
      ggplot(aes(consultation_month, n, text = paste("Consultations :", n))) +
      geom_area(fill = "#007C91", alpha = .16) +
      geom_line(colour = "#007C91", linewidth = 1.1) +
      geom_point(colour = "#007C91", size = 2.4) +
      scale_x_date(date_labels = "%b %Y") +
      labs(x = NULL, y = "Consultations") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
  })

  output$diagnosis_plot <- renderPlotly({
    p <- filtered() |>
      filter(diagnosis != "Non renseigné") |>
      count(diagnosis, sort = TRUE) |>
      slice_head(n = 8) |>
      mutate(diagnosis = fct_reorder(diagnosis, n)) |>
      ggplot(aes(n, diagnosis, text = paste(diagnosis, ":", n))) +
      geom_col(fill = "#E16B4B", width = .7) +
      labs(x = "Consultations", y = NULL) +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
  })

  output$stockout_plot <- renderPlotly({
    p <- filtered() |>
      group_by(region) |>
      summarise(taux = mean(medication_stockout), .groups = "drop") |>
      mutate(region = fct_reorder(region, taux)) |>
      ggplot(aes(taux, region, text = paste(region, percent(taux, accuracy = .1)))) +
      geom_col(fill = "#D7A431", width = .7) +
      scale_x_continuous(labels = percent) +
      labs(x = "Taux de rupture", y = NULL) +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
  })

  output$table <- renderDT({
    filtered() |>
      select(
        consultation_date, region, district, facility_name, patient_age,
        gender, diagnosis, treatment_cost, medication_available,
        consultation_type, insurance_status
      ) |>
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 12, scrollX = TRUE)
      )
  })

  output$audit_table <- renderDT({
    readr::read_csv("outputs/audit_qualite.csv", show_col_types = FALSE) |>
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })

  output$download <- downloadHandler(
    filename = function() paste0("consultations_filtrees_", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered(), file, na = "")
  )
}

shinyApp(ui, server)

