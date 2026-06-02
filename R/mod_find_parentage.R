#' Find Parentage module UI
#'
#' @param id Module id
#'
#' @noRd
mod_find_parentage_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      
      # Column 1: Inputs
      shiny::column(
        width = 3,
        bs4Dash::box(
          title       = "Inputs",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = TRUE,
          shiny::p(
            "Upload genotype, parents, and progeny files to assign parentage.",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shiny::fileInput(ns("genotypes_file"), "Genotypes File", accept = c(".txt", ".tsv", ".csv")),
          shiny::fileInput(ns("parents_file"),   "Parents File",   accept = c(".txt", ".tsv", ".csv")),
          shiny::fileInput(ns("progeny_file"),   "Progeny File",   accept = c(".txt", ".tsv", ".csv")),
          shiny::hr(),
          shiny::p("Parameters:", style = "color: #6c757d; font-size: 12px; margin-bottom: 5px;"),
          shiny::selectInput(
            ns("method"), "Method",
            choices = c(
              "Best pair"    = "best_pair",
              "Best male parent"         = "best_male_parent",
              "Best female parent"       = "best_female_parent",
              "Best match"  = "best_match"
            ),
            selected = "best_pair"
          ),
          shiny::numericInput(ns("error_threshold"), "Error Threshold (%)",   value = 5.0,  min = 0, max = 100, step = 0.1),
          shiny::numericInput(ns("min_markers"),     "Min Markers",           value = 10,   min = 1, step = 1),
          shiny::checkboxInput(ns("show_ties"),            "Show ties",              value = TRUE),
          shiny::checkboxInput(ns("allow_parent_selfing"), "Allow parent selfing",   value = FALSE),
          shiny::checkboxInput(ns("exclude_self_match"),   "Exclude self-match",     value = TRUE),
          shiny::hr(),
          shiny::actionButton(ns("run_parentage"), "Run Parentage Assignment"),
          shiny::hr(),
          shinyjs::disabled(
            shiny::downloadButton(ns("download_parentage_all"), "Download Results")
          ),
          shiny::hr(),
          shiny::div(
            style = "text-align: center; margin-top: 5px;",
            shiny::actionButton(
              ns("help_btn"),
              shiny::tagList(shiny::icon("circle-question"), "Help"),
              style = "background-color: #FFD700; color: #000000; border: none; padding: 8px 16px; border-radius: 5px;"
            )
          )
        )
      ),
      
      # Column 2: Results
      shiny::column(
        width = 6,
        bs4Dash::box(
          title       = "Parentage Assignment Results",
          status      = "info",
          solidHeader = FALSE,
          width       = 12,
          height      = 650,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id   = ns("parentage_results_tabs"),
            type = "tabs",
            shiny::tabPanel(
              "Instructions",
              shiny::fluidRow(
                shiny::column(12, shiny::wellPanel(shiny::HTML('
                  <ul>
                    <li>Upload a genotypes file with an <code>id</code> column followed by marker columns coded as 0, 1, 2.</li>
                    <li>Upload a parents file with an <code>id</code> column and an optional <code>sex</code> column (<code>M</code>, <code>F</code>, or <code>A</code>).</li>
                    <li>Upload a progeny file with an <code>id</code> column.</li>
                    <li>Select a method and set parameters, then click <strong>Run Parentage Assignment</strong>.</li>
                    <li>Results are split into:</li>
                    <ul>
                      <li><strong>Pass</strong> — progeny with a confident assignment below the error threshold.</li>
                      <li><strong>High Error</strong> — progeny exceeding the error threshold.</li>
                      <li><strong>Low Markers</strong> — progeny with insufficient markers.</li>
                    </ul>
                    <li>Review results in the Issue Tables tab, then export as a .zip file.</li>
                  </ul>
                ')))
              ),
              style = "overflow-y: auto; height: 550px"
            ),
            shiny::tabPanel(
              "Issue Tables",
              shiny::uiOutput(ns("results_ui")),
              style = "overflow-y: auto; height: 550px; padding: 10px;"
            ),
            shiny::tabPanel(
              "Mendelian Error Plot",
              shiny::plotOutput(ns("error_plot"), height = "450px"),
              style = "overflow-y: auto; height: 550px; padding: 10px;"
            )
          )
        )
      ),
      
      # Column 3: Status + Summary
      shiny::column(
        width = 3,
        bs4Dash::box(
          title       = "Status",
          width       = 12,
          collapsible = TRUE,
          status      = "info",
          shinyWidgets::progressBar(
            id          = ns("pb_parentage"),
            value       = 0,
            status      = "info",
            display_pct = TRUE,
            striped     = TRUE,
            title       = " "
          )
        ),
        bs4Dash::box(
          title       = "Run Summary",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = TRUE,
          shiny::uiOutput(ns("summary_banner"))
        ),
        bs4Dash::box(
          title       = "Plot Controls",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = TRUE,
          shiny::div(
            style = "display:inline-block; float:left",
            shinyWidgets::dropdownButton(
              shiny::tags$h3("Save Image"),
              shiny::selectInput(ns("plot_image_type"), "File Type",
                                 choices = c("jpeg", "tiff", "png", "svg"), selected = "jpeg"),
              shiny::sliderInput(ns("plot_image_res"),    "Resolution", value = 300, min = 50,  max = 1000, step = 50),
              shiny::sliderInput(ns("plot_image_width"),  "Width",      value = 8,   min = 1,   max = 20,   step = 0.5),
              shiny::sliderInput(ns("plot_image_height"), "Height",     value = 5,   min = 1,   max = 20,   step = 0.5),
              shiny::downloadButton(ns("download_parentage_plot"), "Save Image"),
              circle  = FALSE,
              status  = "danger",
              icon    = shiny::icon("floppy-disk"),
              width   = "300px",
              label   = "Save",
              tooltip = shinyWidgets::tooltipOptions(title = "Click to see inputs!")
            )
          )
        )
      )
    )
  )
}

#' Find Parentage module server
#'
#' @param id Module id
#' @param parent_session Parent (app) session
#'
#' @noRd
mod_find_parentage_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    
    `%||%` <- function(x, y) if (is.null(x)) y else x
    parentage_results <- shiny::reactiveVal(NULL)
    
    make_collapse_panel <- function(panel_id, icon_name, label, body_content) {
      shiny::tags$div(
        class = "card mb-1",
        style = "border: 1px solid #dee2e6; border-radius: 4px;",
        shiny::tags$div(
          class = "card-header p-0",
          style = "background-color: #f8f9fa;",
          shiny::tags$button(
            class           = "btn btn-link btn-sm w-100 text-left d-flex align-items-center",
            style           = "color: #343a40; text-decoration: none; font-size: 13px; padding: 8px 12px; gap: 6px;",
            `data-toggle`   = "collapse",
            `data-target`   = paste0("#", panel_id),
            `aria-expanded` = "false",
            shiny::icon(icon_name),
            shiny::tags$span(label)
          )
        ),
        shiny::tags$div(
          id    = panel_id,
          class = "collapse",
          shiny::tags$div(
            class = "card-body",
            style = "padding: 12px 14px; font-size: 13px;",
            body_content
          )
        )
      )
    }
    
    sanitize_id <- function(x) gsub("[^A-Za-z0-9]", "_", tolower(x))
    
    # Help button
    shiny::observeEvent(input$help_btn, {
      shiny::showModal(
        shiny::modalDialog(
          title     = shiny::tagList(shiny::icon("circle-question"), " Find Parentage — Help"),
          size      = "l",
          easyClose = TRUE,
          footer    = shiny::modalButton("Close"),
          help_content_find_parentage(collapse_fn = make_collapse_panel, id_prefix = "modal")
        )
      )
    })
    
    # Run parentage assignment
    shiny::observeEvent(input$run_parentage, {
      shiny::req(input$genotypes_file, input$parents_file, input$progeny_file)
      parentage_results(NULL)
      shinyjs::disable("download_parentage_all")
      
      tryCatch({
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_parentage",
          value = 10, status = "info",
          title = "Reading input files..."
        )
        
        read_flex_ui <- function(file_input) {
          ext <- tolower(tools::file_ext(file_input$name))
          if (ext == "csv") {
            utils::read.csv(file_input$datapath, header = TRUE,
                            stringsAsFactors = FALSE, check.names = FALSE)
          } else {
            utils::read.table(file_input$datapath, header = TRUE, sep = "\t",
                              stringsAsFactors = FALSE, check.names = FALSE,
                              comment.char = "", quote = "")
          }
        }
        
        geno_raw    <- read_flex_ui(input$genotypes_file)
        parents_raw <- read_flex_ui(input$parents_file)
        progeny_raw <- read_flex_ui(input$progeny_file)
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_parentage",
          value = 35, status = "info",
          title = "Running parentage assignment..."
        )
        
        report <- BIGr::find_parentage(
          genotypes_file       = geno_raw,
          parents_file         = parents_raw,
          progeny_file         = progeny_raw,
          method               = input$method,
          min_markers          = as.integer(input$min_markers),
          error_threshold      = as.numeric(input$error_threshold),
          show_ties            = input$show_ties,
          allow_parent_selfing = input$allow_parent_selfing,
          exclude_self_match   = input$exclude_self_match,
          verbose              = FALSE,
          plot_results         = TRUE
        )
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_parentage",
          value = 85, status = "info",
          title = "Compiling results..."
        )
        
        parentage_results(report)
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_parentage",
          value = 100, status = "success",
          title = "Finished"
        )
        shinyjs::enable("download_parentage_all")
        shiny::updateTabsetPanel(session, "parentage_results_tabs", selected = "Issue Tables")
        
      }, error = function(e) {
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_parentage",
          value = 100, status = "danger",
          title = paste0("Failed: ", e$message)
        )
      })
    })
    
    # Summary banner
    output$summary_banner <- shiny::renderUI({
      shiny::req(parentage_results())
      report <- parentage_results()
      
      get_count <- function(df) if (is.null(df) || !is.data.frame(df) && !data.table::is.data.table(df)) 0L else nrow(df)
      n_pass   <- get_count(report$pass)
      n_high   <- get_count(report$high_error)
      n_low    <- get_count(report$low_markers)
      total    <- get_count(report$full_results)
      
      banner_color <- if (n_high == 0 && n_low == 0) "#d4edda" else "#fff3cd"
      border_color <- if (n_high == 0 && n_low == 0) "#c3e6cb" else "#ffeeba"
      text_color   <- if (n_high == 0 && n_low == 0) "#155724"  else "#856404"
      headline     <- if (n_high == 0 && n_low == 0) "All progeny assigned confidently!" else
        paste0(n_high + n_low, " progeny flagged. Review the Issue Tables tab.")
      
      shiny::HTML(paste0(
        "<div style='background-color:", banner_color, "; border: 1px solid ", border_color,
        "; padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
        "<p style='color:", text_color, "; margin: 0; font-weight: bold; font-size: 14px;'>",
        headline, "</p>",
        "<p style='color:", text_color, "; margin: 6px 0 0 0; font-size: 12px;'>",
        "- Total progeny: <strong>", total, "</strong> &nbsp;",
        "- Pass: <strong>", n_pass, "</strong> &nbsp;",
        "- High error: <strong>", n_high, "</strong> &nbsp;",
        "- Low markers: <strong>", n_low, "</strong>",
        "</p></div>"
      ))
    })
    
    # Results UI
    output$results_ui <- shiny::renderUI({
      shiny::req(parentage_results())
      report <- parentage_results()
      
      get_count <- function(df) if (is.null(df) || !is.data.frame(df) && !data.table::is.data.table(df)) 0L else nrow(df)
      
      make_section <- function(title, icon_name, df, color_hex) {
        n      <- get_count(df)
        tbl_id <- paste0("tbl_", sanitize_id(title))
        bs4Dash::box(
          title = shiny::tagList(
            shiny::icon(icon_name),
            shiny::strong(paste0(" ", title, ": ")),
            shiny::span(
              if (n == 0) "None found" else paste0(n, " record(s) found"),
              style = paste0("color: ", if (n == 0) "#28a745" else color_hex, ";")
            )
          ),
          width       = 12,
          collapsible = TRUE,
          collapsed   = (n == 0),
          status      = if (n == 0) "success" else "warning",
          style       = paste0("border-left: 4px solid ", color_hex, ";"),
          if (n > 0) {
            DT::DTOutput(session$ns(tbl_id))
          } else {
            shiny::p("No records found.", style = "color: #28a745; margin: 0;")
          }
        )
      }
      
      render_if <- function(title, df) {
        output_id <- paste0("tbl_", sanitize_id(title))
        if (!is.null(df) && (is.data.frame(df) || data.table::is.data.table(df)) && nrow(df) > 0) {
          output[[output_id]] <- DT::renderDT({
            DT::datatable(as.data.frame(df),
                          rownames = FALSE,
                          options  = list(pageLength = 10, scrollX = TRUE),
                          class    = "table-bordered table-sm")
          })
        }
      }
      
      render_if("Pass",        report$pass)
      render_if("High Error",  report$high_error)
      render_if("Low Markers", report$low_markers)
      
      shiny::tagList(
        shiny::h5(
          shiny::tagList(shiny::icon("list-check"), " Parentage Assignment Results"),
          style = "font-weight: bold; margin-bottom: 10px;"
        ),
        make_section("Pass",        "circle-check",         report$pass,        "#28a745"),
        make_section("High Error",  "triangle-exclamation", report$high_error,  "#721c24"),
        make_section("Low Markers", "exclamation",          report$low_markers, "#856404")
      )
    })
    
    # Mendelian error plot
    output$error_plot <- shiny::renderPlot({
      shiny::req(parentage_results())
      report <- parentage_results()
      shiny::validate(shiny::need(!is.null(report$plot), "Run parentage assignment to generate the plot."))
      print(report$plot)
    })
    
    # Figure download
    output$download_parentage_plot <- shiny::downloadHandler(
      filename = function() {
        ext <- input$plot_image_type %||% "jpeg"
        paste0("find_parentage_plot_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        shiny::req(parentage_results())
        p      <- parentage_results()$plot
        shiny::validate(shiny::need(!is.null(p), "No plot available."))
        ext    <- input$plot_image_type   %||% "jpeg"
        width  <- as.numeric(input$plot_image_width  %||% 8)
        height <- as.numeric(input$plot_image_height %||% 5)
        dpi    <- as.numeric(input$plot_image_res    %||% 300)
        if (ext %in% c("png", "jpeg", "tiff")) {
          ggplot2::ggsave(filename = file, plot = p, width = width, height = height, units = "in", dpi = dpi)
        } else {
          ggplot2::ggsave(filename = file, plot = p, width = width, height = height, units = "in")
        }
      }
    )
    
    # Unified data download
    output$download_parentage_all <- shiny::downloadHandler(
      filename = function() {
        paste0("find_parentage_results_", Sys.Date(), ".zip")
      },
      content = function(file) {
        shiny::req(parentage_results())
        report  <- parentage_results()
        tmp_dir <- tempfile("parentage_export")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        # Full results
        if (!is.null(report$full_results) && nrow(report$full_results) > 0) {
          write.table(as.data.frame(report$full_results),
                      file.path(tmp_dir, "full_results.txt"),
                      sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        # Per-status tables
        sections <- list(
          pass        = report$pass,
          high_error  = report$high_error,
          low_markers = report$low_markers
        )
        for (nm in names(sections)) {
          df <- sections[[nm]]
          if (!is.null(df) && nrow(df) > 0) {
            write.table(as.data.frame(df),
                        file.path(tmp_dir, paste0(nm, ".txt")),
                        sep = "\t", row.names = FALSE, quote = FALSE)
          }
        }
        
        zip_files <- list.files(tmp_dir)
        zip::zip(zipfile = file, files = zip_files, root = tmp_dir)
        unlink(tmp_dir, recursive = TRUE)
      },
      contentType = "application/zip"
    )
    
  })
}

## To be copied in the UI
# mod_find_parentage_ui("find_parentage_1")

## To be copied in the server
# mod_find_parentage_server("find_parentage_1")