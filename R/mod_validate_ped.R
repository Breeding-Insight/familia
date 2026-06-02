#' Validate Pedigree module UI
#'
#' @param id Module id
#'
#' @noRd
mod_validate_ped_ui <- function(id) {
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
            "Upload a pedigree file and a genotypes file to validate trios using Mendelian error analysis.",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shiny::fileInput(ns("pedigree_file"),  "Pedigree File",  accept = c(".txt", ".tsv", ".csv")),
          shiny::fileInput(ns("genotypes_file"), "Genotypes File", accept = c(".txt", ".tsv", ".csv")),
          shiny::fileInput(ns("founders_file"),  "Founders File",    accept = c(".txt")),
          shiny::hr(),
          shiny::p("Parameters:", style = "color: #6c757d; font-size: 12px; margin-bottom: 5px;"),
          shiny::numericInput(ns("trio_error_threshold"),
                              "Trio Error Threshold (%)",
                              value = 5.0, min = 0, max = 100, step = 0.1),
          shiny::numericInput(ns("single_parent_error_threshold"),
                              "Single Parent Error Threshold (%)",
                              value = 2.0, min = 0, max = 100, step = 0.1),
          shiny::numericInput(ns("min_markers"),
                              "Min Markers",
                              value = 10, min = 1, step = 1),
          shiny::hr(),
          shiny::actionButton(ns("run_validate"), "Run Validation"),
          shiny::hr(),
          shinyjs::disabled(
            shiny::downloadButton(ns("download_validate_all"), "Download Results")
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
          title       = "Validation Results",
          status      = "info",
          solidHeader = FALSE,
          width       = 12,
          height      = 650,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id   = ns("validate_results_tabs"),
            type = "tabs",
            shiny::tabPanel(
              "Instructions",
              shiny::fluidRow(
                shiny::column(12, shiny::wellPanel(shiny::HTML('
                  <ul>
                    <li>Upload a pedigree file with columns: <code>id</code>, <code>male_parent</code>, <code>female_parent</code>.</li>
                    <li>Upload a genotypes file with an <code>id</code> column followed by marker columns coded as 0, 1, 2.</li>
                    <li>Optionally upload a founders file (single column of founder IDs) to preserve founder trios.</li>
                    <li>Set error thresholds and minimum markers, then click <strong>Run Validation</strong>.</li>
                    <li>Results are split into:</li>
                    <ul>
                      <li><strong>Pass</strong> — trios within the Mendelian error threshold.</li>
                      <li><strong>Fail</strong> — trios exceeding the threshold.</li>
                      <li><strong>Low Markers</strong> — trios with insufficient markers.</li>
                      <li><strong>No Genotype Data</strong> — trios absent from the genotype file.</li>
                      <li><strong>Founders</strong> — trios identified as founders.</li>
                      <li><strong>Missing Parents</strong> — trios with one or both parents coded as 0.</li>
                    </ul>
                    <li>Review the Issue Tables tab, then export results as a .zip file.</li>
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
            id          = ns("pb_validate"),
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
              shiny::downloadButton(ns("download_validate_plot"), "Save Image"),
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

#' Validate Pedigree module server
#'
#' @param id Module id
#' @param parent_session Parent (app) session
#'
#' @noRd
mod_validate_ped_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    
    validate_results <- shiny::reactiveVal(NULL)
    
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
          title     = shiny::tagList(shiny::icon("circle-question"), " Validate Pedigree — Help"),
          size      = "l",
          easyClose = TRUE,
          footer    = shiny::modalButton("Close"),
          help_content_validate_ped(collapse_fn = make_collapse_panel, id_prefix = "modal")
        )
      )
    })
    
    # Run validation
    shiny::observeEvent(input$run_validate, {
      shiny::req(input$pedigree_file, input$genotypes_file)
      validate_results(NULL)
      shinyjs::disable("download_validate_all")
      
      tryCatch({
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 10, status = "info",
          title = "Reading input files..."
        )
        
        ped_ext  <- tolower(tools::file_ext(input$pedigree_file$name))
        geno_ext <- tolower(tools::file_ext(input$genotypes_file$name))
        
        ped_raw <- if (ped_ext == "csv") {
          utils::read.csv(input$pedigree_file$datapath,  header = TRUE,
                          stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          utils::read.table(input$pedigree_file$datapath, header = TRUE, sep = "\t",
                            stringsAsFactors = FALSE, check.names = FALSE)
        }
        
        geno_raw <- if (geno_ext == "csv") {
          utils::read.csv(input$genotypes_file$datapath, header = TRUE,
                          stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          utils::read.table(input$genotypes_file$datapath, header = TRUE, sep = "\t",
                            stringsAsFactors = FALSE, check.names = FALSE)
        }
        
        founders_path <- if (!is.null(input$founders_file)) input$founders_file$datapath else NULL
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 35, status = "info",
          title = "Evaluating trios..."
        )
        
        report <- BIGr::validate_pedigree(
          pedigree_file                 = ped_raw,
          genotypes_file                = geno_raw,
          founders_file                 = founders_path,
          trio_error_threshold          = as.numeric(input$trio_error_threshold),
          min_markers                   = as.integer(input$min_markers),
          single_parent_error_threshold = as.numeric(input$single_parent_error_threshold),
          verbose                       = FALSE,
          plot_results                  = TRUE
        )
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 85, status = "info",
          title = "Compiling results..."
        )
        
        validate_results(report)
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 100, status = "success",
          title = "Finished"
        )
        shinyjs::enable("download_validate_all")
        shiny::updateTabsetPanel(session, "validate_results_tabs", selected = "Issue Tables")
        
      }, error = function(e) {
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 100, status = "danger",
          title = paste0("Failed: ", e$message)
        )
      })
    })
    
    # Summary banner
    output$summary_banner <- shiny::renderUI({
      shiny::req(validate_results())
      report <- validate_results()
      
      get_count <- function(df) if (is.null(df) || !is.data.frame(df) && !data.table::is.data.table(df)) 0L else nrow(df)
      n_pass     <- get_count(report$pass)
      n_fail     <- get_count(report$fail)
      n_low      <- get_count(report$low_markers)
      n_no_geno  <- get_count(report$no_genotype_data)
      n_founders <- get_count(report$founders)
      n_missing  <- get_count(report$missing_parents)
      total      <- get_count(report$full_results)
      
      banner_color <- if (n_fail == 0 && n_low == 0) "#d4edda" else "#fff3cd"
      border_color <- if (n_fail == 0 && n_low == 0) "#c3e6cb" else "#ffeeba"
      text_color   <- if (n_fail == 0 && n_low == 0) "#155724"  else "#856404"
      headline     <- if (n_fail == 0 && n_low == 0) "All trios passed validation!" else
        paste0(n_fail + n_low, " trio(s) flagged. Review the Issue Tables tab.")
      
      shiny::HTML(paste0(
        "<div style='background-color:", banner_color, "; border: 1px solid ", border_color,
        "; padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
        "<p style='color:", text_color, "; margin: 0; font-weight: bold; font-size: 14px;'>",
        headline, "</p>",
        "<p style='color:", text_color, "; margin: 6px 0 0 0; font-size: 12px;'>",
        "- Total trios: <strong>", total, "</strong> &nbsp;",
        "- Pass: <strong>", n_pass, "</strong> &nbsp;",
        "- Fail: <strong>", n_fail, "</strong> &nbsp;",
        "- Low markers: <strong>", n_low, "</strong> &nbsp;",
        "- No genotype data: <strong>", n_no_geno, "</strong> &nbsp;",
        "- Founders: <strong>", n_founders, "</strong> &nbsp;",
        "- Missing parents: <strong>", n_missing, "</strong>",
        "</p></div>"
      ))
    })
    
    # Results UI
    output$results_ui <- shiny::renderUI({
      shiny::req(validate_results())
      report <- validate_results()
      
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
      
      render_if("Pass",              report$pass)
      render_if("Fail",              report$fail)
      render_if("Low Markers",       report$low_markers)
      render_if("No Genotype Data",  report$no_genotype_data)
      render_if("Founders",          report$founders)
      render_if("Missing Parents",   report$missing_parents)
      
      shiny::tagList(
        shiny::h5(
          shiny::tagList(shiny::icon("list-check"), " Validation Results"),
          style = "font-weight: bold; margin-bottom: 10px;"
        ),
        make_section("Pass",             "circle-check", report$pass,             "#28a745"),
        make_section("Fail",             "xmark",        report$fail,             "#721c24"),
        make_section("Low Markers",      "triangle-exclamation", report$low_markers, "#856404"),
        make_section("No Genotype Data", "database",     report$no_genotype_data, "#6c757d"),
        make_section("Founders",         "seedling",     report$founders,         "#0c5460"),
        make_section("Missing Parents",  "user-slash",   report$missing_parents,  "#856404")
      )
    })
    
    # Mendelian error plot
    output$error_plot <- shiny::renderPlot({
      shiny::req(validate_results())
      report <- validate_results()
      shiny::validate(shiny::need(!is.null(report$plot), "Run validation to generate the plot."))
      print(report$plot)
    })
    
    # Figure download
    output$download_validate_plot <- shiny::downloadHandler(
      filename = function() {
        ext <- input$plot_image_type %||% "jpeg"
        paste0("validate_pedigree_plot_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        shiny::req(validate_results())
        p      <- validate_results()$plot
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
    output$download_validate_all <- shiny::downloadHandler(
      filename = function() {
        paste0("validate_pedigree_results_", Sys.Date(), ".zip")
      },
      content = function(file) {
        shiny::req(validate_results())
        report  <- validate_results()
        tmp_dir <- tempfile("validate_export")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        # Corrected pedigree
        if (!is.null(report$corrected_pedigree) && nrow(report$corrected_pedigree) > 0) {
          write.table(as.data.frame(report$corrected_pedigree),
                      file.path(tmp_dir, "corrected_pedigree.txt"),
                      sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        # Full results
        if (!is.null(report$full_results) && nrow(report$full_results) > 0) {
          write.table(as.data.frame(report$full_results),
                      file.path(tmp_dir, "full_results.txt"),
                      sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        # Per-status tables
        sections <- list(
          pass             = report$pass,
          fail             = report$fail,
          low_markers      = report$low_markers,
          no_genotype_data = report$no_genotype_data,
          founders         = report$founders,
          missing_parents  = report$missing_parents
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
# mod_validate_ped_ui("validate_ped_1")

## To be copied in the server
# mod_validate_ped_server("validate_ped_1")