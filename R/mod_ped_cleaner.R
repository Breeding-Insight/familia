
#' Pedigree Cleaner module UI
#'
#' @param id Module id
#'
#' @noRd
mod_ped_cleaner_ui <- function(id) {
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
            "Upload a pedigree file (.txt, .tsv, or .csv) with columns: id, male_parent, female_parent.",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shiny::fileInput(
            ns("ped_file"),
            "Upload Pedigree File",
            accept = c(".txt", ".tsv", ".csv")
          ),
          shiny::actionButton(
            ns("run_check"),
            "Run Pedigree Check",
            style = "width: 100%; background-color: #28a745; color: white; border: none; padding: 10px; border-radius: 5px;"
          ),
          shiny::hr(),
          shinyjs::disabled(
            shiny::downloadButton(
              ns("download_results"),
              "Export Corrected Pedigree + Report",
              style = "width: 100%; background-color: #28a745; color: white; border: none; padding: 10px; border-radius: 5px;"
            )
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
        )  # closes box
      ),  # closes column(width = 3)
      
      # Column 2: Results 
      shiny::column(
        width = 6,
        bs4Dash::box(
          title       = "Pedigree Check Results",
          status      = "info",
          solidHeader = FALSE,
          width       = 12,
          height      = 650,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id   = ns("ped_results_tabs"),
            type = "tabs",
            shiny::tabPanel(
              "Instructions",
              shiny::fluidRow(
                shiny::column(12, shiny::wellPanel(shiny::HTML('
                  <ul>
                    <li>Upload a tab-separated <code>.txt</code> or <code>.tsv</code> pedigree file, or a comma-separated <code>.csv</code> pedigree file.</li>
                    <li>Required columns: <code>id</code>, <code>male_parent</code>, <code>female_parent</code>.</li>
                    <li>Click <strong>Run Pedigree Check</strong> to detect issues.</li>
                    <li>Issues detected:</li>
                    <ul>
                      <li><strong>Exact Duplicates</strong> — fully identical rows are removed.</li>
                      <li><strong>Conflicting IDs</strong> — same ID with different parents; ambiguous parent set to 0.</li>
                      <li><strong>Inconsistent Sex Roles</strong> — individual appears as both male and female parent.</li>
                      <li><strong>Missing Parents</strong> — referenced parents added with unknown parents (0).</li>
                      <li><strong>Cycles / Dependencies</strong> — circular relationships are flagged.</li>
                    </ul>
                    <li>Review results in the <strong>Summary</strong> and <strong>Issue Tables</strong> tabs, then export.</li>
                  </ul>
                ')))
              ),
              style = "overflow-y: auto; height: 550px"
            ),
            shiny::tabPanel(
              "Summary",
              shiny::uiOutput(ns("summary_banner")),
              style = "overflow-y: auto; height: 550px; padding: 10px;"
            ),
            shiny::tabPanel(
              "Issue Tables",
              shiny::uiOutput(ns("results_ui")),
              style = "overflow-y: auto; height: 550px; padding: 10px;"
            )
          )
        )  # closes box
      ),  # closes column(width = 6)
      
      # Column 3: Status 
      shiny::column(
        width = 3,
        bs4Dash::box(
          title       = "Status",
          width       = 12,
          collapsible = TRUE,
          status      = "info",
          solidHeader = TRUE,
          shinyWidgets::progressBar(
            id          = ns("pb_ped"),
            value       = 0,
            status      = "info",
            display_pct = TRUE,
            striped     = TRUE,
            title       = " "
          )
        )
      )  # closes column(width = 3)
    )  # closes fluidRow
  )    # closes tagList
}


#' Pedigree Cleaner module server
#'
#' @param id Module id
#' @param parent_session Parent (app) session
#'
#' @noRd
mod_ped_cleaner_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    
    check_results <- shiny::reactiveVal(NULL)
    
    # Help button 
    shiny::observeEvent(input$help_btn, {
      shiny::showModal(
        shiny::modalDialog(
          title     = shiny::tagList(shiny::icon("circle-question"), " Pedigree Cleaner — Help"),
          size      = "l",
          easyClose = TRUE,
          footer    = shiny::modalButton("Close"),
          help_content_ped_cleaner(collapse_fn = make_collapse_panel, id_prefix = "modal")
        )
      )
    })
    
    # Run check 
    shiny::observeEvent(input$run_check, {
      shiny::req(input$ped_file)
      check_results(NULL)
      shinyjs::disable("download_results")          # ← un-namespaced; shinyjs handles it
      
      tryCatch({
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 10, status = "info",
          title = "Reading pedigree file..."
        )
        
        tmp_path <- input$ped_file$datapath
        file_ext <- tolower(tools::file_ext(input$ped_file$name))
        
        ped_raw <- if (file_ext == "csv") {
          utils::read.csv(tmp_path, header = TRUE,
                          stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          utils::read.table(tmp_path, header = TRUE, sep = "\t",
                            stringsAsFactors = FALSE, check.names = FALSE)
        }
        
        required_cols <- c("id", "male_parent", "female_parent")
        missing_cols  <- setdiff(required_cols, colnames(ped_raw))
        if (length(missing_cols) > 0) {
          stop(paste0(
            "Missing required column(s): ",
            paste(missing_cols, collapse = ", "),
            ". File must contain: id, male_parent, female_parent."
          ))
        }
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 30, status = "info",
          title = "Checking for duplicate records..."
        )
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 50, status = "info",
          title = "Checking for conflicting IDs and inconsistent parent sex roles..."
        )
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 70, status = "info",
          title = "Checking for missing parents..."
        )
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 85, status = "info",
          title = "Detecting cycles and dependencies..."
        )
        
        # Correction 3: write ped_raw to a known-extension temp file 
        tmp_ped_path <- tempfile(fileext = ".txt")
        on.exit(unlink(tmp_ped_path), add = TRUE)
        write.table(ped_raw, tmp_ped_path,
                    sep = "\t", row.names = FALSE, quote = FALSE)
        
        report <- BIGr::check_ped(
          ped.file = tmp_ped_path,
          verbose  = FALSE,
          save_zip = FALSE
        )
        
        # Correction 4: retrieve corrected pedigree from check_ped return 
        # Use the session-local report value instead of reaching into .GlobalEnv
        corrected_ped <- tryCatch(
          report$corrected_ped,
          error = function(e) NULL
        )
        
        check_results(list(
          report        = report,
          corrected_ped = corrected_ped
        ))
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 100, status = "success",
          title = "Finished"
        )
        shinyjs::enable("download_results")          # ← un-namespaced; shinyjs handles it
        
        shiny::updateTabsetPanel(session, "ped_results_tabs", selected = "Summary")
        
      }, error = function(e) {
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 100, status = "danger",
          title = paste0("Failed: ", e$message)
        )
      })
    })
    
    # Summary banner 
    output$summary_banner <- shiny::renderUI({
      shiny::req(check_results())
      report <- check_results()$report
      
      get_count <- function(df) if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)
      n_dupes    <- get_count(report$exact_duplicates)
      n_conflict <- get_count(report$repeated_ids_diff)
      n_messy    <- get_count(report$inconsistent_sex_roles)
      n_missing  <- get_count(report$missing_parents)
      n_cycles   <- get_count(report$dependencies)
      total      <- n_dupes + n_conflict + n_messy + n_missing + n_cycles
      
      banner_color <- if (total == 0) "#d4edda" else "#fff3cd"
      border_color <- if (total == 0) "#c3e6cb" else "#ffeeba"
      text_color   <- if (total == 0) "#155724"  else "#856404"
      headline     <- if (total == 0) "No issues found. Pedigree looks clean!" else
        paste0(total, " issue(s) detected. Review the Issue Tables tab.")
      
      shiny::HTML(paste0(
        "<div style='background-color:", banner_color, "; border: 1px solid ", border_color,
        "; padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
        "<p style='color:", text_color, "; margin: 0; font-weight: bold; font-size: 14px;'>",
        headline, "</p>",
        "<p style='color:", text_color, "; margin: 6px 0 0 0; font-size: 12px;'>",
        "- Exact duplicates removed: <strong>", n_dupes, "</strong> &nbsp;",
        "- Conflicting IDs resolved: <strong>", n_conflict, "</strong> &nbsp;",
        "- Inconsistent parent sex roles flagged: <strong>", n_messy, "</strong> &nbsp;",
        "- Missing parents added: <strong>", n_missing, "</strong> &nbsp;",
        "- Cycles detected: <strong>", n_cycles, "</strong>",
        "</p></div>"
      ))
    })
    
    # Results UI 
    output$results_ui <- shiny::renderUI({
      shiny::req(check_results())
      report <- check_results()$report
      
      get_count <- function(df) if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)
      
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
        if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
          output[[output_id]] <- DT::renderDT({
            DT::datatable(df,
                          rownames = FALSE,
                          options  = list(pageLength = 10, scrollX = TRUE),
                          class    = "table-bordered table-sm")
          })
        }
      }
      
      render_if("Exact Duplicates Removed",       report$exact_duplicates)
      render_if("Conflicting IDs Resolved",       report$repeated_ids_diff)
      render_if("Inconsistent Parent Sex Roles",  report$inconsistent_sex_roles)
      render_if("Missing Parents Added",          report$missing_parents)
      render_if("Cycles / Dependencies Detected", report$dependencies)
      
      shiny::tagList(
        shiny::h5(
          shiny::tagList(shiny::icon("list-check"), " Check Results"),
          style = "font-weight: bold; margin-bottom: 10px;"
        ),
        make_section("Exact Duplicates Removed",       "copy",        report$exact_duplicates,       "#6c757d"),
        make_section("Conflicting IDs Resolved",       "exclamation", report$repeated_ids_diff,      "#856404"),
        make_section("Inconsistent Parent Sex Roles",  "shuffle",     report$inconsistent_sex_roles, "#856404"),
        make_section("Missing Parents Added",          "user-plus",   report$missing_parents,        "#0c5460"),
        make_section("Cycles / Dependencies Detected", "rotate",      report$dependencies,           "#721c24")
      )
    })
    
    # Download
    output$download_results <- shiny::downloadHandler(
      filename = function() {
        paste0("pedigree_check_", Sys.Date(), ".zip")
      },
      content = function(file) {
        shiny::req(check_results())
        results <- check_results()
        report  <- results$report
        tmp_dir <- tempfile("ped_export")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        if (!is.null(results$corrected_ped)) {
          write.table(results$corrected_ped,
                      file.path(tmp_dir, "corrected_pedigree.txt"),
                      sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        sections <- list(
          exact_duplicates       = report$exact_duplicates,
          conflicting_ids        = report$repeated_ids_diff,
          inconsistent_sex_roles = report$inconsistent_sex_roles,
          missing_parents        = report$missing_parents,
          dependencies           = report$dependencies
        )
        
        for (nm in names(sections)) {
          df <- sections[[nm]]
          if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
            write.table(df,
                        file.path(tmp_dir, paste0(nm, ".txt")),
                        sep = "\t", row.names = FALSE, quote = FALSE)
          }
        }
        
        tsv_files <- list.files(tmp_dir)
        zip::zip(zipfile = file, files = tsv_files, root = tmp_dir)
        unlink(tmp_dir, recursive = TRUE)
      },
      contentType = "application/zip"
    )
    
  })
}

# Helpers
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