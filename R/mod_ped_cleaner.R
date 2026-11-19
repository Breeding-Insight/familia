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
          example_files_ui(ns, example_files$ped_cleaner),
          shiny::fileInput(
            ns("ped_file"),
            "Upload Pedigree File",
            accept = c(".txt", ".tsv", ".csv")
          ),
          shiny::hr(),
          shiny::p(
            "Options:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 5px;"
          ),
          shiny::checkboxInput(
            ns("correct_conflicting_trios"),
            "Set conflicting parents to missing (0)",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("correct_inconsistent_sex_roles"),
            "Set parents with inconsistent sex roles to missing (0)",
            value = TRUE
          ),
          shiny::hr(),
          shiny::actionButton(
            ns("run_check"),
            "Run Pedigree Check"
          ),
          shiny::hr(),
          shinyjs::disabled(
            shiny::downloadButton(
              ns("download_results"),
              "Download Results (Excel)"
            )
          ),
          shiny::div(style = "height: 8px;"),
          shinyjs::disabled(
            shiny::downloadButton(
              ns("download_pedigree"),
              "Download Cleaned Pedigree (.txt)"
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
                    <li>The <strong>Data Overview</strong> tab opens when the pedigree is uploaded and counts its records, ids and trios.</li>
                    <li>Choose the options, then click <strong>Run Pedigree Check</strong> to detect issues.</li>
                    <li>A run summary will appear in the panel on the right after the check completes.</li>
                    <li>Issues detected:</li>
                    <ul>
                      <li><strong>Exact Duplicates</strong> - fully identical rows; the extra copies are always removed.</li>
                      <li><strong>Conflicting Trios</strong> - same ID with different parents; the disagreeing parents are set to missing (0) when the option is checked. To keep one of the rows instead, check its <strong>Keep</strong> box in the table and click <strong>Apply selection</strong>.</li>
                      <li><strong>Inconsistent Sex Roles</strong> - individual appears as both male and female parent; set to missing (0) when the option is checked.</li>
                      <li><strong>Missing Parents</strong> - parents without a row of their own are always added with unknown parents (0).</li>
                      <li><strong>Cycles / Dependencies</strong> - circular relationships are flagged and must be fixed by hand.</li>
                    </ul>
                    <li>Review results in the <strong>Issue Tables</strong> tab. <strong>Download Results</strong> gives one Excel file with every record and its issue, one sheet per table and the run settings; <strong>Download Cleaned Pedigree</strong> gives the pedigree for Validate Pedigree.</li>
                  </ul>
                ')))
              ),
              style = "overflow-y: auto; height: 550px"
            ),
            shiny::tabPanel(
              "Data Overview",
              shiny::uiOutput(ns("dashboard")),   # summary tiles, filled in when the pedigree is uploaded
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

      # Column 3: Status + Summary
      shiny::column(
        width = 3,
        bs4Dash::box(
          title       = "Status",
          width       = 12,
          collapsible = TRUE,
          status      = "info",
          shinyWidgets::progressBar(
            id          = ns("pb_ped"),
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

    check_results <- shiny::reactiveVal(NULL)   # uploaded pedigree, reports and options of the run
    kept_rows     <- shiny::reactiveVal(NULL)   # conflicting rows kept by the user

    # Example files
    output$download_examples <- example_download_handler(example_files$ped_cleaner)

    # Help button
    shiny::observeEvent(input$help_btn, {
      shiny::showModal(
        shiny::modalDialog(
          title     = shiny::tagList(shiny::icon("circle-question"), " Pedigree Cleaner - Help"),
          size      = "l",
          easyClose = TRUE,
          footer    = shiny::modalButton("Close"),
          help_content_ped_cleaner(collapse_fn = make_collapse_panel, id_prefix = "modal")
        )
      )
    })

    # Uploaded pedigree, read once for the summary tiles and the run
    ped_input <- shiny::reactive({
      shiny::req(input$ped_file)
      read_table_upload(input$ped_file)
    })

    # Summary tiles (Data Overview tab)
    output$dashboard <- shiny::renderUI({
      empty <- list("Records" = NA, "Unique IDs" = NA, "Complete trios" = NA,
                    "One parent known" = NA, "Both parents unknown" = NA)
      if (is.null(input$ped_file)) return(summary_strip(empty))
      ped <- tryCatch(ped_input(), error = function(e) e)
      if (inherits(ped, "error")) {
        return(summary_strip(empty, paste("Could not read the pedigree file:", conditionMessage(ped))))
      }
      counts <- pedigree_counts(ped)
      if (is.null(counts)) {
        return(summary_strip(empty, "The pedigree file needs id, male_parent and female_parent columns."))
      }
      summary_strip(counts)
    })

    # Uploading the pedigree opens the Data Overview tab
    shiny::observeEvent(input$ped_file, {
      shiny::updateTabsetPanel(session, "ped_results_tabs", selected = "Data Overview")
    })

    # check_ped() with the options of the run
    check_pedigree <- function(ped, set_conflicts, set_sex) {
      BIGpopA::check_ped(
        ped.file                       = ped,
        verbose                        = FALSE,
        correct_conflicting_trios      = set_conflicts,
        correct_inconsistent_sex_roles = set_sex
      )
    }

    # Run check
    shiny::observeEvent(input$run_check, {
      shiny::req(input$ped_file)
      check_results(NULL)
      kept_rows(NULL)
      shinyjs::disable("download_results")
      shinyjs::disable("download_pedigree")

      tryCatch({
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 10, status = "info",
          title = "Reading pedigree file..."
        )

        ped_raw <- ped_input()

        # Column names are matched ignoring case, spaces and dots (as in BIGpopA)
        required_cols <- c("id", "male_parent", "female_parent")
        norm_cols     <- gsub("[ .-]+", "_", tolower(trimws(colnames(ped_raw))))
        missing_cols  <- setdiff(required_cols, norm_cols)
        if (length(missing_cols) > 0) {
          stop(paste0(
            "Missing required column(s): ",
            paste(missing_cols, collapse = ", "),
            ". File must contain: id, male_parent, female_parent."
          ))
        }

        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 50, status = "info",
          title = "Checking duplicates, conflicting trios, sex roles, missing parents and cycles..."
        )

        set_conflicts <- isTRUE(input$correct_conflicting_trios)
        set_sex       <- isTRUE(input$correct_inconsistent_sex_roles)
        report        <- check_pedigree(ped_raw, set_conflicts, set_sex)

        check_results(list(
          ped_input     = ped_raw,          # uploaded pedigree, edited when rows are kept
          base_report   = report,           # run on the uploaded pedigree (every conflict)
          report        = report,           # current report, after the kept rows
          set_conflicts = set_conflicts,
          set_sex       = set_sex,
          file_name     = input$ped_file$name
        ))

        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 100, status = "success",
          title = "Finished"
        )
        shinyjs::enable("download_results")
        shinyjs::enable("download_pedigree")

        shiny::updateTabsetPanel(session, "ped_results_tabs", selected = "Issue Tables")

      }, error = function(e) {
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 100, status = "danger",
          title = paste0("Failed: ", e$message)
        )
      })
    })

    # Keep a conflicting trio: drop the other rows of its id from the uploaded
    # pedigree and run check_ped() again with the same options, so the missing
    # parents, sex roles and cycles are recomputed with the kept parents
    apply_kept <- function(kept) {
      res <- check_results()
      shiny::req(res)
      if (is.null(kept) || nrow(kept) == 0) {
        kept_rows(NULL)
        res$report <- res$base_report
      } else {
        ped      <- res$ped_input
        cols     <- pedigree_columns(ped)
        p_id     <- as.character(ped[[cols[["id"]]]])
        row_key  <- paste(p_id, ped[[cols[["male_parent"]]]], ped[[cols[["female_parent"]]]], sep = "\r")
        kept_key <- paste(kept$id, kept$male_parent, kept$female_parent, sep = "\r")
        drop     <- p_id %in% kept$id & !row_key %in% kept_key
        report   <- tryCatch(check_pedigree(ped[!drop, , drop = FALSE], res$set_conflicts, res$set_sex),
                             error = function(e) e)
        if (inherits(report, "error")) {
          shiny::showNotification(paste("Could not apply the selection:", conditionMessage(report)),
                                  type = "error", duration = 10)
          return(invisible(NULL))
        }
        kept_rows(kept)
        res$report <- report
      }
      check_results(res)
    }

    shiny::observeEvent(input$apply_keep, {
      res <- check_results()
      shiny::req(res)
      conflicts <- as.data.frame(res$base_report$conflicting_trios)
      selected  <- conflicts[input$tbl_conflicting_trios_rows_selected,
                             c("id", "male_parent", "female_parent"), drop = FALSE]
      dup_ids   <- unique(selected$id[duplicated(selected$id)])
      if (length(dup_ids) > 0) {
        shiny::showNotification(
          paste0("Keep at most one row per id. More than one row is checked for: ",
                 paste(dup_ids, collapse = ", "), ". Nothing was applied."),
          type = "error", duration = 10
        )
        return()
      }
      apply_kept(selected)
    })

    shiny::observeEvent(input$reset_keep, {
      apply_kept(NULL)
    })

    # Summary banner
    output$summary_banner <- shiny::renderUI({
      shiny::req(check_results())
      res        <- check_results()
      report     <- res$report
      do_trios   <- res$set_conflicts
      do_sex     <- res$set_sex
      kept       <- kept_rows()

      get_count <- function(df) if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)
      n_dupes    <- get_count(report$exact_duplicates)
      n_conflict <- length(unique(as.data.frame(res$base_report$conflicting_trios)$id))
      n_kept     <- if (is.null(kept)) 0L else nrow(kept)
      n_messy    <- get_count(report$inconsistent_sex_roles)
      n_missing  <- get_count(report$missing_parents)
      n_cycles   <- get_count(report$dependencies)
      total      <- n_dupes + n_conflict + n_messy + n_missing + n_cycles

      banner_color <- if (total == 0) "#d4edda" else "#fff3cd"
      border_color <- if (total == 0) "#c3e6cb" else "#ffeeba"
      text_color   <- if (total == 0) "#155724"  else "#856404"
      headline     <- if (total == 0) "No issues found. Pedigree looks clean!" else
        paste0(total, " issue(s) detected. Review the Issue Tables tab.")

      conflict_msg <- paste0(
        n_conflict, " id(s)",
        if (n_conflict > 0) paste0(" (", n_kept, " kept by you, ", n_conflict - n_kept,
                                   if (do_trios) " set to missing)" else " flagged)") else ""
      )

      options_msg <- if (do_trios && do_sex) {
        "Exact duplicates are always removed and missing parents always added. Conflicting parents and parents with inconsistent sex roles were also set to missing (0) in this run (check help for details)."
      } else if (do_trios && !do_sex) {
        "Exact duplicates are always removed and missing parents always added. Conflicting parents were also set to missing (0) in this run; inconsistent sex roles are only flagged (check help for details)."
      } else if (!do_trios && do_sex) {
        "Exact duplicates are always removed and missing parents always added. Parents with inconsistent sex roles were also set to missing (0) in this run; conflicting trios are only flagged (check help for details)."
      } else {
        "Only exact duplicates were removed and missing parents added in this run; the other issues are only flagged (check help for details)."
      }

      shiny::HTML(paste0(
        "<div style='background-color:", banner_color, "; border: 1px solid ", border_color,
        "; padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
        "<p style='color:", text_color, "; margin: 0; font-weight: bold; font-size: 14px;'>",
        headline, "</p>",
        "<p style='color:", text_color, "; margin: 6px 0 0 0; font-size: 12px;'>",
        "- Exact duplicates: <strong>", n_dupes, "</strong> &nbsp;",
        "- Conflicting trios: <strong>", conflict_msg, "</strong> &nbsp;",
        "- Inconsistent sex roles: <strong>", n_messy, "</strong> &nbsp;",
        "- Missing parents: <strong>", n_missing, "</strong> &nbsp;",
        "- Cycles / dependencies: <strong>", n_cycles, "</strong>",
        "</p>",
        "<p style='color:", text_color, "; margin: 8px 0 0 0; font-size: 11px; font-style: italic;'>",
        options_msg,
        "</p></div>"
      ))
    })

    # Results UI
    output$results_ui <- shiny::renderUI({
      shiny::req(check_results())
      res       <- check_results()
      report    <- res$report
      conflicts <- as.data.frame(res$base_report$conflicting_trios)   # every conflict, kept or not
      kept      <- kept_rows()

      get_count <- function(df) if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)

      make_section <- function(title, icon_name, df, color_hex, extra = NULL) {
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
            shiny::tagList(extra, DT::DTOutput(session$ns(tbl_id)))
          } else {
            shiny::p("No records found.", style = "color: #28a745; margin: 0;")
          }
        )
      }

      render_if <- function(title, df) {
        output_id <- paste0("tbl_", sanitize_id(title))
        if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
          output[[output_id]] <- DT::renderDT({
            results_datatable(df)
          })
        }
      }

      render_if("Exact Duplicates",       report$exact_duplicates)
      render_if("Inconsistent Sex Roles", report$inconsistent_sex_roles)
      render_if("Missing Parents",        report$missing_parents)
      render_if("Cycles / Dependencies",  report$dependencies)

      # Conflicting trios: a Keep box per row (row selection), kept rows pre-checked
      if (nrow(conflicts) > 0) {
        conflict_key <- paste(conflicts$id, conflicts$male_parent, conflicts$female_parent, sep = "\r")
        kept_key     <- if (is.null(kept)) character(0) else
          paste(kept$id, kept$male_parent, kept$female_parent, sep = "\r")
        output$tbl_conflicting_trios <- DT::renderDT({
          results_datatable(
            cbind(Keep = "", conflicts),
            selection = list(mode = "multiple", selected = which(conflict_key %in% kept_key)),
            options   = list(columnDefs = list(list(targets = 0, className = "keep-cell",
                                                    orderable = FALSE, searchable = FALSE)))
          )
        })
      }
      keep_controls <- shiny::tagList(
        shiny::p(
          "To keep one row of an id as entered, check its Keep box (at most one per id) and click Apply selection. ",
          "Ids without a checked row are ", if (res$set_conflicts) "set to missing (0)." else "only flagged.",
          style = "font-size: 12px; color: #6c757d; margin-bottom: 6px;"
        ),
        shiny::actionButton(session$ns("apply_keep"), "Apply selection", icon = shiny::icon("check"),
                            class = "btn-sm"),
        shiny::actionButton(session$ns("reset_keep"), "Reset", icon = shiny::icon("rotate-left"),
                            class = "btn-sm"),
        shiny::div(style = "height: 8px;")
      )

      shiny::tagList(
        shiny::h5(
          shiny::tagList(shiny::icon("list-check"), " Check Results"),
          style = "font-weight: bold; margin-bottom: 10px;"
        ),
        make_section("Exact Duplicates",       "copy",        report$exact_duplicates,       "#6c757d"),
        make_section("Conflicting Trios",      "exclamation", conflicts,                     "#856404",
                     extra = keep_controls),
        make_section("Inconsistent Sex Roles", "shuffle",     report$inconsistent_sex_roles, "#856404"),
        make_section("Missing Parents",        "user-plus",   report$missing_parents,        "#0c5460"),
        make_section("Cycles / Dependencies",  "rotate",      report$dependencies,           "#721c24")
      )
    })

    # Download: one Excel workbook with every record and its issue, one sheet
    # per table, the rows kept by the user and the run settings
    output$download_results <- shiny::downloadHandler(
      filename = function() {
        paste0("pedigree_check_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        shiny::req(check_results())
        res       <- check_results()
        report    <- res$report
        kept      <- kept_rows()
        conflicts <- as.data.frame(res$base_report$conflicting_trios)
        if (nrow(conflicts) > 0) {
          kept_key <- if (is.null(kept)) character(0) else
            paste(kept$id, kept$male_parent, kept$female_parent, sep = "\r")
          conflicts$kept_by_user <- paste(conflicts$id, conflicts$male_parent,
                                          conflicts$female_parent, sep = "\r") %in% kept_key
        }

        write_results_xlsx(list(
          "All Records"            = cleaner_records(res$ped_input, report,
                                                     conflicts     = res$base_report$conflicting_trios,
                                                     kept          = kept,
                                                     set_conflicts = res$set_conflicts,
                                                     set_sex       = res$set_sex),
          "Exact Duplicates"       = report$exact_duplicates,
          "Conflicting Trios"      = conflicts,
          "Inconsistent Sex Roles" = report$inconsistent_sex_roles,
          "Missing Parents"        = report$missing_parents,
          "Cycles - Dependencies"  = report$dependencies,
          "Settings"               = settings_table(
            "Tab"                                       = "Pedigree Cleaner",
            "Pedigree file"                             = res$file_name,
            "Set conflicting parents to missing (0)"    = res$set_conflicts,
            "Set inconsistent sex roles to missing (0)" = res$set_sex,
            "Rows kept by user (id: male x female)"     = if (is.null(kept)) "none" else
              paste0(kept$id, ": ", kept$male_parent, " x ", kept$female_parent),
            "Always applied"                            = "exact duplicates removed; missing parents added with unknown parents (0)"
          )
        ), file)
      }
    )

    # Download: the cleaned pedigree, the input of Validate Pedigree
    output$download_pedigree <- shiny::downloadHandler(
      filename = function() "pedigree_cleaned.txt",
      content  = function(file) {
        shiny::req(check_results())
        utils::write.table(as.data.frame(check_results()$report$corrected_pedigree), file,
                           sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

  })
}
