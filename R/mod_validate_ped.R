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
          example_files_ui(ns, example_files$validate_ped),
          shiny::fileInput(ns("pedigree_file"),  "Pedigree File",  accept = c(".txt", ".tsv", ".csv")),
          shiny::fileInput(ns("genotypes_file"), "Genotypes File (.txt, .csv, .vcf, .vcf.gz, .ped)", accept = genotype_upload_accept),
          shiny::fileInput(ns("founders_file"),  "Founders File (optional)", accept = c(".txt", ".tsv", ".csv")),
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
          shiny::numericInput(ns("ploidy"),
                              "Ploidy",
                              value = 2, min = 2, max = 20, step = 1),
          shiny::hr(),
          shiny::actionButton(ns("run_validate"), "Run Validation"),
          shiny::hr(),
          shinyjs::disabled(
            shiny::downloadButton(ns("download_validate_all"), "Download Results (Excel)")
          ),
          shiny::div(style = "height: 8px;"),
          shinyjs::disabled(
            shiny::downloadButton(ns("download_pedigree"), "Download Validated Pedigree (.txt)")
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
                    <li>Upload a genotypes file, either as a table (.txt tab-separated or .csv) with an <code>id</code> column followed by marker columns coded as allele-B dosage (0, 1, ..., ploidy; e.g. 0, 1, 2 for diploid), as a VCF (<code>.vcf</code> / <code>.vcf.gz</code>), whose <code>GT</code> calls are converted to dosages using the selected ploidy, or as a PLINK <code>.ped</code> file (diploid only, no <code>.map</code> needed).</li>
                    <li>Optionally upload a founders file (one id per line, no header). Individuals with both parents <code>0</code> that are listed in it are reported as Founders instead of Missing Parents.</li>
                    <li>The <strong>Data Overview</strong> tab opens when a file is uploaded and counts the trios, the genotyped individuals and the markers.</li>
                    <li>Set the <strong>Ploidy</strong> (2 = diploid, 4 = tetraploid, ...) to match your data. Odd ploidy such as triploid uses a homozygosity-only check.</li>
                    <li>Set error thresholds and minimum markers, then click <strong>Run Validation</strong>.</li>
                    <li>Results are split into:</li>
                    <ul>
                      <li><strong>Pass</strong> - trios within the Mendelian error threshold.</li>
                      <li><strong>Fail</strong> - trios exceeding the threshold; the <code>recommended_action</code> column names the parent(s) to set to missing.</li>
                      <li><strong>Low Markers</strong> - trios with insufficient markers.</li>
                      <li><strong>No Genotype Data</strong> - trios absent from the genotype file.</li>
                      <li><strong>Founders</strong> - trios identified as founders.</li>
                      <li><strong>Missing Parents</strong> - trios with one or both parents coded as 0.</li>
                    </ul>
                    <li>In the <strong>Mendelian Error Plot</strong>, click a bar to list its trios under the plot.</li>
                    <li>Review the Issue Tables tab. <strong>Download Results</strong> gives one Excel file with every trio and its issue, one sheet per table and the run settings; <strong>Download Validated Pedigree</strong> gives the pedigree with the recommended parents set to missing, for Find Parentage.</li>
                  </ul>
                ')))
              ),
              style = "overflow-y: auto; height: 550px"
            ),
            shiny::tabPanel(
              "Data Overview",
              shiny::uiOutput(ns("dashboard")),   # summary tiles, filled in when the files are uploaded
              style = "overflow-y: auto; height: 550px; padding: 10px;"
            ),
            shiny::tabPanel(
              "Issue Tables",
              shiny::uiOutput(ns("results_ui")),
              style = "overflow-y: auto; height: 550px; padding: 10px;"
            ),
            shiny::tabPanel(
              "Mendelian Error Plot",
              shiny::plotOutput(ns("error_plot"), height = "450px",
                                click = ns("error_plot_click"),
                                hover = shiny::hoverOpts(ns("error_plot_hover"), delay = 150, delayType = "debounce")),
              shiny::div(class = "plot-hint", shiny::textOutput(ns("error_plot_hint"), inline = TRUE)),
              shiny::uiOutput(ns("error_plot_bar_ui")),
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
              circle  = FALSE,
              status  = "info",
              icon    = shiny::icon("sliders"),
              width   = "300px",
              label   = "Image Options",
              tooltip = shinyWidgets::tooltipOptions(title = "File type, resolution and size")
            )
          ),
          # Download button kept outside the dropdown so the link is always active
          shiny::div(
            style = "display:inline-block; float:left; margin-left: 8px;",
            shiny::downloadButton(ns("download_validate_plot"), "Save Image", class = "btn-danger")
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

    `%||%` <- function(x, y) if (is.null(x)) y else x

    validate_results <- shiny::reactiveVal(NULL)
    run_settings     <- shiny::reactiveVal(NULL)   # inputs of the run, for the Settings sheet

    # Example files
    output$download_examples <- example_download_handler(example_files$validate_ped,
                                                         "validate_pedigree_examples.zip")

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
          title     = shiny::tagList(shiny::icon("circle-question"), " Validate Pedigree - Help"),
          size      = "l",
          easyClose = TRUE,
          footer    = shiny::modalButton("Close"),
          help_content_validate_ped(collapse_fn = make_collapse_panel, id_prefix = "modal")
        )
      )
    })

    # Uploaded files, read once for the summary tiles and the run. Genotypes
    # are read without the Ploidy check, which is done when the run starts.
    ped_input <- shiny::reactive({
      shiny::req(input$pedigree_file)
      read_table_upload(input$pedigree_file)
    })
    geno_input <- shiny::reactive({
      shiny::req(input$genotypes_file)
      read_genotype_upload(input$genotypes_file, ploidy = NULL)
    })

    # Summary tiles (Data Overview tab)
    output$dashboard <- shiny::renderUI({
      tiles <- list("Records" = NA, "Unique IDs" = NA, "Complete trios" = NA,
                    "One parent known" = NA, "Both parents unknown" = NA,
                    "Genotyped individuals" = NA, "Markers" = NA, "Testable trios" = NA)
      notes <- character(0)
      ped   <- NULL
      if (!is.null(input$pedigree_file)) {
        ped    <- tryCatch(ped_input(), error = function(e) e)
        counts <- if (inherits(ped, "error")) NULL else pedigree_counts(ped)
        if (inherits(ped, "error")) {
          notes <- c(notes, paste("Could not read the pedigree file:", conditionMessage(ped)))
          ped   <- NULL
        } else if (is.null(counts)) {
          notes <- c(notes, "The pedigree file needs id, male_parent and female_parent columns.")
          ped   <- NULL
        } else {
          tiles[names(counts)] <- counts
        }
      }
      if (!is.null(input$genotypes_file)) {
        geno <- tryCatch(geno_input(), error = function(e) e)
        if (inherits(geno, "error")) {
          notes <- c(notes, paste("Could not read the genotypes file:", conditionMessage(geno)))
        } else {
          gc <- genotype_counts(geno)
          tiles[["Genotyped individuals"]] <- length(gc$ids)
          tiles[["Markers"]]               <- gc$markers
          if (!is.null(ped)) tiles[["Testable trios"]] <- testable_trios(ped, gc$ids)
        }
      }
      summary_strip(tiles, if (length(notes) > 0) paste(notes, collapse = " "))
    })

    # Uploading the pedigree or the genotypes opens the Data Overview tab
    # (NULL until a file is uploaded, so nothing happens at start-up)
    shiny::observeEvent(c(input$pedigree_file$datapath, input$genotypes_file$datapath), {
      shiny::updateTabsetPanel(session, "validate_results_tabs", selected = "Data Overview")
    })

    # Run validation
    shiny::observeEvent(input$run_validate, {
      shiny::req(input$pedigree_file, input$genotypes_file)
      validate_results(NULL)
      shinyjs::disable("download_validate_all")
      shinyjs::disable("download_pedigree")

      tryCatch({
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 10, status = "info",
          title = "Reading input files..."
        )

        ped_raw  <- ped_input()
        geno_raw <- geno_input()
        check_upload_ploidy(geno_raw, input$genotypes_file, input$ploidy)

        founders_path <- if (!is.null(input$founders_file)) input$founders_file$datapath else NULL

        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 35, status = "info",
          title = "Evaluating trios..."
        )

        # BIGpopA also prints the plot: send it to a null device, so it does not
        # open a plot window or write Rplots.pdf in the working directory
        grDevices::pdf(NULL)
        report <- tryCatch(
          BIGpopA::validate_pedigree(
            pedigree_file                 = ped_raw,
            genotypes_file                = geno_raw,
            founders_file                 = founders_path,
            trio_error_threshold          = as.numeric(input$trio_error_threshold),
            min_markers                   = as.integer(input$min_markers),
            single_parent_error_threshold = as.numeric(input$single_parent_error_threshold),
            ploidy                        = as.integer(input$ploidy),
            verbose                       = FALSE,
            plot_results                  = TRUE,
            marker_trio_table             = TRUE
          ),
          finally = grDevices::dev.off()
        )

        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 85, status = "info",
          title = "Compiling results..."
        )

        run_settings(list(
          pedigree_file    = input$pedigree_file$name,
          genotypes_file   = input$genotypes_file$name,
          founders_file    = if (is.null(input$founders_file)) "none" else input$founders_file$name,
          trio_threshold   = as.numeric(input$trio_error_threshold),
          single_threshold = as.numeric(input$single_parent_error_threshold),
          min_markers      = as.integer(input$min_markers),
          ploidy           = as.integer(input$ploidy)
        ))
        validate_results(report)

        shinyWidgets::updateProgressBar(
          session = session, id = "pb_validate",
          value = 100, status = "success",
          title = "Finished"
        )
        shinyjs::enable("download_validate_all")
        shinyjs::enable("download_pedigree")
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

      # Tables show the recommended action in plain language (downloads keep the codes)
      render_if <- function(title, df) {
        output_id <- paste0("tbl_", sanitize_id(title))
        if (!is.null(df) && (is.data.frame(df) || data.table::is.data.table(df)) && nrow(df) > 0) {
          output[[output_id]] <- DT::renderDT({
            results_datatable(plain_actions(df))
          })
        }
      }

      render_if("Pass",              report$pass)
      render_if("Fail",              report$fail)
      render_if("Low Markers",       report$low_markers)
      render_if("No Genotype Data",  report$no_genotype_data)
      render_if("Founders",          report$founders)
      render_if("Missing Parents",   report$missing_parents)
      render_if("Marker Summary",    report$marker_summary)
      render_if("Marker Trio Table", report$marker_trio_table)

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
        make_section("Missing Parents",  "user-slash",   report$missing_parents,  "#856404"),
        make_section("Marker Summary",   "table",        report$marker_summary,   "#0c5460"),
        make_section("Marker Trio Table", "table-cells", report$marker_trio_table, "#0c5460")
      )
    })

    # Mendelian error plot. The ggplot is returned, not printed, so Shiny knows
    # its axes and clicks/hovers arrive in data units (error %)
    output$error_plot <- shiny::renderPlot({
      shiny::req(validate_results())
      report <- validate_results()
      shiny::validate(shiny::need(!is.null(report$plot), "Run validation to generate the plot."))
      report$plot
    })

    # Trios of a bar of the plot: hovering gives the count, clicking lists them
    plot_breaks <- shiny::reactive({
      p <- validate_results()$plot
      shiny::req(p)
      histogram_breaks(p)
    })
    selected_bar <- shiny::reactiveVal(NULL)
    shiny::observeEvent(validate_results(), selected_bar(NULL), ignoreNULL = FALSE)
    shiny::observeEvent(input$error_plot_click, {
      report <- validate_results()
      shiny::req(report$plot)
      selected_bar(histogram_bar_rows(report$full_results, plot_breaks(), input$error_plot_click$x,
                                      "trio_mendelian_error_pct"))
    })
    output$error_plot_hint <- shiny::renderText({
      report <- validate_results()
      shiny::req(report$plot)
      hover <- input$error_plot_hover
      bar   <- if (is.null(hover)) NULL else
        histogram_bar_rows(report$full_results, plot_breaks(), hover$x, "trio_mendelian_error_pct")
      if (is.null(bar)) "Click a bar to list its trios under the plot." else
        paste0("Bar ", bar$label, ": ", nrow(bar$rows), " trio(s). Click to list them.")
    })
    output$error_plot_bar_ui <- shiny::renderUI({
      bar <- selected_bar()
      if (is.null(bar)) return(NULL)
      shiny::tagList(
        shiny::h6(paste0("Trios with a trio error of ", bar$label, " (", nrow(bar$rows), ")"),
                  style = "font-weight: bold; margin-top: 6px;"),
        DT::DTOutput(session$ns("error_plot_bar_table"))
      )
    })
    output$error_plot_bar_table <- DT::renderDT({
      bar <- selected_bar()
      shiny::req(bar)
      results_datatable(plain_actions(bar$rows))
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
        save_plot_file(p, file, ext, width = width, height = height, dpi = dpi)
      }
    )
    # The button sits in a dropdown hidden at start-up; keep its link active
    shiny::outputOptions(output, "download_validate_plot", suspendWhenHidden = FALSE)

    # Results download: one Excel workbook, every trio with its issue first
    output$download_validate_all <- shiny::downloadHandler(
      filename = function() {
        paste0("validate_pedigree_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        shiny::req(validate_results())
        report <- validate_results()
        s      <- run_settings()
        write_results_xlsx(list(
          "All Trios"         = validation_issues(report$full_results, s$trio_threshold, s$min_markers),
          "Pass"              = report$pass,
          "Fail"              = report$fail,
          "Low Markers"       = report$low_markers,
          "No Genotype Data"  = report$no_genotype_data,
          "Founders"          = report$founders,
          "Missing Parents"   = report$missing_parents,
          "Marker Summary"    = report$marker_summary,
          "Marker Trio Table" = report$marker_trio_table,
          "Settings"          = settings_table(
            "Tab"                               = "Validate Pedigree",
            "Pedigree file"                     = s$pedigree_file,
            "Genotypes file"                    = s$genotypes_file,
            "Founders file"                     = s$founders_file,
            "Trio error threshold (%)"          = s$trio_threshold,
            "Single parent error threshold (%)" = s$single_threshold,
            "Min markers"                       = s$min_markers,
            "Ploidy"                            = s$ploidy
          )
        ), file)
      }
    )

    # Pedigree download: the recommended parents set to missing, for Find Parentage
    output$download_pedigree <- shiny::downloadHandler(
      filename = function() "pedigree_validated.txt",
      content  = function(file) {
        shiny::req(validate_results())
        utils::write.table(as.data.frame(validate_results()$corrected_pedigree), file,
                           sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_validate_ped_ui("validate_ped_1")

## To be copied in the server
# mod_validate_ped_server("validate_ped_1")
