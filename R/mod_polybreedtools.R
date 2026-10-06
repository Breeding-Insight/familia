#' PolyBreedTools UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydisconnect
#' @importFrom bs4Dash valueBoxOutput
mod_polybreedtools_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      
      #  Column 1: Inputs
      column(
        width = 3,
        bs4Dash::box(
          title       = "Inputs",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = TRUE,
          fileInput(ns("reference_file"), "Reference Genotypes (.txt, .csv, .vcf, .vcf.gz, .ped)",  accept = genotype_upload_accept),
          uiOutput(ns("reference_map_ui")),    # shown only for a .ped reference
          fileInput(ns("ref_ids_file"),   "Reference IDs",                                          accept = ".txt"),
          fileInput(ns("validation_file"),"Validation Genotypes (.txt, .csv, .vcf, .vcf.gz, .ped)", accept = genotype_upload_accept),
          uiOutput(ns("validation_map_ui")),   # shown only for a .ped validation file
          numericInput(ns("ploidy"), "Ploidy", value = 2, min = 1, max = 20, step = 1),
          numericInput(ns("assign_threshold"), "Assignment threshold (%)", value = 0, min = 0, max = 100, step = 1),
          actionButton(ns("run"), "Run Estimation"),
          shiny::hr(),
          shinyjs::disabled(
            shiny::downloadButton(ns("download_poly_all"), "Download Results")
          ),
          shiny::hr(),
          shiny::div(
            style = "text-align: center; margin-top: 5px;",
            shiny::actionButton(
              ns("help_btn"),
              shiny::tagList(shiny::icon("circle-question"), "Help"),
              style = "background-color: #FFD700; color: #000000; border:none; padding: 8px 16px; border-radius: 5px;"
            )
          )
        )
      ),
      
      #  Column 2: Results
      column(
        width = 6,
        bs4Dash::box(
          title       = "Line/breed content estimation",
          status      = "info",
          solidHeader = FALSE,
          width       = 12,
          height      = 600,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id   = ns("polybreedtools_results_tabs"),
            type = "tabs",
            tabPanel(
              "Instructions",
              fluidRow(
                column(12, shiny::wellPanel(shiny::HTML('
                  <ul>
                    <li>This tool was developed by Breeding Insight.</li>
                    <li>It estimates the proportion of each of the lines/groups included in the <strong>reference population</strong> from genotype samples using methods from
                      <a href="https://www.animalsciencepublications.org/publications/tas/articles/1/1/36" target="_blank">Funkhouser et al. (2017)</a>.</li>
                    <li><strong>Input format:</strong></li>
                    <ul>
                      <li><strong>Reference Genotypes:</strong> Either a genotype matrix (.txt tab-separated or .csv) with samples in rows and SNP markers in columns, where the first column must be <code>ID</code> containing sample IDs and missing values are coded as <code>NA</code>; a VCF file (<code>.vcf</code> / <code>.vcf.gz</code>), whose <code>GT</code> calls are converted to allele-B dosages using the selected ploidy; or a PLINK <code>.ped</code> file (diploid only) together with its <code>.map</code> file, which supplies the marker names.</li>
                      <li><strong>Assignment threshold (%):</strong> A sample is assigned to its highest-proportion line only if that proportion meets the threshold; otherwise it is labelled <code>Undetermined</code>. Use 0 to always assign the highest line. Changes apply immediately without re-running.</li>
                      <li><strong>Reference IDs:</strong> A .txt file with one column per population, listing the reference sample IDs. Header example: <code>Group1</code>, <code>Group2</code>.</li>
                      <li><strong>Validation Genotypes:</strong> Same formats as the reference genotype file. Reference and validation files do not need to be the same format, but marker names must match. The exception is <code>.ped</code>: if either file is a <code>.ped</code>, both must be <code>.ped</code> files, each uploaded with its <code>.map</code>. Markers are matched by the names in the <code>.map</code> files, so their order does not need to match.</li>
                    </ul>
                  </ul>
                ')))
              ),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel("Results Table", DT::DTOutput(ns("preview")),                         style = "overflow-y: auto; height: 500px"),
            shiny::tabPanel("Ancestry Plot", shiny::plotOutput(ns("bar_plot"), height = "450px"), style = "overflow-y: auto; height: 500px")
          )
        ),
        box(
          title       = "Example Inputs",
          status      = "info",
          solidHeader = FALSE,
          width       = 12,
          height      = 400,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id   = ns("example_tabs"),
            type = "tabs",
            tabPanel(
              "Reference IDs",
              tableOutput(ns("example_ids")),
              br(),
              downloadButton(ns("download_ids"), "Download Sample Reference IDs"),
              style = "overflow-y: auto; height: 350px"
            ),
            tabPanel(
              "Genotypes",
              tableOutput(ns("example_genos")),
              br(),
              downloadButton(ns("download_genos"), "Download Sample Genotypes"),
              style = "overflow-y: auto; height: 350px"
            )
          )
        )
      ),
      
      #  Column 3: Status + Plot Controls
      shiny::column(
        width = 3,
        bs4Dash::box(
          title       = "Status",
          width       = 12,
          collapsible = TRUE,
          status      = "info",
          shiny::verbatimTextOutput(ns("status"))
        ),
        box(
          title       = "Plot Controls",
          width       = 12,
          status      = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectInput(
            ns("color_choice"), "Color Palette",
            choices = list(
              "Standard Palettes"   = c("Set1","Set3","Pastel2","Pastel1","Accent","Spectral","RdYlGn","RdGy"),
              "Colorblind Friendly" = c("Set2","Paired","Dark2","YlOrRd","YlOrBr","YlGnBu","YlGn",
                                        "Reds","RdPu","Purples","PuRd","PuBuGn","PuBu","OrRd",
                                        "Oranges","Greys","Greens","GnBu","BuPu","BuGn","Blues",
                                        "RdYlBu","RdBu","PuOr","PRGn","PiYG","BrBG")
            ),
            selected = "Set1"
          ),
          checkboxInput(ns("poly_show_sample_labels"), "Show sample labels",     value = FALSE),
          checkboxInput(ns("poly_sort_by_predicted"),  "Sort by predicted line", value = TRUE),
          sliderInput(ns("poly_label_size"), "Label size", min = 6, max = 14, value = 8, step = 1),
          div(
            style = "display:inline-block; float:left",
            dropdownButton(
              tags$h3("Save"),
              selectInput(ns("poly_image_type"), "File Type",
                          choices  = c("png", "jpeg", "svg", "pdf"),
                          selected = "png"),
              sliderInput(ns("poly_image_res"),    "Resolution (DPI)", value = 300, min = 50,  max = 1000, step = 50),
              sliderInput(ns("poly_image_width"),  "Width (in)",       value = 10,  min = 3,   max = 30,   step = 0.5),
              sliderInput(ns("poly_image_height"), "Height (in)",      value = 5,   min = 3,   max = 20,   step = 0.5),
              circle  = FALSE,
              status  = "info",
              icon    = icon("sliders"),
              width   = "300px",
              label   = "Image Options",
              tooltip = tooltipOptions(title = "File type, resolution and size")
            )
          ),
          # Download button kept outside the dropdown so the link is always active
          div(
            style = "display:inline-block; float:left; margin-left: 8px;",
            downloadButton(ns("download_poly_figure"), "Save Image", class = "btn-danger")
          )
        )
      )
    )
  )
}

#' PolyBreedTools Server Functions
#'
#' @importFrom graphics axis hist points
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom scales comma_format
#' @import openxlsx
#' @import BIGpopA
#'
#' @noRd
mod_polybreedtools_server <- function(input, output, session, parent_session) {
  ns <- session$ns
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
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
  
  #  Help button
  shiny::observeEvent(input$help_btn, {
    shiny::showModal(
      shiny::modalDialog(
        title     = shiny::tagList(shiny::icon("circle-question"), " PolyBreedTools - Help"),
        size      = "l",
        easyClose = TRUE,
        footer    = shiny::modalButton("Close"),
        help_content_polybreedtools(collapse_fn = make_collapse_panel, id_prefix = "modal")
      )
    )
  })
  
  format_percent <- function(x) {
    scales::percent_format(accuracy = 0.1)(x)
  }
  
  #  PLINK .map inputs: shown when the matching genotype upload is a .ped
  output$reference_map_ui <- renderUI({
    req(input$reference_file)
    if (genotype_upload_format(input$reference_file) != "ped") return(NULL)
    fileInput(ns("reference_map"), "Reference Map (.map)", accept = ".map")
  })
  output$validation_map_ui <- renderUI({
    req(input$validation_file)
    if (genotype_upload_format(input$validation_file) != "ped") return(NULL)
    fileInput(ns("validation_map"), "Validation Map (.map)", accept = ".map")
  })

  # Track maps separately so a new .ped upload never reuses an old .map
  ped_maps <- reactiveValues(reference = NULL, validation = NULL)
  observeEvent(input$reference_file,  { ped_maps$reference  <- NULL })
  observeEvent(input$validation_file, { ped_maps$validation <- NULL })
  observeEvent(input$reference_map,   { ped_maps$reference  <- input$reference_map })
  observeEvent(input$validation_map,  { ped_maps$validation <- input$validation_map })

  result_data <- reactiveVal(NULL)
  poly_items  <- reactiveValues(
    prediction        = NULL,   # numeric proportions, samples in rows
    pred_results      = NULL,
    pred_results_long = NULL,
    id_order          = NULL
  )

  #  Line assignment: the line with the highest proportion, or "Undetermined"
  #  when that proportion is below the assignment threshold. Recomputed when
  #  the threshold changes, without re-running the estimation.
  observe({
    req(poly_items$prediction)
    prediction        <- poly_items$prediction
    columns_to_select <- colnames(prediction)

    threshold <- suppressWarnings(as.numeric(input$assign_threshold))
    if (length(threshold) == 0 || is.na(threshold)) threshold <- 0
    threshold <- min(max(threshold, 0), 100) / 100

    top_value      <- apply(prediction[, columns_to_select, drop = FALSE], 1, max, na.rm = TRUE)
    top_line       <- columns_to_select[max.col(prediction[, columns_to_select, drop = FALSE], ties.method = "first")]
    predicted_line <- ifelse(top_value >= threshold, top_line, "Undetermined")

    pred_results <- tibble::rownames_to_column(prediction, var = "ID")
    pred_results <- dplyr::mutate(pred_results, `Predicted line` = predicted_line)
    pred_results <- dplyr::mutate(pred_results, dplyr::across(dplyr::all_of(columns_to_select), ~format_percent(.x)))

    id_order <- data.frame(
      ID              = rownames(prediction),
      predicted_line  = predicted_line,
      predicted_value = top_value,
      stringsAsFactors = FALSE
    )

    pred_results_long <- tibble::rownames_to_column(prediction, var = "ID")
    pred_results_long <- tidyr::pivot_longer(
      pred_results_long,
      cols      = dplyr::all_of(columns_to_select),
      names_to  = "category",
      values_to = "percent"
    )
    pred_results_long$predicted_line <- id_order$predicted_line[match(pred_results_long$ID, id_order$ID)]

    result_data(pred_results)
    poly_items$pred_results      <- pred_results
    poly_items$pred_results_long <- pred_results_long
    poly_items$id_order          <- id_order
  })

  output$preview <- DT::renderDT({
    req(poly_items$pred_results)
    DT::datatable(poly_items$pred_results, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #  Run estimation
  observeEvent(input$run, {
    req(input$reference_file, input$ref_ids_file, input$validation_file)
    shinyjs::disable("download_poly_all")
    output$status <- renderText("Running estimation...")
    
    tryCatch({
      # .ped allele letters can only be coded consistently against another .ped,
      #   and each .ped needs its .map so markers are matched by name
      ref_is_ped <- genotype_upload_format(input$reference_file)  == "ped"
      val_is_ped <- genotype_upload_format(input$validation_file) == "ped"
      if (xor(ref_is_ped, val_is_ped)) {
        stop("When using a PLINK .ped file, both the reference and validation genotypes must be .ped files.")
      }
      if (ref_is_ped && is.null(ped_maps$reference)) {
        stop("Upload the .map file for the reference .ped.")
      }
      if (val_is_ped && is.null(ped_maps$validation)) {
        stop("Upload the .map file for the validation .ped.")
      }

      reference      <- read_genotype_upload(input$reference_file, ploidy = input$ploidy, id_name = "ID",
                                             map_input = ped_maps$reference)
      counted_allele <- attr(reference, "counted_allele")   # NULL unless .ped
      reference      <- dplyr::distinct(reference, ID, .keep_all = TRUE)
      reference <- tibble::column_to_rownames(reference, "ID")
      
      reference_ids <- utils::read.table(input$ref_ids_file$datapath, header = TRUE, sep = "\t")
      ref_ids       <- lapply(as.list(reference_ids), as.character)
      
      # Validation .ped is coded with the reference's counted allele per marker
      validation_raw <- read_genotype_upload(input$validation_file, ploidy = input$ploidy, id_name = "ID",
                                             counted_allele = counted_allele,
                                             map_input = ped_maps$validation)

      # Markers are matched by name, so the two files must share some
      shared_markers <- intersect(setdiff(names(reference), "ID"),
                                  setdiff(names(validation_raw), "ID"))
      if (length(shared_markers) == 0) {
        stop("No marker names are shared between the reference and validation genotypes.")
      }
      
      validation_markers  <- validation_raw[, colnames(validation_raw) != "ID", drop = FALSE]
      sample_call_rate    <- rowSums(!is.na(validation_markers)) / ncol(validation_markers)
      removed_samples     <- validation_raw$ID[sample_call_rate < 0.5]
      validation_filtered <- validation_raw[sample_call_rate >= 0.5, , drop = FALSE]
      
      if (nrow(validation_filtered) == 0) {
        stop("No validation samples remain after filtering for genotyping rate >= 50%.")
      }
      
      validation_marker_filtered <- validation_filtered[, colnames(validation_filtered) != "ID", drop = FALSE]
      col_call_counts <- colSums(!is.na(validation_marker_filtered))
      removed_markers <- colnames(validation_marker_filtered)[col_call_counts == 0]
      validation      <- validation_filtered[, c(TRUE, col_call_counts > 0), drop = FALSE]
      
      warning_messages <- c()
      if (length(removed_samples) > 0) {
        warning_messages <- c(warning_messages, paste(
          "WARNING: The following validation samples were removed due to genotyping rate < 50%:\n",
          paste0("  \u2022 ", removed_samples, collapse = "\n")
        ))
      }
      if (length(removed_markers) > 0) {
        warning_messages <- c(warning_messages, paste(
          "WARNING: The following markers were removed from validation because they had no successful genotype calls:\n",
          paste0("  \u2022 ", removed_markers, collapse = "\n")
        ))
      }
      
      val_ids <- validation[, 1]
      dup_val <- val_ids[duplicated(val_ids)]
      if (length(dup_val) > 0) {
        dup_val_msg <- paste(
          "Error: The following sample IDs have duplicates in your validation file.",
          "Please check your input file and remove or rename the following IDs:\n",
          paste0("  \u2022 ", dup_val, collapse = "\n")
        )
        output$status <- renderText(dup_val_msg)
        return()
      }
      
      validation <- dplyr::distinct(validation, ID, .keep_all = TRUE)
      validation <- tibble::column_to_rownames(validation, "ID")
      
      freq <- BIGpopA::allele_freq_poly(reference, ref_ids, ploidy = input$ploidy)
      
      na_pos <- which(is.na(freq), arr.ind = TRUE)
      if (nrow(na_pos) > 0) {
        na_report <- lapply(unique(na_pos[, 2]), function(col_idx) {
          rows_with_na <- na_pos[na_pos[, 2] == col_idx, 1]
          paste0(
            "  \u2022 ", colnames(freq)[col_idx], ": ",
            paste(rownames(freq)[rows_with_na], collapse = ", ")
          )
        })
        NaN_freq_msg <- paste(
          "Error: The following markers were not successfully genotyped for at least one reference population, please remove or correct them:",
          paste(na_report, collapse = "\n"),
          "\nPlease remove or correct these markers", sep = "\n"
        )
        output$status <- renderText(NaN_freq_msg)
        return()
      }
      
      prediction <- BIGpopA::solve_composition_poly(validation, freq, ploidy = input$ploidy)
      prediction <- as.data.frame(prediction, check.names = FALSE)
      prediction <- prediction[, !colnames(prediction) %in% c("R2"), drop = FALSE]
      prediction[] <- lapply(prediction, as.numeric)

      # Line assignment is derived reactively from the stored proportions
      #   (see "Line assignment" below), so the threshold applies without re-running
      poly_items$prediction <- prediction

      final_status <- "Estimation complete. File ready for download."
      if (length(warning_messages) > 0) {
        final_status <- paste(final_status, "\n\n", paste(warning_messages, collapse = "\n\n"))
      }
      output$status <- renderText(final_status)
      shinyjs::enable("download_poly_all")
      
    }, error = function(e) {
      output$status <- renderText(paste("Error during estimation:", e$message))
    })
  })
  
  #  Ancestry plot
  ancestry_plot <- reactive({
    req(poly_items$pred_results_long, poly_items$id_order)
    dat <- poly_items$pred_results_long
    
    if (isTRUE(input$poly_sort_by_predicted)) {
      # Group by predicted line (Undetermined last), highest proportion first
      io     <- poly_items$id_order
      ord    <- io[order(io$predicted_line == "Undetermined", io$predicted_line, -io$predicted_value), , drop = FALSE]
      dat$ID <- factor(dat$ID, levels = ord$ID)
    } else {
      dat$ID <- factor(dat$ID, levels = unique(dat$ID))
    }
    
    p <- ggplot(dat, aes(x = ID, y = percent, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = input$color_choice) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Individual ID", y = "Ancestry Proportion", fill = "Line") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45, hjust = 1,
          size  = as.numeric(input$poly_label_size %||% 8)
        )
      )
    
    if (!isTRUE(input$poly_show_sample_labels)) {
      p <- p + theme(
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    }
    p
  })
  
  output$bar_plot <- renderPlot({
    req(poly_items$pred_results_long)
    ancestry_plot()
  })
  
  #  Unified data download (Excel only)
  output$download_poly_all <- shiny::downloadHandler(
    filename = function() {
      paste0("polybreedtools_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(poly_items$pred_results)
      
      tmp_dir <- tempfile("poly_export")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      
      openxlsx::write.xlsx(
        poly_items$pred_results,
        file     = file.path(tmp_dir, paste0("lineage_estimation_", Sys.Date(), ".xlsx")),
        rowNames = FALSE
      )
      
      zip_files <- list.files(tmp_dir)
      zip::zip(zipfile = file, files = zip_files, root = tmp_dir)
      unlink(tmp_dir, recursive = TRUE)
    },
    contentType = "application/zip"
  )
  
  #  Figure download (unchanged from original)
  output$download_poly_figure <- downloadHandler(
    filename = function() {
      ext <- input$poly_image_type %||% "png"
      paste0("polybreedtools_ancestry_plot_", format(Sys.Date(), "%Y-%m-%d"), ".", ext)
    },
    content = function(file) {
      req(poly_items$pred_results_long)
      p      <- ancestry_plot()
      ext    <- input$poly_image_type   %||% "png"
      width  <- as.numeric(input$poly_image_width  %||% 10)
      height <- as.numeric(input$poly_image_height %||% 5)
      dpi    <- as.numeric(input$poly_image_res    %||% 300)
      save_plot_file(p, file, ext, width = width, height = height, dpi = dpi)
    }
  )
  # The button sits in a dropdown hidden at start-up; keep its link active
  shiny::outputOptions(output, "download_poly_figure", suspendWhenHidden = FALSE)
  
  #  Example tables
  example_ids_df <- data.frame(
    Group1 = c("SampleAlpha", "S3", "ExampleFour", "", ""),
    Group2 = c("SampleOne", "SampleTwo", "SampleThree", "SampleFour", "SampleFive"),
    Group3 = c("SampleX", "SampleYy", "SampleZzzz", "ExampleEight", "")
  )
  example_genos_df <- data.frame(
    ID      = paste0("Sample", c("1", "2", "3", "4", "5")),
    Marker1 = as.integer(c(0, 0, 1, 2, 1)),
    Marker2 = as.integer(c(NA, 1, 0, 1, 2)),
    Marker3 = as.integer(c(0, 0, NA, 1, 1)),
    Marker4 = as.integer(c(0, 0, 0, 0, 0))
  )
  
  output$example_ids   <- renderTable({ example_ids_df   }, bordered = TRUE)
  output$example_genos <- renderTable({ example_genos_df }, bordered = TRUE)
  
  output$download_ids <- downloadHandler(
    filename = function() "sample_reference_ids.txt",
    content  = function(file) write.table(example_ids_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
  )
  output$download_genos <- downloadHandler(
    filename = function() "sample_genotypes.txt",
    content  = function(file) write.table(example_genos_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
  )
}

## To be copied in the UI
# mod_polybreedtools_ui("polybreedtools_1")

## To be copied in the server
# mod_polybreedtools_server("polybreedtools_1")
