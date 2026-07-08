#' SNMF UI Function
#'
#' @description A Shiny module implementing LEA::snmf() for unsupervised ancestry estimation.
#'
#' @param id Module id.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydisconnect
#' @importFrom DT DTOutput renderDT
#' @importFrom bs4Dash valueBoxOutput
mod_SNMF_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shinydisconnect::disconnectMessage(
        text           = "An unexpected error occurred, please reload the application and check the input file(s).",
        refresh        = "Reload now",
        background     = "white",
        colour         = "grey",
        overlayColour  = "grey",
        overlayOpacity = 0.3,
        refreshColour  = "purple"
      ),
      
      #  Column 1: Inputs
      shiny::column(
        width = 3,
        bs4Dash::box(
          title       = "Inputs",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = TRUE,
          shiny::fileInput(
            ns("snmf_file"),
            "Genotypes (.vcf, .vcf.gz, .geno)",
            accept = c(".vcf", ".gz", ".geno")
          ),
          shiny::numericInput(ns("snmf_ploidy"),      "Ploidy",      value = 2,    min = 1,  step = 1),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("snmf_k_min"), "K min", value = 1,  min = 1, step = 1)),
            shiny::column(6, shiny::numericInput(ns("snmf_k_max"), "K max", value = 10, min = 1, step = 1))
          ),
          shiny::numericInput(ns("snmf_repetitions"), "Repetitions", value = 5,    min = 1,    step = 1),
          shiny::numericInput(ns("snmf_alpha"),       "Alpha",       value = 100,  min = 0),
          shiny::numericInput(ns("snmf_iterations"),  "Iterations",  value = 200,  min = 1,    step = 1),
          shiny::numericInput(ns("snmf_tolerance"),   "Tolerance",   value = 1e-4, min = 0),
          shiny::numericInput(ns("snmf_percentage"),  "Percentage",  value = 0.05, min = 0, max = 1, step = 0.01),
          shiny::numericInput(ns("snmf_cpu"),         "CPU",         value = 1,    min = 1,    step = 1),
          shiny::numericInput(ns("snmf_seed"),        "Seed",        value = 123,  min = 1,    step = 1),
          shiny::radioButtons(
            ns("snmf_select_mode"),
            "Selection mode",
            choices = c(
              "Auto-pick best K (cross-entropy)" = "auto_entropy",
              "Manual K/run (cross-entropy)"     = "manual_entropy",
              "No cross-entropy (manual)"        = "no_entropy"
            ),
            selected = "auto_entropy"
          ),
          shiny::actionButton(ns("snmf_run"), "Run SNMF"),
          shiny::hr(),
          shinyjs::disabled(
            shiny::downloadButton(ns("download_snmf_all"), "Download Results")
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
      shiny::column(
        width = 6,
        bs4Dash::box(
          title       = "Results",
          status      = "info",
          solidHeader = FALSE,
          width       = 12,
          height      = 600,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id   = ns("snmf_results_tabs"),
            type = "tabs",
            shiny::tabPanel(
              "Instructions",
              shiny::HTML(paste0(
                "<p>This tab runs <code>LEA::snmf()</code> to estimate ancestry proportions (Q-matrix) across K.</p>",
                "<ul>",
                "<li>Upload a <code>.vcf</code> / <code>.vcf.gz</code> (will be converted to LEA <code>.geno</code>) or an existing <code>.geno</code>.</li>",
                "<li>Choose a K range and repetitions.</li>",
                "<li>In cross-entropy modes, the app summarizes cross-entropy per K and can auto-pick the best K/run.</li>",
                "</ul>"
              )),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel(
              "Cross-Entropy",
              shiny::plotOutput(ns("snmf_ce_plot"), height = "420px"),
              DT::DTOutput(ns("snmf_ce_table")),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel(
              "Ancestry Plot",
              shiny::plotOutput(ns("snmf_q_plot"), height = "450px"),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel(
              "Q Matrix",
              DT::DTOutput(ns("snmf_q_table")),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel(
              "Logs",
              shiny::verbatimTextOutput(ns("snmf_status")),
              style = "overflow-y: auto; height: 500px"
            )
          )
        )
      ),
      
      #  Column 3: Status + Plot Controls
      shiny::column(
        width = 3,
        bs4Dash::valueBoxOutput(ns("snmf_best_k_box"),  width = NULL),
        bs4Dash::valueBoxOutput(ns("snmf_best_ce_box"), width = NULL),
        bs4Dash::box(
          title       = "Status",
          width       = 12,
          collapsible = TRUE,
          status      = "info",
          shinyWidgets::progressBar(
            id          = ns("pb_snmf"),
            value       = 0,
            status      = "info",
            display_pct = TRUE,
            striped     = TRUE,
            title       = " "
          )
        ),
        bs4Dash::box(
          title       = "Plot Controls",
          width       = 12,
          status      = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          shiny::uiOutput(ns("snmf_selectors_ui")),
          shiny::selectInput(
            ns("snmf_color_choice"), "Color Palette",
            choices = list(
              "Standard Palettes"   = c("Set1","Set3","Pastel2","Pastel1","Accent","Spectral","RdYlGn","RdGy"),
              "Colorblind Friendly" = c("Set2","Paired","Dark2","YlOrRd","YlOrBr","YlGnBu","YlGn",
                                        "Reds","RdPu","Purples","PuRd","PuBuGn","PuBu","OrRd",
                                        "Oranges","Greys","Greens","GnBu","BuPu","BuGn","Blues",
                                        "RdYlBu","RdBu","PuOr","PRGn","PiYG","BrBG")
            ),
            selected = "Set1"
          ),
          shiny::checkboxInput(ns("snmf_show_sample_labels"), "Show sample labels",       value = TRUE),
          shiny::checkboxInput(ns("snmf_sort_by_cluster"),    "Sort by dominant cluster", value = FALSE),
          shiny::sliderInput(ns("snmf_label_size"), "Label size", min = 6, max = 14, value = 8, step = 1),
          shiny::div(
            style = "display:inline-block; float:left",
            shinyWidgets::dropdownButton(
              shiny::tags$h3("Save Image"),
              shiny::selectInput(ns("snmf_figure"),     "Figure",    choices = c("Cross-Entropy Plot", "Ancestry Plot")),
              shiny::selectInput(ns("snmf_image_type"), "File Type", choices = c("jpeg", "tiff", "png", "svg"), selected = "jpeg"),
              shiny::sliderInput(ns("snmf_image_res"),    "Resolution", value = 300, min = 50,  max = 1000, step = 50),
              shiny::sliderInput(ns("snmf_image_width"),  "Width",      value = 8,   min = 1,   max = 20,   step = 0.5),
              shiny::sliderInput(ns("snmf_image_height"), "Height",     value = 5,   min = 1,   max = 20,   step = 0.5),
              shiny::downloadButton(ns("download_snmf_figure"), "Save Image"),
              circle  = FALSE,
              status  = "danger",
              icon    = shiny::icon("floppy-disk"),
              width   = "300px",
              label   = "Save Plot",
              tooltip = shinyWidgets::tooltipOptions(title = "Click to see inputs!")
            )
          )
        )
      )
    )
  )
}

#' SNMF Server Functions
#'
#' @noRd
mod_SNMF_server <- function(input, output, session, parent_session) {
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
        title     = shiny::tagList(shiny::icon("circle-question"), " SNMF — Help"),
        size      = "l",
        easyClose = TRUE,
        footer    = shiny::modalButton("Close"),
        help_content_SNMF(collapse_fn = make_collapse_panel, id_prefix = "modal")
      )
    )
  })
  
  set_status <- function(...) {
    msg <- paste0(...)
    output$snmf_status <- shiny::renderText(msg)
  }
  
  show_error <- function(title, message) {
    shiny::showModal(shiny::modalDialog(
      title     = title,
      easyClose = TRUE,
      footer    = shiny::modalButton("Close"),
      message
    ))
  }
  
  call_with_allowed_named_args <- function(fun, args) {
    allowed <- names(formals(fun))
    if (is.null(allowed)) return(do.call(fun, args))
    keep <- names(args) == "" | names(args) %in% allowed
    do.call(fun, args[keep])
  }
  
  same_path <- function(path_a, path_b) {
    identical(
      normalizePath(path_a, winslash = "/", mustWork = FALSE),
      normalizePath(path_b, winslash = "/", mustWork = FALSE)
    )
  }
  
  copy_file_if_needed <- function(from, to, overwrite = TRUE) {
    if (same_path(from, to)) return(to)
    ok <- file.copy(from, to, overwrite = overwrite)
    if (!isTRUE(ok)) stop("Failed to copy file from ", from, " to ", to, call. = FALSE)
    to
  }
  
  write_vcf_upload_as_geno <- function(vcf_path, geno_path) {
    vcf <- vcfR::read.vcfR(vcf_path, verbose = FALSE)
    gt  <- as.matrix(vcfR::extract.gt(vcf, element = "GT"))
    if (nrow(gt) == 0 || ncol(gt) == 0) {
      stop("No genotype calls were found in the uploaded VCF.", call. = FALSE)
    }
    dosage_cols <- lapply(seq_len(ncol(gt)), function(i) BIGpopA:::convert_to_dosage(gt[, i]))
    dosage_mat  <- do.call(cbind, dosage_cols)
    colnames(dosage_mat) <- colnames(gt)
    rownames(dosage_mat) <- rownames(gt)
    lea_mat <- t(dosage_mat)
    lea_mat[is.na(lea_mat)] <- 9
    storage.mode(lea_mat) <- "integer"
    LEA::write.geno(lea_mat, geno_path)
    list(geno_path = geno_path, sample_ids = colnames(gt))
  }
  
  run_ctx <- new.env(parent = emptyenv())
  run_ctx$run_dir <- NULL
  
  cleanup_run_dir <- function(path = run_ctx$run_dir) {
    if (!is.null(path) && dir.exists(path)) {
      unlink(path, recursive = TRUE, force = TRUE)
    }
    if (identical(run_ctx$run_dir, path)) run_ctx$run_dir <- NULL
  }
  
  state <- shiny::reactiveValues(
    run_dir         = NULL,
    project         = NULL,
    geno_path       = NULL,
    vcf_path        = NULL,
    k_values        = NULL,
    repetitions     = NULL,
    entropy_enabled = FALSE,
    ce_df           = NULL,
    ce_summary      = NULL,
    best_k          = NULL,
    best_run_by_k   = NULL,
    sample_ids      = NULL
  )
  
  #  Value boxes
  output$snmf_best_k_box <- bs4Dash::renderValueBox({
    bs4Dash::valueBox(
      value    = if (!is.null(state$best_k)) state$best_k else "\u2014",
      subtitle = "Best K",
      icon     = shiny::icon("layer-group"),
      color    = "info"
    )
  })
  
  output$snmf_best_ce_box <- bs4Dash::renderValueBox({
    bs4Dash::valueBox(
      value = if (isTRUE(state$entropy_enabled) && !is.null(state$ce_summary)) {
        best_k   <- state$best_k
        best_row <- state$ce_summary[state$ce_summary$K == best_k, , drop = FALSE]
        if (nrow(best_row) == 1) round(best_row$min_cross_entropy, 6) else "\u2014"
      } else {
        "\u2014"
      },
      subtitle = "Min cross-entropy",
      icon     = shiny::icon("chart-line"),
      color    = "olive"
    )
  })
  
  #  Selectors UI
  output$snmf_selectors_ui <- shiny::renderUI({
    if (is.null(state$project) || is.null(state$k_values) || is.null(state$repetitions)) {
      return(shiny::HTML("<em>Run SNMF to enable K/run selectors and downloads.</em>"))
    }
    shiny::tagList(
      shiny::selectInput(
        ns("snmf_selected_k"),
        "Selected K",
        choices  = as.character(state$k_values),
        selected = as.character(state$best_k %||% state$k_values[[1]])
      ),
      shiny::selectInput(
        ns("snmf_selected_run"),
        "Selected run",
        choices  = as.character(seq_len(state$repetitions)),
        selected = "1"
      )
    )
  })
  
  shiny::observeEvent(input$snmf_selected_k, {
    req(state$project, state$k_values, state$repetitions)
    k            <- as.integer(input$snmf_selected_k)
    selected_run <- 1L
    if (!is.null(state$best_run_by_k) && !is.na(state$best_run_by_k[as.character(k)])) {
      selected_run <- as.integer(state$best_run_by_k[as.character(k)])
    }
    shiny::updateSelectInput(
      session, "snmf_selected_run",
      choices  = as.character(seq_len(state$repetitions)),
      selected = as.character(selected_run)
    )
  }, ignoreInit = TRUE)
  
  selected_k <- shiny::reactive({
    req(state$project, state$k_values)
    k <- input$snmf_selected_k
    if (is.null(k) || !nzchar(k)) return(as.integer(state$best_k %||% state$k_values[[1]]))
    as.integer(k)
  })
  
  selected_run <- shiny::reactive({
    req(state$project, state$repetitions)
    r <- input$snmf_selected_run
    if (is.null(r) || !nzchar(r)) return(1L)
    as.integer(r)
  })
  
  #  Q matrix reactive
  q_matrix <- shiny::reactive({
    req(state$project)
    k <- selected_k()
    r <- selected_run()
    q <- call_with_allowed_named_args(LEA::Q, list(state$project, K = k, run = r))
    q <- as.matrix(q)
    if (!is.null(state$sample_ids) && length(state$sample_ids) == nrow(q)) {
      rownames(q) <- state$sample_ids
    } else if (is.null(rownames(q))) {
      rownames(q) <- paste0("ind", seq_len(nrow(q)))
    }
    colnames(q) <- paste0("Cluster", seq_len(ncol(q)))
    q
  })
  
  #  Shared plot reactives
  ce_plot <- shiny::reactive({
    shiny::validate(shiny::need(isTRUE(state$entropy_enabled), "Cross-entropy disabled (see Selection mode)."))
    shiny::validate(shiny::need(!is.null(state$ce_summary),    "Run SNMF to compute cross-entropy."))
    ggplot2::ggplot(state$ce_summary, ggplot2::aes(x = K, y = min_cross_entropy)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = "K", y = "Minimum cross-entropy", title = "SNMF cross-entropy by K") +
      ggplot2::theme_minimal()
  })
  
  ancestry_plot <- shiny::reactive({
    q      <- q_matrix()
    df     <- data.frame(ID = rownames(q), q, check.names = FALSE)
    q_cols <- colnames(q)
    long   <- stats::reshape(
      df,
      varying   = q_cols,
      v.names   = "Q",
      timevar   = "Cluster",
      times     = q_cols,
      direction = "long"
    )
    long$Cluster <- factor(long$Cluster, levels = q_cols)
    
    if (isTRUE(input$snmf_sort_by_cluster)) {
      q_wide           <- as.data.frame(q)
      dominant_cluster <- colnames(q_wide)[max.col(q_wide, ties.method = "first")]
      dominant_value   <- apply(q_wide, 1, max, na.rm = TRUE)
      id_order <- data.frame(
        ID               = rownames(q),
        dominant_cluster = dominant_cluster,
        dominant_value   = dominant_value,
        stringsAsFactors = FALSE
      )
      id_order <- id_order[order(id_order$dominant_cluster, -id_order$dominant_value), ]
      long$ID  <- factor(long$ID, levels = id_order$ID)
    } else {
      long$ID <- factor(long$ID, levels = unique(df$ID))
    }
    
    palette_name <- input$snmf_color_choice %||% "Set1"
    palette_info <- RColorBrewer::brewer.pal.info[palette_name, , drop = FALSE]
    max_colors   <- palette_info$maxcolors[[1]]
    n_base       <- max(3L, max_colors)
    base_colors  <- RColorBrewer::brewer.pal(n_base, palette_name)
    fill_colors  <- grDevices::colorRampPalette(base_colors)(length(q_cols))
    
    p <- ggplot2::ggplot(long, ggplot2::aes(x = ID, y = Q, fill = Cluster)) +
      ggplot2::geom_col(width = 0.9) +
      ggplot2::scale_fill_manual(values = fill_colors, drop = FALSE) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(x = "Individual", y = "Ancestry proportion", fill = "Cluster") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x        = ggplot2::element_text(
          angle = 45, hjust = 1, vjust = 1,
          size  = as.numeric(input$snmf_label_size %||% 8)
        ),
        panel.grid.major.x = ggplot2::element_blank()
      )
    
    if (!isTRUE(input$snmf_show_sample_labels)) {
      p <- p + ggplot2::theme(
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
    }
    p
  })
  
  #  Render outputs
  output$snmf_q_table <- DT::renderDT({
    q  <- q_matrix()
    df <- data.frame(ID = rownames(q), q, check.names = FALSE)
    DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$snmf_q_plot  <- shiny::renderPlot({ ancestry_plot() })
  output$snmf_ce_plot <- shiny::renderPlot({ ce_plot() })
  
  output$snmf_ce_table <- DT::renderDT({
    shiny::validate(shiny::need(isTRUE(state$entropy_enabled), "Cross-entropy disabled (see Selection mode)."))
    shiny::validate(shiny::need(!is.null(state$ce_summary),    "Run SNMF to compute cross-entropy."))
    DT::datatable(state$ce_summary, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #  Run SNMF
  shiny::observeEvent(input$snmf_run, {
    if (!requireNamespace("LEA", quietly = TRUE)) {
      show_error("Missing dependency", "Install the LEA package to use SNMF.")
      return()
    }
    if (is.null(input$snmf_file$datapath)) {
      show_error("Missing input", "Upload a .vcf/.vcf.gz or .geno file.")
      return()
    }
    k_min <- as.integer(input$snmf_k_min)
    k_max <- as.integer(input$snmf_k_max)
    if (is.na(k_min) || is.na(k_max) || k_min < 1 || k_max < 1 || k_min > k_max) {
      show_error("Invalid K range", "K min and K max must be integers with K min \u2264 K max and both \u2265 1.")
      return()
    }
    reps <- as.integer(input$snmf_repetitions)
    if (is.na(reps) || reps < 1) {
      show_error("Invalid repetitions", "Repetitions must be an integer \u2265 1.")
      return()
    }
    ploidy <- as.integer(input$snmf_ploidy)
    if (is.na(ploidy) || ploidy < 1) {
      show_error("Invalid ploidy", "Ploidy must be an integer \u2265 1.")
      return()
    }
    entropy_enabled <- input$snmf_select_mode %in% c("auto_entropy", "manual_entropy")
    cleanup_run_dir()
    
    state$run_dir         <- tempfile("snmf_", tmpdir = tempdir())
    run_ctx$run_dir       <- state$run_dir
    dir.create(state$run_dir, recursive = TRUE, showWarnings = FALSE)
    state$project         <- NULL
    state$geno_path       <- NULL
    state$vcf_path        <- NULL
    state$k_values        <- seq(k_min, k_max)
    state$repetitions     <- reps
    state$entropy_enabled <- entropy_enabled
    state$ce_df           <- NULL
    state$ce_summary      <- NULL
    state$best_k          <- NULL
    state$best_run_by_k   <- NULL
    state$sample_ids      <- NULL
    
    shinyjs::disable("download_snmf_all")
    shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 5,  title = "Preparing input")
    set_status("Preparing input...\n")
    
    uploaded_name <- input$snmf_file$name %||% "genotypes"
    ext_lower     <- tolower(uploaded_name)
    file_base     <- sub("\\.(vcf\\.gz|vcf|geno|gz)$", "", basename(uploaded_name), ignore.case = TRUE)
    geno_path     <- file.path(state$run_dir, paste0(file_base, ".geno"))
    uploaded_path <- input$snmf_file$datapath
    
    if (grepl("\\.geno$", ext_lower)) {
      copy_file_if_needed(uploaded_path, geno_path, overwrite = TRUE)
    } else if (grepl("\\.vcf\\.gz$|\\.vcf$|\\.gz$", ext_lower)) {
      shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 15, title = "Converting VCF \u2192 GENO")
      set_status("Converting VCF to GENO...\n")
      vcf_to_geno_res <- tryCatch(
        write_vcf_upload_as_geno(uploaded_path, geno_path),
        error = function(e) e
      )
      if (!file.exists(geno_path)) {
        geno_candidates <- list.files(state$run_dir, pattern = "\\.geno$", full.names = TRUE)
        if (length(geno_candidates) >= 1) {
          newest <- geno_candidates[which.max(file.info(geno_candidates)$mtime)]
          copy_file_if_needed(newest, geno_path, overwrite = TRUE)
        }
      }
      if (!file.exists(geno_path)) {
        msg <- if (inherits(vcf_to_geno_res, "error")) vcf_to_geno_res$message else "VCF conversion did not produce a .geno file."
        show_error("VCF conversion failed", msg)
        shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 0, title = " ")
        set_status(paste0("ERROR: ", msg, "\n"))
        return()
      }
      if (is.list(vcf_to_geno_res) && !is.null(vcf_to_geno_res$sample_ids)) {
        state$sample_ids <- vcf_to_geno_res$sample_ids
      }
    } else {
      show_error("Unsupported file type", "Upload a .vcf, .vcf.gz, or .geno file.")
      shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 0, title = " ")
      set_status("ERROR: Unsupported file type.\n")
      return()
    }
    
    state$geno_path <- geno_path
    state$vcf_path  <- NULL
    
    shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 35, title = "Running SNMF")
    set_status(
      "Running SNMF...\n",
      "Input GENO: ", basename(state$geno_path), "\n",
      "K: ", k_min, "\u2013", k_max, "\n",
      "Repetitions: ", reps, "\n",
      "Entropy: ", if (entropy_enabled) "enabled" else "disabled", "\n"
    )
    
    # Run SNMF from the run directory; withr restores the previous working
    # directory automatically when this reactive exits (CRAN-safe).
    withr::local_dir(state$run_dir)
    
    snmf_args <- list(
      state$geno_path,
      K           = state$k_values,
      repetitions = reps,
      ploidy      = ploidy,
      entropy     = entropy_enabled,
      alpha       = input$snmf_alpha,
      iterations  = as.integer(input$snmf_iterations),
      tolerance   = as.numeric(input$snmf_tolerance),
      percentage  = as.numeric(input$snmf_percentage),
      CPU         = as.integer(input$snmf_cpu),
      seed        = as.integer(input$snmf_seed)
    )
    
    project <- tryCatch(
      call_with_allowed_named_args(LEA::snmf, snmf_args),
      error = function(e) e
    )
    if (inherits(project, "error")) {
      show_error("SNMF failed", project$message)
      shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 0, title = " ")
      set_status(paste0("ERROR: ", project$message, "\n"))
      return()
    }
    state$project <- project
    
    shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 75, title = "Summarizing results")
    set_status(paste0(capture.output(str(project, max.level = 1)), collapse = "\n"), "\n")
    
    if (entropy_enabled) {
      ce_records <- list()
      for (k in state$k_values) {
        for (r in seq_len(reps)) {
          ce_val <- tryCatch(
            call_with_allowed_named_args(LEA::cross.entropy, list(state$project, K = k, run = r)),
            error = function(e) NA_real_
          )
          ce_records[[length(ce_records) + 1]] <- data.frame(
            K             = as.integer(k),
            run           = as.integer(r),
            cross_entropy = as.numeric(ce_val),
            stringsAsFactors = FALSE
          )
        }
      }
      state$ce_df <- do.call(rbind, ce_records)
      
      min_ce_by_k   <- tapply(state$ce_df$cross_entropy, state$ce_df$K, min, na.rm = TRUE)
      best_run_by_k <- sapply(names(min_ce_by_k), function(k_chr) {
        k_int <- as.integer(k_chr)
        sub   <- state$ce_df[state$ce_df$K == k_int, , drop = FALSE]
        if (nrow(sub) == 0) return(NA_integer_)
        sub$run[which.min(sub$cross_entropy)]
      })
      state$best_run_by_k <- best_run_by_k
      
      ce_summary <- data.frame(
        K                 = as.integer(names(min_ce_by_k)),
        best_run          = as.integer(best_run_by_k[names(min_ce_by_k)]),
        min_cross_entropy = as.numeric(min_ce_by_k),
        stringsAsFactors  = FALSE
      )
      ce_summary       <- ce_summary[order(ce_summary$K), , drop = FALSE]
      state$ce_summary <- ce_summary
      state$best_k     <- ce_summary$K[which.min(ce_summary$min_cross_entropy)]
    } else {
      state$best_k <- state$k_values[[1]]
    }
    
    shiny::updateSelectInput(
      session, "snmf_selected_k",
      choices  = as.character(state$k_values),
      selected = as.character(state$best_k %||% state$k_values[[1]])
    )
    initial_run <- 1L
    if (!is.null(state$best_run_by_k)) {
      br <- state$best_run_by_k[as.character(state$best_k)]
      if (!is.na(br)) initial_run <- as.integer(br)
    }
    shiny::updateSelectInput(
      session, "snmf_selected_run",
      choices  = as.character(seq_len(reps)),
      selected = as.character(initial_run)
    )
    
    shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 100, title = "Complete!")
    set_status("SNMF complete.\n")
    shinyjs::enable("download_snmf_all")
  })
  
  #  Unified data download (Q CSV + cross-entropy CSV)
  output$download_snmf_all <- shiny::downloadHandler(
    filename = function() {
      paste0("snmf_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      shiny::req(state$project)
      
      tmp_dir <- tempfile("snmf_export")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      
      # Q matrix CSV
      q  <- q_matrix()
      df <- data.frame(ID = rownames(q), q, check.names = FALSE)
      utils::write.csv(df,
                       file.path(tmp_dir, paste0("snmf_Q_K", selected_k(),
                                                 "_run", selected_run(), ".csv")),
                       row.names = FALSE)
      
      # Cross-entropy CSV (only if entropy was enabled)
      if (isTRUE(state$entropy_enabled) && !is.null(state$ce_df)) {
        utils::write.csv(state$ce_df,
                         file.path(tmp_dir, "snmf_cross_entropy.csv"),
                         row.names = FALSE)
      }
      
      zip_files <- list.files(tmp_dir)
      zip::zip(zipfile = file, files = zip_files, root = tmp_dir)
      unlink(tmp_dir, recursive = TRUE)
    },
    contentType = "application/zip"
  )
  
  #  Figure download (unchanged from original)
  output$download_snmf_figure <- shiny::downloadHandler(
    filename = function() {
      ext <- input$snmf_image_type %||% "jpeg"
      fig <- input$snmf_figure %||% "Ancestry Plot"
      lbl <- if (fig == "Cross-Entropy Plot") "cross_entropy" else "ancestry_plot"
      paste0("snmf_", lbl, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      req(state$project)
      ext    <- input$snmf_image_type   %||% "jpeg"
      width  <- as.numeric(input$snmf_image_width  %||% 8)
      height <- as.numeric(input$snmf_image_height %||% 5)
      dpi    <- as.numeric(input$snmf_image_res    %||% 300)
      fig    <- input$snmf_figure %||% "Ancestry Plot"
      p <- if (fig == "Cross-Entropy Plot") ce_plot() else ancestry_plot()
      if (ext %in% c("png", "jpeg", "tiff")) {
        ggplot2::ggsave(filename = file, plot = p, width = width, height = height, units = "in", dpi = dpi)
      } else {
        ggplot2::ggsave(filename = file, plot = p, width = width, height = height, units = "in")
      }
    }
  )
  
  session$onSessionEnded(function() {
    cleanup_run_dir()
  })
}

## To be copied in the UI
# mod_SNMF_ui("SNMF_1")

## To be copied in the server
# mod_SNMF_server("SNMF_1")
