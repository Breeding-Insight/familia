# help_content_SNMF.R
#' SNMF help content
#'
#' Returns the UI content for the SNMF help section.
#' Used by both mod_help and the SNMF module's own help button.
#'
#' @param collapse_fn A function with signature (panel_id, icon_name, label, body_content).
#'   Defaults to the internal make_collapse_panel.
#' @param id_prefix A string prefix to namespace panel IDs and avoid duplicate DOM ids.
#'
#' @noRd
help_content_SNMF <- function(collapse_fn = NULL, id_prefix = "") {
  pid <- function(x) if (nchar(id_prefix) > 0) paste0(id_prefix, "_", x) else x
  
  if (is.null(collapse_fn)) {
    collapse_fn <- function(panel_id, icon_name, label, body_content) {
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
  }
  
  # ── Shared cross-entropy table helper ─────────────────────────────
  ce_table <- function(rows) {
    shiny::tags$table(
      class = "table table-bordered table-sm",
      style = "width: auto; font-size: 12px; margin-bottom: 6px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("K"),
          shiny::tags$th("best_run"),
          shiny::tags$th("min_cross_entropy")
        )
      ),
      shiny::tags$tbody(rows)
    )
  }
  
  ce_row <- function(k, run, ce, bg = NULL) {
    style <- if (!is.null(bg)) paste0("background-color:", bg, ";") else ""
    shiny::tags$tr(
      style = style,
      shiny::tags$td(k),
      shiny::tags$td(run),
      shiny::tags$td(ce)
    )
  }
  
  shiny::tagList(
    
    shiny::h6(shiny::tagList(shiny::icon("circle-info"), " Overview"),
              style = "font-weight: bold;"),
    shiny::p(
      "The SNMF module runs unsupervised ancestry estimation using ",
      shiny::tags$code("LEA::snmf()"),
      ". It estimates Q-matrices (ancestry proportions per individual per cluster) across
       a range of K values, optionally using cross-entropy to identify the best K.",
      style = "font-size: 13px;"
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # ── Steps ────────────────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("list-ol"), " Steps"),
              style = "font-weight: bold;"),
    shiny::tags$ol(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Upload</strong> a genotype file in <code>.vcf</code>, <code>.vcf.gz</code>,
         or LEA <code>.geno</code> format. VCF files are automatically converted to
         <code>.geno</code> format."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Set Ploidy</strong> — enter the ploidy of the species (e.g., 2 for diploid)."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Set K range</strong> — define the minimum and maximum number of ancestry clusters to test."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Set Repetitions</strong> — number of independent runs per K value. More repetitions
         improve reliability but increase runtime."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Choose Selection Mode</strong> — controls how the best K and run are determined
         (see Selection Modes below)."
      )),
      shiny::tags$li(shiny::HTML("<strong>Run SNMF</strong> — executes the analysis.")),
      shiny::tags$li(shiny::HTML(
        "<strong>Review</strong> the Cross-Entropy, Ancestry Plot, Q Matrix, and Logs tabs."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Export</strong> the Q matrix as <code>.csv</code>, cross-entropy as <code>.csv</code>,
         or the full LEA project as a <code>.zip</code>."
      ))
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # ── Selection Modes ──────────────────────────────────────────────
    shiny::h6(
      shiny::tagList(shiny::icon("sliders"), " Selection Modes"),
      style = "font-weight: bold;"
    ),
    shiny::p("Click each mode to see how K and run are selected.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("snmf_help_auto_entropy"),
      icon_name    = "wand-magic-sparkles",
      label        = "Auto-pick best K (cross-entropy)",
      body_content = shiny::tagList(
        shiny::p(
          "Cross-entropy is computed for every K and repetition. The K with the lowest
           minimum cross-entropy is automatically selected as the best K, and the run
           with the lowest cross-entropy for that K is pre-selected.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example cross-entropy summary:"),
        ce_table(shiny::tagList(
          ce_row("1", "3", "0.4812"),
          ce_row("2", "1", "0.3204", bg = "#d4edda"),
          ce_row("3", "2", "0.3391")
        )),
        shiny::p(
          "K = 2 has the lowest minimum cross-entropy and is auto-selected (highlighted).",
          style = "color: #155724; font-size: 11px;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("snmf_help_manual_entropy"),
      icon_name    = "hand-pointer",
      label        = "Manual K/run (cross-entropy)",
      body_content = shiny::tagList(
        shiny::p(
          "Cross-entropy is still computed and displayed for all K values and runs, but you
           manually choose the K and run to visualise using the Plot Controls selectors.
           The best run per K is pre-selected as a convenience.",
          style = "margin-bottom: 6px;"
        ),
        shiny::p(
          "Use this mode when you want to inspect the cross-entropy curve yourself before
           committing to a specific K.",
          style = "color: #856404; font-size: 11px;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("snmf_help_no_entropy"),
      icon_name    = "ban",
      label        = "No cross-entropy (manual)",
      body_content = shiny::tagList(
        shiny::p(
          "Cross-entropy is disabled entirely, which can reduce runtime. No cross-entropy
           plot or table will be produced. You select the K and run to display manually
           via the Plot Controls selectors.",
          style = "margin-bottom: 6px;"
        ),
        shiny::p(
          "Useful when you already know the target K or when running many repetitions at large K.",
          style = "color: #6c757d; font-size: 11px;"
        )
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # ── Parameter Reference ──────────────────────────────────────────
    shiny::h6(
      shiny::tagList(shiny::icon("gear"), " Parameter Reference"),
      style = "font-weight: bold;"
    ),
    shiny::p("Click each parameter to see what it controls.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("snmf_help_alpha"),
      icon_name    = "a",
      label        = "Alpha",
      body_content = shiny::p(
        "Regularisation parameter. Higher values enforce sparser (more uniform) ancestry
         estimates. Default is 100. Increase if results appear too noisy; decrease if
         estimates are overly smoothed.",
        style = "margin: 0;"
      )
    ),
    
    collapse_fn(
      panel_id     = pid("snmf_help_iterations"),
      icon_name    = "rotate",
      label        = "Iterations",
      body_content = shiny::p(
        "Maximum number of iterations for the optimisation algorithm per run. Default is 200.
         Increase if the algorithm has not converged (check Logs tab for convergence messages).",
        style = "margin: 0;"
      )
    ),
    
    collapse_fn(
      panel_id     = pid("snmf_help_tolerance"),
      icon_name    = "arrows-left-right-to-line",
      label        = "Tolerance",
      body_content = shiny::p(
        "Convergence threshold. The algorithm stops when the change in the objective function
         falls below this value. Default is 1e-4. Lower values require tighter convergence
         but may increase runtime.",
        style = "margin: 0;"
      )
    ),
    
    collapse_fn(
      panel_id     = pid("snmf_help_percentage"),
      icon_name    = "percent",
      label        = "Percentage",
      body_content = shiny::p(
        "Proportion of masked genotypes used to compute cross-entropy (i.e., the test set
         fraction). Default is 0.05 (5%). Only relevant when cross-entropy is enabled.",
        style = "margin: 0;"
      )
    ),
    
    collapse_fn(
      panel_id     = pid("snmf_help_cpu"),
      icon_name    = "microchip",
      label        = "CPU",
      body_content = shiny::p(
        "Number of CPU threads to use. Increasing this can speed up runs with many K values
         or repetitions, subject to available hardware.",
        style = "margin: 0;"
      )
    ),
    
    collapse_fn(
      panel_id     = pid("snmf_help_seed"),
      icon_name    = "seedling",
      label        = "Seed",
      body_content = shiny::p(
        "Random seed for reproducibility. Using the same seed with the same inputs will
         produce identical results across runs.",
        style = "margin: 0;"
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # ── Export Contents ──────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("download"), " Export Contents"),
              style = "font-weight: bold;"),
    shiny::tags$ul(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<code>Download Q (CSV)</code> — Q-matrix for the currently selected K and run,
         with sample IDs and one column per cluster."
      )),
      shiny::tags$li(shiny::HTML(
        "<code>Download cross-entropy (CSV)</code> — full cross-entropy table across all
         K values and repetitions (only available when cross-entropy is enabled)."
      )),
      shiny::tags$li(shiny::HTML(
        "<code>Save Project (.zip)</code> — complete LEA project directory, allowing you
         to reload results in R using <code>LEA::load.snmfProject()</code>."
      )),
      shiny::tags$li(shiny::HTML(
        "<code>Save Image</code> — exports the Cross-Entropy Plot or Ancestry Plot as
         <code>jpeg</code>, <code>tiff</code>, <code>png</code>, or <code>svg</code>
         at configurable resolution, width, and height."
      ))
    )
  )
}