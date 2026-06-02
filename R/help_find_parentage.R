# help_content_find_parentage.R
#' Find Parentage help content
#'
#' Returns the UI content for the Find Parentage help section.
#' Used by both mod_help and the Find Parentage module's own help button.
#'
#' @param collapse_fn A function with signature (panel_id, icon_name, label, body_content).
#'   Defaults to the internal make_collapse_panel.
#' @param id_prefix A string prefix to namespace panel IDs and avoid duplicate DOM ids.
#'
#' @noRd
help_content_find_parentage <- function(collapse_fn = NULL, id_prefix = "") {
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
  
  # Shared genotype table helper
  geno_table <- function(rows) {
    shiny::tags$table(
      class = "table table-bordered table-sm",
      style = "width: auto; font-size: 12px; margin-bottom: 6px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("id"),
          shiny::tags$th("marker_1"),
          shiny::tags$th("marker_2"),
          shiny::tags$th("...")
        )
      ),
      shiny::tags$tbody(rows)
    )
  }
  
  geno_row <- function(id, m1, m2, dots = "...", bg = NULL) {
    style <- if (!is.null(bg)) paste0("background-color:", bg, ";") else ""
    shiny::tags$tr(
      style = style,
      shiny::tags$td(id),
      shiny::tags$td(m1),
      shiny::tags$td(m2),
      shiny::tags$td(dots)
    )
  }
  
  result_table <- function(rows) {
    shiny::tags$table(
      class = "table table-bordered table-sm",
      style = "width: auto; font-size: 12px; margin-bottom: 6px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("progeny"),
          shiny::tags$th("male_parent"),
          shiny::tags$th("female_parent"),
          shiny::tags$th("error_rate"),
          shiny::tags$th("status")
        )
      ),
      shiny::tags$tbody(rows)
    )
  }
  
  result_row <- function(progeny, sire, dam, error, status, bg = NULL) {
    style <- if (!is.null(bg)) paste0("background-color:", bg, ";") else ""
    shiny::tags$tr(
      style = style,
      shiny::tags$td(progeny),
      shiny::tags$td(sire),
      shiny::tags$td(dam),
      shiny::tags$td(error),
      shiny::tags$td(status)
    )
  }
  
  shiny::tagList(
    
    shiny::h6(shiny::tagList(shiny::icon("circle-info"), " Overview"),
              style = "font-weight: bold;"),
    shiny::p(
      "The Find Parentage module assigns the most likely parents to each progeny individual
      using genotype data and Mendelian error analysis. Upload genotype, parents, and progeny
      files, select a method, set parameters, and run the assignment. Results are categorized
      as Pass, High Error, or Low Markers, and can be exported as a .zip file.",
      style = "font-size: 13px;"
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # Steps
    shiny::h6(shiny::tagList(shiny::icon("list-ol"), " Steps"),
              style = "font-weight: bold;"),
    shiny::tags$ol(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Upload a Genotypes File</strong> — tab-separated .txt/.tsv or comma-separated .csv
        with an <code>id</code> column followed by marker columns coded as <code>0</code>, <code>1</code>, <code>2</code>."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Upload a Parents File</strong> — must include an <code>id</code> column and an optional
        <code>sex</code> column (<code>M</code>, <code>F</code>, or <code>A</code>)."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Upload a Progeny File</strong> — must include an <code>id</code> column."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Select a Method</strong> and configure parameters (error threshold, minimum markers, ties, selfing, self-match)."
      )),
      shiny::tags$li(shiny::HTML("<strong>Click Run Parentage Assignment</strong> to execute the analysis.")),
      shiny::tags$li(shiny::HTML(
        "<strong>Review</strong> the Run Summary panel and the Issue Tables tab for categorized results."
      )),
      shiny::tags$li(shiny::HTML("<strong>Export</strong> all results as a .zip file."))
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # Methods
    shiny::h6(shiny::tagList(shiny::icon("sliders"), " Methods"),
              style = "font-weight: bold;"),
    shiny::p("Click each method to learn when to use it.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("fp_help_best_pair"),
      icon_name    = "users",
      label        = "Best Pair (Mendelian) — default",
      body_content = shiny::tagList(
        shiny::p(
          "Finds the male-female parent pair that minimises the Mendelian error rate for each progeny.
          Use this when both parents are unknown and sex information is available in the parents file.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_best_male"),
      icon_name    = "person",
      label        = "Best Male Parent",
      body_content = shiny::tagList(
        shiny::p(
          "Identifies the best male parent for each progeny while the female parent is already known
          or not of interest. Requires <code>sex</code> column in the parents file.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_best_female"),
      icon_name    = "person",
      label        = "Best Female Parent",
      body_content = shiny::tagList(
        shiny::p(
          "Identifies the best female parent for each progeny. Requires <code>sex</code> column
          in the parents file.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_best_match"),
      icon_name    = "dna",
      label        = "Best Match (Homozygous)",
      body_content = shiny::tagList(
        shiny::p(
          "Finds the single parent (sex-agnostic) with the best genotype match to the progeny.
          Suitable for self-pollinating or vegetatively propagated species.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # Parameters
    shiny::h6(shiny::tagList(shiny::icon("gear"), " Parameters"),
              style = "font-weight: bold;"),
    shiny::p("Click each parameter to see details.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("fp_help_error_threshold"),
      icon_name    = "percent",
      label        = "Error Threshold (%)",
      body_content = shiny::tagList(
        shiny::p(
          "The maximum allowable Mendelian error rate (%) for a parentage assignment to be classified
          as Pass. Assignments with an error rate above this value are placed in the High Error category.
          Default: 5.0%.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_min_markers"),
      icon_name    = "hashtag",
      label        = "Min Markers",
      body_content = shiny::tagList(
        shiny::p(
          "Minimum number of informative markers required to make a parentage call. Progeny with
          fewer markers than this value are placed in the Low Markers category. Default: 10.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_show_ties"),
      icon_name    = "equals",
      label        = "Show Ties",
      body_content = shiny::tagList(
        shiny::p(
          "When checked, all tied best-parent assignments (identical error rates) are reported.
          When unchecked, only the first tied result is returned.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_selfing"),
      icon_name    = "rotate",
      label        = "Allow Parent Selfing",
      body_content = shiny::tagList(
        shiny::p(
          "When checked, the same individual is allowed to appear as both the male and female parent
          (i.e., selfing is a valid assignment). Default: unchecked.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_self_match"),
      icon_name    = "ban",
      label        = "Exclude Self-Match",
      body_content = shiny::tagList(
        shiny::p(
          "When checked, a progeny individual is excluded from being matched to itself as a parent,
          preventing trivial self-assignments. Default: checked.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # Result categories
    shiny::h6(shiny::tagList(shiny::icon("table-list"), " Result Categories"),
              style = "font-weight: bold;"),
    shiny::p("Click each category to see an example.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("fp_help_pass"),
      icon_name    = "circle-check",
      label        = "Pass — confident assignment within error threshold",
      body_content = shiny::tagList(
        shiny::p(
          "Progeny whose best parent assignment has a Mendelian error rate at or below the threshold
          and has sufficient markers.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        result_table(
          result_row("P001", "M01", "F01", "2.1%", "Pass", bg = "#d4edda")
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_high_error"),
      icon_name    = "triangle-exclamation",
      label        = "High Error — assignment exceeds error threshold",
      body_content = shiny::tagList(
        shiny::p(
          "Progeny whose best parent assignment exceeds the error threshold. These may indicate
          incorrect candidate parents, genotyping errors, or true non-parentage.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        result_table(
          result_row("P002", "M03", "F02", "18.4%", "High Error", bg = "#f8d7da")
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("fp_help_low_markers"),
      icon_name    = "exclamation",
      label        = "Low Markers — insufficient markers for a reliable call",
      body_content = shiny::tagList(
        shiny::p(
          "Progeny with fewer informative markers than the Min Markers threshold. The assignment
          cannot be made reliably and is flagged for review.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        result_table(
          result_row("P003", "NA", "NA", "NA", "Low Markers", bg = "#fff3cd")
        )
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # Export
    shiny::h6(shiny::tagList(shiny::icon("download"), " Export Contents"),
              style = "font-weight: bold;"),
    shiny::tags$ul(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML("<code>full_results.txt</code> — all progeny assignments, tab-separated.")),
      shiny::tags$li(shiny::HTML("<code>pass.txt</code> — progeny with confident assignments (if any).")),
      shiny::tags$li(shiny::HTML("<code>high_error.txt</code> — progeny exceeding the error threshold (if any).")),
      shiny::tags$li(shiny::HTML("<code>low_markers.txt</code> — progeny with insufficient markers (if any)."))
    )
  )
}