# help_content_validate_ped.R
#' Validate Pedigree help content
#'
#' Returns the UI content for the Validate Pedigree help section.
#' Used by both mod_help and the Validate Pedigree module's own help button.
#'
#' @param collapse_fn A function with signature (panel_id, icon_name, label, body_content).
#'   Defaults to the internal make_collapse_panel.
#' @param id_prefix A string prefix to namespace panel IDs and avoid duplicate DOM ids.
#'
#' @noRd
help_content_validate_ped <- function(collapse_fn = NULL, id_prefix = "") {
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
  
  # Shared trio table helper
  trio_table <- function(rows) {
    shiny::tags$table(
      class = "table table-bordered table-sm",
      style = "width: auto; font-size: 12px; margin-bottom: 6px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("id"),
          shiny::tags$th("male_parent"),
          shiny::tags$th("female_parent"),
          shiny::tags$th("error_rate"),
          shiny::tags$th("status")
        )
      ),
      shiny::tags$tbody(rows)
    )
  }
  
  trio_row <- function(id, sire, dam, error, status, bg = NULL) {
    style <- if (!is.null(bg)) paste0("background-color:", bg, ";") else ""
    shiny::tags$tr(
      style = style,
      shiny::tags$td(id),
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
      "The Validate Pedigree module checks recorded parent-offspring trios against genotype data
      using Mendelian error analysis. Each trio is classified as Pass, Fail, Low Markers,
      No Genotype Data, Founders, or Missing Parents. A corrected pedigree and per-category
      reports are bundled into a single .zip file for download.",
      style = "font-size: 13px;"
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # Steps
    shiny::h6(shiny::tagList(shiny::icon("list-ol"), " Steps"),
              style = "font-weight: bold;"),
    shiny::tags$ol(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Upload a Pedigree File</strong> - tab-separated .txt/.tsv or comma-separated .csv
        with columns: <code>id</code>, <code>male_parent</code>, <code>female_parent</code>."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Upload a Genotypes File</strong> - must include an <code>id</code> column followed
        by marker columns coded as allele-B dosage (<code>0</code>, <code>1</code>, ..., <code>ploidy</code>;
        e.g. <code>0</code>, <code>1</code>, <code>2</code> for diploid)."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Optionally upload a Founders File</strong> - a single column of founder IDs (.txt).
        Trios involving founders are preserved and reported separately."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Set parameters</strong> - trio error threshold, single-parent error threshold,
        minimum markers, and ploidy."
      )),
      shiny::tags$li(shiny::HTML("<strong>Click Run Validation</strong> to evaluate all trios.")),
      shiny::tags$li(shiny::HTML(
        "<strong>Review</strong> the Run Summary panel and the expandable tables in the Issue Tables tab."
      )),
      shiny::tags$li(shiny::HTML("<strong>Export</strong> the corrected pedigree and reports as a .zip file."))
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # Parameters
    shiny::h6(shiny::tagList(shiny::icon("gear"), " Parameters"),
              style = "font-weight: bold;"),
    shiny::p("Click each parameter to see details.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("vp_help_trio_threshold"),
      icon_name    = "percent",
      label        = "Trio Error Threshold (%)",
      body_content = shiny::tagList(
        shiny::p(
          "The maximum allowable Mendelian error rate (%) for a full trio (progeny + both parents)
          to be classified as Pass. Trios above this value are classified as Fail. Default: 5.0%.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("vp_help_single_threshold"),
      icon_name    = "percent",
      label        = "Single Parent Error Threshold (%)",
      body_content = shiny::tagList(
        shiny::p(
          "The maximum allowable error rate (%) when only one parent is available for comparison
          (e.g., when the other parent is coded as 0). Default: 2.0%.",
          style = "margin-bottom: 0;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("vp_help_min_markers"),
      icon_name    = "hashtag",
      label        = "Min Markers",
      body_content = shiny::tagList(
        shiny::p(
          "Minimum number of informative markers required to validate a trio. Trios with fewer
          markers than this value are placed in the Low Markers category. Default: 10.",
          style = "margin-bottom: 0;"
        )
      )
    ),

    collapse_fn(
      panel_id     = pid("vp_help_ploidy"),
      icon_name    = "dna",
      label        = "Ploidy",
      body_content = shiny::tagList(
        shiny::p(
          "Ploidy level of the species, matching how the genotypes are dosage-coded
          (2 = diploid, 4 = tetraploid, ...). Even ploidy uses the full polysomic
          Mendelian test across all co-genotyped markers. Odd ploidy (e.g. triploid),
          where balanced gametes are undefined, automatically switches to a
          homozygosity-only check that flags a marker only when the progeny is
          homozygous and a parent is the opposite homozygote (reduced power).
          Default: 2.",
          style = "margin-bottom: 0;"
        )
      )
    ),

    shiny::hr(style = "margin: 8px 0;"),

    # Result categories
    shiny::h6(
      shiny::tagList(shiny::icon("triangle-exclamation"), " Result Categories - What Each Means"),
      style = "font-weight: bold;"
    ),
    shiny::p("Click each category to expand an example.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("vp_help_pass"),
      icon_name    = "circle-check",
      label        = "Pass - trio consistent with Mendelian inheritance",
      body_content = shiny::tagList(
        shiny::p(
          "The Mendelian error rate for this trio is at or below the trio error threshold
          and there are sufficient markers. The recorded parentage is supported by the genotype data.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        trio_table(trio_row("A1", "B1", "C1", "1.8%", "Pass", bg = "#d4edda"))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("vp_help_fail"),
      icon_name    = "xmark",
      label        = "Fail - trio inconsistent with Mendelian inheritance",
      body_content = shiny::tagList(
        shiny::p(
          "The Mendelian error rate exceeds the trio error threshold, suggesting the recorded
          parentage is incorrect or that significant genotyping errors exist.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        trio_table(trio_row("A2", "B3", "C2", "21.5%", "Fail", bg = "#f8d7da"))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("vp_help_low_markers"),
      icon_name    = "triangle-exclamation",
      label        = "Low Markers - insufficient markers for reliable validation",
      body_content = shiny::tagList(
        shiny::p(
          "Fewer informative markers than the Min Markers threshold were available for this trio.
          The result is inconclusive and flagged for review.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        trio_table(trio_row("A3", "B2", "C3", "NA", "Low Markers", bg = "#fff3cd"))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("vp_help_no_geno"),
      icon_name    = "database",
      label        = "No Genotype Data - individual absent from genotypes file",
      body_content = shiny::tagList(
        shiny::p(
          "The progeny or one or both parents could not be found in the uploaded genotypes file.
          Validation cannot be performed without genotype data.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        trio_table(trio_row("A4", "B4", "C4", "NA", "No Genotype Data", bg = "#e2e3e5"))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("vp_help_founders"),
      icon_name    = "seedling",
      label        = "Founders - trio identified as a founder",
      body_content = shiny::tagList(
        shiny::p(
          "This individual was listed in the optional founders file and is treated as a founder
          (unknown true parents). Founder trios are reported separately and are not evaluated
          against the error threshold.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        trio_table(trio_row("F1", "0", "0", "NA", "Founder", bg = "#d1ecf1"))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("vp_help_missing_parents"),
      icon_name    = "user-slash",
      label        = "Missing Parents - one or both parents coded as 0",
      body_content = shiny::tagList(
        shiny::p(
          "One or both parents are recorded as 0 (unknown) in the pedigree. If genotype data
          is available for the known parent, the single-parent error threshold is applied;
          otherwise the trio cannot be fully validated.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        trio_table(trio_row("A5", "0", "C5", "1.3%", "Missing Parents", bg = "#fff3cd"))
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # Export
    shiny::h6(shiny::tagList(shiny::icon("download"), " Export Contents"),
              style = "font-weight: bold;"),
    shiny::tags$ul(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML("<code>corrected_pedigree.txt</code> - the validated pedigree, tab-separated.")),
      shiny::tags$li(shiny::HTML("<code>full_results.txt</code> - all trio results combined, tab-separated.")),
      shiny::tags$li(shiny::HTML("One .txt report per result category (only included if records exist):
        <code>pass.txt</code>, <code>fail.txt</code>, <code>low_markers.txt</code>,
        <code>no_genotype_data.txt</code>, <code>founders.txt</code>,
        <code>missing_parents.txt</code>."))
    )
  )
}