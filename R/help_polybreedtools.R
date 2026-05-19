# help_content_polybreedtools.R
#' PolyBreedTools help content
#'
#' Returns the UI content for the PolyBreedTools help section.
#' Used by both mod_help and the PolyBreedTools module's own help button.
#'
#' @param collapse_fn A function with signature (panel_id, icon_name, label, body_content).
#'   Defaults to the internal make_collapse_panel.
#' @param id_prefix A string prefix to namespace panel IDs and avoid duplicate DOM ids.
#'
#' @noRd
help_content_polybreedtools <- function(collapse_fn = NULL, id_prefix = "") {
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
  
  # ── Shared input format table helper ──────────────────────────────────
  geno_table <- function(rows) {
    shiny::tags$table(
      class = "table table-bordered table-sm",
      style = "width: auto; font-size: 12px; margin-bottom: 6px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("ID"),
          shiny::tags$th("Marker1"),
          shiny::tags$th("Marker2"),
          shiny::tags$th("Marker3")
        )
      ),
      shiny::tags$tbody(rows)
    )
  }
  
  geno_row <- function(id, m1, m2, m3, bg = NULL) {
    style <- if (!is.null(bg)) paste0("background-color:", bg, ";") else ""
    shiny::tags$tr(
      style = style,
      shiny::tags$td(id),
      shiny::tags$td(m1),
      shiny::tags$td(m2),
      shiny::tags$td(m3)
    )
  }
  
  ref_ids_table <- function(rows) {
    shiny::tags$table(
      class = "table table-bordered table-sm",
      style = "width: auto; font-size: 12px; margin-bottom: 6px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Group1"),
          shiny::tags$th("Group2")
        )
      ),
      shiny::tags$tbody(rows)
    )
  }
  
  ref_ids_row <- function(g1, g2, bg = NULL) {
    style <- if (!is.null(bg)) paste0("background-color:", bg, ";") else ""
    shiny::tags$tr(
      style = style,
      shiny::tags$td(g1),
      shiny::tags$td(g2)
    )
  }
  
  shiny::tagList(
    
    shiny::h6(shiny::tagList(shiny::icon("circle-info"), " Overview"),
              style = "font-weight: bold;"),
    shiny::p(
      "PolyBreedTools estimates the proportion of each line or breed in a validation population
       using a reference panel of known genotypes. It supports any ploidy level and produces
       both a results table and a stacked ancestry bar plot.",
      style = "font-size: 13px;"
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # ── Steps ────────────────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("list-ol"), " Steps"),
              style = "font-weight: bold;"),
    shiny::tags$ol(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Upload Reference Genotypes</strong> — a tab-separated <code>.txt</code> file with
  samples in rows and SNP markers in columns. The first column must be <code>ID</code>.
  Missing values should be coded as <code>NA</code>."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Upload Reference IDs</strong> — a tab-separated <code>.txt</code> file assigning
  each reference sample to a group/line. Each column is one group; values are sample IDs."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Upload Validation Genotypes</strong> — same format as the reference genotype file
  (samples in rows, SNP markers in columns, first column named <code>ID</code>)."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Set Ploidy</strong> — enter the ploidy level of the species (e.g., 2 for diploid, 4 for tetraploid)."
      )),
      shiny::tags$li(shiny::HTML("<strong>Run Estimation</strong> — computes ancestry proportions for each validation sample.")),
      shiny::tags$li(shiny::HTML("<strong>Review</strong> the Results Table and Ancestry Plot tabs.")),
      shiny::tags$li(shiny::HTML("<strong>Export</strong> results as an <code>.xlsx</code> file or save the plot image."))
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # ── Input Format Details ─────────────────────────────────────────
    shiny::h6(
      shiny::tagList(shiny::icon("file-lines"), " Input Format Details"),
      style = "font-weight: bold;"
    ),
    shiny::p("Click each input type to see a format example.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("pbt_help_ref_genos"),
      icon_name    = "dna",
      label        = "Reference & Validation Genotypes (.txt)",
      body_content = shiny::tagList(
        shiny::p(
          "Tab-separated file with an ID column followed by one column per SNP marker.
           Genotypes are encoded as dosage counts (e.g., 0, 1, 2 for diploid).
           Missing genotypes should be coded as NA.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        geno_table(shiny::tagList(
          geno_row("Sample1", "0", "NA", "1"),
          geno_row("Sample2", "1", "1",  "0"),
          geno_row("Sample3", "2", "0",  "NA")
        )),
        shiny::p(
          "The first column must be named ID. All other columns are treated as SNP markers.",
          style = "color: #6c757d; font-size: 11px;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pbt_help_ref_ids"),
      icon_name    = "users",
      label        = "Reference IDs (.txt)",
      body_content = shiny::tagList(
        shiny::p(
          "Tab-separated file where each column represents one reference group or line.
           Column headers are the group names. Each column lists the sample IDs belonging to that group.
           Columns may have different lengths — empty cells are ignored.",
          style = "margin-bottom: 6px;"
        ),
        shiny::tags$strong("Example:"),
        ref_ids_table(shiny::tagList(
          ref_ids_row("SampleAlpha", "SampleOne"),
          ref_ids_row("S3",          "SampleTwo"),
          ref_ids_row("ExampleFour", "SampleThree")
        )),
        shiny::p(
          "Sample IDs in this file must match IDs present in the Reference Genotypes file.",
          style = "color: #6c757d; font-size: 11px;"
        )
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # ── Warnings & Automatic Filtering ──────────────────────────────
    shiny::h6(
      shiny::tagList(shiny::icon("triangle-exclamation"), " Automatic Filtering & Warnings"),
      style = "font-weight: bold;"
    ),
    shiny::p("Click each item to learn what is filtered automatically.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("pbt_help_low_callrate"),
      icon_name    = "filter",
      label        = "Low Call Rate Samples Removed",
      body_content = shiny::tagList(
        shiny::p(
          "Validation samples with a genotyping call rate below 50% are automatically removed
           before estimation. A warning listing the removed sample IDs will appear in the Status panel.",
          style = "margin-bottom: 6px;"
        ),
        shiny::p(
          "If too many samples are removed, check your validation file for excessive missing data.",
          style = "color: #856404; font-size: 11px;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pbt_help_empty_markers"),
      icon_name    = "filter",
      label        = "Empty Markers Removed",
      body_content = shiny::tagList(
        shiny::p(
          "After sample filtering, any marker (column) with no successful genotype calls across
           all remaining validation samples is removed. A warning listing the removed markers
           will appear in the Status panel.",
          style = "margin-bottom: 6px;"
        ),
        shiny::p(
          "If many markers are removed, verify that reference and validation files share the same marker set.",
          style = "color: #856404; font-size: 11px;"
        )
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pbt_help_dup_ids"),
      icon_name    = "exclamation",
      label        = "Duplicate Sample IDs",
      body_content = shiny::tagList(
        shiny::p(
          "If duplicate sample IDs are detected in the validation file, estimation is halted
           and an error message listing the duplicated IDs is shown in the Status panel.",
          style = "margin-bottom: 6px;"
        ),
        shiny::p(
          "Remove or rename duplicate IDs in your validation file before re-running.",
          style = "color: #721c24; font-size: 11px;"
        )
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # ── Export Contents ──────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("download"), " Export Contents"),
              style = "font-weight: bold;"),
    shiny::tags$ul(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Results Table</strong> — downloadable as <code>.xlsx</code> via <em>Save Excel File</em>.
         Contains each validation sample's estimated ancestry proportion per group and its predicted line."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Ancestry Plot</strong> — stacked bar chart of ancestry proportions.
         Exportable as <code>png</code>, <code>jpeg</code>, <code>svg</code>, or <code>pdf</code>
         via <em>Save Image</em>. Resolution, width, and height are configurable."
      ))
    )
  )
}
