# help_content_ped_cleaner.R
#' Pedigree Cleaner help content
#'
#' Returns the UI content for the Pedigree Cleaner help section.
#' Used by both mod_help and the Pedigree Cleaner module's own help button.
#'
#' @param collapse_fn A function with signature (panel_id, icon_name, label, body_content).
#'   Defaults to the internal make_collapse_panel.
#' @param id_prefix A string prefix to namespace panel IDs and avoid duplicate DOM ids.
#'
#' @noRd
help_content_ped_cleaner <- function(collapse_fn = NULL, id_prefix = "") {
  
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
  
  # Shared pedigree table helpers
  ped_table <- function(rows) {
    shiny::tags$table(
      class = "table table-bordered table-sm",
      style = "width: auto; font-size: 12px; margin-bottom: 6px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("id"),
          shiny::tags$th("male_parent"),
          shiny::tags$th("female_parent")
        )
      ),
      shiny::tags$tbody(rows)
    )
  }
  
  ped_row <- function(id, sire, dam, bg = NULL) {
    style <- if (!is.null(bg)) paste0("background-color:", bg, ";") else ""
    shiny::tags$tr(
      style = style,
      shiny::tags$td(id),
      shiny::tags$td(sire),
      shiny::tags$td(dam)
    )
  }
  
  shiny::tagList(
    
    shiny::h6(shiny::tagList(shiny::icon("circle-info"), " Overview"),
              style = "font-weight: bold;"),
    shiny::p(
      "The Pedigree Cleaner detects common pedigree issues. Exact duplicates are always
      removed and missing parents always added. Conflicting parents and parents with
      inconsistent sex roles are set to missing (0) when their options are checked; the
      app does not guess the right parent. Cycles and dependencies are reported only and
      must be fixed by hand. The results come as one Excel file, and the cleaned pedigree
      as a separate text file for Validate Pedigree. The Example file link in the Inputs
      box downloads a small pedigree with one case of each issue.",
      style = "font-size: 13px;"
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # Steps
    shiny::h6(shiny::tagList(shiny::icon("list-ol"), " Steps"),
              style = "font-weight: bold;"),
    shiny::tags$ol(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Upload</strong> a tab-separated .txt or .tsv, or a comma-separated .csv,
        file with three columns: <code>id</code>, <code>male_parent</code>, <code>female_parent</code>."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Choose the options</strong> -- whether conflicting parents and parents with
        inconsistent sex roles are set to missing (0) or only flagged."
      )),
      shiny::tags$li(shiny::HTML("<strong>Run Pedigree Check</strong> -- scans for all five issue types below.")),
      shiny::tags$li(shiny::HTML(
        "<strong>Review</strong> the Run Summary panel on the right and the expandable result tables
        in the Issue Tables tab. A table's <em>Search</em> box finds the text in any column, ignoring
        case; to search one column, type in the box under its header."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Keep a conflicting trio</strong> (optional) -- in the Conflicting Trios table, check the
        <em>Keep</em> box of the row to keep (at most one per id) and click <em>Apply selection</em>.
        The check runs again with the other rows of that id removed; <em>Reset</em> clears the choices."
      )),
      shiny::tags$li(shiny::HTML("<strong>Export</strong> the results (Excel) and the cleaned pedigree (.txt)."))
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # Issue Types
    shiny::h6(
      shiny::tagList(shiny::icon("triangle-exclamation"), " Issue Types -- What Is Detected and What Is Done"),
      style = "font-weight: bold;"
    ),
    shiny::p("Click each issue type to expand a before/after example.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("pc_help_exact_dup"),
      icon_name    = "copy",
      label        = "Exact Duplicates (always removed)",
      body_content = shiny::tagList(
        shiny::p("Fully identical rows are removed, keeping only one copy. This is always applied.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Before:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("A1", "B1", "C1", bg = "#f8d7da")
        )),
        shiny::p("The highlighted row is an exact copy -- only one row will be kept.",
                 style = "color: #721c24; font-size: 11px; margin-bottom: 6px;"),
        shiny::tags$strong("After:"),
        ped_table(ped_row("A1", "B1", "C1"))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pc_help_conflict"),
      icon_name    = "exclamation",
      label        = "Conflicting Trios (set to missing when the option is checked)",
      body_content = shiny::tagList(
        shiny::p("The same individual ID appears with different parents.
        When the option is checked, the parent that disagrees is set to 0 (unknown)
        and the rows are collapsed to one. To keep one of the rows as entered instead,
        check its Keep box in the Conflicting Trios table and click Apply selection.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Before:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("A1", "B2", "C1", bg = "#f8d7da")
        )),
        shiny::p("A1 has two different male parents (B1 and B2) -- male_parent cannot be determined.",
                 style = "color: #856404; font-size: 11px; margin-bottom: 6px;"),
        shiny::tags$strong("After (set conflicting parents to missing):"),
        ped_table(ped_row("A1", "0", "C1", bg = "#fff3cd")),
        shiny::p("The conflicting male_parent is set to 0 (unknown). female_parent is unchanged since it was consistent.",
                 style = "color: #856404; font-size: 11px;")
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pc_help_messy"),
      icon_name    = "shuffle",
      label        = "Inconsistent Sex Roles (set to missing when the option is checked)",
      body_content = shiny::tagList(
        shiny::p("An individual appears in the male_parent column in one record and the female_parent
        column in another, indicating an inconsistent sex/role assignment across the pedigree.
        When the option is checked, the parent fields referencing that individual are set to 0
        and any resulting exact duplicates are removed.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Before:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("A2", "C1", "B1", bg = "#f8d7da")
        )),
        shiny::p("B1 and C1 swap male_parent/female_parent roles across records -- flagged for review.",
                 style = "color: #856404; font-size: 11px; margin-bottom: 6px;"),
        shiny::tags$strong("After (set inconsistent sex roles to missing):"),
        ped_table(shiny::tagList(
          ped_row("A1", "0", "0", bg = "#fff3cd"),
          ped_row("A2", "0", "0", bg = "#fff3cd")
        )),
        shiny::p("Parent fields referencing conflicting IDs are set to 0. Exact duplicates resulting from this change are removed.",
                 style = "color: #856404; font-size: 11px;")
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pc_help_missing"),
      icon_name    = "user-plus",
      label        = "Missing Parents (always added)",
      body_content = shiny::tagList(
        shiny::p("A parent is referenced in the male_parent or female_parent column but never appears as an individual.
        Missing parents are always added as founders with unknown parents (0).",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Before:"),
        ped_table(ped_row("A1", "B1", "C1")),
        shiny::p("B1 and C1 are referenced but have no row of their own.",
                 style = "color: #0c5460; font-size: 11px; margin-bottom: 6px;"),
        shiny::tags$strong("After:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("B1", "0",  "0",  bg = "#d4edda"),
          ped_row("C1", "0",  "0",  bg = "#d4edda")
        ))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pc_help_cycles"),
      icon_name    = "rotate",
      label        = "Cycles / Dependencies (reported only -- fix by hand)",
      body_content = shiny::tagList(
        shiny::p("An animal appears as its own ancestor, creating a circular relationship.
        These are detected and reported but never changed by the app -- they must be fixed by hand.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Example:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("B1", "A1", "C2", bg = "#f8d7da")
        )),
        shiny::p("A1 is listed as the male_parent of B1, but B1 is also listed as the male_parent of A1 -- flagged for manual review.",
                 style = "color: #721c24; font-size: 11px;")
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # Export Contents
    shiny::h6(shiny::tagList(shiny::icon("download"), " Export Contents"),
              style = "font-weight: bold;"),
    shiny::tags$ul(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML("<strong>Download Results (Excel)</strong> -- one workbook: <em>All Records</em>
        (every uploaded row and every added parent, with its <code>issue</code>, the <code>action</code>
        taken and the parents in the cleaned pedigree), one sheet per issue table (the Conflicting
        Trios sheet marks the rows kept by you) and a <em>Settings</em> sheet with the options of the run.")),
      shiny::tags$li(shiny::HTML("<strong>Download Cleaned Pedigree (.txt)</strong> -- <code>pedigree_cleaned.txt</code>,
        tab-separated, the input of Validate Pedigree. Exact duplicates are always removed and missing
        parents always added, so it is never just the uploaded file."))
    )
  )
}