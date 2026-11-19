# Shared helpers for the results of the tabs: uploaded tables, result tables,
# summary tiles, Excel downloads and the clickable Mendelian error plots.

#' Read an uploaded table (pedigree, parents or progeny file)
#'
#' Comma-separated for .csv, tab-separated otherwise. Every column is read as
#' text, so IDs such as 001 keep their leading zeros, and # is not a comment.
#' Spaces at the start or end of a cell are removed, quoted cells included, so
#' "A1 " and "A1" are the same individual (a typed space would otherwise hide
#' a duplicate or a conflicting trio).
#'
#' @param file_input A Shiny fileInput value (list with name and datapath).
#' @return A data.frame of character columns.
#' @noRd
read_table_upload <- function(file_input) {
  is_csv <- tolower(tools::file_ext(file_input$name)) == "csv"
  x <- utils::read.table(file_input$datapath, header = TRUE,
                         sep              = if (is_csv) "," else "\t",
                         quote            = "\"",
                         comment.char     = "",
                         fill             = is_csv,
                         colClasses       = "character",
                         check.names      = FALSE,
                         stringsAsFactors = FALSE)
  x[] <- lapply(x, trimws)   # strip.white would skip quoted cells (write.csv quotes them)
  x
}

#' Results table with a filter box under each column header
#'
#' The Search box finds the text in any column, ignoring case; the boxes under
#' the headers search a single column. Matches are highlighted.
#'
#' @param df data.frame or data.table.
#' @param page_length Rows per page.
#' @param options Extra DataTables options, added to the defaults.
#' @param ... Passed to DT::datatable() (e.g. selection).
#' @noRd
results_datatable <- function(df, page_length = 10, options = list(), ...) {
  DT::datatable(
    as.data.frame(df),
    rownames = FALSE,
    filter   = "top",
    class    = "table-bordered table-sm",
    options  = utils::modifyList(
      list(pageLength = page_length, scrollX = TRUE, searchHighlight = TRUE),
      options
    ),
    ...
  )
}

#' Summary tiles shown in the Data Overview tab of each tab
#'
#' @param stats Named list: names are the labels, values the counts. Missing
#'   values (before a file is uploaded) show a dash.
#' @param note Optional message shown under the tiles, e.g. a reading error.
#' @noRd
summary_strip <- function(stats, note = NULL) {
  tiles <- lapply(names(stats), function(label) {
    value <- stats[[label]]
    shown <- if (length(value) == 0 || all(is.na(value))) {
      "—"
    } else if (is.numeric(value)) {
      format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
    } else {
      as.character(value)
    }
    shiny::div(
      class = "summary-tile",
      shiny::div(class = "summary-tile-value", shown),
      shiny::div(class = "summary-tile-label", label)
    )
  })
  shiny::tagList(
    shiny::div(class = "summary-strip", tiles),
    if (!is.null(note)) shiny::p(class = "summary-strip-note", note)
  )
}

#' Percentage for a summary tile, e.g. 0.0123 -> "1.2%"
#' @noRd
format_pct <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA)
  paste0(formatC(100 * x, format = "f", digits = 1), "%")
}

#' Names of the id, male_parent and female_parent columns of a pedigree
#'
#' Matched ignoring case, spaces, dots and dashes, as in BIGpopA.
#'
#' @param ped data.frame.
#' @return Named character vector (id, male_parent, female_parent), or NULL
#'   when a column is missing.
#' @noRd
pedigree_columns <- function(ped) {
  norm <- gsub("[ .-]+", "_", tolower(trimws(names(ped))))
  idx  <- match(c("id", "male_parent", "female_parent"), norm)
  if (anyNA(idx)) return(NULL)
  stats::setNames(names(ped)[idx], c("id", "male_parent", "female_parent"))
}

#' TRUE where a parent is known (not 0, NA or empty)
#' @noRd
known_parent <- function(x) {
  x <- trimws(as.character(x))
  !(is.na(x) | x %in% c("", "0", "NA"))
}

#' Counts of an uploaded pedigree for the summary tiles
#'
#' @param ped data.frame with id, male_parent and female_parent columns.
#' @return Named list (records, unique IDs, complete trios, one parent known,
#'   both parents unknown), or NULL when a required column is missing.
#' @noRd
pedigree_counts <- function(ped) {
  cols <- pedigree_columns(ped)
  if (is.null(cols)) return(NULL)
  male   <- known_parent(ped[[cols[["male_parent"]]]])
  female <- known_parent(ped[[cols[["female_parent"]]]])
  list(
    "Records"              = nrow(ped),
    "Unique IDs"           = length(unique(ped[[cols[["id"]]]])),
    "Complete trios"       = sum(male & female),
    "One parent known"     = sum(xor(male, female)),
    "Both parents unknown" = sum(!male & !female)
  )
}

#' Number of complete trios whose three individuals are all genotyped
#'
#' @param ped Pedigree data.frame.
#' @param geno_ids IDs of the genotype file.
#' @noRd
testable_trios <- function(ped, geno_ids) {
  cols <- pedigree_columns(ped)
  if (is.null(cols)) return(NA_integer_)
  id     <- as.character(ped[[cols[["id"]]]])
  male   <- as.character(ped[[cols[["male_parent"]]]])
  female <- as.character(ped[[cols[["female_parent"]]]])
  sum(known_parent(male) & known_parent(female) &
        id %in% geno_ids & male %in% geno_ids & female %in% geno_ids)
}

#' IDs, number of markers and share of missing calls of a genotype table
#'
#' @param geno Result of read_genotype_upload() (an ID column and marker columns).
#' @return list(ids, markers, missing).
#' @noRd
genotype_counts <- function(geno) {
  hit    <- which(tolower(names(geno)) == "id")
  id_col <- if (length(hit) > 0) names(geno)[hit[1]] else names(geno)[1]
  dose   <- as.matrix(geno[, names(geno) != id_col, drop = FALSE])
  list(
    ids     = as.character(geno[[id_col]]),
    markers = ncol(dose),
    missing = if (length(dose) == 0) NA_real_ else mean(is.na(dose))
  )
}

#' Plain-language recommended action for the Validate Pedigree tables
#'
#' The tables show these labels; the downloads keep BIGpopA's codes.
#'
#' @param code recommended_correction values of BIGpopA::validate_pedigree().
#' @noRd
recommended_action_label <- function(code) {
  labels <- c(
    none                             = "None",
    keep_both                        = "Keep both parents",
    remove_male_parent               = "Set male parent to missing",
    remove_female_parent             = "Set female parent to missing",
    remove_both                      = "Set both parents to missing",
    unresolved                       = "Unresolved: review by hand",
    low_markers_keep_both            = "Low markers: keep both parents",
    low_markers_remove_male_parent   = "Low markers: set male parent to missing",
    low_markers_remove_female_parent = "Low markers: set female parent to missing",
    low_markers_remove_both          = "Low markers: set both parents to missing",
    low_markers_unresolved           = "Low markers: unresolved, review by hand"
  )
  code  <- as.character(code)
  label <- unname(labels[code])
  ifelse(is.na(label), code, label)
}

#' Validate Pedigree table with the recommended action in plain language
#' @noRd
plain_actions <- function(df) {
  df  <- as.data.frame(df)
  hit <- names(df) == "recommended_correction"
  if (any(hit)) {
    df$recommended_correction <- recommended_action_label(df$recommended_correction)
    names(df)[hit] <- "recommended_action"
  }
  df
}

#' Every record of a Pedigree Cleaner run, with its issues and the action taken
#'
#' One row per uploaded row, plus one per parent added because it had no row
#' of its own: the "All Records" sheet of the results.
#'
#' @param ped Uploaded pedigree (data.frame).
#' @param report Result of BIGpopA::check_ped() for this run.
#' @param conflicts Conflicting trios found before any row was kept (the
#'   conflicting_trios table of the run on the uploaded pedigree).
#' @param kept Rows of conflicts kept by the user (id, male_parent,
#'   female_parent), or NULL.
#' @param set_conflicts,set_sex Run options: disagreeing parents and parents
#'   with inconsistent sex roles set to missing (TRUE) or only flagged (FALSE).
#' @return data.frame with row, id, male_parent, female_parent, issue, action,
#'   cleaned_male_parent and cleaned_female_parent.
#' @noRd
cleaner_records <- function(ped, report, conflicts = report$conflicting_trios, kept = NULL,
                            set_conflicts = TRUE, set_sex = TRUE) {
  cols   <- pedigree_columns(ped)
  id     <- as.character(ped[[cols[["id"]]]])
  male   <- as.character(ped[[cols[["male_parent"]]]])
  female <- as.character(ped[[cols[["female_parent"]]]])
  n      <- length(id)
  issue  <- character(n)
  action <- character(n)
  add    <- function(x, rows, text) {
    x[rows] <- ifelse(nzchar(x[rows]), paste(x[rows], text, sep = "; "), text)
    x
  }

  # Exact duplicates: identical rows (every column); the later copies are removed
  row_key <- do.call(paste, c(lapply(ped, as.character), sep = "\r"))
  issue   <- add(issue,  duplicated(row_key) | duplicated(row_key, fromLast = TRUE), "exact duplicate")
  action  <- add(action, duplicated(row_key), "row removed (duplicate)")

  # Conflicting trios: the same id with different parents
  conflicts   <- as.data.frame(conflicts)
  in_conflict <- id %in% as.character(conflicts$id)
  kept_ids    <- if (is.null(kept)) character(0) else as.character(kept$id)
  kept_key    <- if (is.null(kept)) character(0) else
    paste(kept$id, kept$male_parent, kept$female_parent, sep = "\r")
  is_kept     <- in_conflict & paste(id, male, female, sep = "\r") %in% kept_key
  issue       <- add(issue,  in_conflict, "conflicting trio")
  action      <- add(action, is_kept, "kept by user")
  action      <- add(action, in_conflict & id %in% kept_ids & !is_kept,
                     "row removed (another row kept by user)")
  action      <- add(action, in_conflict & !id %in% kept_ids,
                     if (set_conflicts) "disagreeing parent(s) set to missing, rows merged" else "flagged, not changed")

  # Inconsistent sex roles: an individual used as male parent and as female parent
  sex_tbl <- as.data.frame(report$inconsistent_sex_roles)
  sex_ids <- if (nrow(sex_tbl) > 0 && all(c("male_parent", "female_parent") %in% names(sex_tbl))) {
    setdiff(intersect(as.character(sex_tbl$male_parent), as.character(sex_tbl$female_parent)), "0")
  } else {
    character(0)
  }
  issue  <- add(issue,  male   %in% sex_ids, "male parent has inconsistent sex role")
  issue  <- add(issue,  female %in% sex_ids, "female parent has inconsistent sex role")
  action <- add(action, male   %in% sex_ids, if (set_sex) "male parent set to missing"   else "flagged, not changed")
  action <- add(action, female %in% sex_ids, if (set_sex) "female parent set to missing" else "flagged, not changed")

  # Cycles and dependencies: the ids named in the messages
  dep      <- as.data.frame(report$dependencies)
  dep_text <- if (nrow(dep) > 0 && ncol(dep) > 0) as.character(dep[[1]]) else character(0)
  tokens   <- unique(unlist(strsplit(sub("^[^:]*:", "", dep_text), "[ ,()]+|->")))
  issue    <- add(issue,  id %in% tokens, "cycle or dependency")
  action   <- add(action, id %in% tokens, "flagged, fix by hand")

  records <- data.frame(row = seq_len(n), id = id, male_parent = male, female_parent = female,
                        issue = issue, action = action, stringsAsFactors = FALSE)

  # Parents added because they had no row of their own
  added <- as.data.frame(report$missing_parents)
  if (nrow(added) > 0) {
    records <- rbind(records, data.frame(
      row = NA_integer_, id = as.character(added$id), male_parent = "0", female_parent = "0",
      issue = "listed as a parent but has no row", action = "added with unknown parents (0)",
      stringsAsFactors = FALSE
    ))
  }
  records$issue[!nzchar(records$issue)]   <- "none"
  records$action[!nzchar(records$action)] <- "kept"
  records$action <- vapply(strsplit(records$action, "; ", fixed = TRUE),
                           function(p) paste(unique(p), collapse = "; "), character(1))

  # Parents in the cleaned pedigree
  cleaned <- as.data.frame(report$corrected_pedigree)
  m       <- match(records$id, as.character(cleaned$id))
  records$cleaned_male_parent   <- as.character(cleaned$male_parent)[m]
  records$cleaned_female_parent <- as.character(cleaned$female_parent)[m]
  records
}

#' Insert a column right after another one (at the end if it is missing)
#' @noRd
insert_after <- function(df, after, name, values) {
  pos <- match(after, names(df))
  if (is.na(pos)) pos <- ncol(df)
  new <- stats::setNames(data.frame(values, stringsAsFactors = FALSE), name)
  cbind(df[, seq_len(pos), drop = FALSE], new, df[, -seq_len(pos), drop = FALSE])
}

#' Validate Pedigree results with an issue column (the "All Trios" sheet)
#'
#' @param full_results full_results of BIGpopA::validate_pedigree().
#' @param trio_threshold Trio error threshold (%) of the run.
#' @param min_markers Minimum markers of the run.
#' @return full_results with an issue column after status, and the recommended
#'   action in plain language after BIGpopA's code.
#' @noRd
validation_issues <- function(full_results, trio_threshold, min_markers) {
  df      <- as.data.frame(full_results)
  status  <- as.character(df$status)
  err     <- suppressWarnings(as.numeric(df$trio_mendelian_error_pct))
  markers <- df$trio_markers_tested
  issue   <- rep("", nrow(df))
  is_fail <- status %in% "fail"
  is_low  <- status %in% "low_markers"
  issue[is_fail] <- sprintf("fail: trio error %.2f%% > %s%%", err[is_fail], trio_threshold)
  issue[is_low]  <- sprintf("low markers: %s testable markers < %s", markers[is_low], min_markers)
  issue[status %in% "no_genotype_data"]      <- "no genotype data for the offspring or a parent"
  issue[status %in% "missing_both_parents"]  <- "both parents unknown"
  issue[status %in% "missing_male_parent"]   <- "male parent unknown"
  issue[status %in% "missing_female_parent"] <- "female parent unknown"
  issue[status %in% "no_data"]               <- "no testable marker"
  issue[!nzchar(issue)] <- "none"

  out <- insert_after(df, "status", "issue", issue)
  if ("recommended_correction" %in% names(out)) {
    out <- insert_after(out, "recommended_correction", "recommended_action",
                        recommended_action_label(out$recommended_correction))
  }
  out
}

#' Find Parentage results with an issue column (the "All Progeny" sheet)
#'
#' @param full_results full_results of BIGpopA::find_parentage().
#' @param error_threshold Error threshold (%) of the run.
#' @param min_markers Minimum markers of the run.
#' @noRd
parentage_issues <- function(full_results, error_threshold, min_markers) {
  df      <- as.data.frame(full_results)
  status  <- as.character(df$status)
  err     <- suppressWarnings(as.numeric(df$mendelian_error_pct))
  markers <- df$markers_tested
  issue   <- rep("", nrow(df))
  is_high <- status %in% "high_error"
  is_low  <- status %in% "low_markers"
  issue[is_high] <- sprintf("high error: best error %.2f%% > %s%%", err[is_high], error_threshold)
  issue[is_low]  <- sprintf("low markers: %s markers tested < %s", markers[is_low], min_markers)
  issue[status %in% "known_parent_no_genotype"] <- "known parent has no genotype"
  if ("tied_candidates" %in% names(df)) {
    ties <- suppressWarnings(as.integer(df$tied_candidates))
    tied <- !is.na(ties) & ties > 1
    issue[tied] <- ifelse(nzchar(issue[tied]),
                          paste0(issue[tied], "; ", ties[tied], " tied candidates"),
                          paste0(ties[tied], " tied candidates"))
  }
  issue[!nzchar(issue)] <- "none"
  insert_after(df, "status", "issue", issue)
}

#' Bar limits of a ggplot histogram (its first layer)
#' @noRd
histogram_breaks <- function(plot) {
  bars <- ggplot2::layer_data(plot, 1)
  sort(unique(c(bars$xmin, bars$xmax)))
}

#' Rows of the histogram bar under a click or hover on a Mendelian error plot
#'
#' Each value goes to its bar with the rule ggplot2 uses to draw the bars
#' (closed on the right, the first one also on the left), so the rows are the
#' ones counted in the bar. Rows without a value are not in the plot.
#'
#' @param data One row per trio or progeny: the full_results the plot was
#'   drawn from.
#' @param breaks Bar limits from histogram_breaks().
#' @param x Clicked x position, in data units.
#' @param column Column of data on the x axis.
#' @return list(label, rows), or NULL when the position is not on a bar.
#' @noRd
histogram_bar_rows <- function(data, breaks, x, column) {
  if (is.null(data) || length(breaks) < 2 || is.null(x) || !is.finite(x)) return(NULL)
  bar <- as.integer(cut(x, breaks, right = TRUE, include.lowest = TRUE))
  if (is.na(bar)) return(NULL)
  data   <- as.data.frame(data)
  values <- suppressWarnings(as.numeric(data[[column]]))
  in_bar <- as.integer(cut(values, breaks, right = TRUE, include.lowest = TRUE)) %in% bar
  if (!any(in_bar)) return(NULL)
  list(
    label = sprintf("%s to %s%%", format(max(0, breaks[bar])), format(breaks[bar + 1])),
    rows  = data[in_bar, setdiff(names(data), "plot_status"), drop = FALSE]
  )
}

#' Run settings as a table (setting, value) for the Settings sheet
#'
#' @param ... Named settings of the run.
#' @noRd
settings_table <- function(...) {
  s <- c(
    list(
      "Date"            = format(Sys.time(), "%Y-%m-%d %H:%M"),
      "Familia version" = as.character(utils::packageVersion("Familia")),
      "BIGpopA version" = as.character(utils::packageVersion("BIGpopA"))
    ),
    list(...)
  )
  data.frame(
    setting = names(s),
    value   = vapply(s, function(v) if (length(v) == 0) "" else paste(as.character(v), collapse = ", "),
                     character(1)),
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
}

#' Write the results of a tab as one Excel workbook, one sheet per table
#'
#' Empty tables get a "None found" note; tables larger than an Excel sheet
#' allows get a note instead of the data.
#'
#' @param sheets Named list of data.frames; the names are the sheet names.
#' @param file Path of the .xlsx file.
#' @noRd
write_results_xlsx <- function(sheets, file) {
  wb     <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(textDecoration = "bold")
  for (nm in names(sheets)) {
    df <- sheets[[nm]]
    df <- if (is.null(df)) data.frame() else as.data.frame(df)
    if (ncol(df) == 0 || nrow(df) == 0) {
      df <- data.frame(note = "None found")
    } else if (ncol(df) > 16384 || nrow(df) > 1048575) {
      df <- data.frame(note = "Too large for an Excel sheet: see the table in the app.")
    }
    sheet <- substr(gsub("[/\\\\?*:]", "-", nm), 1, 31)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, df, headerStyle = header)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(min(ncol(df), 50)), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
