# Shared helpers of the tests (sourced by testthat before the test files):
# uploads in the format of a Shiny fileInput, a recorder of what a module sends
# to the browser, and readers for the summary tiles, banners and downloads.

DASH <- "—"   # value of a summary tile before a file is uploaded

#### uploads ####

# Path of an example file shipped in inst/extdata
extdata <- function(file) {
  path <- system.file("extdata", file, package = "Familia")
  if (!nzchar(path)) stop("Example file not found: ", file)
  path
}

# A Shiny fileInput value. Shiny renames uploads, so the datapath has no
# meaningful extension and only `name` tells the format.
as_upload <- function(path, name) {
  data.frame(name = name, size = file.size(path), type = "", datapath = path,
             stringsAsFactors = FALSE)
}

# Upload of a copy of an example file, deleted at the end of the test
upload_example <- function(file, name = file, env = parent.frame()) {
  path <- withr::local_tempfile(.local_envir = env)
  file.copy(extdata(file), path)
  as_upload(path, name)
}

# Upload of some lines of text, deleted at the end of the test
upload_text <- function(lines, name, env = parent.frame()) {
  path <- withr::local_tempfile(.local_envir = env)
  writeLines(lines, path)
  as_upload(path, name)
}

#### inputs of each tab at the defaults of its UI (checked in test-ui.R) ####

cleaner_defaults <- list(correct_conflicting_trios = TRUE, correct_inconsistent_sex_roles = TRUE)

validate_defaults <- list(trio_error_threshold = 5, single_parent_error_threshold = 2,
                          min_markers = 10, ploidy = 2)

parentage_defaults <- list(method = "best_pair", error_threshold = 5, min_markers = 10, ploidy = 2,
                           top_n = 1, show_ties = TRUE, allow_parent_selfing = FALSE,
                           exclude_self_match = TRUE)

breedtools_defaults <- list(ploidy = 2, assign_threshold = 50, color_choice = "Set1",
                            poly_show_sample_labels = FALSE, poly_sort_by_predicted = TRUE,
                            poly_label_size = 8)

snmf_defaults <- list(snmf_ploidy = 2, snmf_k_min = 1, snmf_k_max = 10, snmf_repetitions = 5,
                      snmf_alpha = 100, snmf_iterations = 200, snmf_tolerance = 1e-4,
                      snmf_percentage = 0.05, snmf_cpu = 1, snmf_seed = 123,
                      snmf_select_mode = "auto_entropy")

#### the example pedigree (data-raw/make_example_data.R) ####

# True parents of the example offspring C01-C38
example_truth <- local({
  special <- data.frame(
    id     = sprintf("C%02d", 1:11),
    male   = c("M1", "M2", "M3", "M2", "M2", "M3", "M2", "M3", "M4", "M3", "M2"),
    female = c("F1", "F2", "F3", "F2", "F3", "F1", "F3", "F2", "F1", "F2", "F3"),
    stringsAsFactors = FALSE
  )
  normal <- expand.grid(male = c("M1", "M2", "M3"), female = c("F1", "F2", "F3"),
                        stringsAsFactors = FALSE)
  normal    <- normal[rep(seq_len(nrow(normal)), 3), ]
  normal$id <- sprintf("C%02d", 11 + seq_len(nrow(normal)))
  out <- rbind(special, normal[, c("id", "male", "female")])
  rownames(out) <- NULL
  out
})

#### what a module sends to the browser ####

# MockShinySession drops these messages, and its methods can be replaced, so
# they are recorded instead: custom messages (progress bar, shinyjs
# enable/disable), input updates (tabs, picker), notifications and modals.
record_session <- function(session) {
  log <- new.env(parent = emptyenv())
  log$custom <- list()
  log$inputs <- list()
  log$notes  <- character(0)
  log$modals <- character(0)
  root <- session$rootScope()
  root$sendCustomMessage <- function(type, message) {
    log$custom[[length(log$custom) + 1L]] <- list(type = type, message = message)
    invisible(NULL)
  }
  root$sendInputMessage <- function(inputId, message) {
    log$inputs[[length(log$inputs) + 1L]] <- list(id = inputId, message = message)
    invisible(NULL)
  }
  root$sendNotification <- function(type, message) {
    if (identical(type, "show")) log$notes <- c(log$notes, as.character(message$html))
    invisible(NULL)
  }
  root$sendModal <- function(type, message) {
    if (identical(type, "show")) log$modals <- c(log$modals, as.character(message$html))
    invisible(NULL)
  }
  log
}

# Same element: ids are sent with or without the module namespace
same_id <- function(sent, id) {
  sent <- as.character(sent)
  length(sent) == 1 && (sent == id || endsWith(sent, paste0("-", id)))
}

# Progress bar updates, in order: list(value, status, title) each
progress_updates <- function(log) {
  msgs <- Filter(function(m) identical(m$type, "update-progressBar-shinyWidgets"), log$custom)
  lapply(msgs, function(m) list(value = m$message$value, status = m$message$status,
                                title = m$message$title))
}
progress_titles <- function(log) vapply(progress_updates(log), `[[`, "", "title")
last_progress   <- function(log) {
  p <- progress_updates(log)
  if (length(p) == 0) NULL else p[[length(p)]]
}

# "enabled" or "disabled" after the last shinyjs message for a button, NA if none
button_state <- function(log, id) {
  state <- NA_character_
  for (m in log$custom) {
    if (m$type %in% c("shinyjs-enable", "shinyjs-disable") && same_id(m$message$id, id)) {
      state <- if (m$type == "shinyjs-enable") "enabled" else "disabled"
    }
  }
  state
}

# Updates sent to an input (e.g. the selected tab), in order
input_updates <- function(log, id) {
  lapply(Filter(function(m) same_id(m$id, id), log$inputs), `[[`, "message")
}

#### readers ####

# HTML of a renderUI value (list with html) or of tags
ui_html <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.list(x) && !is.null(x[["html"]])) as.character(x[["html"]]) else as.character(x)
}

# Summary tiles: named character vector, label = value
tiles <- function(x) {
  html <- ui_html(x)
  pat  <- '<div class="summary-tile-value">([^<]*)</div>\\s*<div class="summary-tile-label">([^<]*)</div>'
  hits <- regmatches(html, gregexpr(pat, html, perl = TRUE))[[1]]
  stats::setNames(sub(pat, "\\1", hits, perl = TRUE), sub(pat, "\\2", hits, perl = TRUE))
}
tiles_note <- function(x) {
  html <- ui_html(x)
  pat  <- '<p class="summary-strip-note">([^<]*)</p>'
  hit  <- regmatches(html, regexpr(pat, html, perl = TRUE))
  if (length(hit) == 0) NA_character_ else sub(pat, "\\1", hit, perl = TRUE)
}

# Run Summary banner: headline, counts ("- Label: <strong>n</strong>") and closing note
banner_headline <- function(x) sub("(?s)^.*?<p[^>]*>([^<]*)</p>.*$", "\\1", ui_html(x), perl = TRUE)
banner_note     <- function(x) sub("(?s)^.*<p[^>]*>([^<]*)</p></div>$", "\\1", ui_html(x), perl = TRUE)
banner_counts   <- function(x) {
  html <- ui_html(x)
  pat  <- "- ([^:<]+): <strong>([^<]*)</strong>"
  hits <- regmatches(html, gregexpr(pat, html, perl = TRUE))[[1]]
  stats::setNames(sub(pat, "\\2", hits, perl = TRUE), sub(pat, "\\1", hits, perl = TRUE))
}

# Result sections: title = "n record(s) found" or "None found"
section_counts <- function(x) {
  html <- ui_html(x)
  pat  <- "<strong> ([^<:]+): </strong>\\s*<span[^>]*>([^<]*)</span>"
  hits <- regmatches(html, gregexpr(pat, html, perl = TRUE))[[1]]
  stats::setNames(sub(pat, "\\2", hits, perl = TRUE), sub(pat, "\\1", hits, perl = TRUE))
}

# Opening tag of the element with this id, and one of its attributes
open_tag <- function(html, id) {
  pat <- paste0('<[a-zA-Z]+\\s[^>]*(?<![-\\w])id="', id, '"[^>]*>')
  hit <- regmatches(html, regexpr(pat, html, perl = TRUE))
  if (length(hit) == 0) NA_character_ else hit
}
tag_attr <- function(tag, name) {
  pat <- paste0('(?<![-\\w])', name, '="([^"]*)"')
  hit <- regmatches(tag, regexpr(pat, tag, perl = TRUE))
  if (length(hit) == 0) NA_character_ else sub(pat, "\\1", hit, perl = TRUE)
}

# A sheet of a results workbook, column names as written
read_sheet <- function(path, sheet) openxlsx::read.xlsx(path, sheet = sheet, sep.names = " ")

# Settings sheet as a named character vector
settings_of <- function(path) {
  s <- read_sheet(path, "Settings")
  stats::setNames(as.character(s$value), s$setting)
}

# Lines of a table written the way the pedigree downloads write it
table_lines <- function(df) {
  utils::capture.output(utils::write.table(as.data.frame(df), sep = "\t",
                                           row.names = FALSE, quote = FALSE))
}

# data.frame with plain row names and unnamed columns (tibbles and
# data.tables included). Columns built with apply() keep the sample names,
# which the tables and downloads do not show.
plain <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- NULL
  x[] <- lapply(x, function(col) { names(col) <- NULL; col })
  x
}

# First bytes of a file, to check its type
file_magic <- function(path, n = 8) readBin(path, "raw", n = n)

#### conditions and console output ####

# Messages of the warnings signalled while evaluating expr
collect_warnings <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  msgs
}

# Evaluate without printing to the console (LEA prints its progress)
quietly <- function(expr) {
  utils::capture.output(out <- suppressMessages(expr))
  invisible(out)
}
