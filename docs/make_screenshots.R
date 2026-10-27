# Screenshots for the Familia user guide
#
# Opens the app in a headless Chrome (shinytest2), loads the example files
# (inst/extdata) in every tab and saves the screenshots in docs/images.
# Run from the package root, after data-raw/make_example_data.R:
#
#   source("docs/make_screenshots.R")
#
# Needs shinytest2 (install.packages("shinytest2")), Google Chrome or Chromium,
# and LEA for the SNMF tab (skipped when LEA is not installed). If a step
# fails, the app log is printed to help find the problem.

#### settings ####
img_dir <- file.path("docs", "images")
ex_dir  <- file.path("inst", "extdata")
win_w   <- 1500     # browser window (px)
win_h   <- 950

if (!file.exists("app.R"))
  stop("Run this script from the Familia package root.")
if (!file.exists(file.path(ex_dir, "example_genotypes.txt")))
  stop("Example files not found: run source(\"data-raw/make_example_data.R\") first.")
if (!requireNamespace("shinytest2", quietly = TRUE))
  stop("Install shinytest2 first: install.packages(\"shinytest2\")")
dir.create(img_dir, showWarnings = FALSE)

#### helpers ####

ex <- function(f) normalizePath(file.path(ex_dir, f), mustWork = TRUE)

# Wait until the app has been idle for half a second
idle <- function(app, timeout = 60 * 1000) {
  app$wait_for_idle(duration = 500, timeout = timeout)
}

# Run JavaScript in the page and give it time to redraw
js <- function(app, code, wait = 0.5) {
  app$run_js(code)
  Sys.sleep(wait)
}

# Page style for the screenshots. Headless Chrome can freeze CSS transitions
# half way, so they are turned off; the usual desktop layout is pinned (sidebar
# 250 px running down the whole page, content to its right); and boxes marked
# shot-compact (maximized results) show their tables without the DataTables
# controls and in a smaller font, so more rows and columns fit.
shot_css <- paste0(
  "*, *::before, *::after { transition: none !important; animation: none !important; }",
  ".main-sidebar { margin-left: 0 !important; width: 250px !important; }",
  ".wrapper { position: relative !important; }",
  ".layout-fixed .main-sidebar { position: absolute !important; top: 0 !important;",
  " bottom: 0 !important; height: auto !important; }",
  ".content-wrapper, .main-header, .main-footer { margin-left: 250px !important; }",
  ".nav-sidebar .nav-link p { opacity: 1 !important; visibility: visible !important; }",
  ".shot-compact .dataTables_length, .shot-compact .dataTables_filter,",
  " .shot-compact .dataTables_paginate { display: none !important; }",
  ".shot-compact .dataTables_scrollHead th, .shot-compact .dataTables_scrollBody td {",
  " font-size: 12.5px !important; padding: 5px 8px !important; }"
)

# Save a screenshot in docs/images; selector "html" is the whole page and clip
# (top, left, width, height) a part of the window. The mouse is moved to the
# top of the page, away from the sidebar and the tables.
shot <- function(app, file, selector = "html", clip = NULL) {
  app$get_chromote_session()$Input$dispatchMouseEvent(type = "mouseMoved",
                                                      x = win_w / 2, y = 15)
  js(app, sprintf("
    document.body.classList.remove('sidebar-collapse', 'sidebar-closed', 'sidebar-open');
    if (!document.getElementById('shot-style')) {
      var s = document.createElement('style');
      s.id = 'shot-style';
      s.textContent = '%s';
      document.head.appendChild(s);
    }", shot_css), wait = 0.8)
  path <- file.path(img_dir, file)
  if (file.exists(path)) unlink(path)   # shinytest2 does not overwrite
  if (is.null(clip)) {
    app$get_screenshot(path, selector = selector, delay = 1)
  } else {
    app$get_screenshot(path, screenshot_args = list(cliprect = clip), delay = 1)
  }
  message("  saved ", path)
}

# Open a tab from the sidebar menu (bs4Dash links carry the tabName in data-value)
open_tab <- function(app, tab) {
  app$click(selector = sprintf(".main-sidebar a[data-value='%s']", tab))
  Sys.sleep(1)
  idle(app)
}

# Open a tab of a results box, e.g. "Mendelian Error Plot"
open_results_tab <- function(app, tabset_id, label) {
  app$click(selector = sprintf("#%s a[data-value='%s']", tabset_id, label))
  Sys.sleep(1)
  idle(app)
}

# Upload an example file to a fileInput and wait for "Upload complete"
upload <- function(app, input_id, file) {
  bar <- sprintf("document.querySelector('#%s_progress .progress-bar')", input_id)
  js(app, sprintf("var b = %s; if (b) b.textContent = '';", bar), wait = 0.2)
  do.call(app$upload_file, c(stats::setNames(list(ex(file)), input_id), wait_ = FALSE))
  app$wait_for_js(sprintf("(%s || {textContent: ''}).textContent.indexOf('complete') >= 0", bar),
                  timeout = 30 * 1000)
  idle(app)
}

# Set an input value (no output has to change)
set_input <- function(app, input_id, value) {
  do.call(app$set_inputs, c(stats::setNames(list(value), input_id), wait_ = FALSE))
  Sys.sleep(0.5)
  idle(app)
}

# Click a Run button and wait until the status text reports success, or stop
# when it reports a failure or the app opens an error window
run_and_wait <- function(app, button_id, status_id, ok, fail = "Failed|Error",
                         timeout = 2 * 60 * 1000) {
  get_text  <- sprintf("(document.getElementById('%s') || {textContent: ''}).textContent", status_id)
  get_modal <- "(document.querySelector('.modal.show .modal-content') || {innerText: ''}).innerText"
  js(app, sprintf("var s = document.getElementById('%s'); if (s) s.textContent = '';", status_id),
     wait = 0.2)
  app$click(input = button_id, wait_ = FALSE)
  app$wait_for_js(sprintf("/%s|%s/.test(%s) || %s.length > 0", ok, fail, get_text, get_modal),
                  timeout = timeout)
  modal <- app$get_js(get_modal)
  if (nzchar(modal)) stop(button_id, " opened an error window: ", gsub("\\s+", " ", modal), call. = FALSE)
  status <- app$get_js(get_text)
  if (grepl(fail, status)) stop(button_id, ": ", status, call. = FALSE)
  Sys.sleep(1)
  idle(app, timeout)
}

# Realign DataTables headers after a box changes size
adjust_tables <- function(app) {
  js(app, "if (window.jQuery && $.fn.dataTable) $.fn.dataTable.tables({visible: true, api: true}).columns.adjust();")
}

# Maximize (on = TRUE) or restore the results box that holds `inner_id`. Its
# tab panels are made taller while maximized so more of each table fits.
maximize <- function(app, inner_id, on = TRUE) {
  js(app, sprintf(
    "var card = document.getElementById('%s').closest('.card');
     card.querySelector('[data-card-widget=\"maximize\"]').click();
     card.classList.toggle('shot-compact', %s);
     var els = [card.querySelector('.card-body')].concat(Array.from(card.querySelectorAll('.tab-pane')));
     els.forEach(function(p) {
       if (p.dataset.height === undefined) p.dataset.height = p.style.height;
       p.style.height = %s;
     });",
    inner_id,
    if (on) "true" else "false",
    if (on) "(p.classList.contains('tab-pane') ? 'calc(100vh - 120px)' : 'auto')" else "p.dataset.height"),
    wait = 1.5)
  adjust_tables(app)
}

# Let the results box that holds `inner_id` grow to show its whole tab panel
# (on = TRUE), or restore its fixed height; the page around it stays visible
expand <- function(app, inner_id, on = TRUE) {
  js(app, sprintf(
    "var card = document.getElementById('%s').closest('.card');
     var els = [card.querySelector('.card-body')].concat(Array.from(card.querySelectorAll('.tab-pane')));
     els.forEach(function(p) {
       if (p.dataset.height === undefined) p.dataset.height = p.style.height;
       p.style.height = %s;
     });",
    inner_id, if (on) "'auto'" else "p.dataset.height"),
    wait = 1)
  adjust_tables(app)
}

# Scroll a tab panel so the section that holds `el_id` is at the top
scroll_to <- function(app, el_id) {
  js(app, sprintf(
    "var sec  = document.getElementById('%s').closest('.card');
     var pane = sec.closest('.tab-pane');
     pane.scrollTop += sec.getBoundingClientRect().top - pane.getBoundingClientRect().top - 8;",
    el_id), wait = 0.8)
}

max_card <- ".card.maximized-card"   # selector of a maximized box

#### screenshots ####
make_screenshots <- function() {
  # Start the app from app.R (it loads this package with pkgload)
  new_args <- list(app_dir = ".", name = "familia_guide", width = win_w, height = win_h,
                   load_timeout = 2 * 60 * 1000, timeout = 60 * 1000, seed = 123)
  if ("options" %in% names(formals(shinytest2::AppDriver$public_methods$initialize)))
    new_args$options <- list(shiny.autoload.r = FALSE)
  app <- do.call(shinytest2::AppDriver$new, new_args)
  on.exit(app$stop(), add = TRUE)

  tryCatch({
    message("Home")
    Sys.sleep(2)
    idle(app)
    shot(app, "01_home.png")
    message("  layout: ", app$get_js(paste0(
      "document.body.className + ' | window width ' + window.innerWidth + ",
      "' | content margin ' + getComputedStyle(document.querySelector('.content-wrapper')).marginLeft")))

    #### Pedigree Cleaner ####
    message("Pedigree Cleaner")
    open_tab(app, "ped_cleaner")
    upload(app, "ped_cleaner_1-ped_file", "example_pedigree.txt")
    shot(app, "02_cleaner_inputs.png")
    run_and_wait(app, "ped_cleaner_1-run_check", "ped_cleaner_1-pb_ped-title", "Finished")
    shot(app, "03_cleaner_results.png")
    maximize(app, "ped_cleaner_1-ped_results_tabs")
    scroll_to(app, "ped_cleaner_1-tbl_inconsistent_sex_roles")
    shot(app, "04_cleaner_tables.png", max_card)
    maximize(app, "ped_cleaner_1-ped_results_tabs", on = FALSE)

    #### Validate Pedigree ####
    message("Validate Pedigree")
    open_tab(app, "validate_ped")
    upload(app, "validate_ped_1-pedigree_file",  "example_pedigree_cleaned.txt")
    upload(app, "validate_ped_1-genotypes_file", "example_genotypes.txt")
    upload(app, "validate_ped_1-founders_file",  "example_founders.txt")
    shot(app, "05_validate_inputs.png")
    run_and_wait(app, "validate_ped_1-run_validate", "validate_ped_1-pb_validate-title", "Finished")
    shot(app, "06_validate_pass.png")
    maximize(app, "validate_ped_1-validate_results_tabs")
    scroll_to(app, "validate_ped_1-tbl_fail")
    shot(app, "07_validate_fail.png", max_card)
    scroll_to(app, "validate_ped_1-tbl_low_markers")
    shot(app, "08_validate_low_nogeno.png", max_card)
    scroll_to(app, "validate_ped_1-tbl_marker_summary")
    shot(app, "09_validate_marker_summary.png", max_card)
    scroll_to(app, "validate_ped_1-tbl_marker_trio_table")
    shot(app, "10_validate_marker_trio.png", max_card)
    maximize(app, "validate_ped_1-validate_results_tabs", on = FALSE)
    open_results_tab(app, "validate_ped_1-validate_results_tabs", "Mendelian Error Plot")
    shot(app, "11_validate_error_plot.png")

    #### Find Parentage: best pair ####
    message("Find Parentage")
    open_tab(app, "find_parentage")
    upload(app, "find_parentage_1-genotypes_file", "example_genotypes.txt")
    upload(app, "find_parentage_1-parents_file",   "example_parents.txt")
    upload(app, "find_parentage_1-progeny_file",   "example_progeny.txt")
    shot(app, "12_parentage_inputs.png")
    picker <- "button[data-id='find_parentage_1-method']"
    message("  Fill in pedigree disabled for a progeny list: ",
            app$get_js("$('#find_parentage_1-method option[value=fill_pedigree]').prop('disabled')"))
    app$click(selector = picker)
    Sys.sleep(1)
    shot(app, "13_parentage_methods.png")
    app$click(selector = picker)   # close the list
    Sys.sleep(1)
    run_and_wait(app, "find_parentage_1-run_parentage", "find_parentage_1-pb_parentage-title", "Finished")
    maximize(app, "find_parentage_1-parentage_results_tabs")
    shot(app, "14_parentage_pass_ties.png", max_card)
    scroll_to(app, "find_parentage_1-tbl_high_error")
    shot(app, "15_parentage_flagged.png", max_card)
    maximize(app, "find_parentage_1-parentage_results_tabs", on = FALSE)

    #### Find Parentage: fill in pedigree ####
    message("Find Parentage, Fill in pedigree")
    upload(app, "find_parentage_1-progeny_file", "example_pedigree_validated.txt")
    set_input(app, "find_parentage_1-method", "fill_pedigree")
    upload(app, "find_parentage_1-founders_file", "example_founders.txt")
    run_and_wait(app, "find_parentage_1-run_parentage", "find_parentage_1-pb_parentage-title", "Finished")
    shot(app, "16_fill_in_inputs.png")
    maximize(app, "find_parentage_1-parentage_results_tabs")
    shot(app, "17_fill_in_pedigree.png", max_card)
    maximize(app, "find_parentage_1-parentage_results_tabs", on = FALSE)

    # Help window of the tab
    app$click(input = "find_parentage_1-help_btn", wait_ = FALSE)
    Sys.sleep(1.5)
    # the help window, cut to the height of the browser window
    js(app, "document.querySelectorAll('.modal.show .modal-body').forEach(function(b) {
               b.style.maxHeight = '760px'; b.style.overflow = 'hidden'; });")
    shot(app, "24_help_modal.png", ".modal.show .modal-content")
    js(app, "$('.modal').modal('hide');", wait = 1)

    #### BreedTools ####
    message("BreedTools")
    open_tab(app, "polybreedtools")
    upload(app, "PolyBreedTools_1-reference_file",  "example_breedtools_reference.txt")
    upload(app, "PolyBreedTools_1-ref_ids_file",    "example_breedtools_ref_ids.txt")
    upload(app, "PolyBreedTools_1-validation_file", "example_breedtools_validation.txt")
    set_input(app, "PolyBreedTools_1-ploidy", 4)
    shot(app, "18_breedtools_inputs.png")
    run_and_wait(app, "PolyBreedTools_1-run", "PolyBreedTools_1-status", "Estimation complete")
    open_results_tab(app, "PolyBreedTools_1-polybreedtools_results_tabs", "Results Table")
    js(app, "$('#PolyBreedTools_1-preview select').val('25').trigger('change');", wait = 1)
    maximize(app, "PolyBreedTools_1-polybreedtools_results_tabs")
    shot(app, "19_breedtools_results.png", max_card)
    maximize(app, "PolyBreedTools_1-polybreedtools_results_tabs", on = FALSE)
    set_input(app, "PolyBreedTools_1-poly_show_sample_labels", TRUE)
    open_results_tab(app, "PolyBreedTools_1-polybreedtools_results_tabs", "Ancestry Plot")
    shot(app, "20_breedtools_plot.png")

    #### SNMF ####
    if (requireNamespace("LEA", quietly = TRUE)) {
      message("SNMF")
      open_tab(app, "snmf")
      upload(app, "SNMF_1-snmf_file", "example_snmf.vcf")
      set_input(app, "SNMF_1-snmf_ploidy",      4)
      set_input(app, "SNMF_1-snmf_k_max",       5)
      set_input(app, "SNMF_1-snmf_repetitions", 3)
      shot(app, "21_snmf_inputs.png")
      message("  running SNMF (K 1-5, 3 repetitions)...")
      run_and_wait(app, "SNMF_1-snmf_run", "SNMF_1-pb_snmf-title", "Complete",
                   timeout = 5 * 60 * 1000)
      open_results_tab(app, "SNMF_1-snmf_results_tabs", "Cross-Entropy")
      expand(app, "SNMF_1-snmf_results_tabs")   # plot and summary table, with the value boxes
      shot(app, "22_snmf_cross_entropy.png")
      expand(app, "SNMF_1-snmf_results_tabs", on = FALSE)
      open_results_tab(app, "SNMF_1-snmf_results_tabs", "Ancestry Plot")
      maximize(app, "SNMF_1-snmf_results_tabs")   # wide plot, so every sample label fits
      idle(app)
      shot(app, "23_snmf_ancestry_plot.png", max_card)
      maximize(app, "SNMF_1-snmf_results_tabs", on = FALSE)
    } else {
      message("LEA is not installed: SNMF screenshots skipped (BiocManager::install(\"LEA\")).")
    }
  }, error = function(e) {
    message("\nScreenshots stopped: ", conditionMessage(e), "\nApp log:")
    print(app$get_logs())
    stop(e)
  })

  message("\nDone. Screenshots are in ", img_dir)
  invisible(TRUE)
}

make_screenshots()
