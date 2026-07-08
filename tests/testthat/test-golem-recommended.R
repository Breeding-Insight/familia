# Smoke tests for the golem application skeleton.
# These construct (but never launch) the app, so they are safe on CRAN.

test_that("run_app() returns a Shiny app object without launching", {
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("app_sys() resolves files shipped in inst/", {
  expect_true(app_sys("golem-config.yml") != "")
})

test_that("app_ui() takes a request argument", {
  expect_true(is.function(app_ui))
  expect_true("request" %in% names(formals(app_ui)))
})

test_that("app_server() has input, output and session arguments", {
  expect_true(is.function(app_server))
  expect_true(all(c("input", "output", "session") %in% names(formals(app_server))))
})
