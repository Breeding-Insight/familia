# The example files offered in every tab (inst/extdata, written by
# data-raw/make_example_data.R) must exist and be readable by the app.

ex_path <- function(f) system.file("extdata", f, package = "Familia")

test_that("every example file listed for a tab exists", {
  files <- unique(unlist(example_files))
  expect_true(all(nzchar(vapply(files, ex_path, character(1)))),
              info = paste(files[!nzchar(vapply(files, ex_path, character(1)))], collapse = ", "))
})

test_that("example genotype files are read like an upload", {
  up   <- list(name = "example_genotypes.txt", datapath = ex_path("example_genotypes.txt"))
  geno <- read_genotype_upload(up, ploidy = 2)
  expect_identical(names(geno)[1], "id")
  expect_true(all(unlist(geno[, -1]) %in% c(0, 1, 2, NA)))

  ped     <- list(name = "example_genotypes.ped", datapath = ex_path("example_genotypes.ped"))
  map     <- list(name = "example_genotypes.map", datapath = ex_path("example_genotypes.map"))
  geno_pd <- read_genotype_upload(ped, ploidy = 2, map_input = map)
  expect_equal(as.matrix(geno_pd[, -1]), as.matrix(geno[, -1]), ignore_attr = TRUE)
})

test_that("example tables preview the first rows and columns", {
  ids <- read_example_table("example_breedtools_ref_ids.txt", n_rows = 8, n_cols = 3)
  expect_identical(names(ids), c("LineA", "LineB", "LineC"))
  expect_null(read_example_table("no_such_file.txt"))
})
