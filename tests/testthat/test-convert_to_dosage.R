# Unit tests for the VCF GT -> dosage conversion helper.

test_that("convert_to_dosage() counts alternate alleles", {
  expect_equal(convert_to_dosage(c("0/0", "0/1", "1/1")), c(0L, 1L, 2L))
})

test_that("convert_to_dosage() handles phased and missing genotypes", {
  expect_equal(convert_to_dosage(c("0|1", "1|1", "./.")), c(1L, 2L, NA_integer_))
})

test_that("convert_to_dosage() supports polyploid calls", {
  expect_equal(convert_to_dosage(c("0/0/0/1", "1/1/1/1")), c(1L, 4L))
})
