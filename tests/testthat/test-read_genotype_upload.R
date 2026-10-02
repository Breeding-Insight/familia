# Unit tests for the genotype upload helper (text, VCF and PLINK .ped).
# Shiny fileInput values are lists with the original name and a renamed datapath,
# so each test mimics that: the datapath has no meaningful extension.

fake_upload <- function(lines, name) {
  path <- tempfile()                      # Shiny-style path, no extension
  writeLines(lines, path)
  list(name = name, datapath = path)
}

test_that("format is detected from the original file name", {
  expect_identical(genotype_upload_format(list(name = "g.vcf")),    "vcf")
  expect_identical(genotype_upload_format(list(name = "G.VCF.GZ")), "vcf")
  expect_identical(genotype_upload_format(list(name = "g.ped")),    "ped")
  expect_identical(genotype_upload_format(list(name = "g.txt")),    "text")
})

test_that("tab-separated and CSV text files are read as-is", {
  txt <- fake_upload(c("id\tS1\tS2", "A\t0\t2", "B\t1\tNA"), "geno.txt")
  csv <- fake_upload(c("id,S1,S2", "A,0,2", "B,1,NA"),        "geno.csv")

  for (up in list(txt, csv)) {
    geno <- read_genotype_upload(up, ploidy = 2)
    expect_identical(names(geno), c("id", "S1", "S2"))
    expect_equal(geno$S2, c(2, NA))
  }
})

test_that("a VCF upload is converted to dosages with the requested ID column", {
  vcf <- fake_upload(c(
    "##fileformat=VCFv4.3",
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    paste("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO",
          "FORMAT", "A", "B", sep = "\t"),
    paste("chr1", "100", "snp1", "A", "T", ".", ".", ".", "GT", "0/0", "0/1", sep = "\t"),
    paste("chr1", "200", "snp2", "C", "G", ".", ".", ".", "GT", "1/1", "./.", sep = "\t")
  ), "geno.vcf")

  geno <- read_genotype_upload(vcf, ploidy = 2, id_name = "ID")
  expect_identical(names(geno), c("ID", "snp1", "snp2"))
  expect_equal(as.numeric(geno$snp1), c(0, 1))
  expect_equal(as.numeric(geno$snp2), c(2, NA))
})

test_that("a .ped upload uses the .map names and keeps the counted alleles", {
  ped <- fake_upload(c("F A 0 0 0 -9 A A G G", "F B 0 0 0 -9 A G G G"), "geno.ped")
  map <- fake_upload(c("1\tsnpX\t0\t100", "1\tsnpY\t0\t200"),         "geno.map")

  geno <- read_genotype_upload(ped, ploidy = 2, id_name = "ID", map_input = map)
  expect_identical(names(geno), c("ID", "snpX", "snpY"))
  expect_equal(geno$snpX, c(0L, 1L))
  expect_false(is.null(attr(geno, "counted_allele")))
})

test_that("a validation .ped is coded with the reference counted allele", {
  # Only allele A at snpX: by default A would be counted (dosage 2)
  val  <- fake_upload("F V 0 0 0 -9 A A G G", "val.ped")
  geno <- read_genotype_upload(val, ploidy = 2,
                               counted_allele = c(SNP1 = "G", SNP2 = "G"))
  expect_equal(geno$SNP1, 0L)
  expect_equal(geno$SNP2, 2L)
})

test_that("invalid uploads give clear errors", {
  ped <- fake_upload("F A 0 0 0 -9 A A", "geno.ped")
  expect_error(read_genotype_upload(ped, ploidy = 4), "diploid")

  gz <- fake_upload("x", "geno.txt.gz")
  expect_error(read_genotype_upload(gz, ploidy = 2), "must be VCFs")
})
