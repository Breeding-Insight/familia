testthat::local_edition(3)

# Strict tests of R/utils.R: genotype uploads (text, VCF, PLINK .ped), the
# Ploidy checks, the SNMF .geno writer, image saving and the example files.

tetra_vcf_lines <- c(
  "##fileformat=VCFv4.3",
  "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
  paste("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO",
        "FORMAT", "A", "B", "C", sep = "\t"),
  paste("chr1", "100", "snp1", "A", "G", ".", ".", ".", "GT", "0/0/0/0", "0/1/1/1", "1/1/1/1", sep = "\t"),
  paste("chr1", "200", "snp2", "C", "T", ".", ".", ".", "GT", "0/0/1/1", "./././.", "0/0/0/1", sep = "\t")
)
tetra_message <- paste("The VCF looks tetraploid (ploidy 4): most genotype calls have 4 alleles,",
                       "but Ploidy is set to 2. Set Ploidy to 4 and run again.")

#### format of an upload ####

test_that("genotype inputs accept these extensions", {
  expect_identical(genotype_upload_accept, c(".txt", ".tsv", ".csv", ".vcf", ".gz", ".ped"))
})

test_that("the genotype format comes from the original file name, in any case", {
  names <- c("a.vcf", "A.VCF", "a.vcf.gz", "a.VCF.GZ", "a.ped", "A.PED",
             "a.txt", "a.csv", "a.tsv", "a.gz", "a.geno", "vcf.txt", "a.ped.txt")
  expect_identical(vapply(names, function(n) genotype_upload_format(list(name = n)), "", USE.NAMES = FALSE),
                   c(rep("vcf", 4), "ped", "ped", rep("text", 7)))
})

#### text genotype files ####

test_that("a tab-separated genotype file is read as typed: quotes and # kept, ids trimmed", {
  geno <- read_genotype_upload(upload_text(c("id\tm1\tm2", "A#1\t0\t2", " B \t1\tNA", "\"Q\"\t2\t0"),
                                           "geno.txt"), ploidy = 2)
  expect_identical(names(geno), c("id", "m1", "m2"))
  expect_identical(geno$id, c("A#1", "B", "\"Q\""))
  expect_identical(geno$m1, c(0L, 1L, 2L))
  expect_identical(geno$m2, c(2L, NA, 0L))
})

test_that("a .csv genotype file is comma-separated and its quoted ids are trimmed", {
  geno <- read_genotype_upload(upload_text(c("ID,m1", "\" A \",1", "B,2"), "GENO.CSV"), ploidy = 2)
  expect_identical(names(geno), c("id", "m1"))
  expect_identical(geno$id, c("A", "B"))
  expect_identical(geno$m1, c(1L, 2L))
})

test_that("the ID column is found in any case and given the name the tab uses", {
  lines <- function(id) c(paste0(id, "\tm1"), "A\t0")
  expect_identical(names(read_genotype_upload(upload_text(lines("Id"), "g.txt"), ploidy = 2)), c("id", "m1"))
  expect_identical(names(read_genotype_upload(upload_text(lines("id"), "g.txt"), ploidy = 2, id_name = "ID")),
                   c("ID", "m1"))
  expect_identical(names(read_genotype_upload(upload_text(lines("sample"), "g.txt"), ploidy = 2)),
                   c("sample", "m1"))   # nothing to rename
  both <- upload_text(c("ID\tid\tm1", "A\ta\t0"), "g.txt")
  expect_identical(names(read_genotype_upload(both, ploidy = 2, id_name = "ID")), c("ID", "id", "m1"))
})

test_that("a compressed file that is not a VCF is refused", {
  expect_error(read_genotype_upload(upload_text("x", "geno.txt.gz"), ploidy = 2),
               "Compressed genotype files must be VCFs (.vcf.gz).", fixed = TRUE)
})

#### VCF and .ped ####

test_that("a VCF is read without the Ploidy check when ploidy is NULL and keeps the ploidy of its calls", {
  vcf  <- upload_text(tetra_vcf_lines, "tetra.vcf")
  geno <- read_genotype_upload(vcf, ploidy = NULL)
  expect_identical(class(geno), "data.frame")
  expect_identical(names(geno), c("id", "snp1", "snp2"))
  expect_identical(geno$id, c("A", "B", "C"))
  expect_identical(geno$snp1, c(0L, 3L, 4L))
  expect_identical(geno$snp2, c(2L, NA, 1L))
  expect_identical(attr(geno, "ploidy"), 4L)
  expect_identical(read_genotype_upload(vcf, ploidy = 4), geno)
  expect_error(read_genotype_upload(vcf, ploidy = 2), tetra_message, fixed = TRUE)
  expect_identical(names(read_genotype_upload(vcf, ploidy = NULL, id_name = "ID"))[1], "ID")
})

test_that("a PLINK .ped is diploid, and without a .map its markers are named by position", {
  ped  <- upload_text(c("F A 0 0 0 -9 A A G G", "F B 0 0 0 -9 A G G G"), "geno.ped")
  geno <- read_genotype_upload(ped, ploidy = NULL)
  expect_identical(names(geno), c("id", "SNP1", "SNP2"))
  expect_identical(geno$id, c("A", "B"))
  expect_identical(geno$SNP1, c(0L, 1L))   # G counted (last letter): AA = 0, AG = 1
  expect_identical(geno$SNP2, c(2L, 2L))
  expect_identical(attr(geno, "counted_allele"), c(SNP1 = "G", SNP2 = "G"))
  expect_identical(read_genotype_upload(ped, ploidy = 2), geno)
  expect_error(read_genotype_upload(ped, ploidy = 4), "PLINK .ped files are diploid; set Ploidy to 2.",
               fixed = TRUE)
  expect_identical(names(read_genotype_upload(ped, ploidy = 2, id_name = "ID"))[1], "ID")
})

#### Ploidy checks ####

test_that("check_upload_ploidy() checks a .ped and a VCF at the run and accepts any ploidy for text", {
  vcf_geno <- read_genotype_upload(upload_text(tetra_vcf_lines, "t.vcf"), ploidy = NULL)
  expect_invisible(check_upload_ploidy(vcf_geno, list(name = "t.vcf"), 4))
  expect_identical(check_upload_ploidy(vcf_geno, list(name = "t.vcf"), 4), TRUE)
  expect_error(check_upload_ploidy(vcf_geno, list(name = "t.VCF.GZ"), 2), tetra_message, fixed = TRUE)
  expect_error(check_upload_ploidy(data.frame(id = "a"), list(name = "g.ped"), 4),
               "PLINK .ped files are diploid; set Ploidy to 2.", fixed = TRUE)
  expect_identical(check_upload_ploidy(data.frame(id = "a"), list(name = "g.ped"), 2), TRUE)
  expect_identical(check_upload_ploidy(data.frame(id = "a"), list(name = "g.txt"), 6), TRUE)
  expect_identical(check_upload_ploidy(data.frame(id = "a"), list(name = "g.vcf"), 2), TRUE)   # ploidy unknown
})

test_that("check_vcf_ploidy() names the ploidy of the file and the one to select", {
  expect_error(check_vcf_ploidy(4, 2), tetra_message, fixed = TRUE)
  expect_error(check_vcf_ploidy(2L, 4),
               paste("The VCF looks diploid (ploidy 2): most genotype calls have 2 alleles,",
                     "but Ploidy is set to 4. Set Ploidy to 2 and run again."), fixed = TRUE)
  expect_error(check_vcf_ploidy(7, 2), "The VCF looks ploidy 7: most genotype calls have 7 alleles",
               fixed = TRUE)
  expect_identical(check_vcf_ploidy(4, 4), TRUE)
  expect_identical(check_vcf_ploidy(4, "4"), TRUE)
  expect_identical(check_vcf_ploidy(NA, 2), TRUE)
  expect_identical(check_vcf_ploidy(c(2, 4), 2), TRUE)
  expect_identical(check_vcf_ploidy(NULL, 2), TRUE)
})

test_that("ploidy_label() names the common ploidy levels", {
  expect_identical(vapply(c(2, 3, 4, 5, 6, 8), ploidy_label, ""),
                   c("diploid (ploidy 2)", "triploid (ploidy 3)", "tetraploid (ploidy 4)",
                     "pentaploid (ploidy 5)", "hexaploid (ploidy 6)", "octoploid (ploidy 8)"))
  expect_identical(ploidy_label(7), "ploidy 7")
  expect_identical(ploidy_label(10), "ploidy 10")
  expect_identical(ploidy_label("4"), "tetraploid (ploidy 4)")
  expect_identical(ploidy_label(4L), "tetraploid (ploidy 4)")
})

#### SNMF .geno ####

test_that("write_geno_upload() writes one line per marker and one digit per sample, 9 = missing", {
  up   <- upload_text(c("ID\tm1\tm2\tm3", "A\t0\t1\t2", "B\tNA\t2\t0"), "geno.txt")
  path <- withr::local_tempfile(fileext = ".geno")
  out  <- write_geno_upload(up, path, ploidy = 2)
  expect_identical(out, list(geno_path = path, sample_ids = c("A", "B")))
  expect_identical(readLines(path), c("09", "12", "20"))

  # calls outside 0..ploidy are written as missing
  write_geno_upload(upload_text(c("id\tm1\tm2", "A\t3\t-1", "B\t2\t0"), "g.txt"), path, ploidy = 2)
  expect_identical(readLines(path), c("92", "90"))
})

test_that("write_geno_upload() stops above ploidy 8 and when there are no calls", {
  up <- upload_text(c("id\tm1", "A\t0"), "g.txt")
  expect_error(write_geno_upload(up, tempfile(), ploidy = 9),
               "SNMF supports ploidy up to 8 (one digit per call in the .geno file).", fixed = TRUE)
  expect_error(write_geno_upload(upload_text(c("id", "A"), "g.txt"), tempfile(), ploidy = 2),
               "No genotype calls were found in the uploaded file.", fixed = TRUE)
  expect_error(write_geno_upload(upload_text("id\tm1", "g.txt"), tempfile(), ploidy = 2),
               "No genotype calls were found in the uploaded file.", fixed = TRUE)
})

#### images ####

test_that("save_plot_file() writes the chosen file type whatever the file name", {
  p    <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()
  path <- withr::local_tempfile()   # a download path has no extension

  expect_invisible(save_plot_file(p, path, "png", width = 2, height = 2, dpi = 50))
  expect_identical(file_magic(path), as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))

  save_plot_file(p, path, "pdf", width = 2, height = 2)
  expect_identical(rawToChar(file_magic(path, 5)), "%PDF-")

  if (capabilities("jpeg")) {
    save_plot_file(p, path, "jpeg", width = 2, height = 2, dpi = 50)
    expect_identical(file_magic(path, 3), as.raw(c(0xff, 0xd8, 0xff)))
  }
  if (capabilities("tiff")) {
    save_plot_file(p, path, "tiff", width = 2, height = 2, dpi = 50)
    expect_true(rawToChar(file_magic(path, 2)) %in% c("II", "MM"))
  }
  if (requireNamespace("svglite", quietly = TRUE) || capabilities("cairo")) {
    save_plot_file(p, path, "svg", width = 2, height = 2)
    expect_match(paste(readLines(path, warn = FALSE), collapse = "\n"), "<svg", fixed = TRUE)
  }
  expect_error(save_plot_file(p, path, "bmp"), "Unsupported image type: bmp", fixed = TRUE)
})

test_that("save_plot_file() tells the user why an image could not be saved", {
  session <- shiny::MockShinySession$new()
  withr::defer(session$close())
  log  <- record_session(session)
  path <- withr::local_tempfile(fileext = ".png")
  # not a plot: ggsave() fails (a missing folder would make ggsave() ask in interactive sessions)
  shiny::withReactiveDomain(session, {
    expect_error(suppressWarnings(save_plot_file("not a plot", path, "png")))
  })
  expect_length(log$notes, 1)
  expect_match(log$notes, "^Could not save the image: ")
})

#### example files ####

test_that("each tab offers these example files", {
  expect_identical(example_files, list(
    ped_cleaner    = "example_pedigree.txt",
    validate_ped   = c("example_pedigree_cleaned.txt", "example_genotypes.txt", "example_founders.txt",
                       "example_genotypes.vcf", "example_genotypes.ped", "example_genotypes.map"),
    find_parentage = c("example_genotypes.txt", "example_parents.txt", "example_progeny.txt",
                       "example_pedigree_validated.txt", "example_founders.txt"),
    polybreedtools = c("example_breedtools_reference.txt", "example_breedtools_ref_ids.txt",
                       "example_breedtools_validation.txt"),
    snmf           = "example_snmf.vcf"
  ))
})

test_that("the Example files link names one file, or a zip of several, and lists them on hover", {
  one  <- ui_html(example_files_ui(shiny::NS("x"), "a.txt"))
  many <- ui_html(example_files_ui(shiny::NS("x"), c("a.txt", "b.txt")))
  tag1 <- open_tag(one, "x-download_examples")
  expect_match(tag1, "shiny-download-link", fixed = TRUE)
  expect_identical(tag_attr(tag1, "title"), "a.txt")
  expect_match(one, "Example file\\s*</a>")
  expect_identical(tag_attr(open_tag(many, "x-download_examples"), "title"), "a.txt, b.txt")
  expect_match(many, "Example files (.zip)", fixed = TRUE)
})

test_that("example downloads give the file itself, a zip of several, or a clear error", {
  files <- c("example_pedigree.txt", "example_founders.txt")
  downloads <- function(id, files) {
    shiny::moduleServer(id, function(input, output, session) {
      output$one  <- example_download_handler(files[1])
      output$many <- example_download_handler(files, "examples.zip")
      output$none <- example_download_handler("no_such_file.txt")
    })
  }
  shiny::testServer(downloads, args = list(files = files), {
    log <- record_session(session)
    one <- output$one
    expect_identical(basename(one), "example_pedigree.txt")
    expect_identical(readBin(one, "raw", 1e6), readBin(extdata("example_pedigree.txt"), "raw", 1e6))

    many <- output$many
    expect_identical(basename(many), "examples.zip")
    expect_identical(zip::zip_list(many)$filename, files)

    expect_error(output$none, "Example files not found: no_such_file.txt", fixed = TRUE)
    expect_identical(log$notes, "Example files not found. Reinstall Familia.")
  })
})

test_that("read_example_table() gives the first rows and columns of an example file", {
  x <- read_example_table("example_breedtools_reference.txt", n_rows = 5, n_cols = 6)
  expect_s3_class(x, "data.frame")
  expect_identical(dim(x), c(5L, 6L))
  expect_identical(names(x)[1], "ID")
  expect_identical(names(read_example_table("example_breedtools_ref_ids.txt", n_rows = 8, n_cols = 3)),
                   c("LineA", "LineB", "LineC"))
  expect_null(read_example_table("no_such_file.txt"))
})
