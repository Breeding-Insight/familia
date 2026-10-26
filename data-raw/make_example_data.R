# Familia example data
#
# Simulates the example files in inst/extdata. They are offered in every tab
# (Example files link), used in the user guide (docs/Familia_user_guide.Rmd) and
# in the guide screenshots (docs/make_screenshots.R). Run from the package root:
#
#   source("data-raw/make_example_data.R")
#
# Diploid pedigree data (Pedigree Cleaner, Validate Pedigree, Find Parentage)
#   Founders M1, M2, M3, F1, F2, F3 (genotyped) and M4 (not genotyped).
#   M1c is a second sample of M1 (a clone or duplicate) with identical calls.
#   Offspring C01-C38; C12-C38 are correct records. C01-C11 carry the cases
#   shown in the guide:
#     C01  M1 x F1, ties with M1c x F1 in Find Parentage
#     C02  duplicated row (exact duplicate)
#     C03  correct record
#     C04  true M2 x F2, recorded M3 x F2      -> remove_male_parent
#     C05  true M2 x F3, recorded M2 x F1      -> remove_female_parent
#     C06  true M3 x F1, recorded M2 x F2      -> remove_both
#     C07  two rows, M2 x F3 and M1 x F3       -> conflicting trio
#     C08  M3 x F2, only 8 markers called      -> low_markers
#     C09  M4 x F1, M4 has no row              -> missing parent; M4 also female of C10
#     C10  true M3 x F2, recorded M3 x M4      -> inconsistent sex role
#     C11  M2 x F3, sample contaminated        -> unresolved
#   X1 and X2 are recorded as each other's parent (cycle) and not genotyped.
#   snp050, snp150 and snp250 are miscalled markers (random calls).
#
# Tetraploid line data (BreedTools and SNMF)
#   Reference lines LineA, LineB, LineC with 8 samples each (RA01-RC08).
#   Validation samples V01-V09 are pure (3 per line), V10 is 70% LineA and
#   30% LineB, V11 is 25/40/35% and V12 an even three-way mix. V13 comes from
#   a fourth line (LineD) that is not in the reference panel (low R2).
#
# The simulation is repeated with a new seed until every case above appears
# (checked with BIGpopA, the same functions the app uses).

#### settings ####
out_dir    <- file.path("inst", "extdata")
n_markers  <- 300                                # diploid markers
bad_snps   <- c("snp050", "snp150", "snp250")    # miscalled markers
error_rate <- 0.003                              # call replaced by another dosage (offspring)
miss_rate  <- 0.02                               # missing calls
n_poly     <- 300                                # tetraploid markers
max_seed   <- 200

if (!requireNamespace("BIGpopA", quietly = TRUE))
  stop("Install BIGpopA (>= 2.2.0) before running this script.")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#### helpers ####

# Unrelated individuals: allele-B dosage under Hardy-Weinberg
sim_unrelated <- function(p, ploidy = 2) stats::rbinom(length(p), ploidy, p)

# Diploid gamete: copies of allele B (0 or 1) passed on at each marker
sim_gamete <- function(g) {
  out      <- as.integer(g == 2)
  het      <- which(g == 1)
  out[het] <- stats::rbinom(length(het), 1, 0.5)
  out
}

# Diploid offspring of a male and a female parent
sim_cross <- function(male, female) sim_gamete(male) + sim_gamete(female)

# Genotyping errors (call replaced by one of the other dosages) and missing calls
add_noise <- function(g, ploidy = 2, error = error_rate, missing = miss_rate) {
  for (j in which(stats::runif(length(g)) < error)) {
    other <- setdiff(0:ploidy, g[j])
    g[j]  <- other[sample.int(length(other), 1)]
  }
  g[stats::runif(length(g)) < missing] <- NA
  g
}

# Tab-separated text file in inst/extdata
write_tsv <- function(x, file) {
  utils::write.table(x, file.path(out_dir, file), sep = "\t", quote = FALSE,
                     row.names = FALSE, na = "NA")
}

# VCF with GT calls: dosage d is written as (ploidy - d) zeros and d ones
write_vcf <- function(mat, file, ploidy, chrom, pos, ref, alt) {
  gt_codes <- vapply(0:ploidy, function(d)
    paste(c(rep("0", ploidy - d), rep("1", d)), collapse = "/"), character(1))
  gt <- matrix(gt_codes[t(mat) + 1], nrow = ncol(mat))   # markers in rows
  gt[is.na(gt)] <- paste(rep(".", ploidy), collapse = "/")
  header <- c(
    "##fileformat=VCFv4.3",
    "##source=Familia example data (simulated)",
    '##FORMAT=<ID=GT,Number=1,Type=String,Description="Genotype">',
    paste(c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT",
            rownames(mat)), collapse = "\t")
  )
  body <- paste(chrom, pos, colnames(mat), ref, alt, ".", "PASS", ".", "GT",
                apply(gt, 1, paste, collapse = "\t"), sep = "\t")
  writeLines(c(header, body), file.path(out_dir, file))
}

# PLINK .ped (six pedigree columns, then two allele letters per marker) and .map
write_plink <- function(mat, stem, chrom, pos, ref, alt, sex) {
  ref_m  <- matrix(ref, nrow(mat), ncol(mat), byrow = TRUE)
  alt_m  <- matrix(alt, nrow(mat), ncol(mat), byrow = TRUE)
  first  <- ifelse(mat == 2, alt_m, ref_m)
  second <- ifelse(mat == 0, ref_m, alt_m)
  first[is.na(mat)]  <- "0"
  second[is.na(mat)] <- "0"
  alleles <- matrix("", nrow(mat), 2 * ncol(mat))
  alleles[, seq(1, by = 2, length.out = ncol(mat))] <- first
  alleles[, seq(2, by = 2, length.out = ncol(mat))] <- second
  ped <- paste("FAM", rownames(mat), 0, 0, sex, -9,
               apply(alleles, 1, paste, collapse = " "))
  writeLines(ped, file.path(out_dir, paste0(stem, ".ped")))
  writeLines(paste(chrom, colnames(mat), 0, pos, sep = "\t"),
             file.path(out_dir, paste0(stem, ".map")))
}

# Collect failed expectations instead of stopping at the first one
new_checker <- function() {
  problems <- character(0)
  list(
    expect   = function(ok, what) if (!isTRUE(all(ok))) problems <<- c(problems, what),
    problems = function() problems
  )
}

#### diploid pedigree data ####

# True parents of every offspring
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
true_parents <- rbind(special, normal[, c("id", "male", "female")])
rownames(true_parents) <- NULL

# Recorded pedigree: the true parents with the deliberate errors listed above
recorded <- true_parents
recorded$male[recorded$id   == "C04"] <- "M3"
recorded$female[recorded$id == "C05"] <- "F1"
recorded$male[recorded$id   == "C06"] <- "M2"
recorded$female[recorded$id == "C06"] <- "F2"
recorded$female[recorded$id == "C10"] <- "M4"

founder_rows <- c("M1", "M1c", "M2", "M3", "F1", "F2", "F3")
pedigree <- rbind(
  data.frame(id = founder_rows, male_parent = "0", female_parent = "0",
             stringsAsFactors = FALSE),
  data.frame(id = recorded$id, male_parent = recorded$male,
             female_parent = recorded$female, stringsAsFactors = FALSE),
  data.frame(id            = c("C02", "C07", "X1", "X2"),   # duplicate, conflict, cycle
             male_parent   = c("M2",  "M1",  "X2", "X1"),
             female_parent = c("F2",  "F3",  "F1", "F2"),
             stringsAsFactors = FALSE)
)
# keep repeated ids next to each other (order() is stable for ties)
pedigree <- pedigree[order(match(pedigree$id, unique(pedigree$id))), ]
rownames(pedigree) <- NULL

# Founders (expected to have unknown parents), candidate parents and progeny
founders <- c("M1", "M1c", "M2", "M3", "M4", "F1", "F2", "F3")
parents  <- data.frame(id  = c("M1", "M1c", "M2", "M3", "F1", "F2", "F3"),
                       sex = c("M",  "M",   "M",  "M",  "F",  "F",  "F"),
                       stringsAsFactors = FALSE)
progeny  <- data.frame(id = true_parents$id, stringsAsFactors = FALSE)

# Genotypes for one seed
simulate_pedigree_data <- function(seed) {
  set.seed(seed)
  markers <- sprintf("snp%03d", seq_len(n_markers))
  p       <- stats::runif(n_markers, 0.1, 0.9)

  # true genotypes: founders, the ungenotyped M4 and a contaminant Z
  true <- list()
  for (id in c("M1", "M2", "M3", "M4", "F1", "F2", "F3", "Z")) true[[id]] <- sim_unrelated(p)
  for (i in seq_len(nrow(true_parents))) {
    true[[true_parents$id[i]]] <- sim_cross(true[[true_parents$male[i]]],
                                            true[[true_parents$female[i]]])
  }

  # C11 is a contaminated sample (C11 and Z DNA mixed): called heterozygous
  # wherever the two differ
  true$C11 <- ifelse(true$C11 == true$Z, true$C11, 1L)

  # observed calls (M4 and Z are not genotyped). Parents get missing calls but
  # no genotyping errors: one wrong call in a parent repeats in every trio of
  # its offspring and would look like another miscalled marker
  geno_ids <- c(founder_rows, true_parents$id)
  obs_ids  <- setdiff(geno_ids, "M1c")
  obs <- t(vapply(obs_ids, function(id)
    add_noise(true[[id]], error = if (id %in% founder_rows) 0 else error_rate),
    numeric(n_markers)))
  colnames(obs) <- markers

  # miscalled markers: random calls in every sample
  obs[, bad_snps] <- sample(0:2, nrow(obs) * length(bad_snps), replace = TRUE)

  # C08 failed in the lab: 8 calls left, all on good markers
  keep <- sample(setdiff(markers, bad_snps), 8)
  obs["C08", setdiff(markers, keep)] <- NA

  # M1c: identical calls to M1
  obs <- rbind(obs, M1c = obs["M1", ])[geno_ids, ]
  storage.mode(obs) <- "integer"

  genotypes <- data.frame(id = rownames(obs), obs, check.names = FALSE,
                          stringsAsFactors = FALSE)
  rownames(genotypes) <- NULL
  list(seed = seed, genotypes = genotypes)
}

# Run the app's functions and check that every case appears
check_pedigree_data <- function(d) {
  chk <- new_checker()

  # every marker carries the ALT allele, so the .ped file counts the same allele
  chk$expect(colSums(d$genotypes[, -1] > 0, na.rm = TRUE) > 0, "genotypes: a marker without ALT calls")

  # Pedigree Cleaner (does not depend on the genotypes)
  clean <- BIGpopA::check_ped(pedigree, verbose = FALSE)
  chk$expect(nrow(clean$exact_duplicates) == 2,       "cleaner: exact duplicates")
  chk$expect(nrow(clean$conflicting_trios) == 2,      "cleaner: conflicting trios")
  chk$expect(nrow(clean$inconsistent_sex_roles) == 2, "cleaner: inconsistent sex roles")
  chk$expect(identical(clean$missing_parents$id, "M4"), "cleaner: missing parent M4")
  chk$expect(nrow(clean$dependencies) >= 1,           "cleaner: cycle")

  # Validate Pedigree on the cleaned pedigree
  val <- BIGpopA::validate_pedigree(clean$corrected_pedigree, d$genotypes,
                                    founders_file = founders, verbose = FALSE,
                                    plot_results = FALSE)
  res    <- as.data.frame(val$full_results)
  status <- stats::setNames(res$status, res$id)
  corr   <- stats::setNames(res$recommended_correction, res$id)
  chk$expect(status[c("C01", "C02", "C03", sprintf("C%02d", 12:38))] == "pass",
             "validate: a correct trio did not pass")
  chk$expect(status[c("C04", "C05", "C06", "C11")] == "fail", "validate: an error trio did not fail")
  chk$expect(corr["C04"] == "remove_male_parent",    "validate: C04 not remove_male_parent")
  chk$expect(corr["C05"] == "remove_female_parent",  "validate: C05 not remove_female_parent")
  chk$expect(corr["C06"] == "remove_both",           "validate: C06 not remove_both")
  chk$expect(corr["C11"] == "unresolved",            "validate: C11 not unresolved")
  chk$expect(corr["C08"] == "low_markers_keep_both", "validate: C08 not low_markers_keep_both")
  chk$expect(status[c("C07", "C09")] == "missing_male_parent" &
               status["C10"] == "missing_female_parent", "validate: missing parents")
  chk$expect(status[setdiff(founders, "M4")] == "founders", "validate: founders")
  chk$expect(status[c("M4", "X1", "X2")] == "no_genotype_data", "validate: no genotype data")
  chk$expect(utils::head(val$marker_summary$marker, 3) %in% bad_snps,
             "validate: miscalled markers are not the top 3 of the marker summary")
  chk$expect(val$marker_summary$mismatch_pct[4] < 0.5 * min(val$marker_summary$mismatch_pct[1:3]),
             "validate: an ordinary marker looks as bad as the miscalled ones")

  # Find Parentage, best pair on all offspring
  fp  <- BIGpopA::find_parentage(d$genotypes, parents, progeny, method = "best_pair",
                                 verbose = FALSE, plot_results = FALSE)
  fpr <- as.data.frame(fp$full_results)
  fps <- stats::setNames(fpr$status, fpr$id)
  ok  <- setdiff(true_parents$id, c("C08", "C09", "C11"))
  tp  <- true_parents[match(ok, true_parents$id), ]
  got <- fpr[match(ok, fpr$id), ]
  chk$expect(fps[ok] == "pass",                  "best pair: a progeny did not pass")
  chk$expect(got$female_parent == tp$female &
               (got$male_parent == tp$male | (tp$male == "M1" & got$male_parent == "M1c")),
             "best pair: wrong pair")
  chk$expect(fps[c("C09", "C11")] == "high_error", "best pair: C09/C11 not high_error")
  chk$expect(fps["C08"] == "low_markers",          "best pair: C08 not low_markers")
  chk$expect(fpr$tied_candidates[fpr$id == "C01"] == 2, "best pair: C01 has no tie")

  # Find Parentage, fill in the validated pedigree
  fill <- BIGpopA::find_parentage(d$genotypes, parents, val$corrected_pedigree,
                                  method = "fill_pedigree", founders_file = founders,
                                  verbose = FALSE, plot_results = FALSE)
  fr  <- as.data.frame(fill$full_results)
  fs  <- stats::setNames(fr$status, fr$id)
  okf <- c("C04", "C05", "C06", "C07", "C10")
  tpf <- true_parents[match(okf, true_parents$id), ]
  gf  <- fr[match(okf, fr$id), ]
  chk$expect(setequal(fr$id, c(okf, "C09")), "fill: unexpected rows searched")
  chk$expect(fs[okf] == "pass",              "fill: a row did not pass")
  chk$expect(gf$male_parent == tpf$male & gf$female_parent == tpf$female, "fill: wrong parents")
  chk$expect(fs["C09"] == "high_error",      "fill: C09 not high_error")

  list(problems = chk$problems(), clean = clean, val = val)
}

#### tetraploid line data ####
line_names <- c("LineA", "LineB", "LineC", "LineD")   # LineD is not in the reference panel
ref_ids <- list(LineA = sprintf("RA%02d", 1:8),
                LineB = sprintf("RB%02d", 1:8),
                LineC = sprintf("RC%02d", 1:8))

# Validation samples: true proportion of LineA, LineB, LineC and LineD
val_q <- rbind(
  V01 = c(1, 0, 0, 0), V02 = c(1, 0, 0, 0), V03 = c(1, 0, 0, 0),
  V04 = c(0, 1, 0, 0), V05 = c(0, 1, 0, 0), V06 = c(0, 1, 0, 0),
  V07 = c(0, 0, 1, 0), V08 = c(0, 0, 1, 0), V09 = c(0, 0, 1, 0),
  V10 = c(0.70, 0.30, 0.00, 0),    # mostly LineA
  V11 = c(0.25, 0.40, 0.35, 0),    # no line reaches 50%
  V12 = c(1, 1, 1, 0) / 3,         # even three-way mix
  V13 = c(0, 0, 0, 1)              # line missing from the reference panel
)
val_expected <- c(rep("LineA", 3), rep("LineB", 3), rep("LineC", 3),
                  "LineA", "Undetermined", "Undetermined")   # V01-V12

simulate_line_data <- function(seed) {
  set.seed(seed)
  markers <- sprintf("snp%03d", seq_len(n_poly))

  # line frequencies around an ancestral frequency (Balding-Nichols, Fst = 0.2)
  fst  <- 0.2
  p0   <- stats::runif(n_poly, 0.1, 0.9)
  freq <- sapply(line_names, function(l)
    stats::rbeta(n_poly, p0 * (1 - fst) / fst, (1 - p0) * (1 - fst) / fst))

  # tetraploid dosage from the mixture of line frequencies, 1% missing calls
  sim_sample <- function(q) {
    add_noise(stats::rbinom(n_poly, 4, as.vector(freq %*% q)), ploidy = 4,
              error = 0, missing = 0.01)
  }
  ref_line  <- rep(names(ref_ids), each = 8)
  reference <- t(sapply(ref_line, function(l) sim_sample(as.numeric(line_names == l))))
  rownames(reference) <- unlist(ref_ids)
  validation <- t(apply(val_q, 1, sim_sample))
  colnames(reference) <- colnames(validation) <- markers
  storage.mode(reference)  <- "integer"
  storage.mode(validation) <- "integer"
  list(seed = seed, reference = reference, validation = validation)
}

check_line_data <- function(d) {
  chk  <- new_checker()
  freq <- BIGpopA::allele_freq_poly(d$reference, ref_ids, ploidy = 4)
  comp <- BIGpopA::solve_composition_poly(d$validation, freq, ploidy = 4)
  tab  <- BIGpopA::assign_composition_line(comp, assign_threshold = 50)
  pred <- stats::setNames(tab[["Predicted line"]], tab$ID)
  r2   <- comp[, "R2"]
  chk$expect(identical(unname(pred[rownames(val_q)[1:12]]), val_expected),
             "breedtools: unexpected predicted lines")
  chk$expect(r2["V13"] < 0.8 * min(r2[names(r2) != "V13"]),
             "breedtools: V13 does not have the lowest R2")
  chk$problems()
}

#### simulate until every case appears ####
run_seeds <- function(simulate, check, label) {
  tally <- character(0)
  for (seed in seq_len(max_seed)) {
    d        <- simulate(seed)
    out      <- suppressWarnings(check(d))
    problems <- if (is.list(out)) out$problems else out
    if (length(problems) == 0) {
      message(label, ": seed ", seed)
      return(list(data = d, check = out))
    }
    tally <- c(tally, problems)
  }
  print(sort(table(tally), decreasing = TRUE))
  stop(label, ": no seed in 1:", max_seed, " gave every case (failed checks above).")
}

ped_run  <- run_seeds(simulate_pedigree_data, check_pedigree_data, "Pedigree data")
line_run <- run_seeds(simulate_line_data,     check_line_data,     "Line data")
ped_data <- ped_run$data
line_dat <- line_run$data

#### write the pedigree files ####
write_tsv(pedigree,                                       "example_pedigree.txt")
write_tsv(ped_run$check$clean$corrected_pedigree,         "example_pedigree_cleaned.txt")
write_tsv(as.data.frame(ped_run$check$val$corrected_pedigree), "example_pedigree_validated.txt")
write_tsv(ped_data$genotypes,                             "example_genotypes.txt")
write_tsv(parents,                                        "example_parents.txt")
write_tsv(progeny,                                        "example_progeny.txt")
writeLines(founders, file.path(out_dir, "example_founders.txt"))

# the same genotypes as VCF and PLINK files
set.seed(1)
allele_pairs <- rbind(c("A", "C"), c("A", "G"), c("A", "T"),
                      c("C", "G"), c("C", "T"), c("G", "T"))  # ALT after REF, as counted in .ped
geno_mat <- as.matrix(ped_data$genotypes[, -1])
rownames(geno_mat) <- ped_data$genotypes$id
pick  <- sample(nrow(allele_pairs), n_markers, replace = TRUE)
chrom <- rep(1:3, each = n_markers / 3)
pos   <- as.integer(rep(seq_len(n_markers / 3) * 100000, 3))   # integer, so no 1e+05 in the files
write_vcf(geno_mat, "example_genotypes.vcf", ploidy = 2, chrom = paste0("chr", chrom),
          pos = pos, ref = allele_pairs[pick, 1], alt = allele_pairs[pick, 2])
sex <- ifelse(grepl("^M", rownames(geno_mat)), 1, ifelse(grepl("^F", rownames(geno_mat)), 2, 0))
write_plink(geno_mat, "example_genotypes", chrom = chrom, pos = pos,
            ref = allele_pairs[pick, 1], alt = allele_pairs[pick, 2], sex = sex)

#### write the line files ####
write_tsv(data.frame(ID = rownames(line_dat$reference), line_dat$reference,
                     check.names = FALSE), "example_breedtools_reference.txt")
write_tsv(data.frame(ID = rownames(line_dat$validation), line_dat$validation,
                     check.names = FALSE), "example_breedtools_validation.txt")
write_tsv(as.data.frame(ref_ids), "example_breedtools_ref_ids.txt")

# SNMF: reference and validation samples together as a tetraploid VCF
snmf_mat <- rbind(line_dat$reference, line_dat$validation)
pick4    <- sample(nrow(allele_pairs), n_poly, replace = TRUE)
write_vcf(snmf_mat, "example_snmf.vcf", ploidy = 4,
          chrom = paste0("chr", rep(1:3, each = n_poly / 3)),
          pos   = as.integer(rep(seq_len(n_poly / 3) * 100000, 3)),
          ref   = allele_pairs[pick4, 1], alt = allele_pairs[pick4, 2])

#### check that the file formats agree ####
# same ids, markers and calls (attributes such as ploidy are ignored)
same_values <- function(a, b) {
  identical(rownames(a), rownames(b)) && identical(colnames(a), colnames(b)) &&
    isTRUE(all.equal(matrix(as.numeric(a), nrow(a)), matrix(as.numeric(b), nrow(b))))
}
ped_back <- BIGpopA::ped_to_dosage(file.path(out_dir, "example_genotypes.ped"),
                                   format = "matrix", verbose = FALSE)
if (!same_values(ped_back, geno_mat)) stop("example_genotypes.ped does not match the dosage table.")
if (requireNamespace("vcfR", quietly = TRUE)) {
  vcf_back <- BIGpopA::vcf_to_dosage(file.path(out_dir, "example_genotypes.vcf"),
                                     format = "matrix", verbose = FALSE)
  if (!same_values(vcf_back, geno_mat)) stop("example_genotypes.vcf does not match the dosage table.")
  snmf_back <- BIGpopA::vcf_to_dosage(file.path(out_dir, "example_snmf.vcf"),
                                      format = "matrix", verbose = FALSE)
  if (!same_values(snmf_back, snmf_mat)) stop("example_snmf.vcf does not match the line data.")
} else {
  message("vcfR is not installed: the VCF files were written but not read back.")
}

message("Example files written to ", out_dir, ":\n  ",
        paste(list.files(out_dir), collapse = "\n  "))
