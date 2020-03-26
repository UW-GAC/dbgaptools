context("Checking phenotype file")

pheno_dd <- system.file("extdata", "2b_dbGaP_SubjectPhenotypesDD.xlsx", package = "dbgaptools",
                        mustWork = TRUE)
pheno_ds <- system.file("extdata", "2a_dbGaP_SubjectPhenotypesDS.txt", package = "dbgaptools",
                        mustWork = TRUE)

dd <- read_dd_file(pheno_dd)
ds <- read_ds_file(pheno_ds, na_vals = c("NA", "N/A", "na", "n/a", "9999"))

test_that("Compliant files run error free", {
  expect_null(check_pheno(ds = pheno_ds, na_vals = c("NA", "N/A", "na", "n/a", "9999")))
  expect_null(check_pheno(ds = pheno_ds, dd = pheno_dd,
                          na_vals = c("NA", "N/A", "na", "n/a", "9999")))
})

test_that("Compliant dataframe run error free", {
  expect_null(check_pheno(ds = ds, na_vals = c("NA", "N/A", "na", "n/a", "9999")))
  expect_null(check_pheno(ds = pheno_ds, dd = dd,
                          na_vals = c("NA", "N/A", "na", "n/a", "9999")))
})

test_that("Missing ID column stops with error", {
  str <- "Please check that ds contains column for subject-level ID"
  expect_error(check_pheno(pheno_ds, subjectID_col = "mysubject"), str, fixed = TRUE)
})

test_that("Warning of non-preferred subject ID col is issued", {
  ds.rev <- read_ds_file(pheno_ds)
  names(ds.rev)[1] <- "INDIVIDUAL_ID"
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")
  str <- "Note preferred subject-level ID column name is 'SUBJECT_ID'"
  expect_warning(check_pheno(ds.rev.fn, subjectID_col = "INDIVIDUAL_ID"), str)
  unlink(ds.rev.fn)
})

# non unique subject ID flags
test_that("Non-unique SUBJECT_ID is reported", {
  ds.rev <- read_ds_file(pheno_ds)
  ds.rev$SUBJECT_ID[4] <- ds.rev$SUBJECT_ID[3]
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")
  expect_true(check_pheno(ds.rev.fn)$flag_nonuniq_subjID)

  unlink(ds.rev.fn)
})

test_that("Extra subjects are detected", {
  ds <- read_ds_file(pheno_ds)
  subj_exp <- ds[, 1]
  subj_exp_less <- subj_exp[-c(3:4)]
  out <- check_pheno(pheno_ds, subj_exp = subj_exp_less)
  expect_equal(out$extra_subjects, c("3", "4"))
})

test_that("Missing subjects are detected", {
  ds <- read_ds_file(pheno_ds)
  subj_exp <- ds[, 1]
  subj_exp_more <- rbind(subj_exp, "999")
  out <- check_pheno(pheno_ds, subj_exp = subj_exp_more)
  expect_equal(out$missing_subjects, c("999"))
})
