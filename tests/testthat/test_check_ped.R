context("Checking pedigree file")

ped_dd <- system.file("extdata", "6b_dbGaP_PedigreeDD.xlsx", package = "dbgaptools",
                      mustWork = TRUE)
ped_dd_df <- read_dd_file(ped_dd)
ped_ds <- system.file("extdata", "6a_dbGaP_PedigreeDS.txt", package = "dbgaptools",
                      mustWork = TRUE)
ped_ds_df <- read_ds_file(ped_ds)

test_that("Compliant files run error free", {
  expect_null(check_ped(ds = ped_ds))
  expect_null(check_ped(ds = ped_ds, dd = ped_dd))
})

test_that("Compliant dataframes run error free", {
  expect_null(check_ped(ds = ped_ds_df))
  expect_null(check_ped(ds = ped_ds_df, dd = ped_dd_df))
})

test_that("Missing ID column stops with error", {
  str <- "Please check that ds contains column for subject-level ID"
  expect_error(check_ped(ped_ds, subjectID_col = "mysubject"), str, fixed = TRUE)
})

test_that("Warning of non-preferred subject ID col is issued", {
  ds.rev <- read_ds_file(ped_ds)
  names(ds.rev)[2] <- "INDIVIDUAL_ID"
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")
  str <- "Note preferred subject-level ID column name is 'SUBJECT_ID'"
  expect_warning(check_ped(ds.rev.fn, subjectID_col = "INDIVIDUAL_ID"), str)
  unlink(ds.rev.fn)
})

test_that("Duplicate subjects are detected", {
  ds.rev <- read_ds_file(ped_ds)
  ds.rev <- rbind(ds.rev, ds.rev[1, ])
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t")

  expect_equal(check_ped(ds.rev.fn)$dup_subjects, ds.rev$SUBJECT_ID[1])
  unlink(ds.rev.fn)
})

test_that("Extra subjects are detected", {
  ds <- read_ds_file(ped_ds)
  subj_exp <- ds[, 2]
  subj_exp_less <- subj_exp[-c(3:4)]
  out <- check_ped(ped_ds, subj_exp = subj_exp_less)
  expect_equal(out$extra_subjects, c("1", "2"))
})

test_that("Missing subjects are detected", {
  ds <- read_ds_file(ped_ds)
  subj_exp_more <- c(ds[, 2], "999")
  out <- check_ped(ped_ds, subj_exp = subj_exp_more)
  expect_equal(out$missing_subjects, c("999"))
})

test_that("Missing and required variable names are detected", {
  ds.rev <- read_ds_file(ped_ds)
  names(ds.rev)[1] <- "FAMILY"
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  expect_equal(check_ped(ds.rev.fn, check_incons = FALSE)$missing_vars, "FAMILY_ID")

  unlink(ds.rev.fn)
})

test_that("Multiple missing and required variable names are detected", {
  ds.rev <- read_ds_file(ped_ds)
  names(ds.rev)[c(1, 3)] <- c("FAMILY", "MOTHER_ID")
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  expect_equal(check_ped(ds.rev.fn, check_incons = FALSE)$missing_vars,
               c("FAMILY_ID", "MOTHER"))

  unlink(ds.rev.fn)
})

# dd error
test_that("DD error is reported for DS variable not in DD", {
  dd.rev <- read_dd_file(ped_dd)
  dd.rev$VARNAME[1] <- "FAMILY"
  dd.rev.fn <- tempfile(fileext = ".txt")
  write.table(dd.rev, file = dd.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")
  str1 <- "Data dictionary missing following dataset variables: FAMILY_ID"
  str2 <- "Data dictionary has extra variables not in dataset: FAMILY"
  expect_warning(out <- check_ped(ped_ds, dd = dd.rev.fn, check_incons = FALSE), str1)
  expect_warning(out <- check_ped(ped_ds, dd = dd.rev.fn, check_incons = FALSE), str2)
  expect_equal(out$dd_errors$missing_dsvars, "FAMILY_ID")
  expect_equal(out$dd_errors$extra_ddvars, "FAMILY")

  unlink(dd.rev.fn)
})

test_that("Extra SEX value is detected", {
  expect_equal(check_ped(ped_ds, male = "M", check_incons = FALSE)$extra_sexvals, "1")
  expect_equal(check_ped(ped_ds, male = "M", female = "F", check_incons = FALSE)$extra_sexvals,
               c("2", "1"))
})

# pedigree check (focus on the issue dbGaP cares
test_that("Missing parental IDs cause pedigree check errors", {
  ds <- read_ds_file(ped_ds)
  # remove rows for some father and mother IDs
  ds.rev <- ds[- (1:2), ]
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")
  out <- check_ped(ds.rev.fn)

  # construct expected pedigree check report
  row <- data.frame(row.num = 1, family = "100", no_individ_entry = "both", parentID = "1001;1002",
                    stringsAsFactors = FALSE)
  df.exp <- rbind(row, row)
  df.exp$row.num[2] <- 2
  expect_equivalent(out$incon_report$parent.no.individ.entry, df.exp)

  unlink(ds.rev.fn)
})


test_that("Incorrect MZ twin column name is reported", {
  ds.rev <- read_ds_file(ped_ds)
  names(ds.rev)[6] <- "TWINS"
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")
  out <- check_ped(ds.rev.fn, check_incons = FALSE)
  str <- "MZ twin column should be named 'MZ_TWIN_ID'"
  expect_equal(out$mztwin_errors$colname, str)

  unlink(ds.rev.fn)
})

test_that("MZ twins in different families are reported", {
  ds.rev <- read_ds_file(ped_ds)
  ds.rev$FAMILY_ID[4] <- "999"
  ds.rev.fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds.rev.fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_ped(ds.rev.fn, check_incons = FALSE)
  # construct expected twins_dat report
  err <- ds.rev[ds.rev$MZ_TWIN_ID %in% 1, ]
  err$chk_family <- TRUE
  err$chk_sex <- err$chk_subjectID <- FALSE

  expect_equivalent(out$mztwin_errors$twins_dat, err)

  unlink(ds.rev.fn)
})
