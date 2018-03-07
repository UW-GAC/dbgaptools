context("Checking subject consent file")

subj_dd <- system.file("extdata", "4b_dbGaP_SubjectDD.xlsx", package = "dbGaPprep", mustWork = TRUE)
subj_ds <- system.file("extdata", "4a_dbGaP_SubjectDS.txt", package = "dbGaPprep", mustWork = TRUE)

test_that("Compliant files run error free",{
  # remove affection status col so we don't get that notification  
  ds.rev <- .read_ds_file(subj_ds)
  ds.rev$AFFECTION_STATUS <- NULL
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t", na="")
  dd.rev <- .read_dd_file(subj_dd)
  dd.rev <- dd.rev[dd.rev$VARNAME != "AFFECTION_STATUS",]
  dd.rev.fn <- tempfile(fileext=".txt")
  write.table(dd.rev, file=dd.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t", na="")  

  expect_null(check_subj(dsfile=ds.rev.fn))
  expect_null(check_subj(dsfile=ds.rev.fn, ddfile=dd.rev.fn))

  # clean up
  unlink(ds.rev.fn)
  unlink(dd.rev.fn)
})

test_that("Missing ID column stops with error",{
  str <- "Please check that dsfile contains column for subject-level ID"
  expect_error(check_subj(subj_ds, subjectID_col="mysubject"), str, fixed=TRUE)  
})

test_that("Missing consent column stops with error",{
  str <- "Please check that dsfile contains column for consent"
  expect_error(check_subj(subj_ds, consent_col="myconsent"), str, fixed=TRUE)  
})

test_that("Incorrect consent column name is detected",{
  ds.rev <- .read_ds_file(subj_ds)
  names(ds.rev)[names(ds.rev) %in% "CONSENT"] <- "myconsent"
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t", na="")
  
  str <- "Consent variable name should be 'CONSENT'"
  expect_warning(check_subj(ds.rev.fn, consent_col="myconsent"), str, fixed=TRUE)
  unlink(ds.rev.fn)
})

test_that("Presence of only one alias column is detected",{
  ds.rev <- .read_ds_file(subj_ds)
  ds.rev$SUBJECT_SOURCE <- NULL
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t", na="")

  str <- "Datafile has SOURCE_SUBJECT_ID, but missing SUBJECT_SOURCE"
  expect_warning(out <- check_subj(ds.rev.fn), str, fixed=TRUE)
  expect_true(out$alias_missvar)
})

test_that("Message about blanks in SOURCE_SUBJECT_ID is returned", {
  str <- "Note missing SUBJECT_SOURCE_ID should be left blank (\"\"), vs using missing value strings such as NA, N/A, etc."
  expect_message(out <- check_subj(subj_ds), str, fixed=TRUE)
})

test_that("Undefined CONSENT=0 does not return dd_error output",{
  expect_null(out <- check_subj(subj_ds, subj_dd)$dd_errors$vals_warnings)
})

test_that("Extra subjects are detected", {
  ds <- .read_ds_file(subj_ds)
  subj_exp <- ds[,1:2]
  subj_exp_less <- subj_exp[-c(3:4),]
  out <- check_subj(subj_ds, subj_exp=subj_exp_less)
  expect_equal(out$extra_subjects, c("3","4"))  
})

test_that("Missing subjects are detected", {
  ds <- .read_ds_file(subj_ds)
  subj_exp <- ds[,1:2]
  ext_rows <- c(SUBJECT_ID=999, CONSENT=1)
  subj_exp_more <- rbind(subj_exp, ext_rows)
  out <- check_subj(subj_ds, subj_exp=subj_exp_more)
  expect_equal(out$missing_subjects, c("999"))
})

test_that("Discrepant consent values are detected",{
  ds <- .read_ds_file(subj_ds)
  subj_exp <- ds[,1:2]
  # preturb one consent value
  subj_exp[14,2] <- 1
  out <- check_subj(subj_ds, subj_exp=subj_exp)
  exp.df <- data.frame(SUBJECT_ID="1001", CONSENT="1", stringsAsFactors=FALSE)
  expect_equivalent(out$consent_diffs, exp.df)
})

test_that("Non integer consent values are reported", {
  ds <- .read_ds_file(subj_ds)
  ds$CONSENT[5:10] <- "HMB"
  ds.rev.fn <-  tempfile(fileext=".txt")
  write.table(ds, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t", na="")
  
  out <- check_subj(ds.rev.fn)
  expect_equal(out$consent_nonints, "HMB")
  unlink(ds.rev.fn)
})

test_that("Phenotype columns are detected", {
  out <- check_subj(subj_ds)
  expect_equal(out$potential_pheno_vars, "AFFECTION_STATUS")
})

# this is still reporting NA as undefined AFFECTION STATUS var
test_that("Unmapped, non-0 consent values are reported", {
  ds <- .read_ds_file(subj_ds)
  ds$CONSENT[5:10] <- 2
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t", na="")

  str <- "For variable CONSENT, the following values are undefined in the dd: 2"
  out <- check_subj(ds.rev.fn, subj_dd)
  expect_equal(out$dd_errors$vals_warnings$undefined_vals_warn, str)
  unlink(ds.rev.fn)
})
