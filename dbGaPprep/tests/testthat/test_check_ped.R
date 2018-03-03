context("Checking pedigree file")

ped_dd <- system.file("extdata", "6b_dbGaP_PedigreeDD.xlsx", package = "dbGaPprep", mustWork = TRUE)
ped_ds <- system.file("extdata", "6a_dbGaP_PedigreeDS.txt", package = "dbGaPprep", mustWork = TRUE)

test_that("Compliant files run error free", {
  expect_null(check_ped(dsfile=ped_ds))
  expect_null(check_ped(dsfile=ped_ds, ddfile=ped_dd))
})

test_that("Missing ID column stops with error",{
  str <- "Please check that dsfile contains column for subject-level ID"
  expect_error(check_ped(ped_ds, subjectID_col="mysubject"), str, fixed=TRUE)  
})

test_that("Warning of non-preferred subject ID col is issued",{
  ds.rev <- .read_ds_file(ped_ds)
  names(ds.rev)[2] <- "INDIVIDUAL_ID"
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t", na="")
  str <- "Note preferred subject-level ID column name is 'SUBJECT_ID'"
  expect_warning(check_ped(ds.rev.fn, subjectID_col="INDIVIDUAL_ID",), str)
  unlink(ds.rev.fn)
})

test_that("Extra subjects are detected", {
  ds <- .read_ds_file(ped_ds)
  subj_exp <- ds[,2]
  subj_exp_less <- subj_exp[-c(3:4)]
  out <- check_ped(ped_ds, subj_exp=subj_exp_less)
  expect_equal(out$extra_subjects, c("1","2"))  
})

test_that("Missing subjects are detected", {
  ds <- .read_ds_file(ped_ds)
  subj_exp_more <- c(ds[,2], "999")
  out <- check_ped(ped_ds, subj_exp=subj_exp_more)
  expect_equal(out$missing_subjects, c("999"))
})

# required variable names

# dd error

# extra male or female value

# pedigree check (focus on the issue dbGaP cares about)

# MZ twin column other name

# MZ twin column ID issues
