context("Checking sample subject mapping (SSM) file")

ssm_dd <- system.file("extdata", "5b_dbGaP_SubjectSampleMappingDD.xlsx", package = "dbgaptools",
                      mustWork = TRUE)
ssm_ds <- system.file("extdata", "5a_dbGaP_SubjectSampleMappingDS.txt", package = "dbgaptools",
                      mustWork = TRUE)

test_that("Compliant files run error free", {
  expect_null(check_ssm(dsfile = ssm_ds))
  expect_null(check_ssm(dsfile = ssm_ds, ddfile = ssm_dd))
})

test_that("Missing ID columns are detected", {
  expect_error(check_ssm(ssm_ds, sampleID_col = "mysample"),
               "Please check that dsfile contains columns for subject-level and sample-level IDs",
               fixed = TRUE)
  expect_error(check_ssm(ssm_ds, subjectID_col = "mysubject"),
               "Please check that dsfile contains columns for subject-level and sample-level IDs",
               fixed = TRUE)
})

test_that("Non-standard ID column names are detected", {
  ds.rev <- read_ds_file(ssm_ds)
  names(ds.rev)[1:2] <- c("mysubject", "mysample")
  ds_rev_fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t")

  expect_warning(check_ssm(dsfile = ds_rev_fn, sampleID_col = "mysample",
                           subjectID_col = "mysubject"),
                 "Note preferred subject-level ID column name is 'SUBJECT_ID'", fixed = TRUE)
  expect_warning(check_ssm(dsfile = ds_rev_fn, sampleID_col = "mysample",
                           subjectID_col = "mysubject"),
                 "Note preferred sample-level ID column name is 'SAMPLE_ID'", fixed = TRUE)
  unlink(ds_rev_fn)
})
                 
test_that("Duplicated sample IDs are detected", {
  ds.rev <- read_ds_file(ssm_ds)
  ds.rev$SAMPLE_ID[3] <- ds.rev$SAMPLE_ID[2]
  ds_rev_fn <- tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t")

  expect_equal(check_ssm(ds_rev_fn)$dup_samples, "S2")
  unlink(ds_rev_fn)
})

test_that("Blank sample IDs are detected", {
  ds.rev <- read_ds_file(ssm_ds)
  ds.rev$SAMPLE_ID[3] <- ""
  ds_rev_fn <-  tempfile(fileext = ".txt")
  write.table(ds.rev, file = ds_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t")

  expect_equal(check_ssm(ds_rev_fn)$blank_idx, 3)
  unlink(ds_rev_fn)
})

test_that("Extra samples and subjects are detected", {
  ds <- read_ds_file(ssm_ds)
  ssm_exp <- ds[, 1:2]
  ssm_exp_less <- ssm_exp[-c(3:4), ]
  out <- check_ssm(ssm_ds, ssm_exp = ssm_exp_less)
  expect_equal(out$extra_subjects, c("3", "4"))
  expect_equal(out$extra_samples, c("S3", "S4"))
})

test_that("Missing samples and subjects are detected", {
  ds <- read_ds_file(ssm_ds)
  ssm_exp <- ds[, 1:2]
  ext_rows <- c(SUBJECT_ID = 999, SAMPLE_ID = "S999")
  ssm_exp_more <- rbind(ssm_exp, ext_rows)
  out <- check_ssm(ssm_ds, ssm_exp = ssm_exp_more)
  expect_equal(out$missing_subjects, "999")
  expect_equal(out$missing_samples, "S999")
})

test_that("Mapping differences detected", {
  ds <- read_ds_file(ssm_ds)
  ssm_exp <- ds[, 1:2]
  ssm_exp$SAMPLE_ID[3] <- "S999"
  out <- check_ssm(ssm_ds, ssm_exp = ssm_exp)
  expect_equivalent(out$ssm_diffs,
                    data.frame(SUBJECT_ID = "3", SAMPLE_ID = "S999", stringsAsFactors = FALSE))
})
