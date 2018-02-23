context("Checking sample attributes file")

sattr_dd <- system.file("extdata", "3b_dbGaP_SampleAttributesDD.xlsx", package = "dbGaPprep", mustWork = TRUE)

sattr_ds <- system.file("extdata", "3a_dbGaP_SampleAttributesDS.txt", package = "dbGaPprep", mustWork = TRUE)

test_that("Compliant files run error free",{
  expect_null(check_sattr(dsfile=sattr_ds))
})

test_that("Missing ID columns are detected",{
  expect_error(check_sattr(sattr_ds, sampleID_col="mysample"), "Please check that dsfile contains column for sample-level ID", fixed=TRUE)
})

test_that("Non-standard ID column names are detected",{
  ds.rev <- .read_ds_file(sattr_ds)
  names(ds.rev)[1] <- "mysample"
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t")

  str <- "Note preferred sample-level ID column name is 'SAMPLE_ID'"
  expect_warning(check_sattr(dsfile=ds.rev.fn, sampleID_col="mysample"), str, fixed=TRUE)
  unlink(ds.rev.fn)
})

test_that("Duplicated sample IDs are detected",{
  ds.rev <- .read_ds_file(sattr_ds)
  ds.rev$SAMPLE_ID[3] <- ds.rev$SAMPLE_ID[2]
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t")

  expect_equal(check_sattr(ds.rev.fn)$dup_samples, "S2")
  unlink(ds.rev.fn)
})

test_that("Blank sample IDs are detected",{
  ds.rev <- .read_ds_file(sattr_ds)
  ds.rev$SAMPLE_ID[3] <- ""
  ds.rev.fn <-  tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t")

  expect_equal(check_sattr(ds.rev.fn)$blank_idx, 3)
  unlink(ds.rev.fn)
})

test_that("Extra samples are detected",{
  ds <- .read_ds_file(sattr_ds)
  samp_exp <- ds[,1]
  samp_exp_less <- samp_exp[-c(3:4)]
  out <- check_sattr(sattr_ds, samp_exp=samp_exp_less)
  expect_equal(out$extra_samples, c("S3","S4"))
})

test_that("Missing samples are detected",{
  ds <- .read_ds_file(sattr_ds)
  samp_exp <- ds[,1]
  samp_exp_more <- c(samp_exp, "S999")
  out <- check_sattr(sattr_ds, samp_exp=samp_exp_more)
  expect_equal(out$missing_samples, "S999")
})

test_that("Missing and required variables are detected",{
  ds.rev <- .read_ds_file(sattr_ds)
  ds.rev$BODY_SITE <- NULL
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t")

  expect_equal(check_sattr(ds.rev.fn)$missing_vars, "BODY_SITE")
  unlink(ds.rev.fn)
})

test_that("Missing and required TOPMed variables are detected",{
  out <- check_sattr(sattr_ds, topmed=TRUE)
  vars <- c("Funding_Source", "TOPMed_Phase", "TOPMed_Project", "Study_Name")
  expect_equal(out$missing_topmed_vars, vars)
})
