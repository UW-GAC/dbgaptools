context("Checking sample subject mapping (SSM) file")

ssm_dd <- system.file("extdata", "5b_dbGaP_SubjectSampleMappingDD.xlsx", package = "dbGaPprep", mustWork = TRUE)
ssm_ds <- system.file("extdata", "5a_dbGaP_SubjectSampleMappingDS.txt", package = "dbGaPprep", mustWork = TRUE)

test_that("Compliant files run error free",{
  expect_equal(check_ssm(dsfile=ssm_ds), list())
  out <- check_ssm(dsfile=ssm_ds, ddfile=ssm_dd)
  expect_equal(out, list())
})

test_that("Missing ID columns are detected",{
  expect_error(check_ssm(ssm_ds, sampleID_col="mysample"), "Please check that dsfile contains columns for subject-level and sample-level IDs", fixed=TRUE)
  expect_error(check_ssm(ssm_ds, subjectID_col="mysubject"), "Please check that dsfile contains columns for subject-level and sample-level IDs", fixed=TRUE)  
})

test_that("Non-standard ID column names are detected",{
  ds.rev <- .read_ds_file(ssm_ds)
  names(ds.rev)[1:2] <- c("mysubject","mysample")
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t")

  expect_warning(check_ssm(dsfile=ds.rev.fn, sampleID_col="mysample", subjectID_col="mysubject"), "Note preferred subject-level ID column name is 'SUBJECT_ID'", fixed=TRUE)
  expect_warning(check_ssm(dsfile=ds.rev.fn, sampleID_col="mysample", subjectID_col="mysubject"), "Note preferred sample-level ID column name is 'SAMPLE_ID'", fixed=TRUE)
  unlink(ds.rev.fn)
})
                 
test_that("Duplicated sample IDs are detected",{
  ds.rev <- .read_ds_file(ssm_ds)
  ds.rev$SAMPLE_ID[3] <- ds.rev$SAMPLE_ID[2]
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t")

  expect_equal(check_ssm(ds.rev.fn)$dup_samples, "S2")
  unlink(ds.rev.fn)
})

test_that("Duplicated sample IDs are detected",{
  ds.rev <- .read_ds_file(ssm_ds)
  ds.rev$SAMPLE_ID[3] <- ""
  ds.rev.fn <-  tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t")

  expect_equal(check_ssm(ds.rev.fn)$blank_idx, 3)
  unlink(ds.rev.fn)
})

test_that("Extra samples and subjects are detected",{
  ds <- .read_ds_file(ssm_ds)
  ssm_exp <- ds[,1:2]
  ssm_exp_less <- ssm_exp[-c(3:4),]
  out <- check_ssm(ssm_ds, ssm_exp=ssm_exp_less)
  expect_equal(out$extra_subjects, c("3","4"))
  expect_equal(out$extra_samples, c("S3","S4"))
})

test_that("Missing samples and subjects are detected",{
  ds <- .read_ds_file(ssm_ds)
  ssm_exp <- ds[,1:2]
  ext_rows <- c(SUBJECT_ID=999, SAMPLE_ID="S999")
  ssm_exp_more <- rbind(ssm_exp, ext_rows)
  out <- check_ssm(ssm_ds, ssm_exp=ssm_exp_more)
  expect_equal(out$missing_subjects, "999")
  expect_equal(out$missing_samples, "S999")
})

test_that("Mapping differences detected",{
  ds <- .read_ds_file(ssm_ds)
  ssm_exp <- ds[,1:2]
  ssm_exp$SAMPLE_ID[3] <- "S999"
  out <- check_ssm(ssm_ds, ssm_exp=ssm_exp)
  expect_equivalent(out$ssm_diffs, data.frame(SUBJECT_ID="3", SAMPLE_ID="S999"))
})

test_that("Non TOPMed sample uses are detected",{
  out <- check_ssm(ssm_ds, topmed=TRUE)
  expect_equal(nrow(out$sampuse_diffs), 20)
  expect_warning(check_ssm(ssm_ds, topmed=TRUE, sample_use="Array_SNP"), "Non TOPMed sample uses were provided; manually setting to 'Seq_DNA_WholeGenome; Seq_DNA_SNP_CNV.' To check other sample_uses, set topmed=FALSE", fixed=TRUE)
})

test_that("Discrepant sample uses are detected",{
  # one sample use value
  out <- check_ssm(ssm_ds, sample_use="Array_SNP")
  expect_equal(nrow(out$sampuse_diffs), 12)

  # data frame of sample uses values
  samp_use <- ds[,c(2,5)]
  samp_use$SAMPLE_USE[1:2] <- "Array_SNP"
  out <- check_ssm(ssm_ds, sample_use=samp_use)
  expect_equal(nrow(out$sampuse_diffs), 2)
})

test_that("Missing and required variables are detected",{
  ds.rev <- .read_ds_file(ssm_ds)
  ds.rev$SAMPLE_USE <- NULL
  ds.rev.fn <- tempfile(fileext=".txt")
  write.table(ds.rev, file=ds.rev.fn, col.names=TRUE, row.names=FALSE,
              quote=FALSE, sep="\t")

  expect_equal(check_ssm(ds.rev.fn)$missing_vars, "SAMPLE_USE")
  unlink(ds.rev.fn)
})


