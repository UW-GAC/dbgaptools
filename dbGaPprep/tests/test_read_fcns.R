library(testthat)
library(dbGaPprep)

context("Reading in dataset and data dictionary files")

# Load in example data files
# Notes examples are downloaded from dbGaP Submission Guide, accessed Sep 30, 2017

satt_dd <- system.file("extdata", "3b_dbGaP_SampleAttributesDD.xlsx", package = "dbGaPprep", mustWork = TRUE)
satt_ds <- system.file("extdata", "3a_dbGaP_SampleAttributesDS.txt", package = "dbGaPprep", mustWork = TRUE)

## maybe delete these?
## subj_dd <- system.file("extdata", "4b_dbGaP_SubjectDD.xlsx", package = "dbGaPprep", mustWork = TRUE)
## subj_ds <- system.file("extdata", "4a_dbGaP_SubjectDS.txt", package = "dbGaPprep", mustWork = TRUE)

## ssm_dd <- system.file("extdata", "5b_dbGaP_SubjectSampleMappingDD.xlsx", package = "dbGaPprep", mustWork = TRUE)
## ssm_ds <- system.file("extdata", "5a_dbGaP_SubjectSampleMappingDS.txt", package = "dbGaPprep", mustWork = TRUE)

# variations on sample attributes DD
dd_nohdr_txt <- system.file("extdata", "sampleAttributesDD_noheader.txt", package = "dbGaPprep", mustWork = TRUE)
dd_nohdr_xls <- system.file("extdata", "sampleAttributesDD_noheader.xlsx", package = "dbGaPprep", mustWork = TRUE)
dd_hdr_txt <- system.file("extdata", "sampleAttributesDD_hasheader.txt", package = "dbGaPprep", mustWork = TRUE)
dd_hdr_xls <- system.file("extdata", "sampleAttributesDD_hasheader.xlsx", package = "dbGaPprep", mustWork = TRUE)

test_that("DD header lines are properly counted", {
  expect_equal(.count_hdr_lines(dd_nohdr_txt, colname="VARNAME"), 0)
  expect_equal(.count_hdr_lines(dd_hdr_txt, colname="VARNAME"), 3)
})

test_that("DD files are read in properly", {
  dd <- .read_dd_file(satt_dd)
  expect_equal(ncol(dd), 17)
  expect_equal(nrow(dd), 12)
})

test_that("DS files are read in properly", {
  # will errors here get propogated to test log file?
  ds <- .read_ds_file(satt_ds)
  expect_equal(ncol(ds), 12)
  expect_equal(nrow(ds), 17)

  # give a DD file with header to check warning message
  expect_warning(.read_ds_file(dd_hdr_txt), "Additional rows are present before column headers and should be removed prior to dbGaP submission")
})

# test complaints about file extensions
