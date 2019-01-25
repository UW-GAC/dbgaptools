context("Reading in dataset and data dictionary files")

# Load in example data files
# Notes examples are downloaded from dbGaP Submission Guide, accessed Sep 30, 2017

satt_dd <- system.file("extdata", "3b_dbGaP_SampleAttributesDD.xlsx", package = "dbgaptools", mustWork = TRUE)
satt_ds <- system.file("extdata", "3a_dbGaP_SampleAttributesDS.txt", package = "dbgaptools", mustWork = TRUE)

# variations on sample attributes DD
dd_nohdr_txt <- system.file("extdata", "sampleAttributesDD_noheader.txt", package = "dbgaptools", mustWork = TRUE)
dd_nohdr_csv <- system.file("extdata", "sampleAttributesDD_noheader.csv", package = "dbgaptools", mustWork = TRUE)
dd_nohdr_xls <- system.file("extdata", "sampleAttributesDD_noheader.xlsx", package = "dbgaptools", mustWork = TRUE)
dd_hdr_txt <- system.file("extdata", "sampleAttributesDD_hasheader.txt", package = "dbgaptools", mustWork = TRUE)
dd_hdr_xls <- system.file("extdata", "sampleAttributesDD_hasheader.xlsx", package = "dbgaptools", mustWork = TRUE)
dd_nohdr_multsheet <- system.file("extdata", "sampleAttributesDD_noheader_multSheets.xlsx", package = "dbgaptools", mustWork = TRUE)

test_that("Header lines are properly counted", {
  expect_equal(.count_hdr_lines(dd_nohdr_txt, colname="VARNAME"), 0)
  expect_equal(.count_hdr_lines(dd_hdr_txt, colname="VARNAME"), 3)
  expect_equal(.count_hdr_lines(satt_ds), 0)
})

test_that("DD files have expected dimensions when read in", {
  dd <- read_dd_file(satt_dd)
  expect_equal(ncol(dd), 17)
  expect_equal(nrow(dd), 12)
})

test_that("DS files have expected dimensions when read in", {
  # will errors here get propogated to test log file?
  ds <- read_ds_file(satt_ds)
  expect_equal(ncol(ds), 12)
  expect_equal(nrow(ds), 17)
})

test_that("DD file with header generates a warning",{
  expect_warning(read_dd_file(dd_hdr_txt), "Additional rows are present before column headers and should be removed prior to dbGaP submission")
  expect_warning(read_dd_file(dd_hdr_xls), "Additional rows are present before column headers and should be removed prior to dbGaP submission")
})

test_that("Non existent file paths generate stop message",{
  expect_error(read_ds_file("myDSfile.txt"), "file.exists(filename) is not TRUE",
               fixed=TRUE)
  expect_error(read_dd_file("myDDfile.xlsx"), "file.exists(filename) is not TRUE",
               fixed=TRUE)
})

test_that("Unexpected file extensions generate expected stop messages",{
  expect_error(read_ds_file(satt_dd),
               "Expected tab-delimited input file (.txt), not .xlsx", fixed=TRUE)
  expect_error(read_dd_file(dd_nohdr_csv),
               "Expected tab-delimited or Excel input file, not .csv", fixed=TRUE)

})

test_that("Mult sheet Excel workbooks reads in first sheet as DD", {
  expect_warning(read_dd_file(dd_nohdr_multsheet),
"Data dictionary Excel contains multiple sheets; assuming first is the DD", fixed=TRUE)
})

test_that("DD with no 'VALUES' column reads in without error", {
    dd <- read_dd_file(satt_dd)
    dd.rev.fn <- tempfile(fileext=".txt")
    # keep only first 3 cols
    write.table(dd[,1:3], file=dd.rev.fn, col.names=TRUE, row.names=FALSE,
                quote=FALSE, sep="\t", na="")
    expect_error(read_dd_file(dd.rev.fn), NA)
})

test_that("read_dd_file works with an xml data dictionary", {
  dd_file <- system.file("extdata", "phs000001.v3.pht002477.v1.AREDS_Subject.data_dict.xml", package = "dbgaptools", mustWork = TRUE)
  dd <- read_dd_file(dd_file)
  expect_equal(names(dd), c("VARNAME", "VARDESC", "TYPE", "VALUES", "X__1"))
  expect_equal(dd$VARNAME, c("ID2", "consent"))
  expect_equal(dd$VARDESC, c("De-identified subject ID", "Consent group description"))
  expect_equal(dd$TYPE, c(NA, NA))
  expect_equal(dd$VALUES, c(NA, "1=EDO (Eye Disease Research Only)"))
  expect_equal(dd$X__1, c(NA, "2=GRU (General Research Use)"))
})

test_that("read_dd_file processes unique key information in xml files", {
  dd_file <- system.file("extdata", "phs000001.v3.pht000378.v2.hospitalization.data_dict.xml", package = "dbgaptools", mustWork = TRUE)
  dd <- read_dd_file(dd_file)
  expect_equal(names(dd), c("VARNAME", "VARDESC", "TYPE", "UNITS", "MIN", "MAX", "UNIQUE_KEY", "VALUES", "X__1", "X__2", "X__3", "X__4", "X__5"))
  expect_equal(dd$UNIQUE_KEY, c("X", NA, NA, NA, "X", NA))
})

# TO ADD:
# read_ds_file and read_dd_file check that returns an error from the tryCatch


