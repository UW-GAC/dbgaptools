context("Checking data dictionary (DD) file")

# use sample attributes as starting 
ddfn <- system.file("extdata", "3b_dbGaP_SampleAttributesDD.xlsx", package = "dbgaptools", mustWork = TRUE)
dsfn <- system.file("extdata", "3a_dbGaP_SampleAttributesDS.txt", package = "dbgaptools", mustWork = TRUE)

test_that("Missing DS type argument stops with error", {
  dd <- .read_dd_file(ddfn)
  str <- "Please specify dstype, one of: pheno, ped, sattr, ssm, subj"
  expect_error(.check_dd(dd), str)
})

test_that("Non-standard DS type argument stops with error",{
   dd <- .read_dd_file(ddfn)  
   expect_error(.check_dd(dd, dstype="other"), "Please specify dstype, one of: pheno, ped, sattr, ssm, subj")
})

test_that("Non-uppercase column names returns a warning",{
  dd <- .read_dd_file(ddfn)
  dd$VARIABLE_TERM <- NULL  
  names(dd)[2] <- tolower(names(dd)[2])
  expect_warning(out <- .check_dd(dd, dstype="sattr"), "All column names should be UPPER CASE", fixed=TRUE)
  expect_true(out$lowercase)
})

test_that("Missing and required initialcolumns stops with error",{
  dd <- .read_dd_file(ddfn)
  dd$VARIABLE_TERM <- NULL
  names(dd)[1] <- "VARIABLE"
  str <- "First two columns required to be 'VARNAME' and 'VARDESC'"
  expect_error(.check_dd(dd, dstype="sattr"), str)
  names(dd)[1:2] <- c("VARNAME","DESC")
  expect_error(out <- .check_dd(dd, dstype="sattr"), str, fixed=TRUE)
})

test_that("Extra variables are reported",{
  # For some reason, dbGaP example file has extra variable (maybe examples will be updated)
  dd <- .read_dd_file(ddfn)
  expect_warning(out <- .check_dd(dd, dstype="sattr"), "DD contains non-standard columns: VARIABLE_TERM")
  expect_equal(out$extra_vars, "VARIABLE_TERM")
})

test_that("Missing variables are reported",{
  dd <- .read_dd_file(ddfn)
  dd$UNITS <- NULL
  # remove known extra variable to avoid that warning
  dd$VARIABLE_TERM <- NULL
  expect_equal(.check_dd(dd, dstype="sattr")$missing_reqvars,"UNITS")
})

test_that("Check VALUES position other than last returns warning",{
  dd <- .read_dd_file(ddfn)
  names(dd)[9] <- "VALUES"
  names(dd)[16] <- "COMMENT1"
  dd$VARIABLE_TERM <- NULL  
  str <- "'VALUES' must be last column"
  expect_equal(.check_dd(dd, dstype="sattr")$vals_warnings$lastcol_warn, str, fixed=TRUE)
})

test_that("VALUES columns with multiple encodings returns warnings",{
  # one encoded var
  dd <- .read_dd_file(ddfn)
  # want one warning to test
  dd$VARIABLE_TERM <- NULL
  dd$VALUES[4] <- "Y=Is Tumor, N/A=not applicable"
  str <- "IS_TUMOR variable(s) has multiple VALUE entries per cell. Only the first entry per cell will be evaluated. Encoded values must be split into one per cell."
  expect_equal(.check_dd(dd, dstype="sattr")$vals_warnings$multiple_vals_warn, str)

  # two encoded var
  dd[9,15:16] <- c("1=one", "2=two, 3=three")
  str <- "IS_TUMOR; TUMOR_GRADE variable(s) has multiple VALUE entries per cell. Only the first entry per cell will be evaluated. Encoded values must be split into one per cell."
  expect_equal(.check_dd(dd, dstype="sattr")$vals_warnings$multiple_vals_warn, str)  
})

test_that("Undefined encoded vars in DS returns warning", {
  # remove first warning
  ds <- .read_ds_file(dsfn)
  ds[,"VARIABLE_TERM"] <- NULL
  dd <- .read_dd_file(ddfn)
  dd$VARIABLE_TERM <- NULL

  # one encoded var
  ds.save <- ds
  ds$IS_TUMOR[sample(1:nrow(ds),4)] <- "M"
  str <- "For variable IS_TUMOR, the following values are undefined in the dd: M"
  expect_equal(.check_dd(dd, ds, dstype="sattr")$vals_warnings$undefined_vals_warn, str)

  # two encoded vars
  ds <- ds.save
  dd[9,15:16] <- c("G2=grade2", "G3=grade3")
  str2 <- "For variable TUMOR_GRADE, the following values are undefined in the dd: G4 GX"
  expect_equal(.check_dd(dd, ds, dstype="sattr")$vals_warnings$undefined_vals_warn, str2)
})

test_that("Difference in DS and DD variables returns warning", {
  # remove first warning
  ds <- .read_ds_file(dsfn)
  ds[,"VARIABLE_TERM"] <- NULL
  dd <- .read_dd_file(ddfn)
  dd$VARIABLE_TERM <- NULL  

  names(ds)[11] <- "GENOTYPING_LAB"
  expect_warning(.check_dd(dd, ds, dstype="sattr"), "Data dictionary has extra variables not in dataset: GENOTYPING_CENTER", fixed=TRUE)
  expect_warning(out <- .check_dd(dd, ds, dstype="sattr"), "Data dictionary missing following dataset variables: GENOTYPING_LAB", fixed=TRUE)
  expect_equal(out$extra_ddvars, "GENOTYPING_CENTER")
  expect_equal(out$missing_dsvars, "GENOTYPING_LAB")
})

test_that("DS values outside of MIN and MAX ranges warning", {
  # remove first warning
  ds <- .read_ds_file(dsfn)
  ds[,"VARIABLE_TERM"] <- NULL
  dd <- .read_dd_file(ddfn)
  dd$VARIABLE_TERM <- NULL
  
  var <- "SEQUENCING_CENTER"
  dd$MIN[dd$VARNAME %in% var] <- 2
  dd$MAX[dd$VARNAME %in% var] <- 5
  expect_equal(.check_dd(dd, ds, dstype="sattr")$min_errors, "SEQUENCING_CENTER")
  
  dd$MIN[dd$VARNAME %in% var] <- 0
  dd$MAX[dd$VARNAME %in% var] <- 1
  expect_equal(.check_dd(dd, ds, dstype="sattr")$max_errors, "SEQUENCING_CENTER")  
})

test_that("Illegal characters in variable names are reported", {
  # remove first warning
  dd <- .read_dd_file(ddfn)
  dd$VARIABLE_TERM <- NULL
  
  dd$VARNAME[3] <- paste0("DBGAP_",dd$VARNAME[3])
  expect_equal(.check_dd(dd, dstype="sattr")$illegal_vars, "DBGAP_ANALYTE_TYPE")

  dd$VARNAME[3] <- "ANALYTE/_TYPE"
  expect_equal(.check_dd(dd, dstype="sattr")$illegal_vars, "ANALYTE/_TYPE")

  dd$VARNAME[3] <- "ANALYTE\\_TYPE"
  expect_equal(.check_dd(dd, dstype="sattr")$illegal_vars, "ANALYTE\\_TYPE")  
})

test_that("Missing VALUES only reported for select filetypes", {
  dd <- .read_dd_file(ddfn)
  dd <- dd[,1:2]
  expect_equal(.check_dd(dd, dstype="pheno")$missing_reqvars[1], "VALUES")
  expect_equal(.check_dd(dd, dstype="ped")$missing_reqvars[1], "VALUES")
  expect_equal(.check_dd(dd, dstype="sattr")$missing_reqvars[1], "VALUES")
  expect_equal(.check_dd(dd, dstype="subj")$missing_reqvars[1], "VALUES")  
})

test_that("Missing UNITS only reported for select filetypes", {
  dd <- .read_dd_file(ddfn)
  dd <- dd[,1:2]
  expect_equal(.check_dd(dd, dstype="pheno")$missing_reqvars[2], "UNITS")
  expect_equal(.check_dd(dd, dstype="sattr")$missing_reqvars[2], "UNITS")  
})

test_that("Unique key marked with something besides 'X' is reported", {
    dd.rev <- .read_dd_file(ddfn)
    dd.rev$UNIQUEKEY[dd.rev$UNIQUEKEY %in% "X"] <- "Y"
    # get rid of this non-standard column
    dd.rev$VARIABLE_TERM <- NULL    
    out <- .check_dd(dd.rev, dstype="sattr")
    str <- "Only 'X' should be used to flag UNIQUE key columns"
    expect_equal(out$uniquekey_flags$wrong_mark, str)
})

test_that("Non-unique uniquekey is reported",{
  ds <- .read_ds_file(dsfn)
  # change UNIQUEKEY
  dd.rev <- .read_dd_file(ddfn)
  # get rid of this non-standard column
  dd.rev$VARIABLE_TERM <- NULL
  dd.rev$UNIQUEKEY[dd.rev$UNIQUEKEY %in% "X"] <- NA
  dd.rev$UNIQUEKEY[dd.rev$VARNAME %in% "GENOTYPING_CENTER"] <- "X"
  out <- .check_dd(dd.rev, ds, dstype="sattr")
  str <- "UNIQUEKEY columns (GENOTYPING_CENTER) do not specify unique records in DS"
  expect_equal(out$uniquekey_flags$notunique, str)
})

test_that("Uniquekey for wrong file type is reportd",{
  dd.rev <- .read_dd_file(ddfn)
  # get rid of this non-standard column
  dd.rev$VARIABLE_TERM <- NULL  
  # pretend this is not a sample attributes file
  out <- .check_dd(dd.rev, dstype="subj")
  str <- "UNIQUEKEY columns(s) should not be defined for dstype subj"
  expect_equal(out$uniquekey_flags$wrong_dstype, str)
})
