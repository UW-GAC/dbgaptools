context("Checking the convenience wrapper function to write.table")

test_that("Blank study name and phs with generate_fn=T stops with error" ,{
  x <- data.frame(col1=1:10, col2=1:10)
  expect_error(write_dbgap(x, generate_fn=TRUE), "To generate file name, one of studyname or phs must be provided")
})

test_that("generate_fn=T without dstype stops with error", {
    x <- data.frame(col1=1:10, col2=1:10)
    expect_error(write_dbgap(x, study="mystudy", generate_fn=TRUE), "Please specify dstype, one of: pheno, ped, sattr, ssm, subj", fixed=TRUE)
})

test_that("Stop with error if no file name is provided and !generate_fn",{
  x <- data.frame(col1=1:10, col2=1:10)
  expect_error(write_dbgap(x), "Please specify filename, or set generate_fn=TRUE")
})

test_that("Check output file name for DS with study name" ,{
  setwd(tempdir())
  x <- data.frame(col1=1:10, col2=1:10)
  write_dbgap(x, study_name="mystudy", dstype="ssm", generate_fn=TRUE)

  exp_fn <- paste0("mystudy_ssm_DS_", format(Sys.Date(), "%Y%m%d"), ".txt")
  expect_true(exp_fn %in% list.files())

})

test_that("Check output file name for DD with study name" ,{
  setwd(tempdir())
  x <- data.frame(col1=1:10, col2=1:10)
  write_dbgap(x, study_name="mystudy", dstype="sattr", DD=TRUE, generate_fn=TRUE)

  exp_fn <- paste0("mystudy_sattr_DD_", format(Sys.Date(), "%Y%m%d"), ".txt")
  expect_true(exp_fn %in% list.files())

})

test_that("Check output file name for provided file name",{
  setwd(tempdir())
  x <- data.frame(col1=1:10, col2=1:10)
  write_dbgap(x, file="myfile.txt", generate_fn=FALSE)
  expect_true("myfile.txt" %in% list.files())  
})

test_that("Check output file name with non .txt extension stops with error",{
  setwd(tempdir())
  x <- data.frame(col1=1:10, col2=1:10)
  expect_warning(write_dbgap(x, file="myfile.csv", generate_fn=FALSE), "Output file name recommended to have .txt extension")
  expect_true("myfile.csv" %in% list.files())  
})

