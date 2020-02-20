context("Checking cross-file checks")

subj_file <- system.file("extdata", "4a_dbGaP_SubjectDS.txt", package = "dbgaptools",
                         mustWork = TRUE)
subj <- read_ds_file(subj_file)
ssm_file <- system.file("extdata", "5a_dbGaP_SubjectSampleMappingDS.txt", package = "dbgaptools",
                        mustWork = TRUE)
ssm <- read_ds_file(ssm_file)
sattr_file <- system.file("extdata", "3a_dbGaP_SampleAttributesDS.txt", package = "dbgaptools",
                          mustWork = TRUE)
sattr <- read_ds_file(sattr_file)
pheno_file <- system.file("extdata", "2a_dbGaP_SubjectPhenotypesDS.txt", package = "dbgaptools",
                          mustWork = TRUE)
pheno <- read_ds_file(pheno_file)
ped_file <- system.file("extdata", "6a_dbGaP_PedigreeDS.txt", package = "dbgaptools",
                        mustWork = TRUE)
ped <- read_ds_file(ped_file)

subject_id_col <- "SUBJECT_ID"
consent_col <- "CONSENT"

test_that("Compliant files run error free", {
  expect_null(check_cross_file(subj_file, ssm_file, molecular_samples = ssm$SAMPLE_ID))

  # ok to have pheno and ped missing non-molecular samples (indeed these are HapMaps)
  out <- check_cross_file(subj_file, ssm_file, molecular_samples = ssm$SAMPLE_ID,
                          sattr_file, pheno_file, ped_file)
  expect_equal(names(out), c("sattr_miss_molecular", "ped_miss_molecular"))
})

test_that("Compliant dataframes run error free", {
  expect_null(check_cross_file(subj, ssm, molecular_samples = ssm$SAMPLE_ID))

  # ok to have pheno and ped missing non-molecular samples (indeed these are HapMaps)
  out <- check_cross_file(subj, ssm, molecular_samples = ssm$SAMPLE_ID,
                          sattr, pheno, ped)
  expect_equal(names(out), c("sattr_miss_molecular", "ped_miss_molecular"))
})

test_that("Missing subject ID column stops with error", {
  str <- "Please check that files contain columns for subject-level ID"
  expect_error(check_cross_file(subj_file, ssm_file, molecular_samples = ssm$SAMPLE_ID,
                                subjectID_col = "mysubject"), str, fixed = TRUE)
})

test_that("Missing consent column stops with error", {
  str <- "Please check that subject file contains consent column"
  expect_error(check_cross_file(subj_file, ssm_file, molecular_samples = ssm$SAMPLE_ID,
                                consent_col = "mycons"), str, fixed = TRUE)
})

test_that("Report molecular data samples missing from ssm", {
  extras <- c("S888", "S999")
  molecular_samples <- c(ssm$SAMPLE_ID, extras)
  out <- check_cross_file(subj_file, ssm_file, molecular_samples)
  expect_equal(out$ssm_miss_molecular, extras)
})

test_that("Invalid consent code removed and reported: negative integer", {
  subj_rev <- subj
  subj_rev$CONSENT[10] <- "-99"
  exp_df <- subj_rev[10, ]
  subj_rev_fn <- tempfile(fileext = ".txt")
  write.table(subj_rev, file = subj_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_rev_fn, ssm_file, molecular_samples = ssm$SAMPLE_ID)
  expect_equal(out$subj_consent_err, exp_df)

  unlink(subj_rev_fn)
})

test_that("Invalid consent code removed and reported: NA/missing", {
  subj_rev <- subj
  subj_rev$CONSENT[11] <- NA
  exp_df <- subj_rev[11, ]
  subj_rev_fn <- tempfile(fileext = ".txt")
  write.table(subj_rev, file = subj_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_rev_fn, ssm_file, molecular_samples = ssm$SAMPLE_ID)
  expect_equal(out$subj_consent_err, exp_df)

  unlink(subj_rev_fn)

})

test_that("Invalid consent code removed and reported: string", {
  subj_rev <- subj
  subj_rev$CONSENT[12] <- "GRU"
  exp_df <- subj_rev[12, ]
  subj_rev_fn <- tempfile(fileext = ".txt")
  write.table(subj_rev, file = subj_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_rev_fn, ssm_file, molecular_samples = ssm$SAMPLE_ID)
  expect_equal(out$subj_consent_err, exp_df)

  unlink(subj_rev_fn)
})

test_that("SSM samples missing from subject file are reported", {
  subj_rev <- subj
  subj_rev <- subj_rev[-c(1:3), ]
  subj_rev_fn <- tempfile(fileext = ".txt")
  write.table(subj_rev, file = subj_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_rev_fn, ssm_file, molecular_samples = ssm$SAMPLE_ID)
  expect_equal(out$subj_miss_ssm, c("1", "2", "3"))
  unlink(subj_rev_fn)
})

test_that("SSM samples not in list of molecular samples are reported ", {
  out <- check_cross_file(subj_file, ssm_file, molecular_samples = ssm$SAMPLE_ID[-c(1:2)])
  expect_equal(out$ssm_no_molecular, c("S1", "S2"))
})

test_that("Sample attribues file samples without consent >= 1 are reported", {
  # change consent to 0 for a sample attributes file sample
  subj_rev <- subj
  subj_rev$CONSENT[3] <- 0
  subj_rev_fn <- tempfile(fileext = ".txt")
  write.table(subj_rev, file = subj_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_rev_fn, ssm_file, molecular_samples = ssm$SAMPLE_ID, sattr_file)
  expect_equal(out$sattr_consent_err, "S3")

  unlink(subj_rev_fn)
})

test_that("Molecular data samples missing from sample attributes are reported", {
  extras <- c("S888", "S999")
  molecular_samples <- c(ssm$SAMPLE_ID, extras)
  out <- check_cross_file(subj_file, ssm_file, molecular_samples, sattr_file)
  # dbgap example files has some a sample in ssm but missing from sample attributes (S18)
  expect_equal(out$sattr_miss_molecular, c("S18", extras))
})

test_that("Subjects in pheno file without consent >= 1 are reported", {
  # change consent to 0 for a pheno file subject
  subj_rev <- subj
  subj_rev$CONSENT[3] <- 0
  subj_rev_fn <- tempfile(fileext = ".txt")
  write.table(subj_rev, file = subj_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_rev_fn, ssm_file, molecular_samples = ssm$SAMPLE_ID,
                          sattr_file, pheno_file)
  expect_equal(out$pheno_consent_err, "3")

  unlink(subj_rev_fn)
})

test_that("Molecular subjects missing from pheno file are reported", {
  # remove pheno records
  pheno_rev <- pheno
  pheno_rev <- pheno_rev[-c(5, 7), ]
  pheno_rev_fn <- tempfile(fileext = ".txt")
  write.table(pheno_rev, file = pheno_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_file, ssm_file, molecular_samples = ssm$SAMPLE_ID,
                          sattr_file, pheno_rev_fn)
  expect_equal(out$pheno_miss_molecular, c("5", "7"))

  unlink(pheno_rev_fn)
})

test_that("Pedigree subjects missing from subject file are reported", {
  # remove subj records
  subj_rev <- subj
  subj_rev <- subj_rev[-c(1:3), ]
  subj_rev_fn <- tempfile(fileext = ".txt")
  write.table(subj_rev, file = subj_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_rev_fn, ssm_file, molecular_samples = ssm$SAMPLE_ID,
                          sattr_file, ped = ped_file)
  expect_equal(out$subj_miss_ped, c("1", "2", "3"))

  unlink(subj_rev_fn)
})

test_that("Pedigree subjects that don't map to molecluar samples should have consent = 0", {
  # set linking subject to consent = 2
  subj_rev <- subj
  subj_rev$CONSENT[14] <- 2
  subj_rev_fn <- tempfile(fileext = ".txt")
  write.table(subj_rev, file = subj_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_rev_fn, ssm_file, molecular_samples = ssm$SAMPLE_ID,
                          sattr_file, ped = ped_file)
  expect_equal(out$ped_consent_err, "1001")

  unlink(subj_rev_fn)
})

test_that("Molecular data samples not in pedigree are repored", {
  # remove pedigree record
  ped_rev <- ped
  ped_rev <- ped_rev[-4, ]
  ped_rev_fn <- tempfile(fileext = ".txt")
  write.table(ped_rev, file = ped_rev_fn, col.names = TRUE, row.names = FALSE,
              quote = FALSE, sep = "\t", na = "")

  out <- check_cross_file(subj_file, ssm_file, molecular_samples = ssm$SAMPLE_ID,
                          sattr_file, ped = ped_rev_fn)
  # Hapmaps are missing, too
  expect_equal(out$ped_miss_molecular, c("2", "A155", "A156"))

  unlink(ped_rev_fn)
})
