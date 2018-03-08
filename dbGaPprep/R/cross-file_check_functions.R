# functions that check for consistency across files, primarily I'm thinking presence of expected sample and subjects

#' Cross file checks
#'
#' Check presence of expected subjects and samples across dbGaP files.
#'
#' @param subj_file Path to subject consent file on disk
#' @param ssm_file Path to sample-subject mapping file on disk
#' @param molecular_samples Vector of sample IDs with molecular data
#' @param sattr_file Path to sample attributes file on disk
#' @param pheno_file Path to phenotype file on disk
#' @param ped_file Path to pedigree file on disk
#' @param subjectID_col Column name for subject-level ID across files
#' @param subjectID_col Column name for consent in subject file
#'
#' @details
#' Checks for presence of expected subjects and samples across a set of dbGaP files.
#' At a minimum, requires a subject consent file, sample-subject mapping file, and list of sample IDs for which molecular data is being submitted.
#' Subjects with consent codes other than 0 and positive integers are returned as an error and excluded from further checks.
#' Including additional files increases the number of pairwise checks done across files.
#' The basic principles behind these checks are:
#' \itemize{
#'   \item{subject file: must contain all subjects in phenotype and pedigree files}
#'   \item{sample-subject mapping file: must contain all samples with molecular data; all samples listed here must map to subjects with consent=0 or consent >=1 in subject file}
#'   \item{pedigree file: subjects not mapping to samples with molecular data (i.e. linking individuals) are expected to have consent=0}
#'   \item{phenotype file: should have no subjects missing consent or with consent=0}
#'   \item{sample attributes file: all samples listed here must map to subjects with consent >= 1 in subject file}
#'}
#' Note issues returned in the report may not always require corrective action - i.e. sometimes there are extenuating circumstances, such as when consented study subjects are missing from current molecular data submissions but expected in future submissions, and are thus retained in dbGaP files with non-zero consent status.
#'
#' @return cross_check_report, a list of the following issues (when present):
#' \item{ssm_miss_molecular}{List of molecular data samples missing from sample-subject mapping file}
#' \item{ssm_no_molecular}{List of samples in the sample-subject mapping file that are not molecular data samples}
#' \item{subj_consent_err}{List of subjects in subject consent file with invalid consent codes, which were excluded from subsequent checks}
#' \item{ssm_consent_err}{List of subject IDs in sample-subject mapping file either missing from subject file, or in subject file but with invalid consent code}
#' \item{sattr_consent_err}{List of sample in sample attributes file that map to subjects with consent other than >= 1}
#' \item{pheno_consent_err}{List of subjects in the phenotype file that have consent other than >= 1}
#' \item{subj_miss_ped}{Subject IDs in pedigree file that are missing from subject consent file}
#' \item{ped_consent_err}{List of subjects in pedigree file having non-0 consent and not mapping to a sample with molecular data}
#'
#' @rdname check_cross_file

check_cross_file <- function(subj_file, ssm_file, molecular_samples,
                             sattr_file=NULL, pheno_file=NULL, ped_file=NULL,
                             subjectID_col="SUBJECT_ID", consent_col="CONSENT"){
  
  # read in required files
  subj <- .read_ds_file(subj_file)
  ssm <- .read_ds_file(ssm_file)

  # cannot proceed without specified subject ID col
  if(!is.element(subjectID_col, names(subj)) | !is.element(subjectID_col, names(ssm))){
    stop("Please check that files contains columns for subject-level ID")
  }

  # cannot proceed without specfied consent col
  if(!is.element(consent_col, names(subj))){
    stop("Please check that files contains columns for subject-level ID")
  }
  
  # check that all samples with molecular data are in ssm
  ssm_miss_molecular <- setdiff(molecular_samples, ssm[,"SAMPLE_ID"])

  # standardize column names
  if(subjectID_col != "SUBJECT_ID"){
    names(subj)[names(subj) %in% subjectID_col] <- "SUBJECT_ID"
    names(ssm)[names(ssm) %in% subjectID_col] <- "SUBJECT_ID"    
  }
  
  if(consent_col != "CONSENT"){
    names(subj)[names(subj) %in% consent_col] <- "CONSENT"
  }

  # only use subjects in subject consent file w/valid consent codes (0, positive integers)
  sel <- is.na(subj$CONSENT) | grepl("\\D", subj$CONSENT)
  # report invalid consents
  subj_consent_err <- subj[sel,]
  
  # continue checks using subjects with valid consent codes
  subj <- subj[!sel,]
  
  # create list of subject ids with different consent status
  subjs_study_cons <- subj$SUBJECT_ID[subj$CONSENT >= 1]
  subjs_zero_cons <- subj$SUBJECT_ID[subj$CONSENT %in% 0]

  # check subj <> ssm: all ssm samples must map to subject with consent = 0 or >= 1
  ssm_consent_err <- setdiff(ssm$SUBEJCT_ID, subj$SUBJECT_ID)
  
  # ssm samples should have molecular data being submited
  ssm_no_molecular <- setdiff(ssm$SAMPLE_ID, molecular_samples)
  
  # check subj <> sattr
  if(!is.null(sattr_file)){
    sattr <- .read_ds_file(sattr_file)
    # all samples listed here must map to subject with consent >= 1 in subj file
    samps_ok <- ssm$SAMPLE_ID[ssm$SUBJECT_ID %in% subjs_study_cons]
    sattr_consent_err <- setdiff(sattr$SAMPLE_ID, samps_ok)
  }
  
  # check subj <> pheno
  if(!is.null(pheno_file)){
    pheno <- .read_ds_file(pheno_file)
    # all subjs listed here must have consent >= 1 in subj file
    pheno_consent_err <- setdiff(pheno[,subjectID_col], subjs_study_cons)
  }
  
  # check subj <> pedigree
  if(!is.null(ped_file)){
    # all subjs should be present in subject consent file. merge the two
    ped_merg <- merge(ped, subj, by.x=subjectID_col, by.y="SUBJECT_ID",
                      all.x=TRUE, all.y=FALSE)
    names(ped_merg)[1] <- "SUBJECT_ID"
    subj_miss_ped <- ped_merg$SUBJECT_ID[is.na(ped_merg$CONSENT)]
    
    # subjs not mapping to samples with molecular data (i.e. linking) should have consent=0
    ped_merg$molecular_sample <- ped_merg[,1] %in%
                                 ssm$SUBJECT_ID[ssm$SAMPLE_ID %in% molecular_samples]
    ped_consent_err <- ped_merg$SUBJECT_ID[!ped_merg$molecular_sample &
                                           ped_merg$CONSENT != 0]    
  }

  # return list of errors
  rpt <- list()
  
  if(length(ssm_miss_molecular) > 0) rpt$ssm_miss_molecular <- ssm_miss_molecular
  if(length(ssm_no_molecular) > 0) rpt$ssm_no_molecular <- ssm_no_molecular
  if(length(subj_consent_err) > 0) rpt$subj_consent_err <- subj_consent_err
  if(length(ssm_consent_err) > 0) rpt$ssm_consent_err <- ssm_consent_err
  if(length(sattr_consent_err) > 0) rpt$sattr_consent_err <- sattr_consent_err
  if(length(pheno_consent_err)> 0) rpt$pheno_consent_err <- pheno_consent_err
  if(length(subj_miss_ped) > 0) rpt$subj_miss_ped <- subj_miss_ped
  if(length(ped_consent_err) > 0) rpt$ped_consent_err > 0 <- ped_consent_err > 0

  if(length(rpt) > 0){
    cross_check_report <- rpt
  } else {
    cros_check_report <- NULL
  }
}

