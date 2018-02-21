#' Check data dictionary (generic)
#'
#' @param dd Data dictionary (DD) object
#' @param ds Corresponding dataset (DS) object
#' @param pheno_dd Logical on whether this is a DD for a phenotype file.
#'
#' @details
#' Data dictionary files can be Excel (.xls, .xlsx) or tab-delimited .txt.
#' Reports errors or issues with DD file.
#' When the corresponding DS file is also provided, checks for consistency between the two.
#' If \code{pheno_dd=TRUE}, checks for required UNITS variable.
#' 
#' @return dd_report, a list of the following issues (when present):
#' \item{lowercase}{Logical flag indicating non-upper case variable names}
#' \item{varname_vardesc}{Logical flag indicating first two variables are not VARNAME, VARDESC}
#' \item{missing_vars}{Missing and required variables}
#' \item{extra_vars}{Extra variables}
#' \item{vals_warnings}{Vector of warnings about VALUES columns}
#' \item{missing_dsvars}{Variables present in DS but not defined in DD}
#' \item{min_errs}{Variables for which DS value are < DD MIN}
#' \item{max_errs}{Variables for which DS value are > DD MAX}
#' \item{illegal_vars}{Variable names containing illegal characters: '\', '/', ',' (comma), or 'dbGaP' are present} 
#'
#' @rdname check_dd

.check_dd <- function(dd, ds=NULL, pheno_dd=FALSE){

  # all colnames should be upper case
  lowercase <- NULL
  upp <- toupper(names(dd))
  if(!all.equal(upp, names(dd))){
    warning("All column names should be UPPER CASE")
    names(dd) <- upp
    lowercase <- TRUE
  }
  
  # required first two columns
  varname_vardesc <- NULL
  if(!all.equal(names(dd)[1:2], c("VARNAME","VARDESC"))){
    warning("First two columns required to be 'VARNAME' and 'VARDESC'")
  }

  # required columns
  req_vars <- c("VARNAME","VARDESC", "VALUES")
  # need to ask dbGaP about this
  if(pheno_dd) req_vars <- c(req_vars, "UNITS")
  miss_vars <- setdiff(req_vars, names(dd))
  missing_vars <- ifelse(length(miss_vars) %in% 0, NA, miss_vars)

  # check existing named columns against all possible columns
  # if there are trailing columns, they are likely because of encoded values -
  # exclude from the check
  all_cols <- names(dd)[!grepl("X__", names(dd))]

  possible_cols <- c("VARNAME", "VARDESC", "DOCFILE", "TYPE", "UNITS", "MIN", "MAX",
                     "RESOLUTION", "COMMENT1", "COMMENT2", "VARIABLE_SOURCE",
                     "SOURCE_VARIABLE_ID", "VARIABLE_MAPPING", "UNIQUEKEY",
                     "COLLINTERVAL", "ORDER", "VALUES")

  extra_vars <- NULL
  extra_cols <- setdiff(all_cols, possible_cols)
  if(length(extra_cols) > 0){
    warning("DD contains non-standard columns:", extra_cols)
    extra_vars <- extra_cols
  }

  # prepare to collect series of warnings about VALUES column
  vals_warnings <- NULL
  
  # if VALUES col exists, needs to be last
  warn0 <- warn1 <- warn2 <- NULL
  if("VALUES" %in% toupper(names(dd))){
    nnames <- length(names(dd)[!grepl("X__", names(dd))])
    if("VALUES" != names(dd)[nnames]){
      warn0 <- "'VALUES' must be last column"
      warning(warn0)
    }

    # extract encoded values
    val_col <- which(names(dd) %in% "VALUES")
    name_col <- which(names(dd) %in% "VARNAME")

    # only proceed with this check if there are non NA entries in VALUES column
    if(sum(!is.na(dd$VALUES)) > 0) {
    
        encoded_vars <- dd[!is.na(dd$VALUES), c(name_col, val_col:ncol(dd))]

        # check for multiple "=" statements within a cell
        eq_count <- apply(encoded_vars, 2, function(x) {stringr::str_count(x, "=")})
        # replace NA with 0
        eq_count[is.na(eq_count)] <- 0

        # if only 1 VARNAME with VALUES, coerce 'eq_count' to 1 row matrix
        if(is.null(dim(eq_count))){
          eq_count <- matrix(eq_count, nrow=1, ncol=length(eq_count))
        }
        
        if(sum(Biobase::rowMax(eq_count) > 1) > 0){
          mult_eq_vars <- encoded_vars$VARNAME[Biobase::rowMax(eq_count) > 1]
          warn1 <- paste(mult_eq_vars, " variable(s) has multiple VALUE entries per cell. Only the first entry per cell will be evaluated. Encoded values must be split into one per cell.")
          warning(warn1)
        }

        # extract encoded values
        encoded_vars %<>%
          mutate_all(function(x) stringr::str_replace(x, "=.*", "")) %>%
            tidyr::gather("column", "row", -VARNAME) %>%
              tidyr::spread(VARNAME, row) %>%
                select(-column)

        # if ds is also provided, check that all encoded values are defined
        if(!is.null(ds)){
          vars_chk <- names(encoded_vars)
          for(var in vars_chk){
            var1 <- as.character(unique(ds[,var]))
            var2 <- dplyr::pull(encoded_vars, var)
            undef_vals <- setdiff(var1, var2)
            if(length(undef_vals) > 0){
              warn2 <- paste("For variable ", var,", the following values are undefined in the dd: ", paste(undef_vals, collapse=", "))
              warning(warn2)
            } # if extra values
          } # loop through encoded vars
        } #  if dataset is provided
    } # if there are non-NA entires in VALUES column
  } # if VALUES col is present
  
  warns <- c(warn0, warn1, warn2)
  if(length(warns)>0) vals_warnings <- warns

  # if dataset provided:
  # check for all vars
  missing_dsvars <- NULL
  min_errors <- max_errors <- NULL
  if(!is.null(ds)){
    miss.vars <- setdiff(names(ds), pull(dd, VARNAME))
    if(length(miss.vars) > 0){
      warning("Data dictionary missing following dataset variables:", miss.vars)
    }
    extra.vars <- setdiff(pull(dd, VARNAME), names(ds))
    if(length(extra.vars) > 0){
      warning("Data dictionary has extra variables not in dataset:", extra.vars)
    }
    
    # check MIN and MAX values
    if("MIN" %in% names(dd)){
      dd.tmp <- dd[,"MIN"]
      ds.tmp <- as.matrix(ds[, dd.tmp$VARNAME])
      # convert to numeric if necessary
      ds.tmp <- apply(ds.tmp, 2, as.numeric)
      dd.tmp$min.ds <- apply(ds.tmp, 2, min)
      range_err <- dd.tmp$VARNAME[dd.tmp$min.ds < as.numeric(MIN)]
      if(length(range.err) > 0) min_errors <- range_err

    }
       
    if("MAX" %in% names(dd)){
      dd.tmp <- dd[,"MAX"]
      ds.tmp <- as.matrix(ds[, dd.tmp$VARNAME])
      # convert to numeric if necessary
      ds.tmp <- apply(ds.tmp, 2, as.numeric)
      dd.tmp$max.ds <- apply(ds.tmp, 2, max)
      range_err <- dd.tmp$VARNAME[dd.tmp$max.ds > as.numeric(MAX)]
      if(length(range.err) > 0) max_errors <- range_err
    }
  }

  # check for illegal characters in variable names: \ / , dbGaP
  illegal_vars <- NULL
  ill_vars_sel <- stringr::str_detect(upp, "DBGAP|\\\\|/|,")
  
  if(sum(ill_vars_sel) > 0){
    ill_vars <- nms[ill_vars_sel]
    illegal_vars <- paste(ill_vars, collapse="; ")
  }

  # construct list object to return
  dd_report  <- list()

  if(!is.null(lowercase)) dd_report$lowercase <- lowercase
  if(!is.null(varname_vardesc))  dd_report$varname_vardesc <- varname_vardesc
  if(!is.null(missing_vars) & !is.na(missing_vars))  dd_report$missing_vars <- missing_vars
  if(!is.null(extra_vars))  dd_report$extra_vars <- extra_vars
  if(!is.null(vals_warnings))  dd_report$vals_warnings <- vals_warnings
  if(!is.null(missing_dsvars))  dd_report$missing_dsvars <- missing_dsvars
  if(!is.null(minmax_errs))  dd_report$minmax_errs <- minmax_erss
  if(!is.null(illegal_vars))  dd_report$illegal_vars <- illegal_vars

  if(length(dd_report) > 0) {
    return(dd_report)
  } else {
    return(NULL)
  }
}

#' Check sample subject mapping file
#'
#' Check contents of a sample subject mapping file for dbGaP posting.
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param ssm_exp Dataframe of expected SAMPLE_ID and SUBJECT_ID, with optionaly third column 'quarantine' (see Details below)
#' @param sampleID_col Column name for sample-level ID
#' @param subjectID_col Column name for subject-level ID
#' @param sample_uses Either a single string for expected SAMPLE_USE across all samples, or a data frame with SAMPLE_ID and SAMPLE_USE values
#' @param topmed Logical to indicate TOPMed study
#'
#' @details
#' The sample subject mapping file should be a tab-delimited .txt file.
#' When (\code{ssm_exp != NULL}), checks for expected correspondence between
#' SAMPLE_ID and SUBJECT_ID. Any differences in mapping between the two,
#' or a difference in the list of expected SAMPLE_IDs or SUBJECT_IDs,
#' will be returned in the output.
#' If (\code{ssm_exp != NULL}) contains an additional logical field 'quarantine,'
#' code will check that SAMPLE_USE is left blank for this record.
#' Quarantined samples will otherwise be treated as
#' other records in terms of checking for missing or extra subjects or samples.
#' 
#' If a data dictionary is provided (\code{ddfile != NULL}), additionally checks 
#' correspondence between column names in data file and entries in data dictionary.
#' Data dictionary files can be Excel (.xls, .xlsx) or tab-delimited .txt.
#'
#' @return ssm_report, a list of the following issues (when present):
#' \item{missing_vars}{Missing and required variables}
#' \item{dup_samples}{List of duplicated sample IDs}
#' \item{blank_idx}{Row index of blank/missing subject or sample IDs}
#' \item{dd_errors}{Differences in fields between data file and data dictionary}
#' \item{extra_subjects}{Subjects in data file missing from \code{ssm_exp}}
#' \item{missing_subjects}{Subjects in \code{ssm_exp} missing from data file}
#' \item{extra_samples}{Samples in data file missing from \code{ssm_exp}}
#' \item{missing_samples}{Samples in \code{ssm_exp} missing from data file}
#' \item{ssm_diffs}{Discrepancies in mapping between SAMPLE_ID and SUBJECT_ID. Lists entries in \code{ssm_exp} that disagree with mapping in the data file}
#' \item{sampuse_diffs}{Discrepancies with expected SAMPLE_USE values}
#' 
#' @rdname check_ssm

check_ssm <- function(dsfile, ddfile=NULL, ssm_exp=NULL,
                      sampleID_col="SAMPLE_ID", subjectID_col="SUBJECT_ID",
                      sample_uses=NULL, topmed=FALSE){

  # read in data file
  ds <- .read_ds_file(dsfile)

  # cannot proceed without subject and sample ID cols
  if(!is.element(subjectID_col, names(ds)) | !is.element(sampleID_col, names(ds))){
    stop("Please check that dsfile contains columns for subject-level and sample-level IDs")
  }

  # issue warning for non-standard subjectID_col or sampleID_col names
  if(subjectID_col != "SUBJECT_ID"){
    warning("Note preferred subject-level ID column name is 'SUBJECT_ID'")
  }
  if(sampleID_col != "SAMPLE_ID"){
    warning("Note preferred sample-level ID column name is 'SAMPLE_ID'")
  }

  # check for duplicated sample IDs
  samplist <- ds[,sampleID_col]
  dup_samples <- samplist[duplicated(samplist)]
  if(length(dup_samples) %in% 0){
    dup_samples <- NULL
  }

  # check for blank subject or sample IDs
  blanks <- c("","NA",NA)
  blank_idx <- which(trimws(ds[,sampleID_col]) %in% blanks |
                     trimws(ds[,subjectID_col]) %in% blanks)
  if(length(blank_idx) %in% 0){
    blank_idx <- NULL
  }
  
  # read in data dictionary if provided
  dd_errors <- NULL
  if(!is.null(ddfile)){
    dd <- .read_dd_file(ddfile)
    dd_errors <- .check_dd(dd, ds=ds)
    # TO DO - need to capture all the 'warning' messages from .check_dd. tryCatch?
  }

  # expected SSM if provided
  ssm_diffs <- missing_subjects <- extra_subjects <- missing_samples <- extra_samples <- NULL
  if(!is.null(ssm_exp)){

    # check for matching subject ids
    missing_subjects <- setdiff(ssm_exp$SUBJECT_ID, ds[,subjectID_col])
    extra_subjects <- setdiff(ds[,subjectID_col], ssm_exp$SUBJECT_ID)

    # check for matching sample ids
    missing_samples <- setdiff(ssm_exp$SAMPLE_ID, ds[,sampleID_col])
    extra_samples <- setdiff(ds[,sampleID_col], ssm_exp$SAMPLE_ID)

    # mapping diffs are where only one of SAMPLE_ID and SUBJECT_ID match
    ssm_exp$map <- with(ssm_exp, paste(SUBJECT_ID, SAMPLE_ID))
    maps_ssm <- paste(ds[,subjectID_col], ds[,sampleID_col])
    ssm_diffs <- ssm_exp[!is.element(ssm_exp$map, maps_ssm),1:2]
    if(nrow(ssm_diffs) %in% 0) {
      ssm_diffs <- NULL
    }
  }

  # if TOPMed, sample uses should be "Seq_DNA_WholeGenome; Seq_DNA_SNP_CNV" for all samples,
  # except quarantine=TRUE samples
  # Note "Seq_DNA_SNP_CNV; Seq_DNA_WholeGenome" is also valid 
  sample_uses_topmed <- "Seq_DNA_WholeGenome; Seq_DNA_SNP_CNV"
  
  if(topmed){
    if(is.null(sample_uses)){
      sample_uses <- sample_uses_topmed
    } else if (!is.null(sample_uses) & !is.element(sample_uses, c(sample_uses_topmed, "Seq_DNA_SNP_CNV; Seq_DNA_WholeGenome"))){
        warning(paste0("Non TOPMed sample uses were provided; manually setting to '",
                   sample_uses_topmed,".' To check other sample_uses, set topmed=FALSE"))
       sample_uses <- sample_uses_topmed
       # if quarantine=TRUE samples are in ssm_exp, change expected sample use to blank
        if(!is.null(ssm_exp) & is.logical(ssm_exp[,3])){
          sample_uses <- data.frame(SAMPLE_ID=ssm_exp$SAMPLE_ID,
                                    SAMPLE_USE=sample_uses_topmed)
          sample_uses$SAMPLE_USE[ssm_exp[,3]] <- ""
        }
      }
    }

  sampuse_diffs <- NULL
  # check for expected sample uses
  if(!is.null(sample_uses)){
    ds.mini <- ds[,c(sampleID_col,"SAMPLE_USE")]
    names(ds.mini)[1] <- "SAMPLE_ID"
    # determing if it's single string or a data frame
    if(is.character(sample_uses)){
      sampuse_diffs <- ds.mini[ds.mini$SAMPLE_USE != sample_uses,]
    } else if(is.data.frame(sample_uses)){
      sampuse_chk <- merge(sample_uses, ds.mini, by="SAMPLE_ID",
                           suffixes=c(".exp",".ds"))
      sampuse_diffs <- sampuse_chk[sampuse_chk$SAMPLE_USE.exp!=sampuse_chk$SAMPLE_USE.ds,]
    } else {
      warning("sample_uses was neither string nor data frame and could not be processed")
    }

    if(nrow(sampuse_diffs) %in% 0){
      sampuse_diffs <- NULL
    }
  }

  # check for required variables
  # req_vars <- c("SUBJECT_ID","SAMPLE_ID","SAMPLE_USE")
  # allow for other ID names
  req_vars <- c(subjectID_col, sampleID_col, "SAMPLE_USE")
  miss_vars <- setdiff(req_vars, names(ds))
  missing_vars <- ifelse(length(miss_vars) %in% 0, NA, miss_vars)
  
  # create and return results list
  ssm_report <- list()

  if(!is.na(missing_vars)) ssm_report$missing_vars <- missing_vars
  if(!is.null(dup_samples)) ssm_report$dup_samples <- dup_samples
  if(!is.null(blank_idx)) ssm_report$blank_idx <- blank_idx
  if(!is.null(dd_errors)) ssm_report$dd_errors <- dd_errors
  if(!is.null(extra_subjects) & length(extra_subjects > 0)){
    ssm_report$extra_subjects <- extra_subjects
  }
  if(!is.null(missing_subjects) & length(missing_subjects > 0)){
    ssm_report$missing_subjects <- missing_subjects
  }  
  if(!is.null(extra_samples) & length(extra_samples > 0)){
    ssm_report$extra_samples <- extra_samples
  }
  if(!is.null(missing_samples) & length(missing_samples > 0)){
    ssm_report$missing_samples <- missing_samples
  }
  if(!is.null(ssm_diffs)) ssm_report$ssm_diffs <- ssm_diffs
  if(!is.null(sampuse_diffs))  ssm_report$sampuse_diffs <- sampuse_diffs


  return(ssm_report)
}

#' Check sample attributes file
#'
#' Check contents of a sample attributes file for dbGaP posting.
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param samp_exp List of expected sample ID
#' @param sampleID_col Column name for sample-level ID
#' @param topmed Logical to indicate TOPMed study
#' 
#' @details
#' The sample attributes file should be a tab-delimited .txt file.
#' When (\code{topmed = TRUE}) checks presence of additional, TOPMed-specific
#' sample attributes variables: SEQUENCING_CENTER, Funding_Source, TOPMed_Phase, 
#' TOPMed_Project, Study_Name.
#'
#' If a data dictionary is provided (\code{ddfile != NULL}), additionally checks 
#' correspondence between column names in data file and entries in data dictionary.
#' Data dictionary files can be Excel (.xls, .xlsx) or tab-delimited .txt.
#' 
#' @return satt_report, a list of the following issues (when present):
#' \item{missing_vars}{Missing and required variables}
#' \item{dup_samples}{List of duplicated sample IDs}
#' \item{blank_idx}{Row index of blank/missing sample IDs}
#' \item{dd_errors}{Differences in fields between data file and data dictionary}
#' \item{extra_samples}{Samples in data file missing from \code{ssm_exp}}
#' \item{missing_samples}{Samples in \code{ssm_exp} missing from data file}
#' \item{missing_topmed_vars}{Missing and required variables for TOPMed}

# check_sattr
check_sattr <- function(dsfile, ddfile=NULL, samp_exp=NULL,
                        sampleID_col="SAMPLE_ID", topmed=FALSE){

  # read in data file
  ds <- .read_ds_file(dsfile)

  # cannot proceed without sample ID col
  if(!is.element(sampleID_col, names(ds))){
    stop("Please check that dsfile contains column for sample-level ID")
  }

  # issue warning for non-standard sampleID_col name
  if(sampleID_col != "SAMPLE_ID"){
    warning("Note preferred sample-level ID column name is 'SAMPLE_ID'")
  }
  
  # check for duplicated sample IDs
  # note might be acceptable where samples have a series of measurements,
  #  or data is longitudinal
  samplist <- ds[,sampleID_col]
  dup_samples <- samplist[duplicated(samplist)]
  if(length(dup_samples) %in% 0){
    dup_samples <- NULL
  }

  # check for blank sample IDs by row idex
  blanks <- c("","NA",NA)
  blank_idx <- which(trimws(samplist) %in% blanks)
  if(length(blank_idx) %in% 0){
    blank_idx <- NULL
  }
  
  # read in data dictionary if provided
  dd_errors <- NULL
  if(!is.null(ddfile)){
    dd <- .read_dd_file(ddfile)
    dd_errors <- .check_dd(dd, ds=ds)
  }  

  # check for required variables
  req_vars <- c(sampleID_col, "BODY_SITE","ANALYTE_TYPE","HISTOLOGICAL_TYPE","IS_TUMOR")
  miss_vars <- setdiff(req_vars, names(ds))
  missing_vars <- ifelse(length(miss_vars) %in% 0, NA, miss_vars)

  # most common analyte type is "DNA"
  if("ANALYTE_TYPE" %in% names(ds) & sum(ds$ANALYTE_TYPE != "DNA") > 0){
    message("Note some entries have ANALYTE_TYPE other than DNA, which is the most common")
  }

  # check for presence of expected samples
  missing_samples <- extra_samples <- NULL
  if(!is.null(samp_exp)){
    missing_samples <- setdiff(samp_exp, ds[,sampleID_col])
    extra_samples <- setdiff(ds[,sampleID_col], samp_exp)
  }

  # if TOPMed, check for TOPMed-specific variables
  missing_topmed_vars <- NULL
  if(topmed){
    topmed_vars <- c(req_vars, "SEQUENCING_CENTER", "Funding_Source",
                     "TOPMed_Phase", "TOPMed_Project","Study_Name")
    missing_topmed_vars <- setdiff(topmed_vars, names(ds))
  }

  # create and return results list
  satt_report <- list()

  if(!is.na(missing_vars))  satt_report$missing_vars <- missing_vars
  if(!is.null(dup_samples)) satt_report$dup_samples <- dup_samples
  if(!is.null(blank_idx)) satt_report$blank_idx <- blank_idx
  if(!is.null(dd_errors)) satt_report$dd_errors <- dd_errors
  if(!is.null(extra_samples) & length(extra_samples > 0)){
    satt_report$extra_samples <- extra_samples
  }
  if(!is.null(missing_samples) & length(missing_samples > 0)){
    satt_report$missing_samples <- missing_samples
  }
  if(!is.null(missing_topmed_vars)) satt_report$missing_topmed_vars <- missing_topmed_vars
  return(satt_report)
}

# check_subj

#' Check subject consent file
#'
#' Check contents of a subject consent file for dbGaP posting.
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param subj_exp Dataframe of expected subject ID (column 1) and consent value (column 2)
#' @param subjectID_col Column name for subject-level ID
#'
#' @details
#' The subject consent file should be a tab-delimited .txt file.
#' When (\code{subj_exp != NULL}), checks for presence of expected subject IDs,
#' and correspondence between subject ID and consent value.
#' If only one of either SUBJECT_SOURCE and SOURCE_SUBJECT_ID is present, returns a warning
#' indicating that both variables must be submitted together.
#' Checks that all consent groups are coded using an integer (1, 2, 3, etc).
#' 
#' If a data dictionary is provided (\code{ddfile != NULL}), additionally checks 
#' correspondence between column names in data file and entries in data dictionary.
#' Data dictionary files can be Excel (.xls, .xlsx) or tab-delimited .txt.
#'
#' @return subj_report, a list of the following issues (when present):
#' \item{consent_varname}{Logical, indicating consent variable is not named 'CONSENT'}
#' \item{alias_missvar}{Logical, indicating when only one of SUBJECT_SOURCE or SOURCE_SUBJECT_ID is submitted}
#' \item{dd_errors}{Differences in fields between data file and data dictionary}
#' \item{extra_subjects}{Subjects in data file missing from \code{subj_exp}}
#' \item{missing_subjects}{Subjects in \code{subj_exp} missing from data file}
#' \item{consent_diffs}{Discrepancies in correspondence between subject ID and consent. Lists entries in \code{subj_exp} that disagree with correspondence in the data file}
#' \item{consent_nonints}{List of non-integer consent values.}
#' \item{potential_pheno_vars}{List of potential phenotype variable names in DS. Note phenotype should only be in one of these two files: phenotype file or subject consent file.}
#' 
#' @rdname check_subj

check_subj <- function(dsfile, ddfile=NULL, subj_exp=NULL,
                      subjectID_col="SUBJECT_ID", consent_col="CONSENT"){

  # read in data file
  ds <- .read_ds_file(dsfile)

  # cannot proceed without subject ID col
  if(!is.element(subjectID_col, names(ds))) {
    stop("Please check that dsfile contains column for subject-level ID")
  }

  # check CONSENT colum name
  consent_varname <- NULL
  if(consent_col != "CONSENT") {
    warning("Consent variable name should be 'CONSENT'")
    consent_varname <- TRUE
  }

  ## the two checks above count as checking for required variables (only 2)
  # check for required variables (only 2)
  # req_vars <- c(subjectID_col, "CONSENT")
  # miss_vars <- setdiff(req_vars, names(ds))
  # missing_vars <- ifelse(length(miss_vars) %in% 0, NA, miss_vars)

  # if one of the alias columns is provided, check that both are
  alias_missvar <- NULL
  alias_vars <- c("SUBJECT_SOURCE","SOURCE_SUBJECT_ID")
  alias_vars_pres <- intersect(alias_vars, names(ds))
  alias_vars_miss <- setdiff(alias_vars, names(ds))
  if(length(alias_vars_pres) > 0 & length(alias_vars_miss) > 0 ){
    warning("Datafile has", alias_vars_pres,", but missing ",alias_vars_miss)
    alias_missvar <- TRUE
  }

  # read in data dictionary if provided
  dd_errors <- NULL
  if(!is.null(ddfile)){
    dd <- .read_dd_file(ddfile)
    dd_errors <- .check_dd(dd, ds=ds)
  }

  # check for presence of expected subjects, with expected consent values
  missing_subjects <- extra_subjects <- consent_diffs <- NULL
  if(!is.null(subj_exp)){
    missing_subjects <- setdiff(subj_exp[,1], ds[,subjectID_col])
    extra_subjects <- setdiff(ds[,subjectID_col], subj_exp[,1])

    # check for expected consent values
    subj_exp$map <- paste(subj_exp[,1], subj_exp[,2])
    maps_subj <- paste(ds[,subjectID_col], ds[,consent_col])
    consent_diffs <- subj_exp[!is.element(subj_exp$map, maps_subj),1:2]
    if(nrow(consent_diffs) %in% 0) consent_diffs <- NULL 

  }

  # check that consent codes are integers (or can be coerced to integers)
  consent_nonints <- NULL
  consents <- unique(ds[,consent_col])
  digits_sel <- grepl("^[[:digit:]]*$", consxents)
  if(sum(!digits_sel) > 0) consent_nonints <- consents[!digits_sel]
  
  # check for column name that looks like phenotype - issue warning that if present here,
  #  should not be included in pheno file, so as to avoid conflicts
  potential_pheno_vars <- NULL
  pheno_vars <- names(ds)[grepl("aff|pheno|case|status", names(ds), ignore.case=TRUE)]
  if(length(pheno_vars) %in% 0)  potential_pheno_vars <- paste(pheno_vars, collapse="; ")

  # create and return results list
  subj_report <- list()
  if(!is.null(consent_varname)) subj_report$consent_varname <- consent_varname
  if(!is.null(alias_missvar)) subj_report$alias_missvar <- alias_missvar
  if(!is.null(dd_errors)) subj_report$dd_errors <- dd_errors
  if(!is.null(missing_subjects)) subj_report$missing_subjects <- missing_subjects
  if(!is.null(extra_subjects)) subj_report$extra_subjects <- extra_subjects
  if(!is.null(consent_diffs)) subj_report$consent_diffs <- consent_diffs
  if(!is.null(consent_nonints))  subj_report$consent_nonints <- consent_nonints
  if(!is.null(potential_pheno_vars)) subj_report$potential_pheno_vars <- potential_pheno_vars
  
} # end function definition



