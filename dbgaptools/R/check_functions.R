#' Check data dictionary (generic)
#'
#' @param dd Data dictionary (DD) object
#' @param ds Corresponding dataset (DS) object
#' @param dstype Type of corresponding DS file, one of "pheno","ped","sattr","ssm","subj."
#'
#' @details
#' Data dictionary files can be Excel (.xls, .xlsx) or tab-delimited .txt.
#' The first two columns must be 'VARNAME' and 'VARDESC' in order for checks to proceed.
#' Reports errors or issues with DD file.
#' When the corresponding DS file is also provided, checks for consistency between the two.
#'
#' Even if DS file is not provided, (\code{ds == NULL}), the (\code{dstype}) must
#' be specified to check for customize check for UNITS and VALUES columns:
#' pheno = phenotype DS; ped = pedigree DS, sattr =sample attributes DS, ssm=sample-subject mapping DS, subj=subject consent DS.
#' Note VALUES are considered required for pheno, ped, sattr, and subj DS types.
#' Additionally, UNITS are considered required for pheno and sattr DS types.
#' Note some studies may not actually require VALUES and UNITS in these files types,
#' but when missing they are reported out here for convenience.
#' 
#' @return dd_report, a list of the following issues (when present):
#' \item{lowercase}{Logical flag indicating non-uppercase variable names}
#' \item{missing_reqvars}{Missing and required variables, based on dstype}
#' \item{extra_vars}{Extra variables}
#' \item{uniquekey_flags}{Returns warning when UNIQUEKEY column is populated for file types other than phenotype (most common need for UNIQUEKEY) or sample attributes (which in some cases requires UNIQUEKEY). Returns warning if, in pheno or sattr dstypes, the UNIQUEKEY variable(s) do not specify unique rows in the DS.}
#' \item{vals_warnings}{Vector of warnings about VALUES columns}
#' \item{missing_dsvars}{Variables present in DS but not defined in DD}
#' \item{min_errors}{Variables for which DS value are < DD MIN}
#' \item{max_errors}{Variables for which DS value are > DD MAX}
#' \item{illegal_vars}{Variable names containing illegal characters: '\', '/', ',' (comma), or 'dbGaP' are present} 
#'
#' @rdname check_dd
#' @export

.check_dd <- function(dd, ds=NULL, dstype=""){

  # check for required dstype argument
  dstypes <- c("pheno","ped","sattr","ssm","subj")
  if(!dstype %in% dstypes) stop("Please specify dstype, one of: ", paste(dstypes, collapse=", "))
  
  # all colnames should be upper case
  lowercase <- NULL
  upp <- toupper(names(dd))
  if(sum(upp != names(dd)) > 0 ){
    warning("All column names should be UPPER CASE")
    names(dd) <- upp
    lowercase <- TRUE
  }
  
  # required first two columns - needed to proceed with checks
  if(sum(names(dd)[1:2] != c("VARNAME","VARDESC")) > 0){
    stop("First two columns required to be 'VARNAME' and 'VARDESC'")
  }

  ### required columns, based on DS type
  req_vars <- c("VARNAME","VARDESC")

  # consent will always require encoded values
  # sex likely will (in pedigree and pheno)
  if(dstype %in% c("ped","pheno","sattr", "subj")) req_vars <- c(req_vars, "VALUES")

  # add UNITS for sample attributes and pheno
  if(dstype %in% c("pheno","sattr")) req_vars <- c(req_vars, "UNITS")
  
  missing_reqvars <- setdiff(req_vars, names(dd))
  if(length(missing_reqvars) %in% 0) missing_reqvars <- NULL

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
    warning("DD contains non-standard columns: ", paste(extra_cols, collapse="; "))
    extra_vars <- extra_cols
  }

  # return flag if UNIQUEKEY is indicated in file other than phenotype or sample attributes
  uniquekey_flags <- list()
  if("UNIQUEKEY" %in% names(dd)) {
    # only proceed with checks if a variable is actually marked as a unique key
    keysel <- !is.na(dd$UNIQUEKEY)
    if(sum(keysel) > 0) {
        uniqkey_cols <- dd$VARNAME[!is.na(dd$UNIQUEKEY)]
        if(!is.element(dstype, c("pheno","sattr")) & length(uniqkey_cols) > 0 ){
          uniquekey_flags[["wrong_dstype"]] <- paste("UNIQUEKEY columns(s) should not be defined for dstype", dstype)
        }
        # check for only "X" in the columns
        uniqkey_marks <- unique(dd[dd$VARNAME %in% uniqkey_cols,"UNIQUEKEY"])
        if(uniqkey_marks != "X") uniquekey_flags[["wrong_mark"]] <- "Only 'X' should be used to flag UNIQUE key columns"

        # check that uniquekey columns are unique
        if(!is.null(ds)){
          ds.uniq <- ds[,uniqkey_cols,drop=FALSE]
          if(nrow(ds.uniq) != nrow(unique(ds.uniq))){
            uniquekey_flags[["notunique"]] <- paste0("UNIQUEKEY columns (",paste(uniqkey_cols, collapse=","),") do not specify unique records in DS")
          } # if unique key is non-unique
        } # if ds is also provided
      }# if a VARNAME is marked as unique
  } # end unique key columns
  
  # prepare to collect series of warnings about VALUES column
  vals_warnings <- list()
  
  # if VALUES col exists, needs to be last
  if("VALUES" %in% names(dd)){
    nnames <- length(names(dd)[!grepl("X__", names(dd))])
    if("VALUES" != names(dd)[nnames]){
      vals_warnings[["lastcol_warn"]]<- "'VALUES' must be last column"
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
          str <- paste(paste(mult_eq_vars,collapse="; "), "variable(s) has multiple VALUE entries per cell. Only the first entry per cell will be evaluated. Encoded values must be split into one per cell.")
          vals_warnings[["multiple_vals_warn"]] <- str

        }

        # if ds is also provided, check that all encoded values are defined
        if(!is.null(ds)){

         # extract encoded values
          encoded_vars %<>%
            dplyr::mutate_all(function(x) stringr::str_replace(x, "=.*", "")) %>%
              tidyr::gather("column", "row", -VARNAME) %>%
                tidyr::spread(VARNAME, row) %>%
                  dplyr::select(-column)

          vars_chk <- names(encoded_vars)
          warn_undef <- NULL
          for(var in vars_chk){
            var1 <- unique(ds[,var])
            # remove NAs as encoded values
            var1 <- var1[!is.na(var1)]
            var2 <- dplyr::pull(encoded_vars, var)
            var2 <- var2[!is.na(var2)]
            # remove leading or trailing white space
            undef_vals <- setdiff(trimws(var1), trimws(var2))
            if(length(undef_vals) > 0){
                warn_tmp <- paste0("For variable ", var,", the following values are undefined in the dd: ", paste(sort(undef_vals), collapse=" "))
                warn_undef <- c(warn_undef, warn_tmp)
            }
          } # loop through encoded vars

         # save value warnings in returned objects   
         vals_warnings[["undefined_vals_warn"]] <- warn_undef
            
        } #  if dataset is provided
    } # if there are non-NA entrie in VALUES column
  } # if VALUES col is present

  # if dataset provided:
  # check for all vars
  missing_dsvars <- NULL
  extra_ddvars <- NULL
  min_errors <- max_errors <- NULL
  if(!is.null(ds)){
    missing_dsvars <- setdiff(names(ds), dplyr::pull(dd, VARNAME))
    if(length(missing_dsvars) > 0){
      warning("Data dictionary missing following dataset variables: ",
              paste(missing_dsvars, collapse=", "))
    } else {
      # set back to null
      missing_dsvars <- NULL
    }
    extra_ddvars <- setdiff(dplyr::pull(dd, VARNAME), names(ds))
    if(length(extra_ddvars) > 0){
      warning("Data dictionary has extra variables not in dataset: ",
              paste(extra_ddvars, collapse=", "))
    } else {
      # set back to null
      extra_ddvars <- NULL
    }
    
    # check MIN and MAX values
    if("MIN" %in% names(dd)){
      if(sum(!is.na(dd$MIN)) > 0){
          dd.tmp <- dd[!is.na(dd$MIN),c("VARNAME","MIN")]
          ds.tmp <- as.matrix(ds[, dd.tmp$VARNAME])
          # remove greater than or less than signs from DD MIN (e.g., >, < .etc)
          dd.tmp$MIN <- stringr::str_replace_all(dd.tmp$MIN, ">|<", "")
          # convert to numeric if necessary
          ds.tmp <- apply(ds.tmp, 2, as.numeric)
          dd.tmp$min.ds <- apply(ds.tmp, 2, min, na.rm=TRUE)
          # return df of variable name, expeced and observed min
          range_err <- dd.tmp[dd.tmp$min.ds < as.numeric(dd.tmp$MIN),]
          if(nrow(range_err) > 0) min_errors <- range_err
        } # if non-NA MINS
    } # if MIN is col
       
    if("MAX" %in% names(dd)){
      if(sum(!is.na(dd$MAX)) > 0) {
          dd.tmp <- dd[!is.na(dd$MAX),c("VARNAME","MAX")]
          ds.tmp <- as.matrix(ds[, dd.tmp$VARNAME])
          # remove greater than or less than signs from DD MAX (e.g., >, < .etc)
          dd.tmp$MAX <- stringr::str_replace_all(dd.tmp$MAX, ">|<", "")
          # convert DS cols to numeric class
          ds.tmp <- apply(ds.tmp, 2, as.numeric)
          dd.tmp$max.ds <- apply(ds.tmp, 2, max, na.rm=TRUE)
          # return df of variable name, expeced and observed max
          range_err <- dd.tmp[dd.tmp$max.ds > as.numeric(dd.tmp$MAX),]
          if(nrow(range_err) > 0) max_errors <- range_err
        } # if non-NA MAX
    } # if MAX is col
  } # if DS is provided

  # check for illegal characters in variable names: \ / , dbGaP
  illegal_vars <- NULL
  ill_vars_sel <- stringr::str_detect(dd$VARNAME, "DBGAP|\\\\|/|,")
  
  if(sum(ill_vars_sel) > 0){
    ill_vars <- dd$VARNAME[ill_vars_sel]
    illegal_vars <- paste(ill_vars, collapse="; ")
  }

  # construct list object to return
  dd_report  <- list()

  if(!is.null(lowercase)) dd_report$lowercase <- lowercase
  if(!is.null(missing_reqvars)) dd_report$missing_reqvars <- missing_reqvars
  if(!is.null(extra_vars)) dd_report$extra_vars <- extra_vars
  if(length(uniquekey_flags) > 0) dd_report$uniquekey_flags <- uniquekey_flags
  if(length(vals_warnings) > 0)  dd_report$vals_warnings <- vals_warnings  
  if(!is.null(missing_dsvars))  dd_report$missing_dsvars <- missing_dsvars
  if(!is.null(extra_ddvars)) dd_report$extra_ddvars <- extra_ddvars
  if(!is.null(illegal_vars))  dd_report$illegal_vars <- illegal_vars
  if(!is.null(min_errors)) dd_report$min_errors <- min_errors
  if(!is.null(max_errors)) dd_report$max_errors <- max_errors  

  # if list is empty, return NULL
  if(length(dd_report) == 0) dd_report <- NULL

  return(dd_report)
}

#' Check sample subject mapping file
#'
#' Check contents of a sample subject mapping file for dbGaP posting.
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param na_vals Vector of strings that should be read in as NA/missing in data file (see details of \code{.read_ds_file})
#' @param ssm_exp Dataframe of expected SAMPLE_ID and SUBJECT_ID, with optionaly third column 'quarantine' (see Details below)
#' @param sampleID_col Column name for sample-level ID
#' @param subjectID_col Column name for subject-level ID
#' @param sample_uses Either a single string for expected SAMPLE_USE across all samples, or a data frame with SAMPLE_ID and SAMPLE_USE values
#' @param topmed Logical to indicate TOPMed study
#'
#' @details
#' The sample subject mapping file should be a tab-delimited .txt file.
#' When \code{ssm_exp != NULL}, checks for expected correspondence between
#' SAMPLE_ID and SUBJECT_ID. Any differences in mapping between the two,
#' or a difference in the list of expected SAMPLE_IDs or SUBJECT_IDs,
#' will be returned in the output.
#' If \code{ssm_exp != NULL} contains an additional logical field 'quarantine,'
#' code will check that SAMPLE_USE is left blank (read in as 'NA') for this record.
#' Quarantined samples will otherwise be treated as
#' other records in terms of checking for missing or extra subjects or samples.
#'
#' If \code{topmed}, then SAMPLE_USE is expected to be either "Seq_DNA_WholeGenome; Seq_DNA_SNP_CNV" or "Seq_DNA_SNP_CNV; Seq_DNA_WholeGenome", except for
#' samples marked as quarantine in \code{ssm_exp}.
#' 
#' If a data dictionary is provided \code{ddfile != NULL}, additionally checks 
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
#' @export

check_ssm <- function(dsfile, ddfile=NULL,
                      na_vals=c("NA","N/A","na","n/a"),
                      ssm_exp=NULL,
                      sampleID_col="SAMPLE_ID", subjectID_col="SUBJECT_ID",
                      sample_uses=NULL, topmed=FALSE){

  # read in data file
  ds <- .read_ds_file(dsfile, na_vals=na_vals)

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
  
  # check for required variables
  # req_vars <- c("SUBJECT_ID","SAMPLE_ID","SAMPLE_USE")
  # allow for other ID names
  req_vars <- c(subjectID_col, sampleID_col, "SAMPLE_USE")
  missing_vars <- setdiff(req_vars, names(ds))
  if(length(missing_vars) %in% 0) missing_vars <- NULL  
  
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
    dd_errors <- .check_dd(dd, ds=ds, dstype="ssm")
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

  # if TOPMed, sample uses should be either "Seq_DNA_WholeGenome; Seq_DNA_SNP_CNV"
  # or "Seq_DNA_SNP_CNV; Seq_DNA_WholeGenome" for all samples,
  # except where quarantine=TRUE in ssm_exp
  sample_uses_topmed <- c("Seq_DNA_WholeGenome; Seq_DNA_SNP_CNV",
                          "Seq_DNA_SNP_CNV; Seq_DNA_WholeGenome")
  
  if(topmed){
    # if a sample use dataframe was submitted, take only first value and return warning
    if(is.data.frame(sample_uses)){
      warning("Expecting unique sample_uses value for TOPMed; taking first value of sample_uses data frame")
      sample_uses <- sample_uses[1,2]
    }
    
    # if no sample use was provided, set to TOPMed value
    if(is.null(sample_uses)){
      sample_uses <- sample_uses_topmed[1] # use first order
    } else if (!is.null(sample_uses) & !sample_uses %in% sample_uses_topmed){
      # 'unique' above takes care of a data frame being submitted as sample_uses
        warning(paste0("Non TOPMed sample use was provided; manually setting to '",
                   sample_uses_topmed[1],".' To check other sample_uses, set topmed=FALSE"))
       sample_uses <- sample_uses_topmed[1]
      }
    
   # if quarantine=TRUE samples are in ssm_exp, change expected sample use to blank
    if(!is.null(ssm_exp) & is.logical(ssm_exp[,3])){
      sample_uses <- data.frame(SAMPLE_ID=ssm_exp$SAMPLE_ID,
                                SAMPLE_USE=sample_uses)
      sample_uses$SAMPLE_USE[ssm_exp[,3]] <- NA
      }
    } # close if topmed

  sampuse_diffs <- NULL
  # check for expected sample uses
  if(!is.null(sample_uses)){
    ds.mini <- ds[,c(sampleID_col,"SAMPLE_USE")]
    names(ds.mini)[1] <- "SAMPLE_ID"

    # determine if it's single string or a data frame
    if(is.character(sample_uses)){
      sampuse_diffs <- ds.mini[ds.mini$SAMPLE_USE != sample_uses |
                               is.na(ds.mini$SAMPLE_USE),]
    } else if(is.data.frame(sample_uses)){
      sampuse_chk <- merge(sample_uses, ds.mini, by="SAMPLE_ID",
                           all=TRUE, suffixes=c(".exp",".ds"))

      # check if one is NA and the other isn't, or if they're non-NA and differing vals
      bothNA <- with(sampuse_chk, is.na(SAMPLE_USE.exp) & is.na(SAMPLE_USE.ds))
      flag <- with(sampuse_chk, xor(is.na(SAMPLE_USE.exp), is.na(SAMPLE_USE.ds)) |
                   SAMPLE_USE.exp != SAMPLE_USE.ds & !bothNA)
      # flag will be NA when both values are NA
      sampuse_diffs <- sampuse_chk[flag,]
    } else {
      warning("sample_uses was neither string nor data frame and could not be processed")
    }

    if(nrow(sampuse_diffs) %in% 0){
      sampuse_diffs <- NULL
    }
  }

  # create and return results list
  ssm_report <- list()

  if(!is.null(missing_vars)) ssm_report$missing_vars <- missing_vars
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


  # if list is empty, return NULL
  if(length(ssm_report) == 0) ssm_report <- NULL
  
  return(ssm_report)
}

#' Check sample attributes file
#'
#' Check contents of a sample attributes file for dbGaP posting.
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param na_vals Vector of strings that should be read in as NA/missing in data file (see details of \code{.read_ds_file})
#' @param samp_exp List of expected sample IDs
#' @param sampleID_col Column name for sample-level ID
#' @param topmed Logical to indicate TOPMed study
#' 
#' @details
#' The sample attributes file should be a tab-delimited .txt file.
#' When (\code{topmed = TRUE}) checks presence of additional, TOPMed-specific
#' sample attributes variables: SEQUENCING_CENTER, Funding_Source, TOPMed_Phase, 
#' TOPMed_Project, Study_Name.
#'
#' Note that none of the BioSample variables (BODY_SITE, ANALYTE_TYPE, HISTOLOGICAL_TYPE, IS_TUMOR) are strictly required in the sense that their absence will not break dbGaP processing pipeline or delay study release. However, their inclusion is strongly encouraged, and indeed necessary for cancer studies and other tissue-specific studies, and are thus considered "required" variables for the purposes of this checking script.
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
#'
#' @rdname check_sattr
#' @export


check_sattr <- function(dsfile, ddfile=NULL,
                        na_vals=c("NA","N/A","na","n/a"),
                        samp_exp=NULL,
                        sampleID_col="SAMPLE_ID", topmed=FALSE){

  # read in data file
  ds <- .read_ds_file(dsfile, na_vals=na_vals)

  # cannot proceed without sample ID col
  if(!is.element(sampleID_col, names(ds))){
    stop("Please check that dsfile contains column for sample-level ID")
  }

  # issue warning for non-standard sampleID_col name
  if(sampleID_col != "SAMPLE_ID"){
    warning("Note preferred sample-level ID column name is 'SAMPLE_ID'")
  }

  # check for required variables
  req_vars <- c(sampleID_col, "BODY_SITE","ANALYTE_TYPE","HISTOLOGICAL_TYPE","IS_TUMOR")
  missing_vars <- setdiff(req_vars, names(ds))
  if(length(missing_vars) %in% 0) missing_vars <- NULL  
  
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
    dd_errors <- .check_dd(dd, ds=ds, dstype="sattr")
  }  

  ## ## removing this assumption
  ## # most common analyte type is "DNA"
  ## if("ANALYTE_TYPE" %in% names(ds) & sum(ds$ANALYTE_TYPE != "DNA") > 0){
  ##   message("Note some entries have ANALYTE_TYPE other than DNA, which is the most common")
  ## }

  # check for presence of expected samples
  missing_samples <- extra_samples <- NULL
  if(!is.null(samp_exp)){
    missing_samples <- setdiff(samp_exp, ds[,sampleID_col])
    extra_samples <- setdiff(ds[,sampleID_col], samp_exp)
  }

  # if TOPMed, check for TOPMed-specific variables
  missing_topmed_vars <- NULL
  if(topmed){
    topmed_vars <- c("SEQUENCING_CENTER", "Funding_Source",
                     "TOPMed_Phase", "TOPMed_Project","Study_Name")
    missing_topmed_vars <- setdiff(topmed_vars, names(ds))
  }

  # create and return results list
  satt_report <- list()

  if(!is.null(missing_vars)) satt_report$missing_vars <- missing_vars
  if(!is.null(dup_samples)) satt_report$dup_samples <- dup_samples
  if(!is.null(blank_idx)) satt_report$blank_idx <- blank_idx
  if(!is.null(dd_errors)) satt_report$dd_errors <- dd_errors
  if(!is.null(extra_samples) & length(extra_samples > 0)){
    satt_report$extra_samples <- extra_samples
  }
  if(!is.null(missing_samples) & length(missing_samples > 0)){
    satt_report$missing_samples <- missing_samples
  }
  if(!is.null(missing_topmed_vars) & length(missing_topmed_vars) > 0) {
    satt_report$missing_topmed_vars <- missing_topmed_vars
  }

  # if list is empty, return NULL
  if(length(satt_report) == 0) satt_report <- NULL
  
  return(satt_report)
}

#' Check subject consent file
#'
#' Check contents of a subject consent file for dbGaP posting.
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param na_vals Vector of strings that should be read in as NA/missing in data file (see details of \code{.read_ds_file})
#' @param subj_exp Dataframe of expected subject ID (column 1) and consent value (column 2)
#' @param subjectID_col Column name for subject-level ID
#' @param consent_col Column name for consent variable
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
#' for agreement between data file and data dictionary.
#' Assumes that CONSENT=0 need not be defined in data dictionary, as dbGaP automatically codes as subjects used as genotyping controls and/or pedigree linking members.
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
#' @export

check_subj <- function(dsfile, ddfile=NULL,
                       na_vals=c("NA","N/A","na","n/a"),
                       subj_exp=NULL,
                       subjectID_col="SUBJECT_ID", consent_col="CONSENT"){

  # read in data file
  ds <- .read_ds_file(dsfile, na_vals=na_vals)

  # cannot proceed without subject ID col
  if(!is.element(subjectID_col, names(ds))) {
    stop("Please check that dsfile contains column for subject-level ID")
  }

  # cannot proceed without consent col
  if(!is.element(consent_col, names(ds))) {
    stop("Please check that dsfile contains column for consent")
  }

  # issue warning for non-standard subjectID_col
  if(subjectID_col != "SUBJECT_ID"){
    warning("Note preferred subject-level ID column name is 'SUBJECT_ID'")
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
  if(length(alias_vars_pres) > 0 ) {
    message("Note missing SUBJECT_SOURCE_ID should be left blank (\"\"), vs using missing value strings such as NA, N/A, etc.")
    if(length(alias_vars_miss) > 0 ){
      warning("Datafile has ", alias_vars_pres,", but missing ",alias_vars_miss)
      alias_missvar <- TRUE
    }
  }

  # read in data dictionary if provided
  dd_errors <- NULL
  if(!is.null(ddfile)){
    dd <- .read_dd_file(ddfile)
    dd_errors <- .check_dd(dd, ds=ds, dstype="subj")

    # for subject consent file, dbGaP will define consent=0 for user
    # remove this error (but not other mapping errors for consent or othe rvars)
    str <- paste0("For variable ",consent_col, ", the following values are undefined in the dd:")
    val_warns <- dd_errors$vals_warnings$undefined_vals_warn

    # only clean up this warning if it exists
    if(!is.null(val_warns)) {
      # set aside any other varnames with undefined values warning
      keep_warns <- val_warns[!grepl(str, val_warns)]
      if(length(keep_warns) %in% 0) keep_warns <- NULL

      # strip off the "0" from list of undefined consent values
      fix_warns <- gsub(": 0", ":", val_warns[grepl(str, val_warns)])
      if(length(fix_warns) > 0 & fix_warns == str) fix_warns <- NULL

      dd_errors$vals_warnings$undefined_vals_warn <- c(fix_warns, keep_warns)
    
      # if that's all that was in dd_errors$vals_warnings, remove
       if(length(dd_errors$vals_warnings) %in% 0) dd_errors$vals_warnings <- NULL

      # if that's all that was in dd_errors, remove
      if(length(dd_errors) == 0) dd_errors <- NULL
    }
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
  # if empty, convert back to NULL
  if(length(missing_subjects) == 0) missing_subjects <- NULL
  if(length(extra_subjects) == 0) extra_subjects <- NULL
  if(length(consent_diffs) == 0) consent_diffs <- NULL  

  # check that consent codes are integers (or can be coerced to integers)
  consent_nonints <- NULL
  consents <- unique(ds[,consent_col])
  digits_sel <- grepl("^[[:digit:]]*$", consents)
  if(sum(!digits_sel) > 0) consent_nonints <- consents[!digits_sel]
  
  # check for column name that looks like phenotype - issue warning that if present here,
  #  should not be included in pheno file, so as to avoid conflicts
  potential_pheno_vars <- NULL
  pheno_vars <- names(ds)[grepl("aff|pheno|case|status", names(ds), ignore.case=TRUE)]
  if(length(pheno_vars) > 0) potential_pheno_vars <- paste(pheno_vars, collapse="; ")

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

  # if list is empty, return NULL
  if(length(subj_report) == 0) subj_report <- NULL

  return(subj_report)
  
} # end function definition


#' Check pedigree file
#'
#' Check contents of a pedigree file for dbGaP posting
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param na_vals Vector of strings that should be read in as NA/missing in data file (see details of \code{.read_ds_file})
#' @param subj_exp Vector of expected subject IDs
#' @param subjectID_col Column name for subject-level ID
#' @param check_incons Logical whether to report pedigree inconsistencies, using \code{GWASTools pedigreeCheck}
#' @param male Encoded value for male in SEX column
#' @param female Encoded value for female in SEX column
#'
#' @details
#' If an MZ twin column is detected, returns issues including column name other than 'MZ_TWIN_ID' and a data frame of all twin pairs with logical flags to indicate > 1 family ID per pair (\code{chk_family=TRUE}); non-unique subject ID (\code{chk_subjectID=TRUE}); > 1 sex, which could indicate dizygotic twins are included (\code{chk_sex=TRUE}).
#'
#' If a data dictionary is provided (\code{ddfile != NULL}), additionally checks 
#' correspondence between column names in data file and entries in data dictionary.
#' Data dictionary files can be Excel (.xls, .xlsx) or tab-delimited .txt.
#' 
#' @return ped_report, a list of the following issues (when present):
#' \item{lowercase}{Logical flag indicating non-upper case variable names}
#' \item{missing_vars}{Missing and required variables}
#' \item{dd_errors}{Differences in fields between data file and data dictionary}
#' \item{extra_subjects}{Subjects in data file missing from \code{ssm_exp}}
#' \item{missing_subjects}{Subjects in \code{ssm_exp} missing from data file}
#' \item{extra_sexvals}{Additional values in SEX column beyond what's specified by \code{male} and \code{female} function arguments}
#' \item{mztwin_errors}{List of potential errors with MZ twins}
#'
#' @rdname check_ped
#' @export

check_ped <- function(dsfile, ddfile=NULL,
                      na_vals=c("NA","N/A","na","n/a"),
                      subj_exp=NULL,
                      subjectID_col="SUBJECT_ID", check_incons=TRUE,
                      male=1, female=2){

  # read in data file
  ds <- .read_ds_file(dsfile, na_vals=na_vals)

  # cannot proceed without subject ID col
  if(!is.element(subjectID_col, names(ds))) {
    stop("Please check that dsfile contains column for subject-level ID")
  }

  # issue warning for non-standard subjectID_col
  if(subjectID_col != "SUBJECT_ID"){
    warning("Note preferred subject-level ID column name is 'SUBJECT_ID'")
  }

  # all colnames should be upper case
  lowercase <- NULL
  upp <- toupper(names(ds))
  if(sum(upp != names(ds)) > 0 ){
    warning("All column names should be UPPER CASE")
    names(ds) <- upp
    lowercase <- TRUE
  }
  
  # check for required variable names
  req_vars <- c(subjectID_col, "FAMILY_ID","FATHER","MOTHER","SEX")
  missing_vars <- setdiff(req_vars, names(ds))
  if(length(missing_vars) %in% 0) missing_vars <- NULL
  
  # read in data dictionary if provided
  dd_errors <- NULL
  if(!is.null(ddfile)){
    dd <- .read_dd_file(ddfile)
    dd_errors <- .check_dd(dd, ds=ds, dstype="ped")
  }

  # check male and female values
  sexvals <- with(ds, unique(SEX))
  extra_sexvals <- setdiff(sexvals, c(male, female))
  if(length(extra_sexvals) %in% 0) extra_sexvals <- NULL

  # perform GWASTools pedigree check
  incon_report <- NULL
  if(check_incons){
    if(!is.null(missing_vars)){
      warning("Cannot check for pedigree inconsistencies until missing and required variables are added")
    } else {
      message("\nRunning GWASTools pedigree check\n")
      # prepare pedigree
      ped <- ds[,c("FAMILY_ID",subjectID_col,"MOTHER","FATHER","SEX")]
      names(ped) <- tolower(names(ped))
      names(ped)[1:2] <- c("family","individ")
      ped$sex[ped$sex %in% male] <- "M"
      ped$sex[ped$sex %in% female] <- "F"
      incon_report <- GWASTools::pedigreeCheck(ped)
    }
  }

  # check for expected subjects
  missing_subjects <- extra_subjects <- NULL
  if(!is.null(subj_exp)){
    missing_subjects <- setdiff(subj_exp, ds[,subjectID_col])
    extra_subjects <- setdiff(ds[,subjectID_col], subj_exp)    
  }
  
  # check for MZ twin column
  mztwin_errors <- list()
  twincol <- names(ds)[grepl("twin|mz", names(ds), ignore.case=TRUE)]
  if(length(twincol) > 0){
    if(twincol != "MZ_TWIN_ID") {
      mztwin_errors$colname <- "MZ twin column should be named 'MZ_TWIN_ID'"
    }
    
    twins_dat <- ds[!is.na(ds[,twincol]),]
    twins_dat$chk_sex <- twins_dat$chk_subjectID <- twins_dat$chk_family <- FALSE
    twins <- unlist(unique(twins_dat[twincol]))
    
    for(tw in twins) {
      idx <- which(twins_dat[,twincol] %in% tw)
      # twins should be in same family
      if(length(unique(twins_dat$FAMILY_ID[idx])) > 1) twins_dat$chk_family[idx] <- TRUE
      # twins should have different subject IDs (could be triplets)
      if(length(unique(twins_dat[idx, subjectID_col])) != length(idx)) twins_dat$chk_subjectID[idx] <- TRUE
      # twins should be the same sex (otherwise could be dizygotic twin
      if(length(unique(twins_dat$SEX[idx])) > 1) twins_dat$chk_sex[idx] <- TRUE
    } # end loop through twins

    # if errors, return twins.dat
    errSums <- rowSums(twins_dat[,grep("chk",names(twins_dat))])
    if(max(errSums) > 0) mztwin_errors$twins_dat <- twins_dat[errSums > 0,]
  } # if twin col is present

  # create and return results list
  ped_report <- list()

  if(!is.null(lowercase)) ped_report$lowercase <- lowercase
  if(!is.null(missing_vars)) ped_report$missing_vars <- missing_vars
  if(!is.null(dd_errors)) ped_report$dd_errors <- dd_errors
  if(!is.null(incon_report)) ped_report$incon_report <- incon_report
  if(!is.null(extra_subjects)) ped_report$extra_subjects <- extra_subjects
  if(!is.null(missing_subjects) & length(missing_subjects) > 0 ) ped_report$missing_subjects <- missing_subjects
  if(length(mztwin_errors) > 0) ped_report$mztwin_errors <- mztwin_errors
  if(!is.null(extra_sexvals)) ped_report$extra_sexvals <- extra_sexvals

  # if list is empty, return NULL
  if(length(ped_report) == 0) ped_report <- NULL

  return(ped_report)
}

#' Check phenotype file
#'
#' Check contents of a phenotype file for dbGaP posting
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param na_vals Vector of strings that should be read in as NA/missing in data file (see details of \code{.read_ds_file})
#' @param subj_exp Vector of expected subject IDs
#' @param subjectID_col Column name for subject-level ID
#'
#' @details
#' Because of the variability of phenotype file contents, the only required column checked here is the subject-level ID. Note dbGaP requests variables (1) described in the study description and/or study config; (2) affection status, if not already included in the subject consent file; (3) sex; and (4) race/ethnicity/ancestry/heritage.
#'
#' If a data dictionary is provided (\code{ddfile != NULL}), additionally checks 
#' correspondence between column names in data file and entries in data dictionary.
#' Data dictionary files can be Excel (.xls, .xlsx) or tab-delimited .txt.
#' 
#' @return pheno_report, a list of the following issues (when present):
#' \item{flag_nonuniq_subjID}{TRUE when subject ID column is not unique, which would require definition of UNIQUEKEY columns in the corresponding data dictionary}
#' \item{dd_errors}{Differences in fields between data file and data dictionary}
#' \item{extra_subjects}{Subjects in data file missing from \code{ssm_exp}}
#' \item{missing_subjects}{Subjects in \code{ssm_exp} missing from data file}
#'
#' @rdname check_pheno
#' @export

check_pheno <- function(dsfile, ddfile=NULL,
                        na_vals=c("NA","N/A","na","n/a"),
                        subj_exp=NULL,
                        subjectID_col="SUBJECT_ID"){

  # read in data file
  ds <- .read_ds_file(dsfile, na_vals=na_vals)

  # cannot proceed without subject ID col
  if(!is.element(subjectID_col, names(ds))) {
    stop("Please check that dsfile contains column for subject-level ID")
  }

  # issue warning for non-standard subjectID_col
  if(subjectID_col != "SUBJECT_ID"){
    warning("Note preferred subject-level ID column name is 'SUBJECT_ID'")
  }  
  
  # issue warning if subject ID is not unique
  dup_subjs <- sum(duplicated(ds[,subjectID_col])) > 0

  # if unique key is specified but not actually unique, .check_dd will catch it

  # read in data dictionary if provided
  dd_errors <- NULL
  if(!is.null(ddfile)){
    dd <- .read_dd_file(ddfile)
    dd_errors <- .check_dd(dd, ds=ds, dstype="pheno")
  }

  # check for expected subjects
  missing_subjects <- extra_subjects <- NULL
  if(!is.null(subj_exp)){
    missing_subjects <- setdiff(subj_exp, ds[,subjectID_col])
    extra_subjects <- setdiff(ds[,subjectID_col], subj_exp)    
  }
  
  pheno_report <- list()

  if(dup_subjs) pheno_report$flag_nonuniq_subjID <- dup_subjs
  if(!is.null(dd_errors)) pheno_report$dd_errors <- dd_errors
  if(!is.null(extra_subjects) & length(extra_subjects) > 0) pheno_report$extra_subjects <- extra_subjects
  if(!is.null(missing_subjects) & length(missing_subjects) > 0) pheno_report$missing_subjects <- missing_subjects

  # if list is empty, return NULL
  if(length(pheno_report) == 0) pheno_report <- NULL

  return(pheno_report)  

}
