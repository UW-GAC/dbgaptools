# Building functions for dbGaP files R package
# SN, 1/24/18

file_types <- c("samp_subj_mapping",
                "samp_attributes",
                "subj_consent",
                "phenotype",
                "pedigree")
           
#### 0. utility functions

## adapted from dbTopmed::.countHeaderLines
#' Count the header lines in a data file
#'
#' @param filename The path to the data file on disk
#' @param colname One or more expected column names
#'
#' @details
#' Header lines are considered to start with # or to be a blank line.
#' Optionally, providing a \code{colname} argument, a character vector
#' of one or more colunm names,  will consider any rows before
#' that containing the specified column name(s) to be a header row.
#'
#' @return
#' the number of header lines in the file
#'
#' @rdname count_hdr_lines

.count_hdr_lines <- function(filename, colname=NA) {
  con <- file(filename, "r")
  nskip <- 0
  done <- FALSE
  while (!done) {
    tmp <- readLines(con, n = 1)

    chkname <- FALSE
    if (!is.na(colname)){
      chkname <- !grepl(colname, tmp)
    }

    if ( substr(tmp, 1, 1) %in% c("#", "") | chkname) {
      nskip <- nskip + 1
    } else {
      done <- TRUE
    }
  }
  close(con)
  nskip
}

## adapted from dbTopmed::.readTraitFile

#' Read in a data file
#' 
#' Works for tab-delimited (.txt) data files
#'
#' @param filename The path to the file on disk
#' @param dd Logical, where \code{TRUE} indicates a data dictionary file
#'
#' @details
#' The only string considered to be NA is the blank string: "". "NA" does not necessarily indicate
#' NA: for example, it could be an encoded value meaning "North America".
#'
#' @return
#' A data frame from the file
#'
#' @details
#' dbGaP dataset files should have column headers as the first row. If the input violates this, e.g. additional header rows are present, a warning is returned but the file is still read in.
#' 
#' @rdname read_ds_file
.read_ds_file <- function(filename, dd=FALSE) {

  stopifnot(file.exists(filename))

  ## exit if file extension indicates other than .txt (e.g., csv, xlsx)
  ext <- tools::file_ext(filename)
  if(ext != "txt") {
    stop("Expected tab-delimited input file (.txt), not .", ext)
  }

  ## add name of file to error message in case of failure
  tryCatch({
    ## may be comment characters in the data fields. first decide how many lines to skip
    if(!dd) {
      nskip <- .count_hdr_lines(filename)
    } else if (dd) {
      nskip <- .count_hdr_lines(filename, colname="VARNAME")
    }
    
    if(nskip > 0){
      warning("Additional rows are present before column headers and should be removed prior to dbGaP submission")
    }
    header <- scan(filename, sep = "\t", skip = nskip, nlines = 1, what = "character", quiet = TRUE)
    empty_check <- stringr::str_match(header[1], REGEX_BLANK_DATA_FILE)
    # TO DO - see if I really need the REGEX in constants.R
    if (!is.na(empty_check[1, 1])) {
      # there are no data lines in this file
      return(NULL)
    }
    col_classes <- rep("character", length(header))
    # suppressWarnings because we get cols  =  3 != length(data)  =  4 when there are
    # missing end delimiters. unfortunately we have to suppress *all* warnings
    dat <- suppressWarnings(utils::read.table(filename, header = FALSE, sep = "\t", as.is = TRUE,
                                        check.names = FALSE, skip = nskip + 1, fill = TRUE,
                                        strip.white = TRUE, quote = "", comment.char = "",
                                        colClasses = col_classes, na.strings = ""))
    names(dat) <- header
    ## # deal with extra delimiters at end of line. thanks, phs001013.
    ## extra_columns <- is.na(names(dat))
    ## for (column in rev(which(extra_columns))) {
    ##   # reverse the loop because we are removing columns; otherwise column numbers shift lower
    ##   dat[[column]] <- NULL
    ## }

    ## remove rows and columns with all NAs
    ## note in DDs, some rows with encoded VALUEs will lack header
    blank.rows <- rowSums(!is.na(dat)) %in% 0
    blank.cols <- colSums(!is.na(dat)) %in% 0 & names(dat) %in% "" 
    dat <- dat[!blank.rows,!blank.cols]
     
  }, error = function(e) {
    stop(paste("in reading file", filename, ":\n", e$message), call. = FALSE)
  })

  dat
}

#' Read data dictionary file
#' 
#' @param filename The path to the file on disk
#'
#' @details
#' Expects .txt or .xlsx file. 
#' dbGaP data dictionary files should have column headers as the first row. If the input violates this, e.g. additional header rows are present, a warning is returned but the file is still read in.
#' @return
#' A data frame from the file
#'
#' @rdname read_dd_file

.read_dd_file <- function(filename){

  stopifnot(file.exists(filename))
  
  ## read in data dictionary files. could be txt or Excel
  ## exit if file extension indicates other than .txt or .xlsx)
  ext <- tools::file_ext(filename)
  if(!ext %in% c("txt", "xlsx","xls")) {
    stop("Expected tab-delimited or Excel input file, not .", ext)
  }  
  ## add name of file to error message in case of failure
  tryCatch({

    ## method for reading in DD depends on file type
    if(ext %in% "txt"){
      dd <- .read_ds_file(filename, dd=TRUE)
      # tibbles can't have unnamed columns
      names(dd)[names(dd) %in% ""] <- paste0("X__",1:sum(names(dd) %in% ""))
      dd <- as_tibble(dd)
    } else if (ext %in% c("xls","xlsx")) {

      sheet_arg <- NULL
      # check if there are multiple sheets
      sheets <- readxl::excel_sheets(filename)
      if(length(sheets) > 1){
        warning("Data dictionary Excel contains multiple sheets; assuming first is the DD")
        sheetArg <- sheets[1]
      }
      dd <- readxl::read_excel(filename, sheet=sheet_arg, col_types="text")
      
      # identify if first row was not column headers
      if(!is.element("VARNAME", toupper(names(dd)))){
        warning("Detected extra header rows (before column names) in your Excel data dictionary; these should be removed")
        colnames_row <- which(stringr::str_detect(dd, "VARDESC") |
                              stringr::str_detect(dd, "vardesc"))
        dd <- readxl::read_excel(filename, sheet=sheet_arg,
                                 skip=colnames_row+1, col_types="text")
      }
    }
  }, error = function(e) {
    stop(paste("in reading file", filename, ":\n", e$message), call. = FALSE)
  })
    dd
}

#### I. checking dbGaP files  

#' Check data dictionary (generic)
#'
#' @param dd Data dictionary object
#' @param ds Corresponding dataset object
#'
#' @details
#' Reports errors or issues as warnings.
#' 
#' @rdname check_dd

# TO DO - return issues in a list, vs. echoing as warnings

.check_dd <- function(dd, ds=NULL){

  # all colnames should be upper case
  upp <- toupper(names(dd))
  if(!all.equal(upp, names(dd))){
    warning("All column names should be UPPER CASE")
    names(dd) <- upp
  }
  
  # required first two columns
  if(!all.equal(names(dd)[1:2], c("VARNAME","VARDESC"))){
    warning("First two columns required to be 'VARNAME' and 'VARDESC'")
  }

  # check existing named columns against all possible columns
  # if there are trailing columns, they are likely because of encoded values -
  # exclude from the check
  all_cols <- names(dd)[!grepl("X__", names(dd))]

  possible_cols <- c("VARNAME", "VARDESC", "DOCFILE", "TYPE", "UNITS", "MIN", "MAX",
                     "RESOLUTION", "COMMENT1", "COMMENT2", "VARIABLE_SOURCE",
                     "SOURCE_VARIABLE_ID", "VARIABLE_MAPPING", "UNIQUEKEY",
                     "COLLINTERVAL", "ORDER", "VALUES")

  extra_cols <- setdiff(all_cols, possible_cols)
  if(length(extra_cols) > 0){
    warning("DD contains non-standard columns:", extra_cols)
  }

  # if VALUES col exists, needs to be last
  if("VALUES" %in% toupper(names(dd))){
    nnames <- length(names(dd)[!grepl("X__", names(dd))])
    if("VALUES" != names(dd)[nnames]){
      warning("'VALUES' must be last column")
    }

    # extract encoded values
    val_col <- which(names(dd) %in% "VALUES")
    name_col <- which(names(dd) %in% "VARNAME")

    # only proceed with this check if there are non NA entries in VALUES column
    if(sum(!is.na(dd$VALUES)) > 0) {
    
        encoded_vars <- dd[!is.na(dd$VALUES), c(name_col, val_col:ncol(dd))]

        # check for multiple "=" statements within a cell
        eq_count <- apply(encoded_vars, 2, function(x) {stringr::str_count(x, "=")})

        # if only 1 VARNAME with VALUES, coerce 'eq_count' to 1 row matrix
        if(is.null(dim(eq_count))){
          eq_count <- matrix(eq_count, nrow=1, ncol=length(eq_count))
        }
        
        if(sum(Biobase::rowMax(eq_count) > 1) > 0){
          mult_eq_vars <- encoded_vars$VARNAME[Biobase::rowMax(eq_count) > 1]
          warning(mult_eq_vars, " variable(s) has multiple VALUE entries per cell. Only the first entry per cell will be evaluated. Encoded values must be split into one per cell.")
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
              warning("For variable ", var,", the following values are undefined in the dd: ",
                      paste(undef_vals, collapse=", "))
            } # if extra vars
          } # loop through encoded vars
        } #  if dataset is provided
  } # if there are non-NA entires in VALUES column
  } # if VALUES col is present
  
  # if dataset provided, check for all vars
  if(!is.null(ds)){
    miss.vars <- setdiff(names(ds), pull(dd, VARNAME))
    if(length(miss.vars) > 0){
      warning("Data dictionary missing following dataset variables:", miss.vars)
    }
    extra.vars <- setdiff(pull(dd, VARNAME), names(ds))
    if(length(extra.vars) > 0){
      warning("Data dictionary has extra variables not in dataset:", extra.vars)
    }
  }

  # check for illegal characters in variable names: \ / , dbGaP
  nms <- toupper(names(ds))
  # nms[4:6] <- c("hi/ya", "VARDBGAP", "my,var")
  ill_vars_sel <- stringr::str_detect(nms, "DBGAP|\\\\|/|,")
  
  if(sum(ill_vars_sel) > 0){
    ill_vars <- nms[ill_vars_sel]
    warning("Illegal characers '\', '/', ',' (comma), or 'dbGaP' are present in the following variable(s): ", paste(ill_vars, collapse="; "))
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
#'
#' @return ssm_report, a list of the following issues (when present):
#' \item{missing_vars}{Missing and required variables}
#' \item{dup_samples}{List of duplicated sample IDs}
#' \item(blank_idx}{Row index of blank/missing subject or sample IDs}
#' Additionally, if (\code{ddfile != NULL}):
#' \item{dd_errors}{Differences in fields between data file and data dictionary}
#' Additionally, if (\code{ssm_exp != NULL}):
#' \item{extra_subjects}{Subjects in data file missing from \code{ssm_exp}}
#' \item{missing_subjects}{Subjects in \code{ssm_exp} missing from data file}
#' \item{extra_samples}{Samples in data file missing from \code{ssm_exp}}
#' \item{missing_samples}{Samples in \code{ssm_exp} missing from data file}
#' \item{ssm_diffs}{Discrepancies in mapping between SAMPLE_ID and SUBJECT_ID. Lists entries in \code{ssm_exp} that disagree with mapping in the data file. }
#' Additionally, if (\code{sample == uses}):
#' \item{sampuse_diffs}{Discrepancies with expected SAMPLE_USE values}
#' Additionally, if (\code{topmed == TRUE}):
#' \item{missing_topmed_vars}{Missing and required variables for TOPMed}
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

  # check for required columns
  # req_vars <- c("SUBJECT_ID","SAMPLE_ID","SAMPLE_USE")
  # allow for other ID names
  req_vars <- c(subjectID_col, sampleID_col, "SAMPLE_USE")
  miss_vars <- setdiff(req_vars, names(ds))
  missing_vars <- ifelse(length(miss_vars %in% 0), NA, miss_vars)
  
  # create and return results list
  ssm_report <- list()

  if(!is.na(missing_vars)){
    ssm_report$missing_vars <- missing_vars
  }
  if(!is.null(dup_samples)){
   satt_report$dup_samples <- dup_samples
  }
  if(!is.null(blank_idx)){
   satt_report$blank_idx <- blank_idx
  }
  if(!is.null(dd_errors)){
    ssm_report$dd_errors <- dd_errors
  }
  if(!is.null(extra_subjects & length(extra_subjects > 0))){
    ssm_report$extra_subjects <- extra_subjects
  }
  if(!is.null(missing_subjects & length(missing_subjects > 0))){
    ssm_report$missing_subjects <- missing_subjects
  }  
  if(!is.null(extra_samples & length(extra_samples > 0))){
    ssm_report$extra_samples <- extra_samples
  }
  if(!is.null(missing_samples) & length(missing_samples > 0)){
    ssm_report$missing_samples <- missing_samples
  }
  if(!is.null(ssm_diffs)){
    ssm_report$ssm_diffs <- ssm_diffs
  }
  if(!is.null(sampuse_diffs)){
    ssm_report$sampuse_diffs <- sampuse_diffs
  }
  if(!is.null(missing_topmed_vars)){
    ssm_report$missing_topmed_vars <- missing_topmed_vars
  }

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
#' @detail
#' When (\code{topmed = TRUE}) checks presence of additional, TOPMed-specific
#' sample attributes variables: SEQUENCING_CENTER, Funding_Source, TOPMed_Phase, 
#' TOPMed_Project, Study_Name.
#'
#' @return satt_report, a list of the following issues (when present):
#' \item{missing_vars}{Missing and required variables}
#' \item{dup_samples}{List of duplicated sample IDs}
#' \item(blank_idx}{Row index of blank/missing sample IDs}
#' Additionally, if (\code{ddfile != NULL}):
#' \item{dd_errors}{Differences in fields between data file and data dictionary}
#' Additionally, if (\code{samp_exp != NULL}):
#' \item{extra_samples}{Samples in data file missing from \code{ssm_exp}}
#' \item{missing_samples}{Samples in \code{ssm_exp} missing from data file}
#' Additionally, if (\code{topmed == TRUE}):
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

  # report any blank sample IDs by row idex
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
  missing_vars <- ifelse(length(miss_vars %in% 0), NA, miss_vars)

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

  if(!is.na(missing_vars)){
   satt_report$missing_vars <- missing_vars
  }
  if(!is.null(dup_samples)){
   satt_report$dup_samples <- dup_samples
  }
  if(!is.null(blank_idx)){
   satt_report$blank_idx <- blank_idx
  }
  if(!is.null(dd_errors)){
    satt_report$dd_errors <- dd_errors
  }
  if(!is.null(extra_samples) & length(extra_samples > 0)){
    satt_report$extra_samples <- extra_samples
  }
  if(!is.null(missing_samples) & length(missing_samples > 0)){
    satt_report$missing_samples <- missing_samples
  }
  return(satt_report)
}

# check_subj



#### II. writing dbGaP files
# expected inputs? R datas? 
