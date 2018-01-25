# Building functions for dbGaP files R package
# SN, 1/24/18

file_types <- c("samp_subj_mapping",
                "samp_attributes",
                "subj_consent",
                "phenotype",
                "pedigree")
           
#### 0. utility functions

## borrowed from dbTopmed::.countHeaderLines
#' Count the header lines in a data file
#'
#' @param filename The path to the data file on disk
#'
#' @details
#' Header lines are considered to start with # or to be a blank line
#'
#' @return
#' the number of header lines in the file
#'
#' @rdname count_hdr_lines
.count_hdr_lines <- function(filename) {
  con <- file(filename, "r")
  nskip <- 0
  done <- FALSE
  while (!done) {
    tmp <- readLines(con, n = 1)
    if (substr(tmp, 1, 1) %in% c("#", "")) {
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
#'
#' @details
#' The only string considered to be NA is the blank string: "". "NA" does not necessarily indicate
#' NA: for example, it could be an encoded value meaning "North America".
#'
#' @return
#' A data frame from the file
#'
#' @rdname read_ds_file
.read_ds_file <- function(filename) {

  ## exit if file extension indicates other than .txt (csv, xlsx)
  ext <- tools::file_ext(filename)
  if(ext != "txt") {
    stop("Expected tab-delimited input file (.txt), not .", ext)
  }

  ## add name of file to error message in case of failure
  tryCatch({
    ## may be comment characters in the data fields. first decide how many lines to skip
    nskip <- .count_hdr_lines(filename)
    header <- scan(filename, sep = "\t", skip = nskip, nlines = 1, what = "character", quiet = TRUE)
    empty_check <- stringr::str_match(header[1], REGEX_BLANK_DATA_FILE)
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
    # deal with extra delimiters at end of line. thanks, phs001013.
    extra_columns <- is.na(names(dat))
    for (column in rev(which(extra_columns))) {
      # reverse the loop because we are removing columns; otherwise column numbers shift lower
      dat[[column]] <- NULL
    }
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
#'
#' @return
#' A data frame from the file
#'
#' @rdname read_dd_file

.read_dd_file <- function(filename){
  
  ## read in data dictionary files. could be txt or Excel
  ## exit if file extension indicates other than .txt or .xlsx)
  ext <- tools::file_ext(filename)
  if(ext %in% c("txt", "xlsx","xls")) {
    stop("Expected tab-delimited or Excel input file, not .", ext)
  }  
  ## add name of file to error message in case of failure
  tryCatch({
    ## TBD how to deal with identifying header lines in either .txt or .xls*

    ## method for readin in DD depends on file type
    if(ext %in% "txt"){
      dd <- .read_ds_file(filename)
      dd <- as_tibble(dd)
    } else if (ext %in% c("xls","xlsx") ){
      sheet_arg <- NULL
      # check if there are multiple sheets
      sheets <- readxl::excel_sheets(filename)
      if(length(sheets) > 1){
        warning("Data dictionary Excel contains multiple sheets; assuming first is the DD")
        sheetArg <- sheets[1]
      }

      dd <- readxl::read_excel(filename, sheet=sheet_arg, col_types="text")
    }

    dd
}

# adapt my 'preview' function for use witha verbose/preview option?

# create R markdown report of any issues?


#' Check sample subject mapping file
#'
#' Check contents of a sample subject mapping file for dbGaP posting.
#'
#' @param dsfile Path to the data file on disk
#' @param ddfile Path to the data dictionary file on disk
#' @param ssm_exp Dataframe of expected SAMPLE_ID and SUBJECT_ID
#' @param sample_uses Either a single string for expected SAMPLE_USE across all samples, or a data frame with SAMPLE_ID and SAMPLE_USE values
#' @param topmed Logical to indicate TOPMed study
#'
#' @details
#' When (\code{ssm_exp != NULL}), checks for expected correspondence between
#' SAMPLE_ID and SUBJECT_ID. Any differences in mapping between the two,
#' or a difference in the list of expectd SAMPLE_IDs or SUBJECT_IDs,
#' will be returned in the output.
#' If a data dictionary is provided (\code{ddfile != NULL}), additionally checks 
#' correspondence between column names in data file and entries in data dictionary.
#' When (\code{TOPMed = TRUE}) checks for presence of additional, TOPMed-specific
#' sample attributes variables: SEQUENCING_CENTER, Funding_Source, TOPMed_Phase, 
#' TOPMed_Project, Study_Name.
#'
#' @return ssm_report, a list of the following:
#' \item{missing_vars}{Missing and required variables}
#' Additionally, if (\code{ddfile != NULL}):
#' \item{dd_errors}{Differences in fields bewteen data file and data dictionary}
#' Additionally, if (\code{ssm_exp != NULL}):
#' \item{extra_subjects}{Subjects in data file missing from \code{ssm_exp}}
#' \item{missing_subjects}{Subjects in \code{ssm_exp}} missing from data file}
#' \item{extra_samples}{Samples in data file missing from \code{ssm_exp}}
#' \item{missing_samples}{Samples in \code{ssm_exp}} missing from data file}
#' \item{ssm_diffs}{Discrepancies in mapping between SAMPLE_ID and SUBJECT_ID}
#' Additionally, if (\code{sample == uses}):
#' \item{sampuse_diffs}{Discrepancies with expected SAMPLE_USE values}
#' Additionally, if (\code{topmed == TRUE}):
#' \item{missing_topmed_vars}{Missing and required variables for TOPMed}
#' 
#' @rdname check_ssm

#### I. checking dbGaP files
check_ssm <- function(dsfile, ddfile=NULL, ssm_exp=NULL,
                      sample_uses=NULL, topmed=TRUE, verbose=TRUE){

  # read in data file
  ds <- .read_ds_file(dsfile)

  # cannot proceed without "SUBJECT_ID" and "SAMPLE_ID" columns
  if(!is.element(names(ds), "SUBJECT_ID") | !is.element(names(ds), "SAMPLE_ID")){
    stop("Please check that dsfile contains columns 'SUBJECT_ID' and 'SAMPLE_ID'")
  }
  
  # read in data dictionary if provided
  if(!is.null(ddfile)){
    dd <- .read_dd_file(ddfile)
  }


  
  # if TOPMed, check for TOPMed-specific variables




}

# check_sattr

# check_subj



#### II. writing dbGaP files
# expected inputs? R datas? 
