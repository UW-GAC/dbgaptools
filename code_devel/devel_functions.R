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

# adapt my 'preview' function for use with a verbose/preview option?

#### I. checking dbGaP files  

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

check_ssm <- function(dsfile, ddfile=NULL, ssm_exp=NULL,
                      sample_uses=NULL, topmed=TRUE, verbose=TRUE){

  ## read in data file
  ds <- .read_ds_file(dsfile)

  ## cannot proceed without "SUBJECT_ID" and "SAMPLE_ID" columns
  if(!is.element("SUBJECT_ID", names(ds)) | !is.element("SAMPLE_ID", names(ds))){
    stop("Please check that dsfile contains columns 'SUBJECT_ID' and 'SAMPLE_ID'")
  }

  # read in data dictionary if provided
  if(!is.null(ddfile)){
    dd <- .read_dd_file(ddfile)
    dd.chk <- .check_dd(dd)

  }

  # expected SSM if provided

  # check for expected sample uses

  # check for required columns

  
  # if TOPMed, check for TOPMed-specific variables

}

#' Check data dictionary (generic)
#'
#' @param dd Data dictionary object
#' @param ds Corresponding dataset object
#'
#' @details
#' Reports errors or issues as warnings.
#' 
#' @rdname .check_dd

.check_dd <- function(dd, ds=NULL){

  # all colnames should be upper case
  upp <- toupper(names(dd))
  if(!all.equal(upp, names(dd))){
    warning("All column names should be UPPER CASE")
    names(dd) <- upp
  }
  
  # required first three columns
  if(!all.equal(names(dd)[1:3], c("VARNAME","VARDESC"))){
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
    encoded_vars <- dd[!is.na(dd$VALUES), c(name_col, val_col:ncol(dd))]

    # check for multiple "=" statements within a cell
    eq_count <- apply(encoded_vars, 2, function(x) {str_count(x, "=")} )
    if(sum(rowMax(eq_count) > 1) > 0){
      mult_eq_vars <- encoded_vars$VARNAME[rowMax(eq_count) > 1]
      warning(mult_eq_vars, " variable(s) have multiple VALUE entries per cell. Only the first entry per cell will be evaluated. Encoded values must be split into one per cell.")
    }

    # extract encoded values
    encoded_vars %<>%
      mutate_all(function(x) str_replace(x, "=.*", "")) %>%
        gather("column", "row", -VARNAME) %>%
          spread(VARNAME, row) %>%
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


# check_sattr

# check_subj



#### II. writing dbGaP files
# expected inputs? R datas? 
