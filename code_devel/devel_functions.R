# Building functions for dbGaP files R package
# SN, 1/24/18

file_types <- c("samp_subj_mapping",
                "samp_attributes",
                "subj_consent",
                "phenotype",
                "pedigree")
           
#### 0. utility functions

#' Count the header lines in a dbgap file
#'
#' @param filename The path to the dbgap file on disk
#'
#' @details
#' Header lines are considered to start with # or to be a blank line
#'
#' @return
#' the number of header lines in the file
#'
#' @rdname countHeaderLines
.countHeaderLines <- function(filename) {
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

# this is in constants.R
REGEX_BLANK_DATA_FILE <- "This file is intentionally blank because this data table does not include subjects for the (.+?) consent group."

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
    nskip <- .countHeaderLines(filename)
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
    extra_columns <- is.na(names(phen))
    for (column in rev(which(extra_columns))) {
      # reverse the loop because we are removing columns; otherwise column numbers shift lower
      dat[[column]] <- NULL
    }
  }, error = function(e) {
    stop(paste("in reading file", filename, ":\n", e$message), call. = FALSE)
  })

  dat
}


.read_dd_file <- function(){
  # read in data dictionary files. could be txt or Excel



}

# adapt my 'preview' function for use witha verbose/preview option?

# create R markdown report of any issues?


#### I. checking dbGaP files




#### II. writing dbGaP files
