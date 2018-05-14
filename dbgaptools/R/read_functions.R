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
#' @export

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
#' @param na_vals Vector of strings that should be read in as NA/missing (see details)
#' @param remove_empty_row Logical of whether to exclude empty (i.e. all missing values) rows. Defaults to TRUE
#' @param remove_empty_col Logical of whether to exclude empty (i.e. all missing values) rowcolumns. Defaults to FALSE
#'
#' @details
#' Missing values: The blank string "" will always be considered an NA or missing value. Additional strings that should be read in as missing values can be specified in the \code{na_vals} argument.
#' The default set of additional NA values is "NA","N/A","na","n/a."
#' Users should change the default if these values represent something beside missing ---
#'  for example, "NA" could be an encoded value meaning "North America".
#' Users may wish to add a value to the list, e.g. \code{na_vals=c("NA","N/A","na","n/a", "9999")}.
#'
#' @return
#' A data frame from the file
#'
#' @details
#' dbGaP dataset files should have column headers as the first row. If the input violates this, e.g. additional header rows are present, a warning is returned but the file is still read in.
#' 
#' @rdname read_ds_file
#' @export

.read_ds_file <- function(filename, dd=FALSE, na_vals=c("NA","N/A","na","n/a"),
                          remove_empty_row=TRUE, remove_empty_col=FALSE) {

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
    dat <- suppressWarnings(utils::read.table(filename, header = FALSE, sep = "\t",
                                              as.is = TRUE, check.names = FALSE,
                                              skip = nskip + 1, fill = TRUE,
                                              strip.white = TRUE, quote = "",
                                              comment.char = "", colClasses = col_classes,
                                              na.strings = c("", na_vals)))
    names(dat) <- header
    ## # deal with extra delimiters at end of line. thanks, phs001013.
    ## extra_columns <- is.na(names(dat))
    ## for (column in rev(which(extra_columns))) {
    ##   # reverse the loop because we are removing columns; otherwise column numbers shift lower
    ##   dat[[column]] <- NULL
    ## }

    ## remove rows with all blanks/NAs
    if(remove_empty_row) {
        blank.rows <- rowSums(!is.na(dat)) %in% 0
        dat <- dat[!blank.rows,]
        }
    
    ## remove columns with all blanks/NAs (FALSE by default - removes too many DD cols)
    if(remove_empty_col) {
        blank.cols <- colSums(!is.na(dat)) %in% 0
        dat <- dat[,!blank.cols]
        }

  }, error = function(e) {
    stop(paste("in reading file", filename, ":\n", e$message), call. = FALSE)
  })

  dat
}

#' Read data dictionary file
#' 
#' @param filename The path to the file on disk
#' @param remove_empty_row Logical of whether to exclude empty (i.e. all missing values) rows. Defaults to TRUE
#' @param remove_empty_col Logical of whether to exclude empty (i.e. all missing values) rowcolumns. Defaults to FALSE
#' 
#' @details
#' Expects (tab-delimited) .txt or .xlsx file. 
#' dbGaP data dictionary files should have column headers as the first row. If the input violates this, e.g. additional header rows are present, a warning is returned but the file is still read in.
#' @return
#' A data frame from the file
#'
#' @rdname read_dd_file
#' @export

.read_dd_file <- function(filename, remove_empty_row=TRUE, remove_empty_col=FALSE){

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
      
      # rename extra columns after VALUES as "X__*" and save as tibble            
      if("VALUES" %in% toupper(names(dd))){
          idx <- (grep("VALUES", names(dd), ignore.case=TRUE) + 1):ncol(dd)
          new.nms <- paste0("X__", 1:length(idx))
          names(dd)[idx] <- new.nms
          }
      
      # save as tibble (for consistency with Excel input processing, partly)
      dd <- tibble::as_tibble(dd)
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
        warning("Additional rows are present before column headers and should be removed prior to dbGaP submission")
        colnames_row <- which(stringr::str_detect(dd, stringr::regex("VARDESC", ignore.case=TRUE)))
        dd <- readxl::read_excel(filename, sheet=sheet_arg,
                                 skip=colnames_row+1, col_types="text")
      }
    }
  }, error = function(e) {
    stop(paste("in reading file", filename, ":\n", e$message), call. = FALSE)
  })

    ## remove rows with all blanks/NAs
    if(remove_empty_row) {
        blank.rows <- rowSums(!is.na(dd)) %in% 0
        dd <- dd[!blank.rows,]
        }
    
    ## remove columns with all blanks/NAs (FALSE by default - removes too many DD cols)
    if(remove_empty_col) {
        blank.cols <- colSums(!is.na(dd)) %in% 0
        dd <- dd[,!blank.cols]
        }    
    
    dd
}


