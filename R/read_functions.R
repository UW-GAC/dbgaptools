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
#' @param processed Logical indicator of whether the file has been processed by dbGaP.
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
#' @rdnameread_ds_file
#' @export

read_ds_file <- function(filename, dd=FALSE, processed = FALSE,
                         na_vals=c("NA","N/A","na","n/a"),
                          remove_empty_row=TRUE, remove_empty_col=FALSE) {

  stopifnot(file.exists(filename))

  # Exit if file extension indicates other than .txt (e.g., csv, xlsx).
  ext <- tools::file_ext(filename)
  if(ext != "txt") {
    stop("Expected tab-delimited input file (.txt), not .", ext)
  }

  ## add name of file to error message in case of failure
  tryCatch({
    # May be legitimate # characters in the data fields that do not indicate comments,
    # but we need to skip header lines that start with a comment character.
    # First, decide how many header lines to skip.
    if(!dd) {
      nskip <- .count_hdr_lines(filename)
    } else if (dd) {
      nskip <- .count_hdr_lines(filename, colname="VARNAME")
    }

    if (!processed & (nskip > 0)) {
      warning("Additional rows are present before column headers and should be removed prior to dbGaP submission")
    }
    header <- scan(filename, sep = "\t", skip = nskip, nlines = 1, what = "character", quiet = TRUE)
    empty_check <- stringr::str_match(header[1], REGEX_BLANK_DATA_FILE)
    if (!is.na(empty_check[1, 1])) {
      # There are no data lines in this file.
      return(NULL)
    }
    col_classes <- rep("character", length(header))
    # suppressWarnings because we get cols  =  3 != length(data)  =  4 when there are
    # missing end delimiters. Unfortunately we have to suppress *all* warnings.
    dat <- suppressWarnings(utils::read.table(filename, header = FALSE, sep = "\t",
                                              as.is = TRUE, check.names = FALSE,
                                              skip = nskip + 1, fill = TRUE,
                                              strip.white = TRUE, quote = "",
                                              comment.char = "", colClasses = col_classes,
                                              na.strings = c("", na_vals)))
    names(dat) <- header

    # Remove rows with all blanks/NAs.
    if(remove_empty_row) {
        blank.rows <- rowSums(!is.na(dat)) %in% 0
        dat <- dat[!blank.rows,]
        }

    # Remove columns with all blanks/NAs (FALSE by default - removes too many DD cols).
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
#' Expects (tab-delimited) .txt, .xlsx, or .xml file.
#' For .txt and .xlsx files, dbGaP data dictionary files should have column headers as the first row. If the input violates this, e.g. additional header rows are present, a warning is returned but the file is still read in.
#' .xml file types should be those provided by dbGaP.
#' Only a subset ofpossible child nodes of a variable node are processed:
#' \code{type}
#' \code{unit}
#' \code{logical_min}
#' \code{logical_max}
#' These names are converted to the names expected in a user-submitted data dictionary.
#' Finally, if any variables are flagged as "unique keys", a column is added to the output data frame and populated correctly (with X's).
#' Otherwise, the "UNIQUEKEY" column does not exist in the output.
#'
#' @return
#' A data frame from the file
#'
#' @rdnameread_dd_file
#' @export

read_dd_file <- function(filename, remove_empty_row=TRUE, remove_empty_col=FALSE){

  stopifnot(file.exists(filename))

  allowed_text_exts <- c("txt")
  allowed_xls_exts <- c("xlsx", "xls")
  allowed_xml_exts <- c("xml")
  allowed_exts <- c(allowed_text_exts, allowed_xls_exts, allowed_xml_exts)
  ## read in data dictionary files. could be txt or Excel
  ## exit if file extension indicates other than .txt or .xlsx)
  ext <- tools::file_ext(filename)
  if(!ext %in% allowed_exts) {
    stop("Expected tab-delimited or Excel input file, not .", ext)
  }
  ## add name of file to error message in case of failure
  tryCatch({

    ## method for reading in DD depends on file type
    if(ext %in% allowed_text_exts){
      dd <- .read_dd_txt(filename)
    } else if (ext %in% allowed_xls_exts) {
      dd <- .read_dd_xls(filename)
    } else if (ext %in% allowed_xml_exts) {
      dd <- .read_dd_xml(filename)
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

.rename_val_cols <- function(dd) {
  # rename extra columns after VALUES as "X__*"
  val.col <- grep("VALUES", names(dd), ignore.case = TRUE)
  if (length(val.col) > 0) {
    if (val.col < ncol(dd)) {
      idx <- (val.col + 1):ncol(dd)
      new.nms <- paste0("X__", 1:length(idx))
      names(dd)[idx] <- new.nms
    }
  }
  return(dd)
}

.read_dd_txt <- function(filename) {
  dd <- read_ds_file(filename, dd = TRUE)

  dd <- .rename_val_cols(dd)

  # save as tibble (for consistency with Excel input processing, partly)
  dd <- tibble::as_tibble(dd)

  return(dd)
}


.read_dd_xls <- function(filename) {
  sheet_arg <- NULL
  # check if there are multiple sheets
  sheets <- readxl::excel_sheets(filename)
  if (length(sheets) > 1) {
    warning("Data dictionary Excel contains multiple sheets; assuming first is the DD")
    sheetArg <- sheets[1]
  }
  dd <- readxl::read_excel(filename, sheet = sheet_arg, col_types = "text")

  # identify if first row was not column headers
  if (!is.element("VARNAME", toupper(names(dd)))) {
    warning("Additional rows are present before column headers and should be removed prior to dbGaP submission")
    colnames_row <- which(stringr::str_detect(dd, stringr::regex("VARDESC", ignore.case = TRUE)))
    dd <- readxl::read_excel(filename, sheet = sheet_arg,
                             skip = colnames_row + 1, col_types = "text")
  }

  dd <- .rename_val_cols(dd)

  return(dd)
}


.read_dd_xml <- function(filename) {
  # Set parent_dd_file to the filename of the XML data dictionary on disk
  xml_dd <- xml2::read_xml(filename)

  # Select variable nodes
  variable_nodes <- xml2::xml_find_all(xml_dd, "/data_table/variable")

  # Create a one-line data frame for each variable node.
  required_nodes <- c(
    VARNAME = "name",
    VARDESC = "description"
  )
  # Process some optional nodes; others are ignored.
  optional_nodes <- c(
    TYPE = "type",
    UNITS = "unit",
    MIN = "logical_min",
    MAX = "logical_max"
  )
  unique_keys <- xml2::xml_find_all(xml_dd, "/data_table/unique_key") %>%
    xml2::xml_text()
  df_list <- lapply(variable_nodes, function(x) {
    df <- data.frame(stringsAsFactors = FALSE, row.names = 1)
    for (n in names(required_nodes)) {
      xpath <- sprintf(".//%s", required_nodes[[n]])
      text <- xml2::xml_find_all(x, xpath) %>%
        xml2::xml_text()
      df[[n]] <- text
    }
    for (n in names(optional_nodes)) {
      xpath <- sprintf(".//%s", optional_nodes[[n]])
      text <- xml2::xml_find_all(x, xpath) %>%
        xml2::xml_text()
      if (length(text) > 0) {
        if (text == "") text <- NA
        df[[n]] <- text
      }
    }
    # Add the colmn that identifies the unique keys.
    if (length(unique_keys) > 0) {
      df$UNIQUEKEY <- ifelse(df$VARNAME %in% unique_keys, "X", NA)
    }

    # VALUES nodes are stored in multiple colmns, so process them separately.
    child_value_nodes <- xml2::xml_find_all(x, ".//value")
    if (length(child_value_nodes) > 0) {
      value_strings <- sprintf(
        "%s=%s",
        unlist(xml2::xml_attrs(child_value_nodes)),
        xml2::xml_text(child_value_nodes)
      )
      value_df <- do.call(data.frame,c(as.list(value_strings), stringsAsFactors = FALSE))
      names(value_df) <- NULL
      df <- dplyr::bind_cols(df, value_df)
      idx <- which(names(df) == "V1")
      names(df)[idx] <- "VALUES"
      n_extra <- ncol(df) - idx
      if (n_extra > 0) {
        names(df)[(idx + 1):ncol(df)] <- paste0("X__", 1:n_extra)
      }
    }
    df
  })

  # Bind the data frames together to create the data frame.
  dd <- do.call(dplyr::bind_rows, df_list)

  # Put the columns in the order required by dbGaP.
  required_column_order <- c("VARNAME", "VARDESC", "TYPE", "UNITS", "MIN", "MAX", "UNIQUEKEY", "VALUES")
  first_column_order <- intersect(required_column_order, names(dd))
  dd <- dd %>%
    dplyr::select(tidyselect::one_of(first_column_order),
                  tidyselect::everything())

  # Convert to tibble for consistency with other read functions.
  dd <- tibble::as_tibble(dd)

  return(dd)
}
