#### writing dbGaP files

##' Write dbGaP files
##'
##' Wrapper to \code{write.table} function to output dbGaP dataset (DS) and data dictionary (DD) files.
##' 
##' @param x Object to write out
##' @param file Filename. Can leave unspecified if \code{generate_fn=TRUE}
##' @param study_name Study name
##' @param phs dbGaP PHS accession number
##' @param DD logical flag, set to TRUE for DD file, FALSE for DS
##' @param dstype Type of file, one of "pheno","ped","sattr","ssm","subj." Can be left as default/NULL if \code{generate_fn=FALSE}
##' @param generate_fn logical flag on whether to have filename generated for you
##'
##' @details If \code{generate_fn=FALSE}, the argument to \code{file} will be used as the file name. File paths will be followed; if none given, will write to current working directory. If \code{generate_fn=TRUE}, one of either \code{study_name} or \code{phs} must be provided. If \code{generate_fn=TRUE}, fill will always be written to current working directory.
##'
##' @return Write file to disk.
##' 
##' @rdname write_dbgap
##'

write_dbgap <- function(x, file="", 
                        study_name=NULL, phs=NULL, DD=FALSE,
                        dstype="", generate_fn=FALSE){

  # if generating_fn, check for required info
  if(generate_fn) {
    if(is.null(study_name) & is.null(phs)){
    stop("To generate file name, one of studyname or phs must be provided")
    }
    dstypes <- c("pheno","ped","sattr","ssm","subj")
    if(!dstype %in% dstypes){
      stop("Please specify dstype, one of: ", paste(dstypes, collapse=", "))
    }
    # generate file name
     if(is.null(study_name)){
       pt1 <- phs
     } else if (is.null(phs)){
       pt1 <- study_name
     } else {
       pt1 <- paste(study_name, phs, sep="_")
     }

    ftype <- ifelse(DD, paste(dstype,"DD", sep="_"), paste(dstype,"DS", sep="_"))

    file <- paste0(pt1, "_", ftype, "_", format(Sys.Date(), "%Y%m%d"), ".txt")
  }  

  # stop with error if no file name is provided
  if(!generate_fn & file=="") stop("Please specify filename, or set generate_fn=TRUE")

  # if provided file name is not .txt, give warning
  ext <- tools::file_ext(file)
  if(ext != "txt") warning("Output file name recommended to have .txt extension")

  # write file
  write.table(x, file, sep="\t",  na="",  col.names=TRUE, row.names=FALSE, quote=FALSE)

}
