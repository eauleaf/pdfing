#' OCR a pdf and embed text
#' R implementation of OCRmyPDF for linux
#' To install tesseract:
#' https://tesseract-ocr.github.io/tessdoc/Installation.html
#' https://github.com/UB-Mannheim/tesseract/wiki
#' https://ocrmypdf.readthedocs.io/en/latest/installation.html#installing-on-windows
#' To install on Linux https://ocrmypdf.readthedocs.io/en/latest/installation.html
#'
#' To see all options, in terminal type: man ocrmypdf
#'
#' @param path_read text: path to the pdf to ocr (uses here)
#' @param ...  text: (optional) args for OCRmyPDF (https://ocrmypdf.readthedocs.io/en/latest/cookbook.html#)
#' @param path_save text: (optional) save location/filename.pdf (if used, argument must be specified by name)
#'
#' @return a path to the OCR'd output pdf file
#'
#' @export
#'
#' @examples \dontrun{
#' pdf_embed_ocr('/home/user/Desktop/somefile.pdf','--deskew', '--clean')
#' pdf_embed_ocr('/home/user/Desktop/somefile.tif')
#' list.files(pattern = '(?i)pdf$', full.names = T, recursive = T) |> #' purrr::map(
#' ~pdf_embed_ocr(., '--deskew', '--rotate-pages', '--threshold', '--skip-text',
#' '--redo-ocr', '--clean','-remove-background'))
#' }
#'
pdf_embed_ocr <- function(path_read, ..., path_save = NULL){

  if( Sys.info()[1] != 'Linux'){ stop(
    'Embedded OCR is only implemented on Linux. Try pdftools::pdf_ocr_text() instead.'
    )}

  replace_file <- FALSE

  path_read <- here::here(path_read)
  if(is.null(path_save)){
    path_save <- stringr::str_replace(path_read, '(?i)\\.pdf$|$','_ocr.pdf')
  } else {
    path_save <- here::here(path_save)
  }


  # do you need to create a delete file
  if(path_read == path_save){
    path_save <- stringr::str_replace(path_read, '(?i)\\.pdf$|$','_ocr.pdf')
    delete_folder <- file.path(dirname(path_read),'delete')
    dir.create(delete_folder, showWarnings = F)
    replace_file = TRUE

  }


  sys_args <- c(glue::glue("'{unlist(list(...))}'"), glue::glue("'{path_read}'"), glue::glue("'{path_save}'"))
  message('Passing arguments:')
  purrr::map(sys_args, ~cat(.,'\n'))
  system2('ocrmypdf', args = sys_args)


  if(replace_file){

    file.copy(path_read, file.path(delete_folder, basename(path_read)), overwrite = T)
    file.remove(path_read)
    file.rename(path_save, path_read)
    path_save <- path_read

  }

  path_save

}


