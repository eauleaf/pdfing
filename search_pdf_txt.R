#' Search a folder with PDFs with successive regular expressions
#'
#' function must have write access to the pdf folder location to save an RDS file of the text within the pdfs
#'
#' @param search_regexes vector of regexes to filter down the pdfs you're searching through
#' @param pdf_folder location of the folder with pdfs you want to read (uses here())
#' @param reject_regex single regex to reject any files that match the regex (broaden with regex 'or' pipes '|')
#' @param recursive search in folders as well as sub-folders
#' @param open_folder open folder locations in folder viewer
#' @param return_pdf_text return the text contents of the pdfs rather than their names
#' @param ocr read pdfs via optical character recognition rather than embedded pdf text
#' @param reread function saves an RDS of the pdf text in the folder, do you want to delete the RDS file and reread the pdfs
#'
#' @return a list of PDF locations that satisfy the regex
#' @export
#'
#' @examples search_pdf_txt(c('1099', '(?i)corrected'), here('PDF_folder'))
search_pdf_txt <- function(
    search_regexes = c('', ''),
    pdf_folder = here::here(),
    reject_regex = NULL,
    include_pathnames = T,
    recursive = F,
    open_folder = F,
    return_pdf_text = F,
    ocr = F,
    reread = F,
    ignore_locns_with = NULL
){

  # print(search_regexes)

  if(!ocr){
    locn <- here::here(pdf_folder,'pdf_search_txt.rds')
  } else {
    locn <- here::here(pdf_folder,'pdf_search_txt_ocr.rds')
  }

  if( open_folder ){ xlr::sys_open(pdf_folder) }
  if( reread && file.exists(locn) ){ file.remove(locn) }


  # read or save a series of pdf docs ----------------------------------------
  if(!file.exists(locn)){

    pdf_file_names <- here::here(pdf_folder) |> list.files(pattern = '(?i)\\.pdf$', full.names = TRUE, recursive = recursive)
    pdf_file_names_short <- stringr::str_sub(pdf_file_names, nchar(here::here())+2)
    pdf_file_names <- pdf_file_names |> rlang::set_names(pdf_file_names_short)

    cat('\nReading & saving the contents of:', length(pdf_file_names), 'PDF files.\n')

    future::plan("future::multisession")

    if(!ocr){
      safe_pdf_txt <- purrr::possibly(pdftools::pdf_text)
      # pdf_contents <- pdf_file_names |> purrr::map(possibly(pdftools::pdf_text))
      pdf_contents <- pdf_file_names |> furrr::future_map(safe_pdf_txt, .progress = TRUE)
    } else {
      # pdf_contents <- pdf_file_names |> purrr::map(pdftools::pdf_ocr_text)
      pdf_contents <- pdf_file_names |> furrr::future_map(pdftools::pdf_ocr_text, .progress = TRUE)
    }

    readr::write_rds(pdf_contents, locn)

  }

  # get filtered pdf names ----------------------------------------------
  pdf_contents <- readr::read_rds(here::here(locn))

  content_names <- filter_pdf_contents(search_regexes, pdf_contents, reject_regex)
  if(include_pathnames){
    pathnames <- filter_pathnames(search_regexes, names(pdf_contents), reject_regex)
    # pathnames <- filter_pathnames(search_regexes, list.files(here::here(locn), recursive = recursive), reject_regex)
    content_names <- union(pathnames, content_names)
    cat(paste0('\n\nUnion of search results: ', length(content_names), '\n'))
  }

  if(!is.null(ignore_locns_with)){

    reject_these <- content_names[stringr::str_detect(content_names, ignore_locns_with)]
    content_names <- setdiff(content_names, reject_these)

    cat(paste0('\n - Excluded ', length(reject_these),' for a location-name with "',ignore_locns_with, '" \n'))
    cat(paste0('\n - returning: ', length(content_names), '\n'))

  }

  cat('\n')

  # return
  if(return_pdf_text){
    pdf_contents[content_names]
  } else {
    content_names
  }


}
