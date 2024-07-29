#' Search folder for PDF's whose content matches your regular expression
#'
#' Returns the names (or the internal text) of all PDF's that match your input regular expressions.
#'
#' Note that the user function must have write access to the pdf folder location being searched
#' to save an RDS file of the text within the pdfs.
#'
#' @param search_regexes vector of regular expressions to filter down the pdfs you're searching through
#' @param search_folder a folder location that contains the pdfs you want to read (uses here())
#' @param reject_regex single regex to reject any files that match the regex (broaden with stringr::regex() 'or' pipes '|')
#' @param recursive search in specified folder as well as its sub-folders
#' @param include_pathnames search for the regex text in pathnames as well
#' @param reject_pathnames_regex single regex to reject any pathnames that match (broaden with stringr::regex() 'or' pipes '|')
#' @param ignore_duplicate_filenames remove duplicated pdf filenames, e.g. /downloads/doc1.pdf and documents/doc1.pdf would return only one file.
#' @param output_form one of c("doc_paths","doc_text","matching_pages","text_lines") returns the contents of the pdfs rather than their names
#' @param reread the function saves an RDS of the pdf text in the search folder, do you want to delete the saved RDS file and reread the pdfs
#'
#' @return a list of PDF path locations that satisfy the regex (or alternatively a list of page numbers, the full text from the documents, or text from the specific lines where the regex text was detected)
#' @export
#'
#' @examples \dontrun{ pdf_search_txt(c('1099', '(?i)corrected'), here('search_folder')) }
pdf_search_txt <- function(
    search_regexes = c('', ''),
    search_folder = here::here(),
    recursive = FALSE,
    reject_regex = NULL,
    include_pathnames = TRUE,
    reject_pathnames_regex = NULL,
    ignore_duplicate_filenames = FALSE,
    output_form = c("doc_paths","doc_text","matching_pages","text_lines"),
    reread = FALSE
){

  # output_form <- checkmate::assert_character(.return[[1]], null.ok = FALSE)

  locn <- here::here(search_folder,'pdf_search_txt.rds')

  if( reread && file.exists(locn) ){ file.remove(locn) }


  # read or save a series of pdf docs ----------------------------------------
  if(!file.exists(locn)){

    pdf_file_names <- here::here(search_folder) |> list.files(pattern = '(?i)\\.pdf$', full.names = TRUE, recursive = recursive)
    pdf_file_names_short <- stringr::str_sub(pdf_file_names, nchar(here::here())+2)
    pdf_file_names <- pdf_file_names |> rlang::set_names(pdf_file_names_short)

    cat('\nReading & saving the contents of:', length(pdf_file_names), 'PDF files.\n')

    future::plan("future::multisession")
    safe_pdf_txt <- purrr::possibly(pdftools::pdf_text)
    pdf_contents <- pdf_file_names |> furrr::future_map(safe_pdf_txt, .progress = TRUE)
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

  if(!is.null(reject_pathnames_regex)){

    reject_these <- content_names[stringr::str_detect(content_names, reject_pathnames_regex)]
    content_names <- setdiff(content_names, reject_these)

    cat(paste0('\n - Excluded ', length(reject_these),' for a location-name with "',reject_pathnames_regex, '" \n'))
    cat(paste0('\n - returning: ', length(content_names), '\n'))

  }

  if(ignore_duplicate_filenames){
    initial_length <- length(content_names)
    content_names <- content_names[!duplicated(basename(content_names))]
    post_length <- length(content_names)
    cat(paste0('\n - Excluded ', initial_length - post_length,' files for duplicated filename(s) \n'))
  }

  cat('\n')
  # return
  output_form <- match.arg(output_form[[1]], c("doc_paths","doc_text","matching_pages","text_lines"), several.ok = FALSE)
  if(output_form == "doc_paths"){
    out <- content_names
  } else if(output_form == "text_lines"){
    page_selections <- pdf_contents[content_names] |> purrr::map(\(.) which_pages_match_regexes(.,search_regexes))
    page_subsets <- page_selections[lengths(page_selections)>0]
    lines_to_search <- pdf_contents[names(page_subsets)] |>
      purrr::map2(page_subsets, \(.x,.y).x[.y]) |>
      purrr::map(\(.)paste(.,collapse = '\\n')) |>
      purrr::map(\(.)stringr::str_split_1(.,'\\n'))
    get_these <- search_regexes |> purrr::map(\(.) detect_lines(lines_to_search,.)) |>
      purrr::list_transpose() |> purrr::map(purrr::reduce,`|`)
    out <- purrr::map2(lines_to_search, get_these, \(.x,.y).x[.y])
  } else if(output_form == "matching_pages"){
    out <- pdf_contents[content_names] |> purrr::map(\(.) which_pages_match_regexes(.,search_regexes))
  } else if(output_form == "doc_text"){
    out <- pdf_contents[content_names]
  }

  return(out)

}


