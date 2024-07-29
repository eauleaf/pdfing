#' filter pdf documents by search via regular expressions
#'
#' (used/called inside pdf_search_txt)
#'
#' @param search_regexes vector of regexes to filter down the pdfs you're searching through
#' @param pdf_contents the list of pdfs that have been read as text either by pdftools::pdf_text or ocr
#' @param reject_regex single regex to reject any files that match the regex (broaden with regex 'or' pipes '|')
#'
#' @return path names of the pdf documents that satisfy your regexes
#' @examples  \dontrun{ filter_pdf_contents(c('(?i)blue','Green'), contents) }
#'
filter_pdf_contents <- function(search_regexes, pdf_contents, reject_regex = NULL) {

  content_names <- names(pdf_contents)
  # print(content_names[1:3])

  cat('\nDocuments searched:', length(content_names), '\n')

  for (search_regex in search_regexes) {

    content_names <- pdf_contents[content_names] |>
      purrr::map(\(.) stringr::str_detect(.,search_regex)) |>
      purrr::map(any) |> purrr::simplify() |>
      purrr::keep(~.) |> names()


    cat(paste0('\n - regex "', search_regex, '" returned: ', length(content_names), '\n'))

  }

  if(!is.null(reject_regex)) {

    reject_names <- pdf_contents[content_names] |>
      purrr::map(~stringr::str_detect(.,reject_regex)) |>
      purrr::map(any) |> purrr::simplify() |>
      purrr::keep(~.) |> names()

    content_names <- setdiff(content_names, reject_names)

    cat(paste0('\n - exclusion regex "', reject_regex, '" rejected: ', length(reject_names), '\n'))

  }


  cat(paste0('\n - returning: ', length(content_names), '\n'))
  return(content_names)

}


# -------------------------------------------------------------
# helper functions used in pdf_search_txt()

# pdf_contents <- readr::read_rds("/MAIN/Backup Docs/pdf_search_txt.rds")
# pages that match a regex (internal function to which_pages_match_regexes)
which_pages_match <- function(a_pdf, regex = 'hi'){
  a_pdf |> purrr::map_lgl(\(.) stringr::str_detect(.,regex)) |> which()
}

# union of various regex search results (parent function of which_pages_match)
which_pages_match_regexes <- function(a_pdf, search_regexes = c('hi','bye')){
  search_regexes |> purrr::map(\(.) which_pages_match(a_pdf, .)) |> purrr::reduce(union)
}


# select lines with the text --------------------------------------------------
# purrr::map(~str_extract_all(.,paste0('.*',regexes,'.*'))) |> purrr::map(simplify) |> purrr::map(trimws)
detect_lines <- function(pdf_txt, regex = "hi") purrr::map(pdf_txt, \(.)stringr::str_detect(.,pattern = regex))

