#' filter pdf documents by search via regular expressions
#'
#' (used/called inside search_pdf_txt)
#'
#' @param search_regexes vector of regexes to filter down the pdfs you're searching through
#' @param pdf_contents the list of pdfs that have been read as text either by pdftools::pdf_text or ocr
#' @param reject_regex single regex to reject any files that match the regex (broaden with regex 'or' pipes '|')
#'
#' @return path names of the pdf documents that satisfy your regexes
#' @examples  filter_pdf_contents(c('(?i)blue','Green'), contents)
#'
filter_pdf_contents <- function(search_regexes, pdf_contents, reject_regex = NULL) {

  content_names <- names(pdf_contents)
  # print(content_names[1:3])

  cat('\nDocuments searched:', length(content_names), '\n')

  for (search_regex in search_regexes) {

    content_names <- pdf_contents[content_names] %>%
      purrr::map(~stringr::str_detect(.,search_regex)) %>%
      purrr::map(any) %>% purrr::simplify() %>%
      purrr::keep(~.) %>% names()


    cat(paste0('\n - regex "', search_regex, '" returned: ', length(content_names), '\n'))

  }

  if(!is.null(reject_regex)) {

    reject_names <- pdf_contents[content_names] %>%
      purrr::map(~stringr::str_detect(.,reject_regex)) %>%
      purrr::map(any) %>% purrr::simplify() %>%
      purrr::keep(~.) %>% names()

    content_names <- setdiff(content_names, reject_names)

    cat(paste0('\n - exclusion regex "', reject_regex, '" rejected: ', length(reject_names), '\n'))

  }


  cat(paste0('\n - returning: ', length(content_names), '\n'))
  return(content_names)

}
