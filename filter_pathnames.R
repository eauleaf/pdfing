#' filter directory names by regular expressions
#'
#' (used/called inside search_pdf_txt)
#'
#' @param search_regexes vector of regexes to filter down the paths you are searching
#' @param paths character vector of directory paths
#' @param reject_regex single regex string to reject any filenames that match the regex (broaden with regex 'or' pipes '|')
#'
#' @return path names of the pdf documents that satisfy your regexes
#' @examples
#'   regexes <- c('(?i)wire', '\\.pdf')
#'   filter_pathnames(regexes, list.files(here::here(), recursive = T))
#'
filter_pathnames <- function(search_regexes, paths, reject_regex = NULL) {

  cat('\nDocument pathnames searched:', length(paths), '\n')

  .content_names <- paths

  for (search_regex in search_regexes) {

    .content_names <- .content_names[stringr::str_detect(.content_names, search_regex)]

    cat(paste0('\n - regex "', search_regex, '" returned: ', length(.content_names), '\n'))

  }

  if(!is.null(reject_regex)) {

    tmp_len <- length(.content_names)
    .content_names <- .content_names[stringr::str_detect(.content_names, reject_regex, negate = T)]
    cat(paste0('\n - exclusion regex "', reject_regex, '" rejected: ', tmp_len - length(.content_names), '\n'))

  }


  # cat('\n')
  cat(paste0('\n - returning: ', length(.content_names), '\n'))
  return(.content_names)

}

# content_names <- setdiff(content_names, reject_names)
