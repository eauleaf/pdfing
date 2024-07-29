#' Build a new pdf from a subset of pdf pages selected from other pdf's by regular expressions
#'
#' @param pdf_path a path to a pdf or vector of pdf paths
#' @param search_regexes vector of regular expressions to filter the pdf pages you're searching through
#' @param reject_regex single regex to reject any returned pages that match
#' @param save_path the location and name of the new pdf (if none provided, saves to a tmp directory)
#' @param remove_whitespace remove whitespace from the returned text
#' @param open_pdf open the new pdf after compiling document
#' @param stor_txt questioning... creates a rds file of the text for fast successive searches
#' @param sleep_time time to wait before opening compiled pdf
#'
#' @return a tibble of the pdfs and pages selected
#' @export
#'
#' @examples \dontrun{
#' pdf_abridge(doc,search_regexes = c('(?i)LIMITATION OF COV.*','(?i)POLICY NUM'))
#' }
pdf_abridge <- function(pdf_path,
                        search_regexes = NULL,
                        reject_regex = NULL,
                        save_path = NULL,
                        remove_whitespace = FALSE,
                        open_pdf = TRUE,
                        stor_txt = FALSE,
                        sleep_time = 1
){

  line_text <- NULL

  # create savename
  mk_tempfile <- is.null(save_path)

  # if path not specified, make temporary savename
  # if specified, make sure savename has extension '.pdf'
  # otherwise, make sure the given save path is fully specified or force it into a working locn
  pdf_name <- stringr::str_remove(string = basename(pdf_path), pattern = "(?i)\\.pdf$")
  if (mk_tempfile){
    save_path <- tempfile(pattern = paste0(pdf_name,'-tmp_'), fileext = '.pdf')
  } else if (!stringr::str_detect(save_path, '(?i)\\.pdf$')) {
    save_path <- paste0(here::here(save_path),'.pdf')
  } else {
    save_path <- here::here(save_path)
  }


  # Read pdf
  rds_path <- stringr::str_replace(pdf_path, '(?i)\\.pdf$' ,'_txt.rds')
  if (file.exists(rds_path)) {
    txt <- readr::read_rds(rds_path)
  } else {
    txt <- pdftools::pdf_text(pdf_path) |> purrr::map(readr::read_lines)
    if(stor_txt){readr::write_rds(txt, rds_path)}
  }

  # initialize
  remaining_pages <- 1:length(txt)

  # exclude pages -----------------------------------------------------------
  if(!is.null(reject_regex)){

    rejected_pages <- txt[remaining_pages] |>
      purrr::map(\(.) stringr::str_detect(.,reject_regex)) |>
      purrr::map(any) |>
      purrr::simplify() |>
      which()

    remaining_pages <- setdiff(remaining_pages, rejected_pages)

    cat(paste0('\n Excluded ', length(rejected_pages),' from exlusion regex: "', reject_regex, '"\n'))

  }




  # subset by regular expressions -------------------------------------------

  for(pattern in search_regexes){

    cat('\n Pattern: ' ,pattern, '\n')

    search_results <- txt[remaining_pages] |>
      purrr::map(\(.) stringr::str_extract(., pattern)) |>
      rlang::set_names(remaining_pages) |>
      purrr::map(\(.) purrr::discard(.,is.na)) |>
      purrr::compact() |>
      purrr::map(\(.) .[1]) |>
      purrr::simplify() |>
      as.data.frame() |>
      tibble::rownames_to_column()

    cat('\n Results:  ')
    if( nrow(search_results) > 0 ){

      search_results <- search_results |> tibble::tibble() |>
        rlang::set_names(c('page_number','line_text'))
      remaining_pages <- search_results |> dplyr::pull(1) |> as.numeric()
      if(remove_whitespace){ search_results <- dplyr::mutate(search_results, line_text = stringr::str_squish(line_text)) }
      print(search_results)

    } else {

      cat('No Results \n')
      return(tibble::tibble())

    }


  }


  # write the pdf with the subset pages
  pdftools::pdf_subset(
    pages = remaining_pages,
    input = pdf_path,
    output = save_path
  )

  cat('\n\n PDF Savepath: ', save_path,'\n\n')

  if(open_pdf){ xlr::sys_open(save_path) }
  if(mk_tempfile){ later::later(~unlink(save_path), delay = 10) }


  return(search_results)

}

