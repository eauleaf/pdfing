#' insert one PDF into another at a page location
#'
#' @param main_pdf string: the primary pdf that takes the insert
#' @param insert_pdf string: the pdf to insert into main_pdf
#' @param after_page integer: page number (if no entry, inserted pdf will pre-pend)
#' @param output string: output pdf name or path with output pdf name
#' @param remove_main_pages integers: vector of pages from the primary pdf document to remove
#' @param remove_inserted_pages integers: vector of pages from the insert pdf document to remove
#' @param password string: password to the pdfs if there is one
#'
#' @return character path to the output pdf
#' @export
#'
#' @examples /dontrun {pdf_insert('folder/main_document.pdf', 'folder/insert_this.pdf', after_page = 10)}
pdf_insert <- function(main_pdf = '', insert_pdf = '', after_page = 0,
                       remove_main_pages = c(0), remove_inserted_pages = c(0),
                       output = NULL, password = ''){


  after_page <- base::as.numeric(after_page[1])
  remove_main_pages <- base::as.numeric(remove_main_pages)
  remove_inserted_pages <- base::as.numeric(remove_inserted_pages)
  length_main <- pdftools::pdf_length(main_pdf)
  length_insert <- pdftools::pdf_length(insert_pdf)

  path_save <- base::ifelse(
    base::is.null(output),
    stringr::str_replace(main_pdf, '(?i)\\.pdf$','_w_insert.pdf'),
    here::here(output)
  )

  if( after_page < 0 ) base::stop('Insertion point "after_page" ',  after_page, ' must not be negative. Aborting. ')
  if( after_page > length_main ) base::warning('Insertion point "after_page" ',  after_page, ' is larger than ', length_main, ' the length of your pdf. Appending insert.')


  main_pages <- tibble(
    index = 1:length_main
  ) %>%
    mutate(
      delete = index %in% remove_main_pages,
      order = index)

  # print(main_pages)

  insert_pages <- tibble(
    index = (1:length_insert)
  ) %>%
    mutate(
      delete = index %in% remove_inserted_pages,
      index = index + length_main,
      order = base::seq(after_page + 0.1, after_page + 0.9, along.with = index)
    )

  # print(insert_pages)

  reordered_pages <- bind_rows(main = main_pages, insert = insert_pages, .id = 'pdf') %>%
    filter(delete == FALSE) %>%
    arrange(order) %>%
    mutate(new_index = row_number())

  tmp <- pdftools::pdf_combine(input = c(main_pdf, insert_pdf), output = tempfile(), password = password)

  pdftools::pdf_subset(
    input = tmp,
    pages = reordered_pages[['index']],
    output = path_save,
    password = password
  )

  file.remove(tmp)

  new_index_locns <- filter(reordered_pages, pdf == 'insert' )[['new_index']]
  cat(glue::glue('\nThe created pdf is {base::nrow(reordered_pages)} pages long.\n\n'))
  cat(glue::glue('\n\nNewly inserted pages are now at: {paste0(new_index_locns, collapse = ",")}\n\n'))
  cat(glue::glue('\n\nPDF save location is: \n"{path_save}"\n\n\n'))
  path_save

}

