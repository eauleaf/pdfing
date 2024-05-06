pdf_from_jpg <- function(path_jpg, ..., path_pdf = NULL){

  if( Sys.info()[1] != 'Linux'){
    stop('Function is only implemented on Linux.')
  }

  if(!stringr::str_detect(path_jpg, pattern = '(?i)\\.(jpg)|(jpeg)|(jpe)|(jif)|(jfif)$')[1]){
    stop('\n\nThe file is not one of ( .jpg , .jpeg , .jpe , .jif , .jfif ); aborting.')
  }

  path_jpg <- here::here(path_jpg)[1]

  if(is.null(path_pdf)){
    path_pdf <- stringr::str_replace(path_jpg, '(?i)\\.jpg$','.pdf')
  } else {
    path_pdf <- here::here(path_pdf)[1]
  }


  sys_args <- c(glue::glue("'{unlist(list(...))}'"), glue::glue("'{path_jpg}'"), glue::glue("> '{path_pdf}'"))
  message('Passing arguments:')
  purrr::map(sys_args, ~cat(.,'\n'))
  system2('cupsfilter', args = sys_args)

  path_pdf

}

