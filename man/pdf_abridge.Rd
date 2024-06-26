% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_abridge.R
\name{pdf_abridge}
\alias{pdf_abridge}
\title{Subset pdf pages via regular expressions}
\usage{
pdf_abridge(
  pdf_path,
  patterns = NULL,
  exclude_pattern = NULL,
  save_path = NULL,
  remove_whitespace = FALSE,
  open_pdf = TRUE,
  stor_txt = TRUE,
  sleep_time = 1
)
}
\arguments{
\item{sleep_time}{}
}
\description{
Subset pdf pages via regular expressions
}
\examples{
\dontrun{
abridge_pdf(doc,patterns = c('(?i)LIMITATION OF COV.*','(?i)POLICY NUM'))
}
}
