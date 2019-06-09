#remotes::install_github("kwb-r/kwb.logger@dev")

if (FALSE)
{
  timestamp_list <- kwb.logger::getExampleTimestamps()

  invisible(lapply(timestamp_list, function(x) {
    h <- head(x); writeLines(h); cat("\n")
  }))

  templates <- sapply(timestamp_list, "[", 1)

  formats <- kwb.datetime::matchingTimeFormat(templates, method = 2)
  
  data.frame(template = templates, format = formats)
}
