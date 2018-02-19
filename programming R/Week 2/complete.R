complete <- function(directory, id=1:332)
{
  files = list.files(directory, full.names = TRUE)

  ids <- integer()
  cases <- integer()
  idx = 1
  for(counter in id)
  {
    dat <- read.csv(files[counter])
    ids[idx] = dat[1,"ID"]
    cases[idx] = nrow(dat[complete.cases(dat),])
    idx <- idx + 1
  }
  df <- data.frame(id=ids, nobs=cases, stringsAsFactors = FALSE)
}