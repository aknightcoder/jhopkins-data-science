Question1 <- function()
{
  library(httr)

  library(rjson)
  
  oauth_endpoints("github")

  myapp <- oauth_app("github",
                   key = "c9b195cfc16257863ff1",
                   secret = "f885e6a66e60d7f4aa7c67b615defaaca578b35e")

  github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

  gtoken <- config(token = github_token)

  req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

  stop_for_status(req)

  df <- jsonlite::fromJSON(toJSON(content(req)))
  
  df <- df[df$name == 'datasharing',]
  
  creation_dt <- df$created_at
}

Question2 <- function()
{
  
}
