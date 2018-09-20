library(rvest)
library(stringr) 
library(tidyr)
library(dplyr)

page <- read_html("https://en.wikipedia.org/wiki/Friends_(season_1)")

tmp <- page %>% html_nodes(".description") %>%      
  html_text() %>% 
  # Trim additional white space
  str_trim() %>%                       
  # Convert the list into a vector
  unlist() 
df <- data.frame(season = 1, summary = tmp)

n.ep <- dim(df)[1]
df$ep <- 0
df$link <- ""
charac <- page %>% html_nodes(".vevent")

for (ii in seq(n.ep)){
  
  df$ep[ii] <- charac[ii+1] %>% html_nodes("td") %>% .[1] %>%
    html_text() %>% as.numeric()
  aa <-  charac[ii+1] %>% html_nodes("td") %>% .[2] %>%
    html_nodes("a")
  if (length(aa)>0) {
    df$link[ii] <- aa %>% .[1] %>% html_attr("href")   
  }
}

for (seas in seq(2,10)) {
  url_ <- paste("https://en.wikipedia.org/wiki/Friends_(season_", seas, ")", sep="")
  page <- read_html(url_)

  tmp <- page %>% html_nodes(".description") %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist() 
  df.plus <- data.frame(season = seas, summary = tmp)
  
  n.ep <- dim(df.plus)[1]
  df.plus$ep <- 0
  df.plus$link <- ""
  charac <- page %>% html_nodes(".vevent")
  
  for (ii in seq(n.ep)){
    
    df.plus$ep[ii] <- charac[ii+1] %>% html_nodes("td") %>% .[1] %>%
      html_text() %>% as.numeric()
    aa <-  charac[ii+1] %>% html_nodes("td") %>% .[2] %>%
      html_nodes("a")
    if (length(aa)>0) {
      df.plus$link[ii] <- aa %>% .[1] %>% html_attr("href")   
    }
  }
  
  df <- bind_rows(df, df.plus)
}
