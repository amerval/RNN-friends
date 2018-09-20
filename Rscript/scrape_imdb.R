
page <- read_html("https://www.imdb.com/title/tt0108778/episodes?season=1")

tmp <- page %>% html_nodes(".item_description") %>%      
  html_text() %>% 
  # Trim additional white space
  str_trim() %>%                       
  # Convert the list into a vector
  unlist() 
df.imdb <- data.frame(season = 1, summary = tmp)

all.a <- page %>% html_nodes("strong a")

df.imdb$ep <- 0
df.imdbf$link <- ""
df.imdb$title <- ""
no.ep <- 0
for (ii in seq(length(all.a))) {
  tt <- all.a[ii] %>% html_attr("title")
  if (!is.na(tt)) {
    no.ep <- no.ep +1
    df.imdb$ep <- no.ep
    df.imdb$title[no.ep] <- tt
    df.imdb$link[no.ep] <- all.a[ii] %>% html_attr("href")
  }
}


for (seas in seq(2,10)) {
  url_ <- paste("https://www.imdb.com/title/tt0108778/episodes?season=", seas, sep="")
  page <- read_html(url_)

  
  tmp <- page %>% html_nodes(".item_description") %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist() 
  df.imdb.plus <- data.frame(season = seas, summary = tmp)
  
  all.a <- page %>% html_nodes("strong a")
  
  df.imdb.plus$ep <- 0
  df.imdb.plus$link <- ""
  df.imdb.plus$title <- ""
  no.ep <- 0
  for (ii in seq(length(all.a))) {
    tt <- all.a[ii] %>% html_attr("title")
    if (!is.na(tt)) {
      no.ep <- no.ep +1
      df.imdb.plus$ep <- no.ep
      df.imdb.plus$title[no.ep] <- tt
      df.imdb.plus$link[no.ep] <- all.a[ii] %>% html_attr("href")
    }
  }
  df.imdb <- bind_rows(df.imdb, df.imdb.plus)
}
