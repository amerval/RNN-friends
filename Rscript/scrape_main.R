library(rvest)
library(stringr) 
library(tidyr)
library(dplyr)

## 1. scrape wikipedia ####

page <- read_html("https://en.wikipedia.org/wiki/Friends_(season_1)")

tmp <- page %>% html_nodes(".description") %>%      
  html_text() %>% 
  # Trim additional white space
  str_trim() %>%                       
  # Convert the list into a vector
  unlist() 
df <- data.frame(source="wikipedia", season = 1, summary = tmp)

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
  df.plus <- data.frame(source="wikipedia", season = seas, summary = tmp)
  
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

df.full <- df
## 2. scrape imdb ####


page <- read_html("https://www.imdb.com/title/tt0108778/episodes?season=1")

tmp <- page %>% html_nodes(".item_description") %>%      
  html_text() %>% 
  # Trim additional white space
  str_trim() %>%                       
  # Convert the list into a vector
  unlist() 
df.imdb <- data.frame(source = "imdb", season = 1, summary = tmp)

all.a <- page %>% html_nodes("strong a")

df.imdb$ep <- 0
df.imdb$link <- ""
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
  df.imdb.plus <- data.frame(source = "imdb", season = seas, summary = tmp)
  
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

df.full <- bind_rows(df.full, df.imdb)

## 3. scrape fandom ####


df.season <- data.frame(source = 'fandom', season = seq(10), summary="")
df.season$summary <- as.character(df.season$summary)
for (seas in seq(10)) {
  url_ <- paste("http://friends.wikia.com/wiki/Season_", seas, sep="")
  page <- read_html(url_)
  
  tt <- page %>% html_nodes('p')
  summ_ <- ""
  for (ll in seq(2,length(tt))) {
    test_ <- length(html_nodes(tt[ll], 'b')) 
    if (test_ == 0 & (!grepl(' of 2.', html_text(tt[ll])))) {
      summ_ <- paste(summ_, html_text(tt[ll]))
    }
  }
  df.season$summary[seas] <- summ_
  
  nodes <- page %>% html_nodes('td') 
  
  col_ <- page %>% html_nodes('tr td')  
  un <- (html_attr(col_, "rowspan")==2)
  un[is.na(un)] <- FALSE
  style_ <- col_[un][1] %>% html_attr("style")
  
  un <- (html_attr(nodes, "style")==style_) & (html_attr(nodes, "bgcolor")=="#FFFFFF")
  un[is.na(un)] <- FALSE
  
  n.ep <- sum(un)
  
  summary <- nodes[un] %>% html_text() 
  
  un <- (html_attr(nodes, "bgcolor")=="#FFFFFF")
  un[is.na(un)] <- FALSE
  link <- nodes[un] %>% html_nodes('b a') %>% html_attr("href")
  
  synopsis <- rep("", n.ep)
  for (ep in seq(length(link))) {
    url_ <- paste("http://friends.wikia.com/", link[ep], sep="")
    url_ <- gsub('%26', 'And', url_)
    page <- read_html(url_)
    
    tt <- page %>% html_nodes('p')
    start_ <- FALSE 
    stop_ <- FALSE
    ind <- 1
    summ_ <- ""
    while (!stop_) {
      ind <- ind +1
      test_ <- length(html_nodes(tt[ind], 'b'))
      if (test_>0 & !start_) {start_ <- TRUE}
      test_ <- ( html_attr(html_nodes(tt[ind], "b a"), 'href') == "/wiki/Jennifer_Aniston")
      if (length(test_)>0) {
        if (test_[1]) {
          start_ <- FALSE; 
          stop_<-TRUE}  
      }
      
      
      if (start_){ summ_ <- paste(summ_, html_text(tt[ind])) }
      if (ind == length(tt)){stop_<-TRUE}
    }
    synopsis[ep] <-  summ_
    
  }
  
  
  df.plus <- data.frame(source = 'fandom', season=seas, ep=seq(n.ep), summary=summary, link=link, synopsis=synopsis )  
  if (seas == 1) {df <- df.plus} else {
    df <- bind_rows(df, df.plus)
  }
}

df.full <- bind_rows(df.full, df)
