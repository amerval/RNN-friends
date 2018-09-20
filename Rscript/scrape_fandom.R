

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


url_ <- paste("http://friends.wikia.com/", link[2], sep="")
url_ <- gsub('%26', 'And', url_)
page <- read_html(url_)

tt <- page %>% html_nodes('p')
start_ <- FALSE 
stop_ <- FALSE
ind <- 1
summ_ <- ""
while (!stop_) {
  ind <- ind +1
  
  print(ind)
  test_ <- length(html_nodes(tt[ind], 'b'))
  if (test_>0 & !start_) {start_ <- TRUE; print('start')}
  test_ <- ( html_attr(html_nodes(tt[ind], "b a"), 'href') == "/wiki/Jennifer_Aniston")
  if (length(test_)>0) {
    if (test_[1]) {
      start_ <- FALSE;
      print('stop') 
      stop_<-TRUE}  
  }
}
