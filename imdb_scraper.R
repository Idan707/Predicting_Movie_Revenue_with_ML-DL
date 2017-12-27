####################
# Import libraries
####################
library(rvest)
library(XML)
library(xml2)
library(stringr)
library(data.table)

####################
# Start scrape IMDB
####################

imdb_scraper <- function(url_content){

  #Rankings 
  rank_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-primary')
  rank_data <- html_text(rank_data_html)%>% ifelse(. == "", NA, .)
  rank_data<-as.data.frame(as.numeric(rank_data))
  
  #Title
  title_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.lister-item-header a')
  title_data <- html_text(title_data_html)
  
  #Description 
  description_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.ratings-bar+ .text-muted')
  description_data <- html_text(description_data_html)
  description_data<-gsub("\n","",description_data)
  
  #Movie runtime
  runtime_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted .runtime')
  runtime_data <- html_text(runtime_data_html)
  runtime_data<-gsub(" min","",runtime_data)
  runtime_data<-as.numeric(runtime_data)
  
  #Genre
  genre_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.genre')
  genre_data <- html_text(genre_data_html)
  genre_data<-gsub("\n","",genre_data)
  genre_data<-gsub(" ","",genre_data)
  
  #IMDB rating section
  rating_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.ratings-imdb-rating strong')
  rating_data <- html_text(rating_data_html)
  rating_data<-as.numeric(rating_data)
  
  #Votes section
  votes_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.sort-num_votes-visible span:nth-child(2)')
  votes_data <- html_text(votes_data_html)
  votes_data<-gsub(",","",votes_data)
  votes_data<-as.numeric(votes_data)
  
  #Directors section
  directors_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(1)')
  directors_data <- html_text(directors_data_html)
  directors_data<-as.factor(directors_data)
  
  #Actors section
  actor_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(3)')
  actor_data1 <- html_text(actor_data_html)
  
  actor_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(4)')
  actor_data2 <- html_text(actor_data_html)
  
  actor_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(5)')
  actor_data3 <- html_text(actor_data_html)
  
  
  #Metascore section
  metascore_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.metascore')
  metascore_data <- html_text(metascore_data_html)
  metascore_data <- gsub(" ","",metascore_data)
  metascore_data <- as.numeric(metascore_data)
  
  #Gross section
  gross_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.ghost~ .text-muted+ span')
  gross_data <- html_text(gross_data_html)
  gross_data<-as.numeric(gsub("[$,\\M]","",gross_data))
  
  
  #Combining all the lists to form a data frame
  movies_df<-data.frame(Rank = rank_data, Title = title_data,
                        
                        Description = description_data, Runtime = runtime_data,
                        
                        Genre = genre_data, Rating = rating_data,
                        
                        Metascore = metascore_data, Votes = votes_data,
                        
                        Director = directors_data, Actor_1 = actor_data1,
                        
                        Actor_2 = actor_data2, Actor_3 = actor_data3,
                        
                        Gross_Rev = gross_data)
  return(movies_df)
}

#Pick number of pages to go over

df = list()
for (i in 1:3){
  urlImdb <- paste('http://www.imdb.com/search/title?year=2017&title_type=feature&sort=moviemeter,asc&page=',i, sep = "")
  content <- read_html(urlImdb)
  imdb_data <- imdb_scraper(content)
  df[[i]] <- imdb_data
  #Pause between loops to avoid blockage
  Sys.sleep(3)
}

#Bind to one df
df_imdb = rbindlist(df)
