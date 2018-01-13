####################
# Import library
####################
library(rvest)
library(XML)
library(xml2)
library(stringr)
library(data.table)
library(tidyverse)

####################
# Start scrape IMDB
####################

#Main Df imdb scraper
imdb_scraper <- function(url_content){
  #Get imdb url ; Return data frame with main movie features
  
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
  directors_nm_code_html <- html_nodes(content,'.text-muted+ p') %>% html_node('a') %>% html_attr('href')
  directors_nm_code_data <- unlist(regmatches(directors_nm_code_html,regexec('nm\\d+',directors_nm_code_html)))
  
  #Actors section
  actor_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(3)')
  actor_data1 <- html_text(actor_data_html)
  actor_nm_code_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(3)') %>% html_attr('href')
  actor_nm_code_1 <- gsub(" ","",regmatches(actor_nm_code_html,regexec('nm\\d+',actor_nm_code_html)))
  actor_nm_code_1 <- unlist(lapply(actor_nm_code_1,function(x) if(identical(x,"character(0)")) NA else x))
  
  actor_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(4)')
  actor_data2 <- html_text(actor_data_html)
  actor_nm_code_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(4)') %>% html_attr('href')
  actor_nm_code_2 <- gsub(" ","",regmatches(actor_nm_code_html,regexec('nm\\d+',actor_nm_code_html)))
  actor_nm_code_2 <- unlist(lapply(actor_nm_code_2,function(x) if(identical(x,"character(0)")) NA else x))
  
  actor_data_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(5)')
  actor_data3 <- html_text(actor_data_html)
  actor_nm_code_html <- html_nodes(content,'div.lister-item-content') %>% html_node('.text-muted+ p a:nth-child(5)') %>% html_attr('href')
  actor_nm_code_3 <- gsub(" ","",regmatches(actor_nm_code_html,regexec('nm\\d+',actor_nm_code_html)))
  actor_nm_code_3 <- unlist(lapply(actor_nm_code_3,function(x) if(identical(x,"character(0)")) NA else x))
  
  
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
                        
                        Director = directors_data, Director_IMDB_code = directors_nm_code_data,
                        
                        Actor_1 = actor_data1, Actor_1_IMDB_code = actor_nm_code_1,
                        
                        Actor_2 = actor_data2, Actor_2_IMDB_code = actor_nm_code_2,
                        
                        Actor_3 = actor_data3, Actor_3_IMDB_code = actor_nm_code_3,
                        
                        Gross_Rev = gross_data)
  return(movies_df)
}

#Pick # of pages to scrape and run imdb scraper function on them
df = list()
for (i in 1:1){
  urlImdb <- paste('http://www.imdb.com/search/title?year=2017&title_type=feature&sort=moviemeter,asc&page=',i, sep = "")
  content <- read_html(urlImdb)
  imdb_data <- imdb_scraper(content)
  df[[i]] <- imdb_data
  #Pause between loops to avoid imdb blockage
  Sys.sleep(3)
}

#Bind the data frames from every imdb pages to 1 data frame
df_imdb = rbindlist(df)

###########################
# Scrape directors numbers
###########################
#Baesd on previous data frame take director names and scrape there movies info

#imdb_director_numbers function
imdb_director_numbers <- function(director_nm_code){
  #Get director imdb code (part of director profile page in imdb) ; Return director main features
  
  if (is.na(director_nm_code)){
    
    director_movie_num = 0
    director_movie_written_num = 0
    director_movie_edited_num = 0
    return(c(director_movie_num, director_movie_written_num, director_movie_edited_num))
  }
    else{
    urlImdb <- paste('http://www.imdb.com/name/',director_nm_code, sep = "")
    content <- read_html(urlImdb)
    
    #Number of movies by director
    director_movie_num_html <- html_nodes(content,'#filmo-head-director')
    director_movie_num <- html_text(director_movie_num_html)
    director_movie_num <- unlist(regmatches(director_movie_num,regexec('\\d+',director_movie_num)))
    director_movie_num <- ifelse(is.null(director_movie_num), 0, as.numeric(director_movie_num))
    
    #Number of movies written by the director
    director_movie_num_html <- html_nodes(content,'#filmo-head-writer')
    director_movie_written_num <- html_text(director_movie_num_html)
    director_movie_written_num <- unlist(regmatches(director_movie_written_num,regexec('\\d+',director_movie_written_num)))
    director_movie_written_num <- ifelse(is.null(director_movie_written_num), 0,  as.numeric(director_movie_written_num))
    
    #Number of movies edited by the director
    director_movie_num_html <- html_nodes(content,'#filmo-head-editor')
    director_movie_edited_num <- html_text(director_movie_num_html)
    director_movie_edited_num <- unlist(regmatches(director_movie_edited_num,regexec('\\d+',director_movie_edited_num)))
    director_movie_edited_num <- ifelse(is.null(director_movie_edited_num), 0, as.numeric(director_movie_edited_num))
    
    #Pause between loops to avoid imdb blockage
    Sys.sleep(1)
    return(c(director_movie_num, director_movie_written_num, director_movie_edited_num))
  }
  
}

#Scrap director numbers and bind to main data frame
df_imdb$temp <- lapply(df_imdb$Director_IMDB_code, imdb_director_numbers)
df_imdb[,c('director_num_of_movies', 'director_num_of_written', 'director_num_of_edited')] <- as.data.frame(do.call(rbind, df_imdb$temp))
df_imdb <- select(df_imdb, -temp)


#######################
# Scrape actors numbers
#######################
#Baesd on main data frame take actor names and scrape there movies info

#imdb_actors_numbers function
imdb_actor_numbers <- function(actor_nm_code){
  #Get director imdb code (part of director profile page in imdb) ; Return director main features
  
  if (is.na(actor_nm_code)){
    
    actor_movie_num = 0
    actor_movie_written_num = 0
    actor_movie_self_num = 0
    actor_movie_produce_num = 0
    return(c(actor_movie_num, actor_movie_written_num, actor_movie_self_num, actor_movie_produce_num))
  }
  else{
    urlImdb <- paste('http://www.imdb.com/name/',actor_nm_code, sep = "")
    content <- read_html(urlImdb)
    
    #Number of movies by actor
    actor_movie_num_html <- html_nodes(content,'#filmo-head-actor')
    actor_movie_num <- html_text(actor_movie_num_html)
    actor_movie_num <- unlist(regmatches(actor_movie_num,regexec('\\d+',actor_movie_num)))
    if (is.null(actor_movie_num)){
      actor_movie_num_html <- html_nodes(content,'#filmo-head-actress')
      actor_movie_num <- html_text(actor_movie_num_html)
      actor_movie_num <- as.numeric(unlist(regmatches(actor_movie_num,regexec('\\d+',actor_movie_num))))
    }
    actor_movie_num <- ifelse(is.null(actor_movie_num), 0, as.numeric(actor_movie_num))
    
    #Number of movies written by the actor
    actor_movie_num_html <- html_nodes(content,'#filmo-head-writer')
    actor_movie_written_num <- html_text(actor_movie_num_html)
    actor_movie_written_num <- unlist(regmatches(actor_movie_written_num,regexec('\\d+',actor_movie_written_num)))
    actor_movie_written_num <- ifelse(is.null(actor_movie_written_num), 0, as.numeric(actor_movie_written_num))
    
    #Number of times actor play itself
    actor_movie_num_html <- html_nodes(content,'#filmo-head-self')
    actor_movie_self_num <- html_text(actor_movie_num_html)
    actor_movie_self_num <- unlist(regmatches(actor_movie_self_num,regexec('\\d+',actor_movie_self_num)))
    actor_movie_self_num <- ifelse(is.null(actor_movie_self_num), 0, as.numeric(actor_movie_self_num))
    
    #Number of movies actor produce
    actor_movie_num_html <- html_nodes(content,'#filmo-head-producer')
    actor_movie_produce_num <- html_text(actor_movie_num_html)
    actor_movie_produce_num <- unlist(regmatches(actor_movie_produce_num,regexec('\\d+',actor_movie_produce_num)))
    actor_movie_produce_num <- ifelse(is.null(actor_movie_produce_num), 0, as.numeric(actor_movie_produce_num))
    
    #Pause between loops to avoid imdb blockage
    Sys.sleep(1)
    return(c(actor_movie_num, actor_movie_written_num, actor_movie_self_num, actor_movie_produce_num))
  }
  
}

###############
##Scrap actor1 
##############
#Scrap actor1 numbers and bind to main data frame
df_imdb$temp <- lapply(df_imdb$Actor_1_IMDB_code, imdb_actor_numbers)
df_imdb[,c('actor_1_movie_num', 'actor_1_movie_written_num', 'actor_1_movie_self_num', 'actor_1_movie_produce_num')] <- 
          as.data.frame(do.call(rbind, df_imdb$temp))

df_imdb <- select(df_imdb, -temp)

###############
##Scrap actor2 
##############
#Scrap actor2 numbers and bind to main data frame
df_imdb$temp <- lapply(df_imdb$Actor_2_IMDB_code, imdb_actor_numbers)
df_imdb[,c('actor_2_movie_num', 'actor_2_movie_written_num', 'actor_2_movie_self_num', 'actor_2_movie_produce_num')] <- 
  as.data.frame(do.call(rbind, df_imdb$temp))

df_imdb <- select(df_imdb, -temp)

###############
##Scrap actor3 
##############
#Scrap actor3 numbers and bind to main data frame
df_imdb$temp <- lapply(df_imdb$Actor_3_IMDB_code, imdb_actor_numbers)
df_imdb[,c('actor_3_movie_num', 'actor_3_movie_written_num', 'actor_3_movie_self_num', 'actor_3_movie_produce_num')] <- 
  as.data.frame(do.call(rbind, df_imdb$temp))

df_imdb <- select(df_imdb, -temp)


