library(rvest)
library(tidyverse)

### creating draft board from master csv file (used in 2027)

d27 <- read.csv('data/draft27.csv')
teams <- sort(unique(d27$orig_team))
for (i in 1:nrow(d27)){
  if (!d27$current_team[i] %in% teams){
    d27$current_team[i] = d27$orig_team[i]
  }
}
d27$Player[which(is.na(d27$Player))] <- ''
d27$Overall = 1:nrow(d27)
rosters <- read.csv('data/rosters27.csv')
d27 <- left_join(d27,rosters |> select(Team,Full_team),by = c('current_team'='Team'))
d27 <- d27 |> select(Pick,orig_team,current_team,Full_team,Player,overall)
colnames(d27) <- c('Pick','orig_team','TM','Team','Player','Overall')
write.csv(d27,file='data/matrix.csv',row.names = F)

# hitters

create_hitters <- function(){
  library(rvest)
  library(tidyverse)
  hit_tables <- read_html('data prep/Batter_Profiles2.html') %>% html_elements("table") %>% html_table()
  hit <- hit_tables[[1]]
  colnames(hit) <- as.vector(hit[2,])
  hit$Team[which(hit$Team=='&nbsp')] <- 'avail'
  hit$Team[which(hit$Team=='')] <- 'avail'
  hit <- hit[3:nrow(hit),]
  hit2 <- hit_tables[[2]]
  colnames(hit2) <- as.vector(hit2[2,])
  hit2 <- hit2[3:nrow(hit2),]
  hit <- cbind(hit,hit2 |> select(PA,OPS,RC27))
  
  field = hit_tables[[7]]
  colnames(field)<-as.vector(field[3,]) |> unlist()
  field<-field[-c(1,2,3),]
  field[field=="&nbsp"]<-""
  field$pos_elig=""
  for (i in 1:nrow(field)){
    v=c()
    for (j in 5:12){
      if (field[i,j]!=""){v=c(v,j-3)}
    }
    field$pos_elig[i]=paste(v,collapse="")
  }
  field <- field[,5:ncol(field)]
  
  hitters <- cbind(hit,field)
  hitters <- hitters |> select(Name,Team,P,Age,PA,AB,AVG,OBP,SPC,OPS,RC27,everything())
  hitters <- hitters %>%
    mutate(across(everything(), ~ str_replace_all(.x, "&nbsp", '')))
  hitters <- hitters %>% mutate(across(4:23, as.numeric))
  hitters <- hitters |> rename(Pos = P, all_pos = pos_elig)
  return(hitters)
}



create_pitchers <- function(){
  library(rvest)
  library(tidyverse)
  pit_tables <- read_html('data prep/Pitcher_Profiles2.html') %>% html_elements("table") %>% html_table()
  pit <- pit_tables[[1]]
  colnames(pit) <- as.vector(pit[2,])
  pit$Team[which(pit$Team=='&nbsp')] <- 'avail'
  pit$Team[which(pit$Team=='')] <- 'avail'
  pit <- pit[3:nrow(pit),]

  pit2 <- pit_tables[[3]]
  colnames(pit2) <- as.vector(pit2[2,])
  pit2 <- pit2[3:nrow(pit2),]

  pit <- cbind(pit,pit2 |> select(OPS,RC27))
  
  ratings <- pit_tables[[6]]
  colnames(ratings) <- as.vector(ratings[3,])
  ratings <- ratings[4:nrow(ratings),13:14]
  
  pitchers <- cbind(pit, ratings) |> 
    select(Name,P,St,Rel,Age,G,GS,ERA,OPS,RC27,everything()) |> 
    relocate(Team, .after = last_col())
  
  pitchers <- pitchers %>% mutate(across(5:25, as.numeric))
  
  return(pitchers)
}

hit <- create_hitters()
write.csv(hit,file='data/hitters.csv',row.names = F)
#save(hit_draft, file = 'rdata/hit_draft.rda')

pit <- create_pitchers()
write.csv(pit,file='data/pitchers.csv',row.names = F)
#save(pit_draft, file = 'rdata/pit_draft.rda')







### creating draft board from .html draft reports in DMB (used in 2026)
draft_round <- function(file){
  url <- paste0('html/',file)
  d1 <- (read_html(url) %>% html_elements("table") %>% html_table())[[1]]
  colnames(d1) = c('round','pick','orig_team','dummy', 'current_team', 'player','overall')
  d1 <- d1[3:nrow(d1),] |> select(-dummy)
  for (i in 1:nrow(d1)){
    if (d1$current_team[i] == "&nbsp"){
      d1$current_team[i] = d1$orig_team[i]}
  }
  d1$player = ''
  d1$round = as.integer(d1$round)
  d1$pick = as.integer(d1$pick)
  d1$overall = as.integer(d1$overall)
  return(d1)
}

create_draft <- function(){
  d1 <- draft_round('Player_draft1.html')
  d2 <- draft_round('Player_draft2.html')
  d2$round <- d2$round+1
  d2$overall <- d2$overall+24
  d3 <- draft_round('Player_draft3.html')
  d3$round = d3$round + 2
  d3$overall = d3$overall + 48
  draft <- rbind(d1,d2,d3)
  
  ## formatting for interactive draft central table
  pick_id = c()
  for (i in 1:nrow(draft)){
    pick_id = c(pick_id,paste0(as.character(draft$round[i]),
                               ifelse(draft$pick[i] < 10,".0","."),
                               as.character(draft$pick[i])
    )
    )
  }
  draft$pick_id = pick_id
  
  draft <- draft |> select(pick_id, orig_team, current_team, player, overall)
  colnames(draft) <- c('pick','orig_team','current_team','player','overall')
  
  return(draft)
}
