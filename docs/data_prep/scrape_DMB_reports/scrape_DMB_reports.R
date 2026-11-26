# scrape rjml season statistics for the rjmlbaseball website
library(tidyverse)
library(rvest)
adjust_report_colnames <- function(df,n=2){
  colnames(df)<-as.vector(df[n,]) |> unlist()
  df <- df[-c(1:n),]
  return(df)
}
## Scrape Draft Results from three default draft files
#Round 1

scrape_draft_table <- function(year = '23'){
  library(tidyverse)
  library(rvest)
  adjust_report_colnames <- function(df,n=2){
    colnames(df)<-as.vector(df[n,]) |> unlist()
    df <- df[-c(1:n),]
    return(df)
  }
  #get html report file names as saved in DMB
  file1 <- paste0('data_prep/scrape_DMB_reports/drafts/rjml',year,'draft1.html')
  file2 <- paste0('data_prep/scrape_DMB_reports/drafts/rjml',year,'draft2.html')
  file3 <- paste0('data_prep/scrape_DMB_reports/drafts/rjml',year,'draft3.html')
  df1 = (read_html(file1) %>% 
           html_elements("table")%>% 
           html_table())[[1]]
  df1 <- adjust_report_colnames(df1)
  df2 = (read_html(file2) %>% 
           html_elements("table")%>% 
           html_table())[[1]]
  df2 <- adjust_report_colnames(df2)
  df3 = (read_html(file3) %>% 
           html_elements("table")%>% 
           html_table())[[1]]
  df3 <- adjust_report_colnames(df3)
  draft <- rbind(df1,df2,df3)
  draft$Overall = 1:312
  colnames(draft) <- c('Round','Pick',"Orig_Team","junk","Curr_Team","Player","Overall")
  draft <- draft |> select(-junk)
  year_to_remove = paste0('20',year," ")
  draft$Player <- str_replace(draft$Player,year_to_remove,"")
  draft <- draft |> 
    mutate(Orig_Team = toupper(Orig_Team),
           Curr_Team = toupper(Curr_Team))
  draft$Player[which(draft$Player=="")]<-'pass'
  return(draft)
}

# Generate two csv files for each draft; one that lives in data_prep; and
#one wide table that lives in data for html display

# The long form of each draft
# 2022
draft22 <-scrape_draft_table('22')
sort(unique(draft22$Orig_Team))
write.csv(draft22, file = 'data_prep/draft22.csv',row.names = F)  

#2023
draft23 <-scrape_draft_table('23')
write.csv(draft23, file = 'data_prep/draft23.csv',row.names = F)  

#2024
draft24 <- scrape_draft_table('24')
## adhoc for draft24
tm = c('GIG','HIG','THU','POW','CAP','SIL','CEN','RED')
TM = c('GH','HV','TB','PR',"CC",'SV','CK')
for (i in 1:length(tm)){
  draft24$Orig_Team <- str_replace(draft24$Orig_Team,tm[i],TM[i])
  draft24$Curr_Team <- str_replace(draft24$Curr_Team,tm[i],TM[i])
}
write.csv(draft24, file = 'data_prep/draft24.csv',row.names = F)

#2025
draft25 <-scrape_draft_table('25')
sort(unique(draft25$Orig_Team))
write.csv(draft25, file = 'data_prep/draft25.csv',row.names = F)  

#2026 done elsewhere and has different format

## Wide form for 22-25

draft_wide_form <- function(year = '22'){
  file <- paste0('draft',year,'.csv')
  df <- read.csv(paste0('data_prep/',file))
  df[is.na(df)]<-""
  for (i in 1:nrow(df)){
    if (df$Curr_Team[i] == ""){
      df$Curr_Team[i] = df$Orig_Team[i]
    }
  }
  df <- df |> tidyr::unite('Pick',Player:Curr_Team,sep=" | ",remove=FALSE)
  m <- matrix(df$Pick,nrow=24,byrow=FALSE)
  # Convert the matrix to a data frame
  df <- as.data.frame(m)
  colnames(df) <- paste("Round", 1:13)
  df$Pick = 1:24
  df <- df |> select(Pick,everything())
  write.csv(df, file = paste0('data/',file),row.names = F)
}

draft_wide_form('22')
draft_wide_form('23')
draft_wide_form('24')
draft_wide_form('25')
## 2024 draft reshaped as 13 x 24 grid

## 2026 draft (file created from live draft website) reshaped as 13 x 24 grid

draft <- read.csv('data_prep/draft26.csv')
for (i in 1:nrow(d26)){
  d26$player[i] <- strsplit(d26$player[i],",")[[1]][1]
}
d26$player <- sub("[[:punct:]]+$", "", d26$player)
d26 <- d26 |> tidyr::unite('Pick',player:current_team,sep=" | ",remove=FALSE)
m <- matrix(d26$Pick,nrow=24,byrow=FALSE)
# Convert the matrix to a data frame
df <- as.data.frame(m)
colnames(df) <- paste("Round", 1:13)
df$Pick = 1:24
df <- df |> select(Pick,everything())
write.csv(df, file = 'data/draft26.csv',row.names = F)










## DMB reports scraped below
#1 hitters (RJML_batting_website.html)
#2 pitching (RJML_pitching_website.html)
#3 game results (gamelog_website.html)

## Note 1: the rjmlbaseball website also requires scraping the DMB report 'League_schedule_website', 
## which is done in the script series_schedule.R

## Note 2: Team hit and Team pit tables also generated below

## Note 3: Team hit leaders and team pit leaders also generated.
## Some qualifying stats depends on games played to date. Change below
## as season progresses








## Hitters

scrape_hit_stats <- function(hit_filepath = "html/RJML_batting_website.html"){
  library(tidyverse)
  library(rvest)
  dfs = (read_html(hit_filepath) %>% 
           html_elements("table")%>% 
           html_table())
  
  hit1 = dfs[[1]]
  hit1 <- adjust_report_colnames(hit1)
  hit1 <- hit1 |> 
    rename(SLG = SPC)
  
  hit2 = dfs[[2]]
  hit2 <- adjust_report_colnames(hit2)
  hit2<-hit2 |> select(GS,PA,SH,SF,GDP,OPS,RC,RC27,ISO,TAVG,SEC,EBH,TB)
  
  hit3 <- dfs[[3]]
  hit3 <- adjust_report_colnames(hit3)
  hit3<-hit3|> select(`PA/G`,IPAVG,TBWH)
  
  vsL = dfs[[4]]
  vsL[3,8] = 'OPSL'
  vsL[3,9] = 'ABL'
  vsL <- adjust_report_colnames(vsL,3)
  vsL <- vsL %>% select(ABL,OPSL)
  
  vsR = dfs[[5]]
  vsR[3,8] = 'OPSR'
  vsR[3,9] = 'ABR'
  vsR <- adjust_report_colnames(vsR,3)
  vsR <- vsR %>% select(ABR,OPSR)
  
  
  
  hit <- cbind(hit1,hit2,hit3,vsL,vsR) 
  hit <- hit |> select(Name,Name_short,everything())
  hit <- hit |> 
    mutate(Team = toupper(Team)) |> 
    mutate(across(5:45, as.numeric)) |>
    mutate(Outs = AB-H+CS+SH+SF+GDP)
  hit$P <- factor(hit$P, 
                  ordered = TRUE, 
                  levels = c("c","1b","2b",'3b','ss','lf','cf','rf','dh'))
  
  hit <- hit |> mutate(PAmin = ifelse(ABmlb>=50,ceiling(PAmlb/2),0),PAmax = ifelse(PAmlb < 600,floor(PAmlb*1.05),1000))
  
  hit_rjml <- hit
  ##
  save(hit_rjml,file="rdata/hit_rjml.rda")
  ##
}

####
scrape_hit_stats()
####

#load('rdata/hit_rjml.rda')





#### Pitching
scrape_pit_stats <- function(pit_filepath = "html/RJML_pitching_website.html"){
  library(tidyverse)
  library(rvest)
  
  pfs<-(read_html(pit_filepath) %>% html_elements("table")%>% html_table())
  
  pit1 <- pfs[[1]]
  pit1[2,3]<-"pos"
  pit1 <- adjust_report_colnames(pit1)
  pit1 <- pit1 |> select(Name,Name_short,Team,pos,SDur,RDur,everything())
  
  pit2 <- pfs[[2]]
  pit2<-adjust_report_colnames(pit2)[,5:ncol(pit2)]
  
  pit3 <- pfs[[3]] 
  pit3<-adjust_report_colnames(pit3)[,5:ncol(pit3)]
  
  start = pfs[[4]]
  start <- adjust_report_colnames(start,2) |> select(QS,`STR%`)
  
  relief = pfs[[5]]
  relief <- adjust_report_colnames(relief,2) |> select(BS,HLD,GF,IR,IRS)
  
  pit <- cbind(pit1,pit2,pit3,start,relief) 
  pit <- pit |> 
    mutate(Team = toupper(Team)) |> 
    mutate(across(7:ncol(pit), as.numeric)) |>
    mutate(Outs = AB-H+CS+SH+SF+GDP)
  pit$IP = floor(pit$INN)+(pit$INN-floor(pit$INN))*3.33333
  pit <- pit |> mutate(AVG = round(H/AB,3),
                       OBP = round((H+BB+HBP)/(AB + BB + HBP + SF),3),
                       SLG =round((H + `2B` + 2*`3B` + 3*HR)/AB,3),
                       `BB%` = round(BB/BF*100,1),
                       `K%` = round(K/BF*100,1),
                       `HR%` = round(HR/BF*100,1))
  
  pit <- pit |> mutate(GSmin = ifelse((GSmlb>=4)&(SDur!=""),ceiling(GSmlb/2),0),
                       GSmax = GSmlb,
                       BFmax = ifelse(RDur!="",ceiling(BFmlb*1.05),NA))
  
  pit_rjml <- pit
  
  save(pit_rjml,file="rdata/pit_rjml.rda")
}
####
scrape_pit_stats()
####





#### Team Hitting and Team Pitching Totals
## hitting
load('rdata/hit_rjml.rda')
team_hit <- hit_rjml |> 
  select(Team,AB,H,`2B`,`3B`,HR,R,RBI,HBP,BB,K,SB,CS,PA,SH,SF,GDP,RC,Outs,TB,TBWH) |> 
  group_by(Team)|>
  summarize_all(sum)

team_hit <- team_hit |> mutate(AVG = round(H/AB,3),
                               OBP = round((H+BB+HBP)/(AB + BB + HBP + SF),3),
                               SLG =round((H + `2B` + 2*`3B` + 3*HR)/AB,3),
                               OPS = OBP + SLG,
                               RC27 = round(RC*27/Outs,1),
                               `BB%` = round(BB/PA*100,1),
                               `K%` = round(K/PA*100,1),
                               `HR%` = round(HR/PA*100,1))
## pitching
load('rdata/pit_rjml.rda')
team_pit <- pit_rjml |>
  select(Team,IP,AB,BF,W, L, S, GS, CG, SHO, H, `2B`, `3B`, HR, R, ER, BB, K,  HBP, WP, BK, SF, RC, Outs) |> 
  group_by(Team)|>
  summarize_all(sum)

team_pit <- team_pit |> mutate(AVG = round(H/AB,3),
                               OBP = round((H+BB+HBP)/(AB + BB + HBP + SF),3),
                               SLG =round((H + `2B` + 2*`3B` + 3*HR)/AB,3),
                               OPS = OBP + SLG,
                               RC27 = round(RC*27/Outs,1),
                               `BB%` = round(BB/BF*100,1),
                               `K%` = round(K/BF*100,1),
                               `HR%` = round(HR/BF*100,1),
                               ERA = round(ER/IP*9,2),
                               WHIP = round((H+BB)/IP,2)
)   
### 
save(team_hit, file = 'rdata/team_hit.rda')
save(team_pit, file = 'rdata/team_pit.rda')
###


### Game logs

#### Game Results (for team game log)

adjust_report_colnames <- function(df,n=2){
  colnames(df)<-as.vector(df[n,]) |> unlist()
  df <- df[-c(1:n),]
  return(df)
}

library(tidyverse)
library(rvest)

scrape_game_results <- function(gl_filepath = "html/gamelog_website.html"){
  df <-adjust_report_colnames((read_html(gl_filepath) %>% 
                                 html_elements("table")%>% 
                                 html_table())[[1]])
  df$Away = toupper(df$Away)
  df$Home = toupper(df$Home)
  df$Inn <- str_replace_all(df$Inn,"&nbsp","9")
  df <- df[,-2]
  colnames(df) = c("Date","Inn","Away",  "R_a", "LOB_a","Home",  "R_h","LOB_h","SP_h","hand_h","SP_a","hand_a",     "Win",   "Loss",  "Save",  "GWRBI")
  df <- df |> mutate(across(c(2,4,5,7,8), as.numeric))
  df$Date_raw <- as.Date(df$Date, format = "%m/%d/%Y")
  df$Month <- format(df$Date_raw, "%b")
  df$Date <- format(df$Date_raw, format = "%b-%d")
  df <- df |> mutate(TeamW = ifelse(R_a > R_h,Away,Home))
  df <- df |> mutate(hand_h=recode(hand_h,
                                   'L' = "*",
                                   'R' = ""),
                     hand_a=recode(hand_a,
                                   'L' = "*",
                                   'R' = ""))
  #df <- df |> unite('SP_h',SP_h:hand_h,sep="") |> unite('SP_a', SP_a:hand_a, sep="")
  game_results <- df
  save(game_results, file = 'rdata/game_results.rda')
}

####
scrape_game_results()
####




# similar to the unused team_record_log() function,
# team_win_logs() builds win_log.rda
# used to build division race plots
# it assumes each team has played 
# the same number of games

load('rdata/game_results.rda')
teamdf <- read.csv('csv/rosters.csv')
df <- game_results

team_win_log <- function(tm){
  load('rdata/game_results.rda')
  teamdf <- read.csv('csv/rosters.csv')
  df <- game_results
  date = unique(df$Date)
  date2 = unique(df$Date_raw)
  n = length(date)
  G = 1:n
  mph = data.frame(Date = date, Date_raw = date2,G)
  df$VAN = (df$Home==tm)+(df$Away == tm)
  
  van_gl <- df |> filter(VAN > 0) |> select(Date,Away,R_a,Home,R_h,Date_raw)
  
  van_gl <- left_join(mph,van_gl, by = c('Date','Date_raw'))
  
  van_gl$result = ''
  van_gl$opponent = ''
  van_gl$score = ''
  for (i in 1:nrow(van_gl)){
    if (is.na(van_gl$R_a[i])){next}
    if (van_gl$Away[i] == tm & (van_gl$R_a[i] < van_gl$R_h[i])){
      van_gl$result[i] = 'L'
      van_gl$opponent[i] = paste0("@",van_gl$Home[i])
      van_gl$score[i] = paste0(van_gl$R_a[i],"-",van_gl$R_h[i])}
    if (van_gl$Away[i] == tm & (van_gl$R_a[i] > van_gl$R_h[i])){
      van_gl$result[i] = 'W'
      van_gl$opponent[i] = paste0("@",van_gl$Home[i])
      van_gl$score[i] = paste0(van_gl$R_a[i],"-",van_gl$R_h[i])}
    if (van_gl$Home[i] == tm & (van_gl$R_a[i] < van_gl$R_h[i])){
      van_gl$result[i] = 'W'
      van_gl$opponent[i] = van_gl$Away[i]
      van_gl$score[i] = paste0(van_gl$R_h[i],"-",van_gl$R_a[i])}
    if (van_gl$Home[i] == tm & (van_gl$R_a[i] > van_gl$R_h[i])){
      van_gl$result[i] = 'L'
      van_gl$opponent[i] = van_gl$Away[i]
      van_gl$score[i] = paste0(van_gl$R_h[i],"-",van_gl$R_a[i])}
  }
  W_vector = ifelse(van_gl$result[1] == 'W',1,0)
  L_vector = ifelse(van_gl$result[1] == 'L',1,0)
  for (i in 2:nrow(van_gl)){
    if (van_gl$result[i] == 'W'){
      W_vector[i] <- 1 + W_vector[i-1]
      L_vector[i] <- L_vector[i-1]
    }
    if (van_gl$result[i] == 'L'){
      W_vector[i] <- W_vector[i-1]
      L_vector[i] <- 1 + L_vector[i-1]
    }
    if (van_gl$result[i]== ''){
      W_vector[i] <- W_vector[i-1]
      L_vector[i] <- L_vector[i-1]
    }
  }
  van_gl <- cbind(van_gl, W = W_vector, L = L_vector)
  
  van_gl$above_500 = van_gl$W - van_gl$L
  van_gl$Team = tm
  van_gl <- left_join(van_gl,teamdf |> select(Team,Division),by='Team')
  van_gl <- van_gl |> mutate(record = paste0(W,"-",L))
  return(van_gl)
}

mph <- team_win_log('VAN')

teams <- teamdf |> pull(Team)
win_log <- team_win_log(teams[1])

for (tm in teams[2:24]){
  win_log <- rbind(win_log,team_win_log(tm))
}

save(win_log, file = 'rdata/win_log.rda')





### team leaders

team_hit_leaders <- function(tm = 'VAN',stat='AVG',order = 'D', n=1, min = 3){
  if (order == 'D'){
    tbl<-hit_rjml |> filter(Team == tm, PA/n_games>=min) |> arrange(desc(!!sym(stat)))
    if (nrow(tbl) > 0){tbl <- tbl |> slice(1:n)}
    else{tbl<-hit_rjml |> filter(Team == tm) |> arrange(desc(!!sym(stat))) |> slice(1:n)}
  }
  else{
    tbl<-hit_rjml |> filter(Team == tm,PA/n_games>=min) |> arrange(!!sym(stat)) |> slice(1:n)
  }
  tbl$stat = stat
  tbl <- tbl |> select(Team,stat,Name,!!sym(stat))
  colnames(tbl) = c('Team','stat','Name','Value')
  if (stat %in% c('AVG','OPS')){
    tbl <- tbl |> mutate(Value = format(round(Value, 3), nsmall = 3))
  }
  if (stat == 'RC27'){
    tbl <- tbl |> mutate(Value = format(round(Value, 1), nsmall = 1))
  }
  tbl$Value = as.character(tbl$Value)
  if (stat %in% c('AVG','OPS')){
    tbl$Value  <- str_replace(tbl$Value,"0\\.","\\.")
  }
  
  return(tbl)
}

#team_hit_leaders("VAN")

team_pit_leaders <- function(stat='ERA',tm = 'VAN',min = 1,order = 'A', n=1){
  if (order == 'A'){
    tbl<- pit_rjml |> filter(Team == tm) |> filter(IP > n_games*min) |> arrange(!!sym(stat))
    if (nrow(tbl) > 0){tbl <- tbl |> slice(1:n)}
    else{tbl<-pit_rjml |> filter(Team == tm) |> arrange(!!sym(stat)) |> slice(1:n)}
  }
  if (order == 'D'){
    tbl<-pit_rjml |> filter(Team == tm) |> filter(IP/n_games >= min) |> arrange(desc(!!sym(stat))) |> slice(1:n)
  }
  tbl$stat = stat
  tbl <- tbl |> select(Team,stat,Name,!!sym(stat))
  colnames(tbl) = c('Team','stat','Name','Value')
  if (stat =='OPS'){
    tbl <- tbl |> mutate(Value = format(round(Value, 3), nsmall = 3))
  }
  if (stat == 'RC27'){
    tbl <- tbl |> mutate(Value = format(round(Value, 1), nsmall = 1))
  }
  if (stat == 'ERA'){
    tbl <- tbl |> mutate(Value = format(round(Value, 2), nsmall = 2))
  }
  tbl$Value = as.character(tbl$Value)
  if (stat == 'OPS'){
    tbl$Value  <- str_replace(tbl$Value,"0\\.","\\.")
  }
  return(tbl)
}


teams <- sort(toupper(unique(hit_rjml$Team)))
tm_h_lead <- data.frame(Team = as.character(c()),
                        Statistic = as.character(c()),
                        Leader = as.character())
for (t in teams){
  for (st in c('HR','RBI','SB')){tm_h_lead <- rbind(tm_h_lead,team_hit_leaders(tm = t, stat = st, order = 'D',min=0))}
  for (st in c('AVG','OPS','RC27')){tm_h_lead <- rbind(tm_h_lead,team_hit_leaders(tm = t, stat = st, order = 'D'))}
}

tm_p_lead <- data.frame(Team = as.character(c()),
                        Statistic = as.character(c()),
                        Name = as.character(),
                        Value = as.character())
for (t in teams){
  for (st in c('K','W','S')){tm_p_lead <- rbind(tm_p_lead,team_pit_leaders(tm = t, stat = st, min = 0, order = 'D'))}
  for (st in c('ERA','OPS','RC27')){{tm_p_lead <- rbind(tm_p_lead,team_pit_leaders(tm = t, min = .6, stat = st))}}
}



save(tm_h_lead,file = 'rdata/tm_h_lead.rda')
save(tm_p_lead,file = 'rdata/tm_p_lead.rda')




