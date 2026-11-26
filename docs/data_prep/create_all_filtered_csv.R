library(dplyr)

#Draft tables
draft   <- read.csv("data_prep/draft27.csv", stringsAsFactors = FALSE)
hit <- read.csv("data_prep/hitters.csv", stringsAsFactors = FALSE)
hit <- hit |> rename(SLG = SPC)
pit <- read.csv("data_prep/pitchers.csv", stringsAsFactors = FALSE)
pit$outs = floor(pit$INN)*3+(pit$INN-floor(pit$INN))*10
pit$WHIP = round((pit$BB+pit$H)/pit$outs*3,2)
pit$`K%` = round(pit$K/pit$BF*100,1)
pit$`BB%` = round(pit$BB/pit$BF*100,1)
pit$`HR9` = round(pit$HR/pit$outs*27,1)
pit <- pit |> rename(Pos = P)
rteams <- read.csv("data_prep/RJMLteaminfo.csv", stringsAsFactors = FALSE)

#Populate draft boards
teams <- sort(unique(draft$orig_team))
for (i in 1:nrow(draft)){
  if (!draft$current_team[i] %in% teams){
    draft$current_team[i] = draft$orig_team[i]
  }
}
draft$Player[which(is.na(draft$Player))] <- ''
draft <- left_join(draft,rteams |> select(Team,Full_team),by = c('current_team'='Team'))
draft <- draft |> select(Pick,orig_team,current_team,Full_team,Player)
draft$Overall <- 1:nrow(draft)
draft$Round <- floor((draft$Overall-1)/24)+1
draft <- draft |> rename(Team = Full_team)

#current pick
current_pick_idx <- which(draft$Player == "")[1]
current_pick <- ifelse(current_pick_idx < nrow(draft),paste(draft$Pick[current_pick_idx],"-",draft$Team[current_pick_idx]),'Draft Complete')

#remaining picks
picks_left <- draft[current_pick_idx:nrow(draft),] |> select(Pick, orig_team, Team)
picks_made <- draft[1:(current_pick_idx-1),] |> select(Pick, Team, Player)
if (current_pick_idx == 1){
  picks_left <- draft |> select(Pick,orig_team,Team)
  picks_made <- data.frame(Pick = '-',Team = '-', Player = '-')
}
if (current_pick_idx == nrow(draft)){
  picks_left <- data.frame(Pick = '-',orig_team = '-',Team = '-')
  picks_made <- draft |> select(Pick,Team,Player)
}

write.csv(picks_left,
          file = "data/picks_left.csv",
          row.names = FALSE)
write.csv(picks_made,
          file = "data/picks_made.csv",
          row.names = FALSE)

## Team Usage
hit_usage <- hit |> 
  filter(Team != 'avail') |>
  group_by(Team) |> 
  summarize(PA_tot = sum(PA),
            c = sum(PA[grepl("2", all_pos)]),
            `1b` = sum(PA[grepl("3", all_pos)]),
            `2b` = sum(PA[grepl("4", all_pos)]),
            `3b` = sum(PA[grepl("5", all_pos)]),
            ss = sum(PA[grepl("6", all_pos)]),
            lf = sum(PA[grepl("7", all_pos)]),
            cf = sum(PA[grepl("8", all_pos)]),
            rf = sum(PA[grepl("9", all_pos)]),
            )

pit_usage <- pit |> 
  filter(Team != 'avail') |>
  group_by(Team) |> 
  summarize(GS = sum(GS),
            Relief_BF = sum(BF[Rel != ""])
  )

team_usage <- left_join(pit_usage,hit_usage,by='Team')

write.csv(team_usage,file='data/team_usage.csv',row.names = F)

## available hitters and pitchers
avail_hit <- hit %>% 
  filter(Team == 'avail') %>%
  select(Name,Pos,Age,PA,AVG,OBP,SLG,OPS,RC27,HR,R,RBI,SB,c:C)

avail_pit <- pit %>%
  filter(Team == 'avail') %>%
  select(Name,Pos,St,Rel,Age,G,GS,INN,BF,ERA,WHIP,OPS,RC27,K,BB,HR,W,L,S) %>%
  rename(Sdur = St, Rdur = Rel)

write.csv(avail_hit,
          file = "data/avail_hit.csv",
          row.names = FALSE)
write.csv(avail_pit,
          file = "data/avail_pit.csv",
          row.names = FALSE)

#trades and FA claims
tr <- read.csv('data_prep/transactions/trades_rjml.csv')
tr_wide = left_join(
  tr[seq(1,nrow(tr),2),],
  tr[seq(2,nrow(tr),2),],
  by=c('Year','Trade.Number'),
  suffix = c('_A','_B'))
tr_wide <- tr_wide |> 
  arrange(desc(`Trade.Number`)) |> 
  arrange(desc(Year))
write.csv(tr_wide,file = 'data/trades.csv',row.names = FALSE)

fa <- read.csv('data_prep/transactions/faclaims_rjml.csv')

fa <- fa |> select(-Year) |> arrange(desc(No))

write.csv(fa,file='data/claims.csv',row.names = FALSE)

## Team Tables
## Generating the three team specific .csv files
# Draft Picks, file name draft_TM.csv; 
# Hitters, file name hit_TM.csv
# Pitchers, file name pit_TM.csv
for (i in 1:length(teams)) {
  df <- draft |> filter(current_team == teams[i]) |> select(Pick,Player)
  h <- hit |> filter(Team == teams[i]) |> select(Name,Age,Pos,all_pos,PA,AB,H,HR,R,RBI,SB,CS,BB,K,AVG,OBP,SLG,OPS,RC27)
  p <- pit |> filter(Team == teams[i]) |> select(Name,Age,Pos,St,Rel,GS,INN,BF,ERA,WHIP,K,BB,OPS,RC27,`K%`,`BB%`,HR9)
  write.csv(df,
            file = paste0("data/",
                          teams[i],
                          "_draft.csv"),
            row.names = FALSE)
  write.csv(h,
            file = paste0("data/",
                          teams[i],
                          "_hit.csv"),
            row.names = FALSE)
  write.csv(p,
            file = paste0("data/",
                          teams[i],
                          "_pit.csv"),
            row.names = FALSE)
}

### create team html files from team.rmd



generate_team_reports <- function(){
  library(rmarkdown)
  rjmlinfo <- read.csv('data_prep/RJMLteaminfo.csv')
  teams <- sort(rjmlinfo$Team)
  
  for (tm in teams) {
    # Define the output file name
    output_file_name <- paste0(tm, ".html")
    
    # Call rmarkdown::render()
    # Pass the parameter value to the 'params' argument as a list
    render(
      input = "team.Rmd",            # The Rmd template file
      output_file = output_file_name,    # The name for the new HTML file
      params = list(team = tm), # The parameters list
      envir = parent.frame()             # Ensures data is accessible in Rmd
    )
  }
}

generate_team_reports()


#df <- read.csv('data/draft25_long.csv')
#df$pick = ""
#for (i in 1:nrow(df)){
#  if (df$Curr_Team[i] == ""){
#    df$Curr_Team[i] <- df$Orig_Team[i]
#    }
#  c = ifelse(nchar(df$Pick[i])==1,paste0("0",df$Pick[i]),df$Pick[i])
#  df$pick[i] = paste0(df$Round[i],".",c)
#}
#write.csv(df,file = 'data/draft25_long.csv',row.names = F)

