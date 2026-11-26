library(tidyverse)
library(kableExtra)

#load prepared data
load('rdata/hit_rjml.rda')
load('rdata/pit_rjml.rda')
load('rdata/team_hit.rda')
load('rdata/team_pit.rda')
load('rdata/tm_h_lead.rda')
load('rdata/tm_p_lead.rda')
load('rdata/hit_mlb26.rda')
load('rdata/pit_mlb26.rda')
load('rdata/win_log.rda')
load('rdata/game_results.rda')

#shorten names of main RJML hitting and pitching tables
hit <- hit_rjml |> rename(XBH = EBH)
pit <- pit_rjml |> mutate(`K-BB%` = `K%`-`BB%`)


### Transactions

load('rdata/matrix.rda')

fa <- read.csv('csv/faclaims_rjml26.csv')
tr <- read.csv('csv/trades_rjml.csv')
tr_wide = left_join(
  tr[seq(1,nrow(tr),2),],
  tr[seq(2,nrow(tr),2),],
  by=c('Year','Trade.Number'),
  suffix = c('_A','_B'))

draft_by_team <- function(tm){
  df <- matrix[,c(1,3:5)]
  if (tm != 'all'){
    df <- df |> filter(current_team == tm)
  }
  return(df)
}

draft27_by_team <- function(tm){
  df <- draft27
  if (tm != 'all'){
    df <- df |> filter(current_team == tm)
  }
  return(df)
}
## draft 27 
draft27 <- read.csv('csv/draft27.csv')
draft27$Team = draft27$orig_team
for (i in 1:nrow(draft27)){
  if (draft27$current_team[i] != ''){
    draft27$Team[i] = paste0(draft27$current_team[i], ' (from ',draft27$orig_team[i],")")
  }
  if (draft27$current_team[i] == ''){
    draft27$current_team[i] = draft27$orig_team[i]
  }
}

#update team affiliation for rosters and MLB 26 stats
keyID <- read.csv('csv/keyID26.csv')
keyID_h <- unique(left_join(keyID |> 
                       filter(type=='H') |> 
                       select(Name,RJML,key_DMB),
                     hit |> 
                       select(key_DMB,Age,P),
                     by='key_DMB') |>
  select(Name,Age,P,RJML))

hit_mlb26 <- left_join(hit_mlb26 |> select(-RJML),
                       keyID |> 
                         filter(type == 'H') |> 
                         select(RJML,key_FG),
                       by = 'key_FG') |> 
  select(RJML,Name,everything())

pit_mlb26 <- left_join(pit_mlb26 |> select(-RJML),
                       keyID |> 
                         filter(type == 'P') |> 
                         select(RJML,key_FG),
                       by = 'key_FG') |> 
  select(RJML,Name,everything())

keyID_p <- unique(left_join(keyID |> 
                       filter(type=='P') |> 
                       select(Name,RJML,key_DMB),
                     pit |> 
                       select(key_DMB,Age,pos),
                     by='key_DMB') |>
  rename(P = pos) |> 
  select(Name,Age,P,RJML))

# MLB26 Stats
#date of last update
mlb26_last_update <- format(file.info('rdata/hit_mlb26.rda')$mtime, "%b %d, %Y")

hit_mlb26 <- hit_mlb26 |> 
  mutate(WAR = round(WAR,1),
         AVG = str_replace(as.character(format(round(AVG, 3), nsmall = 3)),"0\\.","\\."),
         OPS = str_replace(as.character(format(round(OPS, 3), nsmall = 3)),"0\\.","\\."),
         wOBA = str_replace(as.character(format(round(wOBA, 3), nsmall = 3)),"0\\.","\\.")
         )
  
pit_mlb26 <- pit_mlb26 |> 
  mutate(`K%` = round(100*Kpct,1),
         `BB%` = round(100*BBpct,1),
         `K-BB%` = `K%`-`BB%`) |> 
  mutate(ERA= round(ERA,2),
         WHIP = round(WHIP,2),
         xFIP = round(xFIP,2),
         WAR = round(WAR,1))



## team information

teamdf <- read.csv('csv/rosters.csv')
divisions = sort(unique(teamdf$Division))
teams <- teamdf |> pull(Team)


h2h_tiebreak <- function(tm1,tm2){
  mph <- game_results |> filter((Home==tm1&Away==tm2)|(Home==tm2&Away==tm1))
  tm1wins <- sum(mph$TeamW==tm1)
  tiebreak_win <- ifelse(tm1wins > nrow(mph)/2,tm1,
                         ifelse(tm1wins < nrow(mph)/2,tm2,'tie'))
  return(tiebreak_win)
}


div_standings <- function(div){
  teams <- teamdf |> filter(Division == div) |> pull(Team)
  df <- left_join(
    team_hit |> 
      filter(Team %in% teams) |> 
      select(Team,R) |> 
      rename(RF = R),
    team_pit |> 
      filter(Team %in% teams) |> 
      select(Team,W,L,R) |> 
      rename(RA = R) |> 
      mutate(Winpct = round(W/(W+L),3)),
    by = 'Team') |>
    arrange(desc(Winpct))
  df$GB = NA
  df$rank = 1:6
  for (i in 1:5){
    if (df$Winpct[i] == df$Winpct[i+1]){
      if (h2h_tiebreak(df$Team[i],df$Team[i+1])==df$Team[i+1]){
        df$rank[i] = i + 1
        df$rank[i+1] = i
      }
    }
  }
  df <- df |> arrange(rank)
  for (i in 2:6){
    df$GB[i] = ((df$W[1]-df$W[i])+(df$L[i]-df$L[1]))/2
    if (df$GB[i] == 0){df$GB[i] = NA}
  }
  df <- left_join(df,
                  teamdf |>  select(Team,Full_team), 
                  by = 'Team')
  df$Division = div
  df <- df |> select(Division,Full_team,W,L,GB,RF,RA,Winpct,Team)
  return(df)
}


#total number of games played
n_games <- sum(team_pit$GS)/2 #so n_games/12 = average G per team

standings <- rbind(div_standings('Wagner'),
                   div_standings('Hornsby'),
                   div_standings('Alexander'),
                   div_standings('Gehrig'))

# basic team info from the rosters.csv file
scrape_info <- function(tm){
  #teamdf <- read.csv('csv/rosters.csv') |> filter(League == 'RJML')
  tm_idx <- which(teamdf$Team == tm)
  team = teamdf$Full_team[tm_idx]
  logo_url = paste0('logos/',teamdf$Logo[tm_idx])
  manager = teamdf$Manager[tm_idx]
  park = teamdf$Park[tm_idx]
  division = teamdf$Division[tm_idx]
  W = standings$W[which(standings$Full_team == team)]
  L = standings$L[which(standings$Full_team == team)]
  n = which((standings |> filter(Division == teamdf$Division[tm_idx]))$Full_team == team)
  if (n == 1){place <- "1st"}
  if (n == 2){place <- "2nd"}
  if (n == 3){place <- "3rd"}
  if (n > 3){place <- paste0(as.character(n),"th")}
  position <- paste0(division," (",place,")")
  record = paste0(W,'-',L)
  color = teamdf$team_color[tm_idx]
  return(c(team,logo_url, manager,park,position,record,color))
}

### Division standings with logos
logo_stand <- function(div){
  
  logo_standings <- left_join(div_standings(div),
                              teamdf |> select(Team,Logo_url,team_color),by = 'Team')
  logo_standings$team_url <- paste0(logo_standings$Team,".html")
  logo_standings <- logo_standings |> mutate(" " = '') |> select(" ",everything())
  logo_standings |> 
    #filter(Division == div) |> 
    select(" ",Full_team,W,L,GB,RF,RA) |>
    mutate(across(GB, ~replace_na(as.character(.), "-"))) |>
    kbl(booktabs = T,
        escape = F,
        align="clccccccc",
        col.names = c(" ","Team","W","L","GB","RF","RA"),
        caption = paste0("<span style='color: seagreen;'><strong>",div," Division</strong></span>")) %>%
    #add_header_above(c(' ' = 2,"Hitting" = 7,"Pitching" = 6,' ' = 2)) %>%
    #column_spec(4,border_right=T) |>
    column_spec(1, image = spec_image((logo_standings |> filter(Division == div))$Logo_url, 80, 80) # dimensions of the images
    ) |> 
    #column_spec(c(1,11), border_right = T) %>%
    #column_spec(ncol(df),background = "wheat") %>%
    kable_styling(font_size = 14,full_width = T,bootstrap_options = c("hover", "condensed", "responsive"))
}



# division race plot
# works on modified win_log file which works
# mid-month, when teams may have played an unequal number of games

div_above500_plot <- function(div){
  mp <- win_log |> filter(Division == div)
  
  color_data <- teamdf |> filter(Division == div) |> select(Team, team_color)
  team_colors <- setNames(color_data$team_color, color_data$Team)
  p <- suppressWarnings(ggplotly(ggplot(mp,aes(x = G,
                                               y = above_500,
                                               group = Team,
                                               col = Team
  )
  )+
    geom_line(aes(text = paste0("Team: ", Team,
                                "\nDate: ", Date,
                                "\nOpp: ", opponent,
                                "\nResult: ", result, " ", score,
                                "\nRecord: ", record)))+
    geom_vline(xintercept = c(30,60,85,110,135,160),linetype='dashed',col='lightgray')+
    scale_x_continuous(
      breaks = c(30,60,85,110,135,160),                       # Where the tick marks go
      labels = c("May", "June", "July", "Aug","Sep","160")    # What the labels say
    )+
    scale_color_manual(values = team_colors) +
    geom_hline(aes(yintercept = 0), linewidth = .5,col = 'red',linetype='dashed')+
    #geom_text(aes(label = Team),
    #             nudge_x = 0.25, nudge_y = 0.25, # Nudge text slightly away from points
    #             check_overlap = TRUE)+
    labs(title = paste(div,"Division Race - hover on line for team info"),
         y = 'Games above 500',
         x = '')+
    ylim(c(-35,max(win_log$above_500)+3))+
    theme_classic()+
    theme(legend.position = "none"),
  tooltip = "text"
  ))
  p %>% config(displayModeBar = FALSE)
}

## wildcard race

wildcard <- function(conf = 'Younger'){
  if (conf == 'Younger'){
    df1 <- div_standings('Wagner')
    df2 <- div_standings('Hornsby')
  }
  if (conf == 'Ruthian'){
    df1 <- div_standings('Alexander')
    df2 <- div_standings('Gehrig')
  }
  div_leaders <- c(df1$Team[1],df2$Team[1])
  df <- rbind(df1,df2) |> arrange(desc(Winpct))
  df_wc <- rbind(df |> filter(Team %in% div_leaders),
                 df |> filter(!Team %in% div_leaders)) |> 
    select(Full_team,W,L,GB,Winpct)
  df_wc$GB[3]=paste0("+",((df_wc$W[3]-df_wc$W[4])+(df_wc$L[4]-df_wc$L[3]))/2)
  df_wc$GB[c(1,2,4)] <- NA
  for (i in 5:12){
    df_wc$GB[i] = ((df_wc$W[4]-df_wc$W[i])+(df_wc$L[i]-df_wc$L[4]))/2
    #if (df$GB[i] == 0){df$GB[i] = NA}
  }
  #df_wc[1:2, 1] <- cell_spec(df_wc[1:2, 1], color = "steelblue", bold = TRUE)
  #df_wc[3:4, 1] <- cell_spec(df_wc[1:2, 1], color = "seagreen", bold = TRUE)
  return(df_wc |> 
           mutate(across(GB, ~replace_na(as.character(.), "-"))) |>
           mutate(Full_team = cell_spec(Full_team, color = ifelse(row_number() %in% c(1, 2), "steelblue", 
                                                      ifelse(row_number() %in% c(3,4),"seagreen","black")))) |>
           kbl(format = "html",
               align = "lccrc",
               col.names = c('Team','W','L','GBwc','W%'),
               caption = paste0("<span style='color:Navy ;'><strong>",conf, " League</strong></span>"),
               escape=FALSE) |> 
           row_spec(2, extra_css = "border-bottom: 2px dashed black;") |> 
           column_spec(1, bold = ifelse(1:nrow(df) <= 4, TRUE, FALSE)) |>
           kable_styling(font_size = 14,full_width = F,bootstrap_options = c("hover", "condensed", "responsive")))
}
wc_younger <- wildcard('Younger') 

wc_ruthian <- wildcard('Ruthian') 

## Team leaders Function 
## the tables tm_h_lead and tm_p_lead created in scrape_rjml_stats script
team_leaders <- function(tm){
  ## team leaders
  load('rdata/tm_h_lead.rda')
  load('rdata/tm_p_lead.rda')
  df <- rbind(tm_h_lead |> filter(Team == tm),tm_p_lead |> filter(Team == tm)) |> 
    select(-Team) 
  return(df)
}
               
### preparing league averages
lg_avg <- function(){
  x <- colSums(hit_rjml |> select(AB,H,`2B`,`3B`,HR,
                                  R,HBP,BB,K,SB,
                                  CS,PA,SF,SH,GDP,
                                  RC,Outs))
  lg_avg <- round(x[2]/x[1]*1000,0)
  lg_obp <- round((x[2]+x[8]+x[7])/(x[1]+x[8]+x[7]+x[13])*1000,0) # (H + BB + HBP)/(AB + BB + HBP + SF)
  lg_slg <- round((x[2]+x[3]+2*x[4]+3*x[5])/x[1]*1000,0)
  lg_ops <- lg_obp+lg_slg
  lg_rpg <- round(x[6]/(n_games),1)
  lg_hrpct <- round(x[5]/x[12]*100,1)
  lg_kpct <- round(x[9]/x[12]*100,1)
  lg_bbpct <- round(x[8]/x[12]*100,1)
  lg_rc27 <- round(x[16]/x[17]*27,1)
  y <- colSums(pit_rjml |> select(IP,H,BB,ER))
  lg_whip <- round((y[2]+y[3])/y[1],2)
  lg_era <- round(y[4]/y[1]*9,2)
  
  df <- data.frame(
    stat = c('Runs/G','AVG','OBP','SLG','Kpct','BBpct','HRpct','WHIP','ERA','RC27'),
    RJML26 = as.character(c(lg_rpg,lg_avg,lg_obp,lg_slg,lg_kpct,lg_bbpct,lg_hrpct,lg_whip,lg_era,lg_rc27)),
    MLB25 = c('8.9','.245','.315','.404','22.3','8.4','3.1','1.29','4.15','4.4')) 
  df[2:4,2] = paste0('.',df[2:4,2])
  return(df)
}

lg_averages <- lg_avg() 

## Preparing League Leaderboard stats

hit_stats_low <- c()
hit_stats_high <- c('AVG','OBP','SLG','OPS','RC27','XBH','TBWH','R','RBI','HR','RC','2B','3B','SB','BB','K')
pit_stats_low <- c('ERA','WHIP','RC27','CERA','BB%','HR%')
pit_stats_high <- c('K%','K-BB%','K','W','GS','INN','BF','QS')
hit_stats <- sort(c(hit_stats_high, hit_stats_low))
pit_stats <- sort(c(pit_stats_low,pit_stats_high,'S'))


## Team Game log
team_game_log <- function(tm = 'VAN'){
  load('rdata/game_results.rda')
  teamdf <- read.csv('csv/rosters.csv') #|> filter(League == 'RJML')
  df_home <- game_results |> 
    filter(Home == tm) |> 
    select(Date,Inn,Away,R_h,R_a,SP_h, SP_a,GWRBI,Date_raw) |> 
    rename(Opp = Away,
           SP = SP_h,
           Versus = SP_a,
           RF = R_h,
           RA = R_a) |> 
    mutate(H_A = ' ') |> 
    select(Date,H_A,Opp,RF,RA,everything())
  df_away <- game_results |> 
    filter(Away == tm) |> 
    select(Date,Inn,Home,R_a,R_h,SP_a, SP_h,GWRBI,Date_raw) |> 
    rename(Opp = Home,
           SP = SP_a,
           Versus = SP_h,
           RF = R_a,
           RA = R_h) |> 
    mutate(H_A = '@') |> 
    select(Date,H_A,Opp,RF,RA,everything())
  df <- rbind(df_home,df_away) |> arrange(Date_raw)
  gl <- left_join(df,teamdf |> select(Team,City),by=c('Opp'='Team'))
  gl <- gl |> mutate(Result = ifelse(RF > RA,"W","L"))
  gl$series_num = sort(rep(1:(nrow(gl)/5),5))
  gl <- gl |> rename(Opponent=City) |> select(Date,H_A,Opp,Result,everything())
  return(gl)
}

### team detailed record

opp_loc <- function(tm,opp){
  df <- read.csv('csv/rosters.csv')
  tmid <- which(df$Team==tm)
  div <- df$Division[tmid]
  conf <- df$Conference[tmid]
  oppid <- which(df$Team==opp)
  if (df$Division[oppid]==div){return('div')}
  if (df$Conference[oppid]==conf){return('league')}
  return('nonleague')
}

team_detailed_record <- function(tm){
  mph <- team_game_log(tm)
  n = nrow(mph)/5
  df <- mph |> group_by(series_num) |> summarize(W = sum(Result=='W'))
  series_opp<-mph$Opp[seq(1,nrow(mph),5)]
  df$Opp <- series_opp
  df$Loc <- mph$H_A[seq(1,nrow(mph),5)]
  df$Type <- sapply(series_opp, function(x) opp_loc(tm, x))
  #df <- df |> mutate(Result = paste0(W,"-",5-W))
  df$L = 5 - df$W
  divW = sum((df |> filter(Type=='div'))$W)
  divL = sum((df |> filter(Type=='div'))$L)
  lgW = sum((df |> filter(Type=='league'))$W)+divW
  lgL = sum((df |> filter(Type=='league'))$L)+divL
  inlgW = sum((df |> filter(Type=='nonleague'))$W)
  inlgL = sum((df |> filter(Type=='nonleague'))$L)
  homeW = sum((df |> filter(Loc == ' '))$W)
  homeL = sum((df |> filter(Loc == ' '))$L)
  awayW = sum((df |> filter(Loc == '@'))$W)
  awayL = sum((df |> filter(Loc == '@'))$L)
  vs_tm_rcd <- df |> arrange(Type,Opp) |> 
    #  filter(Type=='div') |>
    select(-c(L,series_num)) |> 
    pivot_wider(names_from=Loc,values_from=W) |> 
    select(Type,Opp,` `,`@`) |> 
    mutate(Home = paste0(` `,"-",5-` `),
           Away = paste0(`@`,"-",5-`@`)) |> 
    select(Type,Opp,Home,Away)
  vs_tm_rcd[vs_tm_rcd == 'NA-NA'] <- '-'
  #colnames(vs_tm_rcd) <- c('Opp','Away','Home')
  tbl <- vs_tm_rcd |> 
    select(-Type) |> 
    #mutate(across(everything(), ~tidyr::replace_na(as.character(.x), "-"))) %>%
    kbl(format = 'html',
        caption = "<span style='color: seagreen; font-size: 1.2em;'>Detailed Record</span>",
        align = 'ccc',col.names = c('Opp',
                                    paste0('Home (',homeW,"-",homeL,')'),
                                    paste0('Away (',awayW,"-",awayL,')'))) |> 
    #row_spec(1:5,  color = "white", background = "steelblue") |>
    #row_spec(6:11, color='white',background = 'seagreen') |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),font_size = 15,full_width=F)  |> 
    pack_rows(paste0("Division (",divW,"-",divL,")"), 1, 5) %>%
    pack_rows(paste0("League (",lgW,"-",lgL,")"), 6, 11) |>
    pack_rows(paste0("Inter League (",inlgW,"-",inlgL,")"),12,nrow(vs_tm_rcd))
  return(tbl)
}

### Team series plot

team_series_plot <- function(tm){
  tm.gl <- team_game_log(tm)
  team_long <- teamdf$City[which(teamdf$Team == tm)]
  tm.sl <- tm.gl |> 
    group_by(series_num) |> 
    mutate(W = sum(Result=='W')) |> 
    mutate(park=recode(H_A,
                       '@' = 'away',
                       ' ' = 'home'
    )
    )
  tm.sl <- unique(tm.sl |> select(series_num,Opp,park,W))
  tm.sl_binned <- tm.sl |> 
    group_by(W) %>%
    mutate(
      # Stack the dots upwards inside each bin
      y_pos = row_number()
    ) %>%
    ungroup() 
  
  p <- ggplot(tm.sl_binned, aes(x = W, y = y_pos, fill = park, label=Opp)) +
    # Use geom_point with shape 21 for a filled circle; 22 for square
    geom_point(alpha = .8,shape = 21, color = "black", size = 16) +
    scale_fill_manual(values = c('wheat','steelblue')) +
    guides(fill = guide_legend(override.aes = list(size = 8)))+
    # Overlay string labels directly inside the dots
    geom_text(size = 5, color = "black", fontface = "bold") +
    # Clean up axes and labels
    #scale_y_continuous(breaks = 1:max(binned_df$y_pos)) +
    ylim(c(0.5,max(tm.sl_binned$y_pos)+.5))+
    xlim(c(0,5))+
    #scale_x_discrete(breaks = 0:5)+
    labs(
      title = paste(team_long,"wins by series"),
      x = "wins",
      y = ""
    ) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 16),
          legend.title = element_blank(),
          #legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.position = "bottom")
  return(p)
}

## color by month option
team_series_plot_months <- function(tm){
  tm.gl <- team_game_log(tm)
  team_long <- teamdf$City[which(teamdf$Team == tm)]
  tm.sl <- tm.gl |> 
    group_by(series_num) |> 
    mutate(W = sum(Result=='W')) 
  
  tm.sl <- unique(tm.sl |> select(series_num,H_A,Opp,W))
  tm.sl <- tm.sl |> 
    unite('Opp',H_A:Opp,sep="")|>
    mutate(Opp = trimws(Opp),
           month = cut(series_num,
                       breaks = c(0,6.5,12.5,17.5,22.5,27.5,33),
                       labels = c('April','May','June','July','Aug','Sep')
    )
    )
  tm.sl_binned <- tm.sl |> 
    group_by(W) %>%
    mutate(
      # Stack the dots upwards inside each bin
      y_pos = row_number()
    ) %>%
    ungroup() 
  
  p <- ggplot(tm.sl_binned, aes(x = W, y = y_pos, fill = month, label=Opp)) +
    # Use geom_point with shape 21 for a filled circle; 22 for square
    geom_point(color = 'black',alpha = .8,shape = 21,  size = 16,stroke=.5) +
    scale_fill_manual(values = c('#FFFDD0','thistle','seagreen','dodgerblue','orange','#AA2704')) +
    guides(fill = guide_legend(override.aes = list(size = 8)))+
    # Overlay string labels directly inside the dots
    geom_text(size = 4, color = "black", fontface = "bold") +
    # Clean up axes and labels
    #scale_y_continuous(breaks = 1:max(binned_df$y_pos)) +
    ylim(c(0.5,max(tm.sl_binned$y_pos)+.5))+
    xlim(c(0,5))+
    #scale_x_discrete(breaks = 0:5)+
    labs(
      title = paste(team_long,"wins by series"),
      x = "wins",
      y = ""
    ) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 16),
          legend.title = element_blank(),
          #legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.position = "bottom")
  return(p)
}


### Remaining Series

team_remaining_series <- function(tm='VAN'){
  load('rdata/series_schedule.rda')
  tm.gl <- team_game_log(tm)
  #team_long <- teamdf$City[which(teamdf$Team == tm)]
  tm.sl <- unique(tm.gl |> select(series_num,H_A,Opp) 
  |> unite('Opp',H_A:Opp,sep=""))
  tm.sched <- series_schedule |> filter(Team == tm)
  rem.series <- tm.sched |> filter(Opp %in% setdiff(tm.sched$Opp,gsub(" ", "", tm.sl$Opp)))
  return(rem.series)
}

# which teams have played their home series for the month?
home_month_results <- function(tm = 'VAN',m = 'Aug'){
  return(ifelse(nrow(team_remaining_series(tm) |> filter(Month == m, !str_detect(Opp, "@")))>0,
                'No','Yes'))
}

league_home_results <- function(month = 'Aug'){
  h_results <- c()
  for (tm in teamdf$Team){
    h_results <- c(h_results,home_month_results(tm,month))
  }
  return(
    kbl(data.frame(Team = teamdf$Team,results = h_results) |> 
          mutate(results=recode(results,
                               'Yes'='\u2713',#checkmark!
                               'No'=' ')) |> 
          mutate(results = cell_spec(
            results, 
            color = ifelse(results == "\u2713", "seagreen", "red"),
            bold = F
          )),
        align = 'lc',
        format = 'html',
        #caption = paste(month,"results submitted"),
        escape = FALSE) |> 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),font_size = 14,full_width=F))
}




  