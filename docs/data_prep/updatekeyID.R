#initially adding the debut players from previous keyID file
library(dplyr)
#k26 <- read.csv('csv/keyID26.csv')
#k26 <- k26[-which(is.na(k26$key_DMB)),]
#kdeb <- read.csv('csv/keyID26debut.csv')
#kdeb <- kdeb %>% select(-X) %>% rename(type = Type)
#k27 <- rbind(k26 %>% select(Name,type,key_DMB,key_FG,key_MLB),kdeb)
#k27 <- left_join(k27,k26 |> select(type,key_DMB,RJML,CJPL,TBBL), by = c('key_DMB','type'))
#write.csv(k27,file = 'csv/keyID27.csv',row.names = F)                 


### Manually change some team affiliations
#load current keyID_draft data
keyID <- read.csv('data_prep/keyID27.csv')

#updating individual player

lg = 'RJML'
tm = c('GRF','VAN')
pl = c('Colt Keith','Jacob T. Wilson')

for (i in 1:length(pl)){
  keyID[[lg]][which(keyID$Name==pl[i])]<-tm[i]
}

write.csv(keyID,file='data_prep/keyID27.csv',row.names = F)


### updating/adding DMB_batter_ID and DMB_pitcher_ID reports
#first define function to graph html report
keyID_for_league <- function(league='RJML',type='B'){
  if (type=='H'){
    file = paste0('html/',league,'_batter_ID.html')
  }
  if (type=='P'){
    file = paste0('html/',league,'_pitcher_ID.html')
  }
  tb = read_html(file) %>% 
    html_elements("table") %>% html_table()
  df = tb[[1]]
  colnames(df)<-df[2,]
  df <- df[-c(1,2),]
  df$key_DMB = as.integer(df$UID)
  df$Team[which(df$Team=='&nbsp')] = 'avail'
  df$type = type
  colnames(df)[4]=league
  return(df)
}
#second, run update of player ownership for a given league
lg_to_update = 'RJML'

lg_hit = keyID_for_league(league=lg_to_update,type='H')
lg_pit = keyID_for_league(league=lg_to_update,type='P')
df <- keyID
mph <- left_join(df %>% filter(type=='H') %>% select(-lg_to_update),
                 lg_hit %>% select(key_DMB,lg_to_update),by='key_DMB')
mkm <- left_join(df %>% filter(type=='P') %>% select(-lg_to_update),
                 lg_pit %>% select(key_DMB,lg_to_update),by='key_DMB')
keyID <- rbind(mph,mkm) %>% select(Name,type,T,B,RJML,SSBL,TBBL,CJPL,key_DMB,key_FG,key_MLB)

write.csv(keyID,file='csv/keyID27.csv',row.names = F)