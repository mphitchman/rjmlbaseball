
## EDIT PICK OWNERS

load('rdata/matrix.rda')
load('rdata/hit_draft.rda')
load('rdata/pit_draft.rda')
picks = c('6.09')
teams = c('DV')

for (i in 1:length(picks)){
  matrix$current_team[which(matrix$pick == picks[i])] <- teams[i]
}

save(matrix, file = 'rdata/matrix.rda')

## ENTER PICKS
load('rdata/matrix.rda')
load('rdata/hit_draft.rda')
load('rdata/pit_draft.rda')
pck = '1.20'
player = 'Jac Caglianone*'


# if a batter
if (player %in% hit_draft$Name){
  pos <- hit_draft$Pos[which(hit_draft$Name == player)]
  hit_draft$Team[which(hit_draft$Name == player)] <- matrix$current_team[which(matrix$pick == pck)]
  
}

#if a pitcher
if (player %in% pit_draft$Name){
  pos <- pit_draft$P[which(pit_draft$Name == player)]
  pit_draft$Team[which(pit_draft$Name == player)] <- matrix$current_team[which(matrix$pick == pck)]
}

#update matrix
matrix[matrix$pick == pck, "player"] <- paste0(player, ", ",pos)


save(matrix, file = 'rdata/matrix.rda')
save(hit_draft, file = 'rdata/hit_draft.rda')
save(pit_draft, file = 'rdata/pit_draft.rda')

## EDIT ROSTERS FROM TRADE


#hitters traded
hit <- read.csv('data_prep/hitters.csv')
hit |> filter(Team == 'VAN') |> select(Name)
hit$Team[which(hit$Name == 'Jacob T. Wilson')]<-'VAN'
names = c('Jacob T. Wilson','Colt Keith*')
teams = c('VAN','GRF')
for (i in 1:length(names)){
  hit$Team[which(hit$Name == names[i])]<-teams[i]
}
write.csv(hit,file = 'data_prep/hitters.csv',row.names = F)
#pitchers traded
pit <- read.csv('data_prep/pitchers.csv')
pnames = c()
pteams = c()
for (i in 1:length(pnames)){
  pit$Team[which(pit$Name == pnames[i])]<-pteams[i]
}
write.csv(pit,file = 'data_prep/pitchers.csv',row.names = F)



## EDIT ROSTERS

### Hitters

load('rdata/hit_draft.rda')

nm = 'J.T. Realmuto'
tm = 'DV'

hit_draft$Team[which(hit_draft$Name == nm)] <- tm

save(hit_draft, file = 'rdata/hit_draft.rda')

### Pitchers

load('rdata/pit_draft.rda')

nm = 'Reed Garrett'
tm = 'HOM'

pit_draft$Team[which(pit_draft$Name == nm)] <- tm

save(pit_draft, file = 'rdata/pit_draft.rda')




