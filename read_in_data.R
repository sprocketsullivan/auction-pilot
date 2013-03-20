#read in data
base.dir<-("D:\\UlfT\\Experimente\\Human_experiments\\Auction\\Pilot1\\")
dir(base.dir)
#read in participant data
part.data<-read.table(paste(base.dir,"Probanden.csv",sep=""),header=T,sep=";",dec=".")
for (i in 1:nrow(part.data)){
  if(i==1) {
    my.data<-read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=2,sep=",",nrow=199)    
    my.data$id<-i
    pref_before<-read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=1,sep=",",nrow=1)    
    pref_after<-read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=201,sep=",",nrow=1)    
  }
  else
  {
    help.data<-read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=2,sep=",",nrow=199)
    help.data$id<-i
    my.data<-rbind(my.data,help.data)
    pref_before<-rbind(pref_before,read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=1,sep=",",nrow=1))    
    pref_after<-rbind(pref_after,read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=201,sep=",",nrow=1))
  }
}
#remove column with NAs
my.data<-my.data[,-10]
rm(help.data)
names(my.data)<-c("item","preference","own_bid","other_bid","lottery","a_won","l_won","t_click1","t_click2","id")
#for player 2 preferences were reversed!
my.data$preference[my.data$id==2]<-6-my.data$preference[my.data$id==2]

names(my.data)
###############
#preference other player
##############
my.data$pref_o<-0
part.data$coop_num<-0
for(i in 1:nrow(part.data)){
  if(part.data$computer[i]==1) part.data$coop_num[i]<-part.data$part_num[part.data$session==part.data$session[i]&part.data$computer==3]
  if(part.data$computer[i]==2) part.data$coop_num[i]<-part.data$part_num[part.data$session==part.data$session[i]&part.data$computer==4]
  if(part.data$computer[i]==3) part.data$coop_num[i]<-part.data$part_num[part.data$session==part.data$session[i]&part.data$computer==1]
  if(part.data$computer[i]==4) part.data$coop_num[i]<-part.data$part_num[part.data$session==part.data$session[i]&part.data$computer==2]
}
for(i in 1:max(my.data$item))
  for(j in 1:max(my.data$id))
    my.data$pref_o[my.data$item==i&my.data$id==j]<-my.data$own_bid[my.data$item==i&my.data$id==part.data$coop_num[part.data$part_num==j]&my.data$item_bid_count==1]
#calculate competition

my.data$competition<-(5-abs(my.data$preference-my.data$pref_o))/(my.data$preference+my.data$pref_o)
my.data$trial<-rep(seq(1,length(my.data$id[my.data$id==1])),max(my.data$id))
my.data$first_item_bid<-0
for (i in 1:max(my.data$item))
  for(j in 1:max(my.data$id))
    my.data$first_item_bid[my.data$item==i&my.data$id==j][1]<-1
my.data$item_bid_count<-0
for (i in 1:max(my.data$item))
  for(j in 1:max(my.data$id))
    my.data$item_bid_count[my.data$item==i&my.data$id==j]<-seq(1,length(my.data$item_bid_count[my.data$item==i&my.data$id==j]))
with(subset(my.data,item_bid_count>=20),aggregate(own_bid,list(id,item),mean))$x
my.data$own_value<-my.data$own_bid*my.data$lottery/100
helper.sort<-cbind(my.data$own_bid[my.data$first_item_bid==1]*my.data$lottery[my.data$first_item_bid==1]/100,my.data$item[my.data$first_item_bid==1],my.data$pref_compare[my.data$first_item_bid==1],my.data$id[my.data$first_item_bid==1],my.data$preference[my.data$first_item_bid==1])
helper.sort<-data.frame(helper.sort)
helper.sort<-helper.sort[order(helper.sort$X2,helper.sort$X4),]
helper.sort$last_bids<-with(subset(my.data,item_bid_count>=20),aggregate(own_value,list(id,item),mean))$x
names(helper.sort)<-c("ini_bid","item","pref_comp","id","preference","last_bids")
helper.sort$utility_change<-(helper.sort$ini_bid-helper.sort$last_bids)
require(ggplot2)
ggplot(aes(x=pref_comp,y=utility_change,col=factor(item)),data=helper.sort)+geom_line()+geom_point()+facet_wrap(~id)
ggplot(aes(x=factor(pref_comp),y=utility_change),data=helper.sort)+geom_boxplot()
with(helper.sort,aggregate(last_bids-ini_bid,list(preference),mean))
ggplot(aes(y=utility_change,x=factor(preference)),data=helper.sort)+geom_boxplot()

ggplot(aes(y=ini_bid,x=preference),data=helper.sort)+geom_point()+facet_wrap(~id)
with(helper.sort,aggregate(ini_bid,list(preference),mean))

names(my.data)
my.data$pref_compare<-0
my.data$pref_compare<-with(my.data,ifelse(preference>pref_o,1,ifelse(preference<pref_o,-1,0)))
with(my.data,aggregate(own_bid,list(id,preference),var))




names(my.data)

