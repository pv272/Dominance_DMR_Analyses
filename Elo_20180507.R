
##########The aim of the project is to compute the dominance index of each individual in a group and to understand what predicts such score 



#########Get Package

library(RMySQL)
library(getPass)
library(EloRating)
library(tidyverse)
library(rstan)
library(lubridate)


##################################################################################### connect to Molerat database to extract the information related to submissive calls in scan and focals
#####################################################################################

con <- dbConnect(MySQL(), user = 'philippev',password = getPass(),  
                 dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')

set.seed(123)

##################################################################################### Get Scan sub call 
#####################################################################################

##### Extract sub call collected during the scan
ScanCall <- con %>%
  dbGetQuery("SELECT *
FROM user_philippev.SubCall_Scan
") %>% 
mutate(ObsDate=as.Date(ObsDate))


##################################################################################### Get focal sub call
#####################################################################################

FocalCall <- con %>%
  dbGetQuery("SELECT * FROM user_philippev.SubCall_Focal") %>% 
mutate(ObsDate=as.Date(ObsDate))



######################################################################################Data wrangling on the focal data 
#####################################################################################

#We would like to have the winner and the loser in a different column. In the current table this is given by the value in the column Received where 0 means winner and 1 means loser

###Make use of the Received modifier to determine the winner and loser of each interaction and put the focal data in a format that it can be joined to scan data 
FocalCall_Tidy<- FocalCall %>% 
  filter (Received == 1) %>% 
  rename(Loser=AnimalID, Winner=Partner) %>% 
  bind_rows(FocalCall %>% 
  filter (Received == 0) %>% 
  rename(Winner=AnimalID, Loser=Partner)) %>% 
  select(-Received) 

str(FocalCall_Tidy)



##################################################################################### connect to Molerat database to extract the group composition
#####################################################################################

#getting the exact group composition will be necessary to exclude from the data all interactions that involved an animal that was currently not present in the group 

#need update the list of date until today 
con %>%dbGetQuery("CALL user_philippev.FillCalendar('2013-02-02', NOW())")

#extract updated group Composition from the database. This gives for every day since the beginning of the project the exact group composition of every group



names(GroupComp)
View(GroupComp)
str(GroupComp)


########################################################################################Merge Join focal and Scan Data.
#####################################################################################

#If I want to add the HBO1 observation 
HBO1_Call<-read.csv('SubCall_HBO1_All.csv') %>% 
  mutate(ObsType='Scan',BehaviourCount=1,NbCall=NBcall,Date=ymd(Date)) %>% 
  select(Colony,Date,Time,Loser,BehaviourCount,Winner,Context,NbCall,ObsType) %>% 
  mutate(Context=ifelse(Context=='Unclear',0,
    ifelse(Context=='Self initiated',1,
      ifelse(Context=='Resp to Aggress',2,
        ifelse(Context=='in soc interact',3,4)))))#change the context into a number. But I should really do it with text for the dominance analyses because the number of context id different between scan and focal. Also, the context in project may be different than the one associated with code in DB. 


#####give a unique ID to each interaction
#####randomize the interaction within Focal 
#####Give a unique reference to each interaction







#The start time is not in the same format in HBO1 as it is for focal and scan  

HBO1_Call %>% distinct(Context2)

str(HBO1_Call)
names(HBO1_Call)
names(ScanCall)
View(HBO1_Call)

#The context given a number is wrong BECAUSE the value between scan and focal are not corresponding. Thus I would beed to bering the text. But for now it has no importance
SubCall_All<-FocalCall_Tidy %>% 
  bind_rows(ScanCall) %>% 
  bind_rows(HBO1_Call) %>% #remove if I want not to add the HBO1 calls
  filter (Winner != Loser) %>% #eliminate animals that interacts with themselves
  inner_join(.,GroupComp,by= c("Date" = "Date","Loser" = "AnimalID","Colony"="Colony" )) %>% #this is to exclude rows where recorded loser were not part of the correct group
  rename(LoserRef=AnimalRef) %>% 
inner_join(.,GroupComp,by= c("Date" = "Date","Winner" = "AnimalID","Colony"="Colony" )) %>% #this is to exclude rows where recorded winner were not part of the correct group
    rename(WinnerRef=AnimalRef) %>% 
  lapply(., function(x) rep(x,.$BehaviourCount)) %>% #to repeat the rows for interactions that have happened several times
  as.data.frame(.)#to convert as data frame as lapply creates a list 

View(SubCall_All)
names(SubCall_All)

######Animal that only interacted once in case they would be useful to exclude
test<-SubCall_All %>% 
  group_by(Winner) %>% 
  summarise(NbInteraction=n()) %>% 
filter(NbInteraction<2)
View(test)

#######################################################################################add individual characteristics to the sub call data 
######################################################################################

#Sex
#Birth date which I need for age 
#Weight 
#Weight Rank
#breeding status 


####get the query individual characteristic from the DB 
ID_Characteristic<-con %>%
  dbGetQuery("SELECT *
FROM user_philippev.ID_Characteristic") %>% 
  mutate(BirthDate=as.Date(BirthDate)) %>% 
  rename(AnimalRef=Rowref)
head(ID_Characteristic)

####add Sex and age 
SubCall_D1<-inner_join(SubCall_All,ID_Characteristic %>% 
      select (AnimalID,Sex,BirthDate),by=c("Loser"="AnimalID")) %>% #add loser sex
  rename(LoserSex=Sex,LoserDOB=BirthDate) %>% 
  inner_join(.,ID_Characteristic %>% 
      select (AnimalID,Sex,BirthDate),by=c("Winner"="AnimalID")) %>% #add winner sex
  rename(WinnerSex=Sex,WinnerDOB=BirthDate) %>% 
  mutate(LoserAge=Date-LoserDOB,WinnerAge=Date-WinnerDOB)#compute age in days
View(SubCall_D1)

#########################################################################################add weight
#####################################################################################


#get weight from DTB
Weight<-con %>%
  dbGetQuery("SELECT *
FROM MR_MainData.tblWeights") %>% 
select(AnimalRef,DateWeighed,Weight) %>%
mutate(DateWeighed=as.Date(DateWeighed))
head(Weight)

###get the weight of all animal present in the colony when there was observation 
GroupComp_S<-SubCall_D1 %>% 
  distinct(Date,Colony,Sex) %>% #there are 2383 combination of date and colony
  inner_join(.,GroupComp) %>%  #there are 28263  individual weights that need be found which is less than the 600000
  inner_join(.,Weight) %>% #Join weight data
  inner_join(.,ID_Characteristic %>% select(AnimalRef,Sex)) %>% 
  mutate(DayDiff=as.integer(abs(DateWeighed-Date))) %>% #compute weight difference
  group_by(Colony,Date,AnimalRef,AnimalID,Sex) %>% 
  filter(DayDiff==min(DayDiff)) %>% #return min day diff
  ungroup() %>% 
  group_by(Colony,Date,AnimalRef,AnimalID,Sex,DayDiff) %>% 
  summarize(Weight=mean(Weight)) %>% #mean of the min weight
  ungroup() %>%
  group_by(Colony,Date,Sex) %>% 
  mutate(QueueRank=dense_rank(desc(Weight))) %>% #weight rank by sex, could do it after excluding breeders
  ungroup() %>%
  group_by(Colony,Date) %>% 
  mutate(WeightRank=dense_rank(desc(Weight))) %>% #weight rank not according to sex, could do it after excluding breeders
  ungroup() %>% 
  arrange(AnimalID)

#####################################################################################Aattach the loser and winner information 
#####################################################################################
names(SubCall_D1)
dim(SubCall_D1)
names(GroupComp_S)
dim(GroupComp_S)



SubCall_D2<-inner_join (SubCall_D1, GroupComp_S %>% select(Date,AnimalID,Weight,QueueRank,WeightRank),by=c("Loser"="AnimalID","Date")) %>%
  rename(Loser_Weight=Weight,Loser_QueueRank=QueueRank,Loser_WeightRank=WeightRank) %>% inner_join(.,GroupComp_S %>% select(Date,AnimalID,Weight,QueueRank,WeightRank),by=c("Winner"="AnimalID","Date")) %>%
  rename(Winner_Weight=Weight,Winner_QueueRank=QueueRank,Winner_WeightRank=WeightRank)
  
View(SubCall_D2)



dim(SubCall_D2 %>% 
  filter(Winner=="G3F002"))


dim(SubCall_D2 %>% 
  filter(Loser=="G3f002"))














#####get the closest weight: GroupComp contains the group composition for evrey group for every day since the beginning of the project (1 entry per animal per day). For every row we would want to add a weight, that was collected at the closest time since animal are not weighed every day. Thus for each row of GroupComp we have to calculatetthe day difference with the dates of weight collection of that animal from the Weight table. Day difference can be positive or negative so we take the absolute value and return the minimum difference of day and the weight associated to it. If there are several weight with the same day difference we average them  


#using a for loop, Jack's way 
head(GroupComp)

GroupComp$DayDiff <- NA # create a day difference column, time between weight events
GroupComp$Weight <- NA  # create a weight column, weight at the closest distance

for (j in 1:nrow(GroupComp )) {

  print(j)  
  WeightSub <- subset(Weight , AnimalRef == as.character(GroupComp$Rowref[j]))
  Dates <- as.numeric(difftime(WeightSub$Date, as.Date(GroupComp$Date[j]), units = "d"))

  GroupComp$Weight[j] <-   mean(WeightSub$Weight[which(abs(Dates) == min(abs(Dates)))], na.rm = TRUE)
  GroupComp$DayDiff[j] <- min(abs(Dates))[1]
}

head(GroupComp)



#####################get the closest weight using a for loop, Philippe Way. I actually never got the code running til the end because it took so long 
WeightSub<-Weight %>% filter(AnimalRef == GroupComp$Rowref[1])#selects all weight info of the animal in the first row of the GroupComp file 

head(Weight)

for (j in 1:nrow(GroupComp )) {

WeightSub<-Weight %>% filter(AnimalRef == GroupComp$Rowref[j])

Temp<-inner_join(GroupComp[j,] %>% select(Rowref,Date),
   WeightSub,by=c("Rowref"="AnimalRef")) %>% 
  mutate(DayDiff=as.integer(abs(DateWeighed-Date))) %>%
  filter(DayDiff==min(DayDiff))

GroupComp$DayDiff[j]<-Temp %>% 
  select(DayDiff) %>% 
  distinct()

GroupComp$Weight[j]<-Temp %>% 
  summarise(Weight=mean(Weight)) %>% 
  select(Weight)
}





  

  






    

