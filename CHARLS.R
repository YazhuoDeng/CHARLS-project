
#Citation
#Deng, Y., & Paul, D. R. (2018). The Relationships between depressive symptoms, 
#functional health status, physical activity, and the availability of recreational 
#facilities: a rural-urban comparison in middle-aged and older Chinese adults. 
#International journal of behavioral medicine, 25(3), 322-330.

#CHARLS
library(foreign)
library(psy)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mapdata)
library(lubridate)
library(effsize)

#########################################
#the funcation counts the # of missing value for each variable
propmiss <- function(dataframe) {
m <- sapply(dataframe, function(x) {
data.frame(
nmiss=sum(is.na(x)), 
n=length(x), 
propmiss=sum(is.na(x))/length(x)
		)
	})
d <- data.frame(t(m))
d <- sapply(d, unlist)
d <- as.data.frame(d)
d$variable <- row.names(d)
	row.names(d) <- NULL
d <- cbind(d[ncol(d)],d[-ncol(d)])
return(d[order(d$propmiss), ])
}

#########################################

#community data 2011
community<-read.dta("community2011.dta")
names(community)
length(community$communityID)
#JB029
facility<-community[,c("communityID","sub_commuID","jb029_1_1_","jb029_1_2_","jb029_1_3_","jb029_1_4_","jb029_1_5_","jb029_1_6_","jb029_1_11_","jb029_1_14_")]
names(facility)

##############################################
#create variable: how may types of facility in this community?
##############################################

#change all yes or no factors to numeric (1=yes,2=no)
facility<-as.data.frame(sapply(facility, as.numeric))
#change missing vaule and 2 to 0 for further add-up
facility[,3:10]<-lapply(facility[,3:10], function(x) replace(x, x %in% c(2,NA), 0))
propmiss(facility)

facility$type_number<-rowSums(facility[3:10])
table(facility$type_number)
hist(facility$type_number)

facility<-facility[,c("communityID","type_number")]

class(facility$communityID)

#---------------------------------------------
#add province name and urban and rural code variable
psu<-read.dta("psu.dta")

#psu1<-read.table("Health_Status_and_Functioning.tab", header=T, sep="\t", fill=T)



names(psu)
table(psu$urban_nbs)
#Rural Urban 
 # 237   213 

#choose only rural and urban variable 
psu<-psu[,-c(2,3,5)]
# code urban variable as numeric 1=rural 2=urban
psu$urban_nbs<-as.numeric(psu$urban_nbs)

#convert communityID to numeric
psu$communityID<-as.numeric(psu$communityID)

facility<-full_join(psu,facility,by="communityID")
#facility<-merge(psu, facility, by=c("communityID"))

table(facility$urban_nbs)

#############################################
#---------------------------------------------
#demographic data

#get the birth date 2011
demogr11<-tbl_df(read.dta("Demographic_Background2011.dta"))
names(demogr11)

#############################################
#transfer 2011 ID to 2013 ID
#2013 ID: householdID + "0" + last two diget of ID
demogr11$ID<-paste(demogr11$householdID,substr(demogr11$ID,10,11),sep="0")
#############################################

#birth year 
table(demogr11$ba002_1)
#birth month
table(demogr11$ba002_2)
#birth day
table(demogr11$ba002_3)

birth2011<-select(demogr11, ID,ba002_1,ba002_2,ba002_3)
propmiss(birth2011)
#birth2011$birthDate<-ymd(paste(birth2011$ba002_1,birth2011$ba002_2,birth2011$ba002_3,spe="-"))

#get birth date 2013
demogr13<-tbl_df(read.dta("Demographic_Background2013.dta"))
#get the birth year 2013
birth2013<-select(demogr13,ID,ba002_1,ba002_2,ba002_3)
propmiss(birth2013)
#birth2013$birthDate<-ymd(paste(birth2013$ba002_1,birth2013$ba002_2,birth2013$ba002_3,spe="-"))

#see how many IDs are matched between 2011 and 2013
length(intersect(birth2011$ID,birth2013$ID))


#fill 2011 birth year to 2013 
birth2013$ba002_1[is.na(birth2013$ba002_1)]<-birth2011$ba002_1[match(birth2013$ID[is.na(birth2013$ba002_1)],birth2011$ID)]

#test
birth2011[birth2011$ID=="326359216002",]
birth2013[birth2013$ID=="326359216002",]

#now only 2013 birth year is needed
table(birth2013$ba002_1)

#calculate age by year
sum(!is.na(birth2013$ba002_1))
birth2013$age<-2013-birth2013$ba002_1
table(birth2013$age)
age2013<-select(birth2013,ID,age)

age2013<-as.data.frame(sapply(age2013, as.numeric))

##############################################

#manage demo variables
demo<-demogr13[,c("ID", "householdID", "communityID","ba000_w2_3","be001")]

#gender
table(demo$ba000_w2_3)
#1=male 2=female 

#married status
table(demo$be001)
# 1 Married with spouse present                        #15069 
#2 Married but not living with spouse temporarily for reasons such as work                   #1067                                                              #3 Separated                                                                       #67                                                               #4 Divorced                                                                      #155                                                                 #5 Widowed                                                                      #2055                                                          #6 Never married                                                                      #156                                                            #7 Cohabitated                                                                       #16 

#change all yes or no factors to numeric
demo[,-c(1:3)]<-tbl_df(sapply(demo[,-c(1:3)], as.numeric))

dim(demo)

#add age
demo<-as.data.frame(sapply(demo, as.numeric))
demo<-left_join(demo,age2013, by="ID")



#---------------------------------------------
#health status and functioning data 2013
#health<-read.dta("Health_Status_and_Functioning2013.dta")
health<-read.table("Health_Status_and_Functioning.tab", header=T, sep="\t", fill=T)

#physical activity data
#DA051

PA<-health[,c(1,428:446)]
names(PA)
table(PA$da051_1_)

#create a new vigorous activities variable
PA$vig[PA$da051_1_==2]<-0
PA$vig[PA$da054_1_==1]<-1
PA$vig[PA$da054_1_==2]<-2
PA$vig[PA$da055_1_==1]<-3
PA$vig[PA$da055_1_==2]<-4
table(PA$vig)
sum(is.na(PA$vig))
#insert 0 activity to 0 days in question how many days
PA$da052_1_[PA$vig=="0"]<-0
PA$vig_total<-PA$vig*PA$da052_1_
#create leisure activity varible
PA$vig_leisure<-PA$vig_total
PA$vig_leisure[PA$da051_1_1_=="1"]<-0
PA$vig_leisure[PA$da054_1_1_=="4"]<-0
#non leisure activity
PA$vig_nonLeisure<-PA$vig_total
PA$vig_nonLeisure[PA$da051_1_1_=="2"]<-0
PA$vig_nonLeisure[PA$da054_1_1_=="3"]<-0

#create a new moderate activities variable
PA$mod[PA$da051_2_==2]<-0
PA$mod[PA$da054_2_==1]<-1
PA$mod[PA$da054_2_==2]<-2
PA$mod[PA$da055_2_==1]<-3
PA$mod[PA$da055_2_==2]<-4
table(PA$mod)
sum(is.na(PA$mod))

PA$da052_2_[PA$mod=="0"]<-0
PA$mod_total<-PA$mod*PA$da052_2_
#create leisure activity varible
PA$mod_leisure<-PA$mod_total
PA$mod_leisure[PA$da051_1_2_=="1"]<-0
PA$mod_leisure[PA$da054_1_2_=="4"]<-0
#non leisure activity
PA$mod_nonLeisure<-PA$mod_total
PA$mod_nonLeisure[PA$da051_1_2_=="2"]<-0
PA$mod_nonLeisure[PA$da054_1_2_=="3"]<-0

#create a new walking activities variable
PA$walk[PA$da051_3_==2]<-0
PA$walk[PA$da054_3_==1]<-1
PA$walk[PA$da054_3_==2]<-2
PA$walk[PA$da055_3_==1]<-3
PA$walk[PA$da055_3_==2]<-4
table(PA$walk)
sum(is.na(PA$walk))
PA$walk<-as.numeric(PA$walk)
PA$da052_3_[PA$walk=="0"]<-0
PA$walk_total<-PA$walk*PA$da052_3_

#create leisure activity varible
PA$walk_leisure<-PA$walk_total
PA$walk_leisure[PA$da051_1_3_=="1"]<-0
PA$walk_leisure[PA$da054_1_3_=="4"]<-0
#non leisure activity
PA$walk_nonLeisure<-PA$walk_total
PA$walk_nonLeisure[PA$da051_1_3_=="2"]<-0
PA$walk_nonLeisure[PA$da054_1_3_=="3"]<-0

PA<-select(PA,ID,vig_total,mod_total,walk_total,vig_leisure,mod_leisure,walk_leisure,vig_nonLeisure,mod_nonLeisure,walk_nonLeisure)

####################################################
#old data version 
####################################################
#create a new vigorous activities variable
PA$vig[PA$da051_1_=="2 No"]<-0
PA$vig[PA$da054_1_=="1 <30 Minutes"]<-1
PA$vig[PA$da054_1_=="2 >=30 Minutes"]<-2
PA$vig[PA$da055_1_=="1 <4 Hours"]<-3
PA$vig[PA$da055_1_=="2 >=4 Hours"]<-4
table(PA$vig)
sum(is.na(PA$vig))
PA$vig<-as.numeric(PA$vig)
PA$da052_1_[PA$vig=="0"]<-0
PA$vig_index<-PA$vig*PA$da052_1_

#create a new moderate activities variable
PA$mod[PA$da051_2_=="2 No"]<-0
PA$mod[PA$da054_2_=="1 <30 Minutes"]<-1
PA$mod[PA$da054_2_=="2 >=30 Minutes"]<-2
PA$mod[PA$da055_2_=="1 <4 Hours"]<-3
PA$mod[PA$da055_2_=="2 >=4 Hours"]<-4
table(PA$mod)
sum(is.na(PA$mod))
PA$mod<-as.numeric(PA$mod)
PA$da052_2_[PA$mod=="0"]<-0
PA$mod_index<-PA$mod*PA$da052_2_

#create a new walking activities variable
PA$walk[PA$da051_3_=="2 No"]<-0
PA$walk[PA$da054_3_=="1 <30 Minutes"]<-1
PA$walk[PA$da054_3_=="2 >=30 Minutes"]<-2
PA$walk[PA$da055_3_=="1 <4 Hours"]<-3
PA$walk[PA$da055_3_=="2 >=4 Hours"]<-4
table(PA$walk)
sum(is.na(PA$walk))
PA$walk<-as.numeric(PA$walk)
PA$da052_3_[PA$walk=="0"]<-0
PA$walk_index<-PA$walk*PA$da052_3_

mean(PA$walk_index,na.rm=T)
mean(PA$mod_index,na.rm=T)
mean(PA$vig_index,na.rm=T)
var(PA$walk_index,na.rm=T)
var(PA$mod_index,na.rm=T)
var(PA$vig_index,na.rm=T)

PA<-PA[,c(1,21,23,25)]
names(PA)

#####################
#physical function mobility
physiFunc<-select(health,db001,db002,db003,db004,db005,db006,db007,db008,db009)

#change all yes or no factors to numeric (1=yes,2=no)
physiFunc<-as.data.frame(sapply(physiFunc, as.numeric))

propmiss(physiFunc)

#replace the skiped values
physiFunc$db002[is.na(physiFunc$db002)]<-physiFunc$db001[is.na(physiFunc$db002)]

physiFunc$db003[is.na(physiFunc$db003)]<-physiFunc$db002[is.na(physiFunc$db003)]

#      1 No, I Don't Have Any Difficulty 
#                                   8281 
#2 I Have Difficulty but Can Still Do It 
#                                   1761 
# 3 Yes, I Have Difficulty and Need Help 
#                                    276 
#                      4 I Can Not Do It 
#                                   7314 

#######################
#instrumental activities of daily
IADL<-health[,c("db016","db017","db018","db019","db020")]
summary(IADL)
#convert factor to numeric data
IADL$db016<-as.numeric(IADL$db016)
IADL$db017<-as.numeric(IADL$db017)
IADL$db018<-as.numeric(IADL$db018)
IADL$db019<-as.numeric(IADL$db019)
IADL$db020<-as.numeric(IADL$db020)

#depressive symptoms
#DC009 I was bothered by things that don’t usually bother me. 我因一些小事而烦恼。
#DC010 I had trouble keeping my mind on what I was doing. 我在做事时很难集中精力。
#DC011 I felt depressed. 我感到情绪低落。
#DC012 I felt everything I did was an effort. 我觉得做任何事都很费劲。
#DC013 I felt hopeful about the future. 我对未来充满希望。
#DC014 I felt fearful. 我感到害怕。
#DC015 My sleep was restless. 我的睡眠不好。
#DC016 I was happy. 我很愉快
#DC017 I felt lonely. 我感到孤独。
#DC018 I could not get ”going.” 我觉得我无法继续我的生活。
depression<-health[,c(1189:1198)]
summary(depression)
#convert factor to numeric data
depression<-as.data.frame(sapply(depression, as.numeric))

#library(psy)
cronbach(depression[,c(1,2,3,4,6,7,9,10)])
cronbach(physiFunc)
cronbach(IADL)
cronbach(PA[,c(2:4)])

#create the function and behavior data frame
behavior<-cbind(PA,IADL,depression)
names(behavior)
behavior<-left_join(demo,behavior, by="ID")
names(behavior)

#delete cases if they have all missing values for certain variables 

behavior<-behavior[rowSums(is.na(behavior[,c("vig_total","mod_total","walk_total")]))<length(behavior[,c("vig_total","mod_total","walk_total")]),]
propmiss(behavior)


#merge data 

mydata<-left_join(behavior, facility, by="communityID")
#mydata<-merge(behavior, facility, by ="communityID")
#mydata<-distinct(mydata)
names(mydata)
dim(mydata)

#delete replicated cases
mydata<-mydata[!duplicated(mydata$ID),]
#check which variable is factor
sapply(mydata,is.factor)

#summarize missingness
propmiss(mydata)

#remove age < 45 
mydata<-filter(mydata,age>=45)

#> 6085-5949
#[1] 136



#replace missing value to -998
#mydata[is.na(mydata)]<--998


###############################################
#impute as median

imputeMedian=function(x){
   x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
   x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
   x #display the column
}
#mydata<-data.frame(apply(mydata,2,imputeMedian))

###############################################
library(mice)

#using predictive mean matching
#can change m=# # of imputations
mydataImp<-mice(data=mydata[,4:32], m=2, seed=22)

#extract the first imputed data (out of 25 imputations)
#action=1----choose first impuated data
mydataImp1<-complete(mydataImp, action=1)
propmiss(mydataImp1)
names(mydataImp1)
mydataImp1<-cbind(mydata[,1:3],mydataImp1)
#mydataImp1<-sapply(mydataImp1,as.numeric)

###############################################
#total PA and leiaure PA score

mydataImp1$TPA<-mydataImp1$vig_total*8+mydataImp1$mod_total*4+mydataImp1$walk_total*3.3

mydataImp1$LPA<-mydataImp1$vig_leisure*8+mydataImp1$mod_leisure*4+mydataImp1$walk_leisure*3.3


###############################################

#summary stat
by(mydataImp1, mydataImp1$urban_nbs,summary)


###############################################


library(foreign)
#sep="\t" tells r that the file is tab-delimited, use " " for space delimited, "," for comma delimited
#col names and row names need to be deleted
write.table(mydata, "mydata.dat",col.names=F,row.names=F,sep="\t")

write.table(mydataImp1, "mydataImp1.dat",col.names=F,row.names=F,sep="\t")


###############################################
#community basic information
###############################################

#community data 2011
community<-read.dta("community2011.dta")
names(community)

comm_bio<-community[,c("communityID","sub_commuID","ja003", "ja003_1", "ja003_2", "jc001")]
comm_bio$communityID<-as.numeric(comm_bio$communityID)

names(facility)

mydata_community<-as.data.frame(unique(mydata$communityID))
colnames(mydata_community)<-"communityID"
nrow(mydata_community)

mycommunity<-left_join(mydata_community,comm_bio,by="communityID")
mycommunity<-left_join(mycommunity,facility,by="communityID")

mycommunity<-mycommunity[!duplicated(mycommunity$communityID),]

mycommunity$ja003_2<-round(mycommunity$ja003_2/1500,2)
mycommunity$ja003_1[is.na(mycommunity$ja003_1)]<-mycommunity$ja003_2[is.na(mycommunity$ja003_1)]

table(mycommunity$ja003_1[mycommunity$urban_nbs==1])

mean(mycommunity$ja003_1[mycommunity$ja003_1<225 & mycommunity$urban_nbs==1],na.rm=T)
sd(mycommunity$ja003_1[mycommunity$ja003_1<225 & mycommunity$urban_nbs==1],na.rm=T)

mean(mycommunity$ja003_1[mycommunity$ja003_1<225 & mycommunity$urban_nbs==2],na.rm=T)
sd(mycommunity$ja003_1[mycommunity$ja003_1<225 & mycommunity$urban_nbs==2],na.rm=T)

#population
mean(mycommunity$jc001[mycommunity$urban_nbs==1],na.rm=T)
sd(mycommunity$jc001[mycommunity$urban_nbs==1],na.rm=T)

mean(mycommunity$jc001[mycommunity$urban_nbs==2],na.rm=T)
sd(mycommunity$jc001[mycommunity$urban_nbs==2],na.rm=T)




###############################################






































