####2020 ANES data for Sorting project####
#Created by Ethan Busby
#13 June 2022
####First, set working directory and read in data####
#setwd("C:/Users/busby89/Google Drive/Sorting Project/Data sources/2020 ANES")
setwd("~/datasets/anes/")
library(readstata13)
library(survey)
library(ggplot2)
anes20 <- read.dta13("anes_timeseries_2020_stata_20220210.dta")
#Saving files for other purposes
#write.csv(anes20, file="2020 ANES.csv", row.names = F)
#setwd("C:/Users/busby/Box/AIProject/ANES Mutual Information/ANES data")
#anes12 <- read_dta("anes_timeseries_2012.dta")
#write.csv(anes12, file="2012 ANES.csv", row.names = F)

View(anes20)
table(anes20$V200003)
prop.table(table(anes20$V200003))

####DATA CLEANING AND INDEX CONSTRUCTION####
#Party as a set of binary variables
anes20$dem=ifelse(anes20$V201231x>0&anes20$V201231x<4, 1, 0)
anes20$rep=ifelse(anes20$V201231x>4, 1, 0)

#continuous party ID (low values, dem)
anes20$pid7=anes20$V201231x
anes20$pid7[anes20$V201231x<0]=NA


#Variables we'll use:
#Ideology
anes20$lib=ifelse(anes20$V201200>0&anes20$V201200<4, 1, 0)
anes20$mod=ifelse(anes20$V201200==4|anes20$V201200==99, 1, 0)
anes20$con=ifelse(anes20$V201200>4&anes20$V201200!=99, 1, 0)
#continuous ideology
anes20$ideo=anes20$V201200
anes20$ideo[anes20$V201200<0]=NA
anes20$ideo[anes20$V201200==99]=4
#Reverse code so low is conservative
anes20$ideo=abs(anes20$ideo-8)

#Sex
anes20$male=ifelse(anes20$V201600==1, 1,0)
anes20$female=ifelse(anes20$V201600==2, 1,0)
#Race
anes20$white=ifelse(anes20$V201549x==1, 1,0)
anes20$black=ifelse(anes20$V201549x==2, 1,0)
anes20$asian=ifelse(anes20$V201549x==4, 1,0)
anes20$nativeamerican=ifelse(anes20$V201549x==5, 1,0)
anes20$multiracial=ifelse(anes20$V201549x==6, 1,0)
#Ethnicity
anes20$hisp=ifelse(anes20$V201546==1, 1,0)
#Household is union membership
anes20$union=ifelse(anes20$V201544==1, 1, 0)
#Age
anes20$age=ifelse(anes20$V201507x>0, anes20$V201507x, NA)
#Age range
anes20$age_18=ifelse(anes20$V201507x>0&anes20$V201507x<30, 1, 0)
anes20$age_30=ifelse(anes20$V201507x>29&anes20$V201507x<45, 1, 0)
anes20$age_45=ifelse(anes20$V201507x>44&anes20$V201507x<65, 1,0)
anes20$age_65=ifelse(anes20$V201507x>64, 1, 0)
#Community type (this one is on the post wave)
anes20$urban=ifelse(anes20$V202356==1, 1,0)
anes20$suburb=ifelse(anes20$V202356==2, 1,0)
anes20$smalltown=ifelse(anes20$V202356==3, 1,0)
anes20$rural=ifelse(anes20$V202356==4, 1,0)
#Evangelical
anes20$evang=ifelse(anes20$V201459==2 | anes20$V201459==3, 1, 0)
#Armed forces/veteran
anes20$military=ifelse(anes20$V201516==1 | anes20$V201516==2, 1, 0)
#Gun owner
anes20$gun=ifelse(anes20$V201628>0, 1, 0)
#Class (post wave)
anes20$lowerclass=ifelse(anes20$V202352==1, 1,0)
anes20$workingclass=ifelse(anes20$V202352==2, 1,0)
anes20$middleclass=ifelse(anes20$V202352==3, 1,0)
anes20$upperclass=ifelse(anes20$V202352==4, 1,0)
#Education
anes20$lessHS=ifelse(anes20$V201511x==1, 1, 0)
anes20$HS=ifelse(anes20$V201511x==2, 1, 0)
anes20$somecollege=ifelse(anes20$V201511x==3, 1, 0)
anes20$college=ifelse(anes20$V201511x==4, 1, 0)
anes20$graduate=ifelse(anes20$V201511x==5, 1, 0)
#Parent's immigrants?
anes20$bothparentsUS=ifelse(anes20$V201553==1, 1, 0)
anes20$oneparentUS=ifelse(anes20$V201553==2, 1, 0)
anes20$bothparentsnotUS=ifelse(anes20$V201553==3, 1, 0)
#Income
anes20$income=ifelse(anes20$V201617x>0, anes20$V201617x, NA)
anes20$income25K=ifelse(anes20$V201617x>0&anes20$V201617x<5, 1, 0)
anes20$income50K=ifelse(anes20$V201617x>4&anes20$V201617x<10, 1, 0)
anes20$income75K=ifelse(anes20$V201617x>9&anes20$V201617x<14, 1, 0)
anes20$income100K=ifelse(anes20$V201617x>13&anes20$V201617x<17, 1, 0)
anes20$income150K=ifelse(anes20$V201617x>16&anes20$V201617x<20, 1, 0)
anes20$income250K=ifelse(anes20$V201617x>19&anes20$V201617x<22, 1, 0)
anes20$incomemore250K=ifelse(anes20$V201617x==22, 1, 0)
#Marital status
anes20$married=ifelse(anes20$V201508==1 | anes20$V201508==2, 1, 0)
anes20$single=ifelse(anes20$V201508==3 | anes20$V201508==4 |
                       anes20$V201508==5 | anes20$V201508==6 , 1, 0)
#Sexual orientation
anes20$straight=ifelse(anes20$V201601==1, 1, 0)
anes20$gay=ifelse(anes20$V201601==2, 1, 0)
anes20$bi=ifelse(anes20$V201601==3, 1, 0)
#Children at home
anes20$children=ifelse(anes20$V201567>0, 1, 0)

#Racial resentment
anes20$RR_gen=anes20$V202301
anes20$RR_gen[anes20$RR_gen<0]=NA

anes20$RR_favor=anes20$V202300
anes20$RR_favor[anes20$RR_favor<0]=NA
anes20$RR_favor=abs(anes20$RR_favor-6)

anes20$RR_less=anes20$V202302
anes20$RR_less[anes20$RR_less<0]=NA

anes20$RR_try=anes20$V202303
anes20$RR_try[anes20$RR_try<0]=NA
anes20$RR_try=abs(anes20$RR_try-6)

library(psych)

alpha(cbind(anes20$RR_favor, anes20$RR_gen, anes20$RR_less, anes20$RR_try))
#Alpha is 0.88, no surprise there
fa(cbind(anes20$RR_favor, anes20$RR_gen, anes20$RR_less, anes20$RR_try))
#Good loadings

anes20$RR=(anes20$RR_favor+anes20$RR_gen+anes20$RR_less+anes20$RR_try)/4


#Let's split it into thirds and make binary variables
quantile(anes20$RR, probs = c(.3333, .6666), na.rm=TRUE)
anes20$RR1=ifelse(anes20$RR<=as.numeric(quantile(anes20$RR, probs = .3333, na.rm=TRUE)), 1, 0)
anes20$RR2=ifelse(anes20$RR>as.numeric(quantile(anes20$RR, probs = .3333, na.rm=TRUE))&
                  anes20$RR<=as.numeric(quantile(anes20$RR, probs = .6666, na.rm=TRUE)), 1, 0)
anes20$RR3=ifelse(anes20$RR>as.numeric(quantile(anes20$RR, probs = .6666, na.rm=TRUE)), 1, 0)

#Authoritarianism
anes20$auth1=ifelse(anes20$V202266==2, 1, 0)
anes20$auth1[anes20$V202266<0]=NA

anes20$auth2=ifelse(anes20$V202267==2, 1, 0)
anes20$auth2[anes20$V202267<0]=NA

anes20$auth3=ifelse(anes20$V202268==1, 1, 0)
anes20$auth3[anes20$V202268<0]=NA

anes20$auth4=ifelse(anes20$V202269==2, 1, 0)
anes20$auth4[anes20$V202269<0]=NA

anes20$AUTH=anes20$auth1+anes20$auth2+anes20$auth3+anes20$auth4

#Let's split it into thirds and make binary variables
quantile(anes20$AUTH, probs = c(.3333, .6666), na.rm=TRUE)
anes20$AUTH1=ifelse(anes20$AUTH<=as.numeric(quantile(anes20$AUTH, probs = .3333, na.rm=TRUE)), 1, 0)
anes20$AUTH2=ifelse(anes20$AUTH>as.numeric(quantile(anes20$AUTH, probs = .3333, na.rm=TRUE))&
                    anes20$AUTH<=as.numeric(quantile(anes20$AUTH, probs = .6666, na.rm=TRUE)), 1, 0)
anes20$AUTH3=ifelse(anes20$AUTH>as.numeric(quantile(anes20$AUTH, probs = .6666, na.rm=TRUE)), 1, 0)

#Egalitarianism
anes20$egal1=anes20$V202260
anes20$egal1[anes20$egal1<0]=NA
anes20$egal1=abs(anes20$egal1-6)

anes20$egal2=anes20$V202261
anes20$egal2[anes20$egal2<0]=NA

anes20$egal3=anes20$V202262
anes20$egal3[anes20$egal3<0]=NA

anes20$egal4=anes20$V202263
anes20$egal4[anes20$egal4<0]=NA
anes20$egal4=abs(anes20$egal4-6)

psych::alpha(cbind(anes20$egal1, anes20$egal2, anes20$egal3, anes20$egal4))
#Alpha is 0.76, no surprise there
fa(cbind(anes20$egal1, anes20$egal2, anes20$egal3, anes20$egal4))
#Good loadings; 1 and 4 are a bit lower, still over 0.5

anes20$EGAL=(anes20$egal1+anes20$egal2+anes20$egal3+anes20$egal4)/4

#Sexism
anes20$sexism1=anes20$V202286x
anes20$sexism1[anes20$sexism1<0]=NA

anes20$sexism2=anes20$V202290x
anes20$sexism2[anes20$sexism2<0]=NA
anes20$sexism2=abs(anes20$sexism2-8)

anes20$sexism3=anes20$V202291
anes20$sexism3[anes20$sexism3<0]=NA
anes20$sexism3=abs(anes20$sexism3-6)

anes20$sexism4=anes20$V202292
anes20$sexism4[anes20$sexism4<0]=NA
anes20$sexism4=abs(anes20$sexism4-6)

psych::alpha(cbind(anes20$sexism1, anes20$sexism2, anes20$sexism3, anes20$sexism4))
#Alpha is 0.76, no surprise there
fa(cbind(anes20$sexism1, anes20$sexism2, anes20$sexism3, anes20$sexism4))
#Good loadings; 1 and 4 are a bit lower, still over 0.5

anes20$SEXISM=(anes20$sexism1+anes20$sexism2+anes20$sexism3+anes20$sexism4)/4

#Nationalism
anes20$nat1=anes20$V202270
anes20$nat1[anes20$V202270<0]=NA
anes20$nat1=abs(anes20$nat1-6)

anes20$nat2=anes20$V202273x
anes20$nat2[anes20$V202273x<0]=NA
anes20$nat2=abs(anes20$nat2-6)

anes20$nat3=anes20$V202421
anes20$nat3[anes20$V202421<0]=NA
anes20$nat3=abs(anes20$nat3-5)

anes20$nat4=anes20$V202422
anes20$nat4[anes20$V202422<0]=NA
anes20$nat4=abs(anes20$nat4-5)

anes20$nat5=anes20$V202423
anes20$nat5[anes20$V202423<0]=NA
anes20$nat5=abs(anes20$nat5-5)

anes20$nat6=anes20$V202424
anes20$nat6[anes20$V202424<0]=NA
anes20$nat6=abs(anes20$nat6-5)

psych::alpha(cbind(anes20$nat1, anes20$nat2, anes20$nat3, anes20$nat4, anes20$nat5, anes20$nat6))
#Alpha is 0.79, pretty good.
fa((cbind(anes20$nat1, anes20$nat2, anes20$nat3, anes20$nat4, anes20$nat5, anes20$nat6)))
fa(cbind(anes20$nat1, anes20$nat2, anes20$nat3, anes20$nat4, anes20$nat5, anes20$nat6), nfactors=2)
#item 2 doesn't load well on a single dimension; could split it with 1,2,5,6 on one scale and 3,4 on the other
anes20$NAT=(anes20$nat1+anes20$nat3+anes20$nat4+anes20$nat5+anes20$nat6)/5
anes20$NAT2.1=(anes20$nat1+anes20$nat2+anes20$nat5+anes20$nat6)/4
anes20$NAT2.2=(anes20$nat3+anes20$nat4)/2

#Moral traditionalism
anes20$mortrad1=anes20$V202264
anes20$mortrad1[anes20$V202264<0]=NA

anes20$mortrad2=anes20$V202265
anes20$mortrad2[anes20$V202265<0]=NA
anes20$mortrad2=abs(anes20$mortrad2-6)

cor(anes20$mortrad1, anes20$mortrad2, use = "complete.obs")
#Only correlate at 0.33; let's not include this as an index or average.

#Importance of identity questions:
table(anes20$V201232)
#I'm not quite sure what to do with the party one; how should I combine this with the actual identification?
#3 point party ID
anes20$dem=ifelse(anes20$V201231x>0&anes20$V201231x<4, 1, 0)
anes20$ind=ifelse(anes20$V201231x>0&anes20$V201231x==4, 1, 0)
anes20$rep=ifelse(anes20$V201231x>0&anes20$V201231x>4, 1, 0)

#Partisan importance
anes20$imp_partisan=anes20$V201232
anes20$imp_partisan[anes20$V201232<0]=NA
#Need to reverse code it
anes20$imp_partisan=abs(anes20$imp_partisan-6)

#Look at importance across party lines
t.test(imp_partisan~dem, data=anes20[anes20$dem==1|anes20$rep==1,])
#Dems viewed their party ID as more important (by a small amount)
#What if we compare to Independents?
t.test(imp_partisan~dem, data=anes20[anes20$dem==1|anes20$ind==1,])
#No difference

#Create other importance items:
#Location:
table(anes20$V202357)
anes20$imp_place=anes20$V202357
anes20$imp_place[anes20$V202357<0]=NA
#Evangelical:
anes20$imp_evang=NA
anes20$imp_evang[anes20$V201459!=2&anes20$V201459!=3]=0
anes20$imp_evang[anes20$V201459==2|anes20$V201459==3]=anes20$V201461[anes20$V201459==2|anes20$V201459==3]
anes20$imp_evang[anes20$imp_evang<0]=NA
anes20$imp_evang[anes20$imp_evang>0&!is.na(anes20$imp_evang)]=abs(anes20$imp_evang[anes20$imp_evang>0&!is.na(anes20$imp_evang)]-6)
#Fundamentalists
anes20$imp_fund=NA
anes20$imp_fund[anes20$V201459!=1&anes20$V201459!=3]=0
anes20$imp_fund[anes20$V201459==1|anes20$V201459==3]=anes20$V201460[anes20$V201459==1|anes20$V201459==3]
anes20$imp_fund[anes20$imp_fund<0]=NA
anes20$imp_fund[anes20$imp_fund>0&!is.na(anes20$imp_fund)]=abs(anes20$imp_fund[anes20$imp_fund>0&!is.na(anes20$imp_fund)]-6)

####Media sources####
##Social media:
anes20$facebook=ifelse(anes20$V202541a==1, 1, 0)
anes20$facebook[anes20$V202541a<0]=NA

anes20$twitter=ifelse(anes20$V202541b==1, 1, 0)
anes20$twitter[anes20$V202541b<0]=NA

anes20$instagram=ifelse(anes20$V202541c==1, 1, 0)
anes20$instagram[anes20$V202541c<0]=NA

anes20$reddit=ifelse(anes20$V202541d==1, 1, 0)
anes20$reddit[anes20$V202541d<0]=NA

anes20$youtube=ifelse(anes20$V202541e==1, 1, 0)
anes20$youtube[anes20$V202541e<0]=NA

anes20$snapchat=ifelse(anes20$V202541f==1, 1, 0)
anes20$snapchat[anes20$V202541f<0]=NA

anes20$tiktok=ifelse(anes20$V202541g==1, 1, 0)
anes20$tiktok[anes20$V202541g<0]=NA

anes20$sm_other=ifelse(anes20$V202541h==1, 1, 0)
anes20$sm_other[anes20$V202541h<0]=NA

##How political social media use is (averaging across platforms)
anes20$fb_political=ifelse(anes20$V202543>0,abs(anes20$V202543-6) , NA)
anes20$tw_political=ifelse(anes20$V202545>0,abs(anes20$V202545-6) , NA)
anes20$rd_political=ifelse(anes20$V202547>0,abs(anes20$V202547-6) , NA)
anes20$sm_political=(anes20$fb_political+anes20$tw_political+anes20$rd_political)/3
anes20$sm_political=ifelse(is.na(anes20$fb_political), (anes20$tw_political+anes20$rd_political)/2, anes20$sm_political)
anes20$sm_political=ifelse(is.na(anes20$tw_political), (anes20$fb_political+anes20$rd_political)/2, anes20$sm_political)
anes20$sm_political=ifelse(is.na(anes20$rd_political), (anes20$fb_political+anes20$tw_political)/2, anes20$sm_political)
anes20$sm_political=ifelse(is.na(anes20$tw_political)&is.na(anes20$fb_political),( anes20$rd_political), anes20$sm_political)
anes20$sm_political=ifelse(is.na(anes20$rd_political)&is.na(anes20$fb_political), (anes20$tw_political), anes20$sm_political)
anes20$sm_political=ifelse(is.na(anes20$tw_political)&is.na(anes20$rd_political), (anes20$fb_political), anes20$sm_political)
#May not use this one because of the high amount of missing data

###Partisan news sources
anes20$news_fox=ifelse(anes20$V201630b==1 | anes20$V201630c==1 | anes20$V201630f==1 |
                       anes20$V201630g==1 | anes20$V201630h==1 | anes20$V201630k==1 |
                       anes20$V201631k==1
                       , 1, 0)

anes20$news_msnbc=ifelse(anes20$V201630d==1 |  anes20$V201630e==1 | anes20$V201631m==1 |
                         anes20$V201631n==1 | anes20$V201631p==1
                         , 1, 0)

anes20$news_cnn=ifelse(anes20$V201630i==1 |  anes20$V201630j==1 | anes20$V201630q==1 |
                       anes20$V201631b==1
                         , 1, 0)

####Political knowledge####

anes20$know_senateterm=ifelse(anes20$V201644==-5, NA,
                          ifelse(anes20$V201644==6, 1, 0))
anes20$know_spend=ifelse(anes20$V201645==-5, NA,
                          ifelse(anes20$V201645==1, 1, 0))
anes20$know_house=ifelse(anes20$V201646==-5, NA,
                          ifelse(anes20$V201646==1, 1, 0))
anes20$know_senate=ifelse(anes20$V201647==-5, NA,
                         ifelse(anes20$V201647==2, 1, 0))

anes20$know_scale=(anes20$know_house+anes20$know_senate+anes20$know_senateterm+anes20$know_spend)/4
####MEASURE OF PARTY-IDEOLOGY SORTING (2020)####
#PID strength
anes20$pid7_str[anes20$pid7==4]=1
anes20$pid7_str[anes20$pid7==3|anes20$pid7==5]=2
anes20$pid7_str[anes20$pid7==2|anes20$pid7==6]=3
anes20$pid7_str[anes20$pid7==1|anes20$pid7==7]=4

#Ideo strength
anes20$ideo_str[anes20$ideo==4]=1
anes20$ideo_str[anes20$ideo==3|anes20$ideo==5]=2
anes20$ideo_str[anes20$ideo==2|anes20$ideo==6]=3
anes20$ideo_str[anes20$ideo==1|anes20$ideo==7]=4

#Multiplied party and ideology strength
anes20$pidideostr1=anes20$pid7_str*anes20$ideo_str
#Difference between party ID and ideology
anes20$overlap=abs(anes20$pid7-anes20$ideo)+1
#Reverse code it
anes20$overlap_rr=abs(anes20$overlap-8)

#Combine all three into the single measure:
anes20$overlapxstr=anes20$overlap_rr*anes20$pidideostr1

#Rescale it so that it ranges from 0-1
anes20$sorting_r=(anes20$overlapxstr-7)/105


####Let's set the weights:####
#Details on this package can be found here:
#       https://stats.idre.ucla.edu/r/seminars/survey-data-analysis-with-r/
#anes20_w       <- svydesign(id      = ~V200010c,
#                            strata  = ~V200010d,
#                            weights = ~V200010a,
#                            nest    = T,
#                            data    = anes20)

#This is set for the pre-election variables; if post-election ones are used,
#replace V200010a with V200010b
#anes20_wp       <- svydesign(id      = ~V200010c,
#                             strata  = ~V200010d,
#                             weights = ~V200010b,
#                             nest    = T,
#                             data    = anes20[!is.na(anes20$V200010b),])

#Weight for RR and AUTH
#anes20_wp2       <- svydesign(id      = ~V200010c,
#                             strata  = ~V200010d,
#                             weights = ~V200010b,
#                             nest    = T,
#                             data    = anes20[!is.na(anes20$RR)&!is.na(anes20$AUTH),])


####Merging in the 2016 data for over time analyses####
#Now we need to read in the data from the 2016 ANES
anes16=read.dta13("anes_timeseries_2016_Stata13.dta")

####2016 Cleaning####
#Let's do the same cleaning we did for 2020 with these data, prior to merging
#Party as a set of binary variables
anes16$dem=ifelse(anes16$V161158x>0&anes16$V161158x<4, 1, 0)
anes16$ind=ifelse(anes16$V161158x>0&anes16$V161158x==4, 1, 0)
anes16$rep=ifelse(anes16$V161158x>4, 1, 0)

mean(anes16$dem, na.rm = TRUE)
mean(anes16$rep, na.rm = TRUE)

#continuous party ID (low values, dem)
anes16$pid7=anes16$V161158x
anes16$pid7[anes16$V161158x<0]=NA

#Variables we'll use:
#Ideology
anes16$lib=ifelse(anes16$V162171>0&anes16$V162171<4, 1, 0)
anes16$mod=ifelse(anes16$V162171==4|anes16$V162171==99, 1, 0)
anes16$con=ifelse(anes16$V162171>4&anes16$V162171!=99, 1, 0)
#continuous ideology
anes16$ideo=anes16$V162171
anes16$ideo[anes16$V162171<0]=NA
anes16$ideo[anes16$V162171==99]=4
#Reverse code so low is conservative
anes16$ideo=abs(anes16$ideo-8)

#Sex
anes16$male=ifelse(anes16$V161342==1, 1,0)
anes16$female=ifelse(anes16$V161342==2, 1,0)
#Race
anes16$white=ifelse(anes16$V161310x==1, 1,0)
anes16$black=ifelse(anes16$V161310x==2, 1,0)
anes16$asian=ifelse(anes16$V161310x==3, 1,0)
anes16$nativeamerican=ifelse(anes16$V161310x==4, 1,0)
anes16$multiracial=ifelse(anes16$V161310x==6, 1,0)
#Ethnicity
anes16$hisp=ifelse(anes16$V161310x==5, 1,0)
#Household is union membership
anes16$union=ifelse(anes16$V161302==1, 1, 0)
#Age
anes16$age=ifelse(anes16$V161267>0, anes16$V161267, NA)
#Age range
anes16$age_18=ifelse(anes16$V161267>0&anes16$V161267<30, 1, 0)
anes16$age_30=ifelse(anes16$V161267>29&anes16$V161267<45, 1, 0)
anes16$age_45=ifelse(anes16$V161267>44&anes16$V161267<65, 1,0)
anes16$age_65=ifelse(anes16$V161267>64, 1, 0)
#Community type could only be recreated with administrative variables
#Evangelical
anes16$evang=ifelse(anes16$V161266d==1, 1, 0)
#Armed forces/veteran
anes16$military=ifelse(anes16$V161274a==1, 1, 0)
#Gun owner
anes16$gun=ifelse(anes16$V161496>0, 1, 0)
#Class (post wave)
anes16$lowerclass=ifelse(anes16$V162132==1|anes16$V162129==0|anes16$V162130==4|anes16$V162131==0, 1,0)
anes16$workingclass=ifelse(anes16$V162132==2|anes16$V162129==2|anes16$V162130==2, 1,0)
anes16$middleclass=ifelse(anes16$V162132==3|anes16$V162129==1|anes16$V162130==1, 1,0)
anes16$upperclass=ifelse(anes16$V162132==4|anes16$V162129==4|anes16$V162130==0|anes16$V162131==4, 1,0)
#Education
anes16$lessHS=ifelse(anes16$V161270<9, 1, 0)
anes16$HS=ifelse(anes16$V161270==9, 1, 0)
anes16$somecollege=ifelse(anes16$V161270>9&anes16$V161270<13, 1, 0)
anes16$college=ifelse(anes16$V161270==13, 1, 0)
anes16$graduate=ifelse(anes16$V161270>13&anes16$V161270<=16, 1, 0)
#Parent's immigrants?
anes16$bothparentsUS=ifelse(anes16$V161315==1, 1, 0)
anes16$oneparentUS=ifelse(anes16$V161315==2, 1, 0)
anes16$bothparentsnotUS=ifelse(anes16$V161315==3, 1, 0)
#Income
anes16$income=ifelse(anes16$V161361x>0, anes16$V161361x, NA)
anes16$income25K=ifelse(anes16$V161361x>0&anes16$V161361x<10, 1, 0)
anes16$income50K=ifelse(anes16$V161361x>9&anes16$V161361x<15, 1, 0)
anes16$income75K=ifelse(anes16$V161361x>14&anes16$V161361x<20, 1, 0)
anes16$income100K=ifelse(anes16$V161361x>19&anes16$V161361x<23, 1, 0)
anes16$income150K=ifelse(anes16$V161361x>22&anes16$V161361x<26, 1, 0)
anes16$income250K=ifelse(anes16$V161361x>25&anes16$V161361x<28, 1, 0)
anes16$incomemore250K=ifelse(anes16$V161361x==28, 1, 0)
#Marital status
anes16$married=ifelse(anes16$V161268==1 | anes16$V161268==2, 1, 0)
anes16$single=ifelse(anes16$V161268==3 | anes16$V161268==4 |
                       anes16$V161268==5 | anes16$V161268==6 , 1, 0)
#Sexual orientation
anes16$straight=ifelse(anes16$V161511==1, 1, 0)
anes16$gay=ifelse(anes16$V161511==2, 1, 0)
anes16$bi=ifelse(anes16$V161511==3, 1, 0)
#Children at home
anes16$children=ifelse(anes16$V161324>0, 1, 0)

#Racial resentment
anes16$RR_gen=anes16$V162212
anes16$RR_gen[anes16$RR_gen<0]=NA

anes16$RR_favor=anes16$V162211
anes16$RR_favor[anes16$RR_favor<0]=NA
anes16$RR_favor=abs(anes16$RR_favor-6)

anes16$RR_less=anes16$V162213
anes16$RR_less[anes16$RR_less<0]=NA

anes16$RR_try=anes16$V162214
anes16$RR_try[anes16$RR_try<0]=NA
anes16$RR_try=abs(anes16$RR_try-6)

library(psych)

alpha(cbind(anes16$RR_favor, anes16$RR_gen, anes16$RR_less, anes16$RR_try))
#Alpha is 0.85, no surprise there
fa(cbind(anes16$RR_favor, anes16$RR_gen, anes16$RR_less, anes16$RR_try))
#Good loadings

anes16$RR=(anes16$RR_favor+anes16$RR_gen+anes16$RR_less+anes16$RR_try)/4

#Let's split it into thirds and make binary variables
quantile(anes16$RR, probs = c(.3333, .6666), na.rm=TRUE)
anes16$RR1=ifelse(anes16$RR<=as.numeric(quantile(anes16$RR, probs = .3333, na.rm=TRUE)), 1, 0)
anes16$RR2=ifelse(anes16$RR>as.numeric(quantile(anes16$RR, probs = .3333, na.rm=TRUE))&
                    anes16$RR<=as.numeric(quantile(anes16$RR, probs = .6666, na.rm=TRUE)), 1, 0)
anes16$RR3=ifelse(anes16$RR>as.numeric(quantile(anes16$RR, probs = .6666, na.rm=TRUE)), 1, 0)

#Authoritarianism
anes16$auth1=ifelse(anes16$V162239==2, 1, 0)
anes16$auth1[anes16$V162239<0]=NA

anes16$auth2=ifelse(anes16$V162240==2, 1, 0)
anes16$auth2[anes16$V162240<0]=NA

anes16$auth3=ifelse(anes16$V162241==1, 1, 0)
anes16$auth3[anes16$V162241<0]=NA

anes16$auth4=ifelse(anes16$V162242==2, 1, 0)
anes16$auth4[anes16$V162242<0]=NA

anes16$AUTH=anes16$auth1+anes16$auth2+anes16$auth3+anes16$auth4

#Let's split it into thirds and make binary variables
quantile(anes16$AUTH, probs = c(.3333, .6666), na.rm=TRUE)
anes16$AUTH1=ifelse(anes16$AUTH<=as.numeric(quantile(anes16$AUTH, probs = .3333, na.rm=TRUE)), 1, 0)
anes16$AUTH2=ifelse(anes16$AUTH>as.numeric(quantile(anes16$AUTH, probs = .3333, na.rm=TRUE))&
                      anes16$AUTH<=as.numeric(quantile(anes16$AUTH, probs = .6666, na.rm=TRUE)), 1, 0)
anes16$AUTH3=ifelse(anes16$AUTH>as.numeric(quantile(anes16$AUTH, probs = .6666, na.rm=TRUE)), 1, 0)

#Egalitarianism
anes16$egal1=anes16$V162243
anes16$egal1[anes16$egal1<0]=NA
anes16$egal1=abs(anes16$egal1-6)

anes16$egal2=anes16$V162244
anes16$egal2[anes16$egal2<0]=NA

anes16$egal3=anes16$V162245
anes16$egal3[anes16$egal3<0]=NA

anes16$egal4=anes16$V162246
anes16$egal4[anes16$egal4<0]=NA
anes16$egal4=abs(anes16$egal4-6)

psych::alpha(cbind(anes16$egal1, anes16$egal2, anes16$egal3, anes16$egal4))
#Alpha is 0.68, no surprise there
fa(cbind(anes16$egal1, anes16$egal2, anes16$egal3, anes16$egal4))
#Good loadings; 1 and 4 are a bit lower, still over 0.5

anes16$EGAL=(anes16$egal1+anes16$egal2+anes16$egal3+anes16$egal4)/4

#Sexism
anes16$sexism1=anes16$V162229x
anes16$sexism1[anes16$sexism1<0]=NA

anes16$sexism2=anes16$V162230x
anes16$sexism2[anes16$sexism2<0]=NA
anes16$sexism2=abs(anes16$sexism2-8)

anes16$sexism3=anes16$V162232
anes16$sexism3[anes16$sexism3<0]=NA
anes16$sexism3=abs(anes16$sexism3-6)

anes16$sexism4=anes16$V162233
anes16$sexism4[anes16$sexism4<0]=NA
anes16$sexism4=abs(anes16$sexism4-6)

psych::alpha(cbind(anes16$sexism1, anes16$sexism2, anes16$sexism3, anes16$sexism4))
#Alpha is 0.55, not as good
fa(cbind(anes16$sexism1, anes16$sexism2, anes16$sexism3, anes16$sexism4))
#OK loadings; 1 and 2 are below 0.25...

anes16$SEXISM=(anes16$sexism1+anes16$sexism2+anes16$sexism3+anes16$sexism4)/4

#Nationalism
anes16$nat1=anes16$V162123
anes16$nat1[anes16$V162123<0]=NA
anes16$nat1=abs(anes16$nat1-6)

anes16$nat3=anes16$V162271
anes16$nat3[anes16$V162271<0]=NA
anes16$nat3=abs(anes16$nat3-5)

anes16$nat4=anes16$V162272
anes16$nat4[anes16$V162272<0]=NA
anes16$nat4=abs(anes16$nat4-5)

anes16$nat5=anes16$V162273
anes16$nat5[anes16$V162273<0]=NA
anes16$nat5=abs(anes16$nat5-5)

anes16$nat6=anes16$V162274
anes16$nat6[anes16$V162274<0]=NA
anes16$nat6=abs(anes16$nat6-5)

psych::alpha(cbind(anes16$nat1, anes16$nat3, anes16$nat4, anes16$nat5, anes16$nat6))
#Alpha is 0.78, pretty good.
fa((cbind(anes16$nat1, anes16$nat3, anes16$nat4, anes16$nat5, anes16$nat6)))
fa(cbind(anes16$nat1, anes16$nat3, anes16$nat4, anes16$nat5, anes16$nat6), nfactors=2)
#Model fit is better for two factor; could split it with 1,5,6 on one scale and 3,4 on the other
anes16$NAT=(anes16$nat1+anes16$nat3+anes16$nat4+anes16$nat5+anes16$nat6)/5
anes16$NAT2.1=(anes16$nat1+anes16$nat5+anes16$nat6)/4
anes16$NAT2.2=(anes16$nat3+anes16$nat4)/2

#Moral traditionalism
anes16$mortrad1=anes16$V162207
anes16$mortrad1[anes16$V162207<0]=NA

anes16$mortrad2=anes16$V162210
anes16$mortrad2[anes16$V162210<0]=NA
anes16$mortrad2=abs(anes16$mortrad2-6)

cor(anes16$mortrad1, anes16$mortrad2, use = "complete.obs")
#Only correlate at 0.28; let's not include this as an index or average.

####Media sources####
##Social media:
anes16$facebook_tw=ifelse(anes16$V161495>0, 1, 0)
anes16$facebook_tw[anes16$V161495<0]=NA

###Partisan news sources
anes16$news_fox=ifelse(anes16$V161370==1 | anes16$V161372==1 | anes16$V161391==1 |
                         anes16$V161409==1
                       , 1, 0)

anes16$news_msnbc=ifelse(anes16$V161386==1 |  anes16$V161393==1
                         , 1, 0)

anes16$news_cnn=ifelse(anes16$V161381==1 |  anes16$V161403==1 | anes16$V161404==1 |
                         anes16$V161415==1
                       , 1, 0)

####Political knowledge####

anes16$know_senateterm=ifelse(anes16$V161513==-5, NA,
                              ifelse(anes16$V161513==6, 1, 0))
anes16$know_spend=ifelse(anes16$V161514==-5, NA,
                         ifelse(anes16$V161514==1, 1, 0))
anes16$know_house=ifelse(anes16$V161515==-5, NA,
                         ifelse(anes16$V161515==2, 1, 0))
anes16$know_senate=ifelse(anes16$V161516==-5, NA,
                          ifelse(anes16$V161516==2, 1, 0))

anes16$know_scale=(anes16$know_house+anes16$know_senate+anes16$know_senateterm+anes16$know_spend)/4


####MEASURE OF PARTY-IDEOLOGY SORTING####
#PID strength
anes16$pid7_str[anes16$pid7==4]=1
anes16$pid7_str[anes16$pid7==3|anes16$pid7==5]=2
anes16$pid7_str[anes16$pid7==2|anes16$pid7==6]=3
anes16$pid7_str[anes16$pid7==1|anes16$pid7==7]=4

#Ideo strength
anes16$ideo_str[anes16$ideo==4]=1
anes16$ideo_str[anes16$ideo==3|anes16$ideo==5]=2
anes16$ideo_str[anes16$ideo==2|anes16$ideo==6]=3
anes16$ideo_str[anes16$ideo==1|anes16$ideo==7]=4

#Multiplied party and ideology strength
anes16$pidideostr1=anes16$pid7_str*anes16$ideo_str
#Difference between party ID and ideology
anes16$overlap=abs(anes16$pid7-anes16$ideo)+1
#Reverse code it
anes16$overlap_rr=abs(anes16$overlap-8)

#Combine all three into the single measure:
anes16$overlapxstr=anes16$overlap_rr*anes16$pidideostr1

#Rescale it so that it ranges from 0-1
anes16$sorting_r=(anes16$overlapxstr-7)/105


####Let's set the weightsfor 2016:####
#Details on this package can be found here:
#       https://stats.idre.ucla.edu/r/seminars/survey-data-analysis-with-r/
#anes16_w       <- svydesign(id      = ~V160202,
#                            strata  = ~V160201,
#                            weights = ~V160101,
#                            nest    = T,
#                            data    = anes16)

#This is set for the pre-election variables; if post-election ones are used,
#replace V200010a with V200010b
#anes16_wp       <- svydesign(id      = ~V160202,
#                             strata  = ~V160201,
#                             weights = ~V160102,
#                             nest    = T,
#                             data    = anes16[!is.na(anes16$V200010b),])

#Weight for RR and AUTH
#anes16_wp2       <- svydesign(id      = ~V160202,
#                              strata  = ~V160201,
#                              weights = ~V160102,
#                              nest    = T,
#                              data    = anes16[!is.na(anes16$RR)&!is.na(anes16$AUTH),])


####Dependent variables####
### Dependent variables:
#(thru line:)
##How much do you feel it is justified for people to use violence to pursue 
##their political goals in this country?

anes20$vio_justscale=NULL
anes20$vio_justscale=ifelse(anes20$V201602>0&anes20$V201602<6, anes20$V201602, NA)
anes20$vio_justy=ifelse(anes20$V201602>1,1,0)
anes20$vio_justy[anes20$V201602<0]=NA
table(anes20$vio_justscale)
table(anes20$vio_justy)


## breakdown into binary
#(0 if they answer 1 (not okay with violence) and 1 if they answer 2-5 (okay with violene to some extent))

anes20$viojustbi=NULL
anes20$viojustbi=ifelse(anes20$vio_justscale>1&anes20$vio_justscale,1,0)
table(anes20$viojustbi)

##perceptions of protestors

table(anes20$V201432x)
anes20$violence=ifelse(anes20$V201432x>0,anes20$V201432x,0)
table(anes20$violence)

anes20$violence1=ifelse(anes20$violence==1,0.2,0)
anes20$violence1=ifelse(anes20$violence==2,0.4,anes20$violence1)
anes20$violence1=ifelse(anes20$violence==3,0.6,anes20$violence1)
anes20$violence1=ifelse(anes20$violence==4,0.8,anes20$violence1)
anes20$violence1=ifelse(anes20$violence==5,1,anes20$violence1)
table(anes20$violence1)

##"What is the best way to deal with the problem of urban unrest
##and rioting? Some say it is more important to use all available
##force to maintain law and order, no matter what results. Others
##say it is more important to correct the problems of racism and
##police violence that give rise to the disturbances. And, of course,
##other people have opinions in between.
##(Scale from 1-7) solve the problems--> keep law and order "


##what is the best way to deal with urban unrest? 
#1. Solve problems of racism and police violence- 7. Use all available force to maintain law and order)

anes20$unrest=NULL

table(anes20$V201429)
anes20$unrest=ifelse(anes20$V201429>0&anes20$V201429<8,anes20$V201429,NA)
table(anes20$unrest)

anes20$unrest=ifelse(anes20$V201429>0&anes20$V201429<10, 1, 0)

hist(anes20$unrest)
table(anes20$unrest)

#recoded to a 0-1 scale 
#1= 0.148
#2= 0.286
#3= 0.428
#4= 0.571
#5= 0.714
#6= 0.857
#7= 1
anes20$unrest1=NULL

anes20$unrest1=ifelse(anes20$unrest==1,0.148,0)
anes20$unrest1=ifelse(anes20$unrest==2,0.286,anes20$unrest1)
anes20$unrest1=ifelse(anes20$unrest==3,0.428,anes20$unrest1)
anes20$unrest1=ifelse(anes20$unrest==4,0.571,anes20$unrest1)
anes20$unrest1=ifelse(anes20$unrest==5,0.714,anes20$unrest1)
anes20$unrest1=ifelse(anes20$unrest==6,0.857,anes20$unrest1)
anes20$unrest1=ifelse(anes20$unrest==7,1,anes20$unrest1)

table(anes20$unrest1)

#make it binary 
#(0 if answered 1-4 (solve problems of police violence), 1 if answered 5-7(keep law and order))

anes20$unrestbi=ifelse(anes20$unrest>4,1,0)
table(anes20$unrestbi)


##"In your view, how often do the following things occur in this country’s elections:
## Votes are counted fairly?"
## responses--> 
##-9. Refused
##-8. Don’t know
##-7. No post-election data, deleted due to incomplete interview
##-6. No post-election interview
##-5. Interview breakoff (sufficient partial IW)
## 1. All of the time
## 2. Most of the time
## 3. About half of the time
## 4. Some of the time
## 5. Never


#(1 for values )

anes20$FAIRELEC=NULL


anes20$fairelec=ifelse(anes20$V202219>0&anes20$V202219<8,anes20$V202219,NA)

table(anes20$fairelec)

#make it binary 

anes20$fairelecbi=ifelse(anes20$fairelec>3,1,0)
table(anes20$fairelecbi)

#coding education variable 
anes20$educ=ifelse(anes20$V201510>0&anes20$V201510<9,anes20$V201510,NA)
table(anes20$educ)

anes16$educ=ifelse(anes16$V161270>0&anes16$V161270<90,anes16$V161270,NA)
table(anes16$educ)

###"On the whole, are you very satisfied, fairly satisfied, not very
### satisfied, or not at all satisfied with the way democracy works in
### the United States?"

#-9. Refused
#-8. Don’t know
#-7. No post-election data, deleted due to incomplete interview
#-6. No post-election interview
#-5. Interview breakoff (sufficient partial IW)
# 1. Very satisfied
# 2. Fairly satisfied
# 4. Not very satisfied
# 5. Not at all satisfied

anes20$demsat=ifelse(anes20$V202440>0&anes20$V202440<6,anes20$V202440,NA)

table(anes20$demsat)

#make it binary

anes20$demsatbi=ifelse(anes20$demsat>3,1,0)

table(anes20$demsatbi)

##Would you prefer a government official who compromises to get 
##things done, or who sticks to their principles no matter what?"

#-9. Refused
#-8. Don’t know
#1. Compromises to get things done
#2. Sticks to their principles no matter what

anes20$compprinc=ifelse(anes20$V201379>0,anes20$V201379,NA)

table(anes20$compprinc)

#Are those who are more sorted more or less willing to compromise?
#Dependent Variable: Willingness to compromise
#Independent Variable: Party id sorting_r
#Needed: code DV - higher value means less willingness to compromise 

table(anes20$V202409)
anes20$comp1=ifelse(anes20$V202409>0,anes20$V202409,NA) 
table(anes20$comp1)


table(anes20$V201379)
anes20$comp2=ifelse(anes20$V201379>0,anes20$V201379,NA)
table(anes20$comp2)


#recoding comp1 to be/1 and going the correct direction (lower value means they think compromise is good, hogher value means it is bad)

anes20$comp1.1=ifelse(anes20$comp1==5,0.20,NA)
anes20$comp1.1=ifelse(anes20$comp1==4,0.40,anes20$comp1.1)
anes20$comp1.1=ifelse(anes20$comp1==3,0.60,anes20$comp1.1)
anes20$comp1.1=ifelse(anes20$comp1==2,0.80,anes20$comp1.1)
anes20$comp1.1=ifelse(anes20$comp1==1,1,anes20$comp1.1)


#recoding comp2 to be 0 and 1 instead of 1 and 2
anes20$comp2.1=ifelse(anes20$comp2==1,0,NA)
anes20$comp2.1=ifelse(anes20$comp2==2,1,anes20$comp2.1)


##variables for political involvement indexes

#V202025: Joined protest, march, rally, or demonstration in the last 12 months

anes20$join2=ifelse(anes20$V202025>0,anes20$V202025, NA)
anes20$joinprotest=ifelse(anes20$join2==1,1,0)


#V202031: Worked with others to deal with issue in community in last 12 months

anes20$community=ifelse(anes20$V202031>0, anes20$V202031, NA)
anes20$commiss=ifelse(anes20$community==1,1,0)

#V202034: Contact any federal elected official in last 12 months
anes20$contact=ifelse(anes20$V202031>=1, anes20$V202034, NA)
anes20$conoff=ifelse(anes20$contact==1,1,0)

#We would like to find out about some of the things people do to help a party or a candidate 
#win an election. During the campaign, did you talk to any people and try to show them why
#they should vote for or against one of the parties or candidates?
anes20$talk009=ifelse(anes20$V202009>0,anes20$V202009,NA)
anes20$talk109=ifelse(anes20$talk009==1,1,0)

#Did you participate in any online political meetings, rallies, speeches, fundraisers, 
#or things like that in support of a particular candidate?
anes20$attend013=ifelse(anes20$V202013>0,anes20$V202013,NA)
anes20$attend113=ifelse(anes20$attend013==1,1,0)

#Did you go to any political meetings, rallies, speeches, dinners, or things like that 
# in support of a particular candidate?
anes20$attend014=ifelse(anes20$V202014>0,anes20$V202014,NA)
anes20$attend114=ifelse(anes20$attend014==1,1,0)

#Did you wear a campaign button, put a campaign sticker on your car, or place a sign 
#in your window or in front of your house?
anes20$wear015=ifelse(anes20$V202015>0,anes20$V202015,NA)
anes20$wear115=ifelse(anes20$wear015==1,1,0)

#Did you do any other work for one of the parties or candidates?
anes20$work016=ifelse(anes20$V202016>0,anes20$V202016,NA)
anes20$work116=ifelse(anes20$work016==1,1,0)

#During an election year people are often asked to make a contribution to support 
#campaigns. Did you give money to an individual candidate running for public office?
anes20$give017=ifelse(anes20$V202017>0,anes20$V202017,NA)
anes20$give117=ifelse(anes20$give017==1,1,0)

#Did you give money to a political party during this election year?
anes20$give019=ifelse(anes20$V202019>0,anes20$V202019,NA)
anes20$give119=ifelse(anes20$give019==1,1,0)

#During the past 12 months, have you contacted or tried to contact a member of the 
#U.S. Senate or U.S. House of Representatives, or have you not done this in the past 12 months?
anes20$contact030=ifelse(anes20$V202030>0,anes20$V202030,NA)
anes20$contact130=ifelse(anes20$contact030==1,1,0)

#In the past twelve months, have you contacted a federal elected official, 
#such as a member of Congress or the President, or someone on the staff of such an official?
anes20$contact034=ifelse(anes20$V202034>0,anes20$V202034,NA)
anes20$contact134=ifelse(anes20$contact034==1,1,0)

#And what about a non-elected official in a federal government agency? 
# Have you contacted such a person in the past twelvemonths?
anes20$contact036=ifelse(anes20$V202036>0,anes20$V202036,NA)
anes20$contact136=ifelse(anes20$contact036==1,1,0)

#What about an elected official on the state or local level, such as a governor, 
# mayor, or a member of the state legislature or city council, or someone on the 
# staff of such an elected official? Have you contacted such a person in the past twelve months?
anes20$give038=ifelse(anes20$V202038>0,anes20$V202038,NA)
anes20$give138=ifelse(anes20$give038==1,1,0)

#And what about a non-elected official in a state or local government agency? 
#Have you contacted such a person in the past twelve months?
anes20$contact040=ifelse(anes20$V202040>0,anes20$V202040,NA)
anes20$contact140=ifelse(anes20$contact040==1,1,0)

##non systemic

#During the past 12 months, have you joined in a protest march,rally, or demonstration, 
# or have you not done this in the past 12 months?
anes20$joined025=ifelse(anes20$V202025>0,anes20$V202025,NA)
anes20$joined125=ifelse(anes20$joined025==1,1,0)

#During the past 12 months, have you signed a petition on the Internet or on paper 
#about a political or social issue, or have you not done this in the past 12 months?
anes20$signed026=ifelse(anes20$V202026>0,anes20$V202026,NA)
anes20$signed126=ifelse(anes20$signed026==1,1,0)

anes20$bought042=ifelse(anes20$V202042>0,anes20$V202042,NA)
anes20$bought142=ifelse(anes20$bought042==1,0,1)
#(0==never done before 1== consolodated 2-5 scale of have done before)

anes20$discuss022=ifelse(anes20$V202022>0,anes20$V202022,NA)
anes20$discuss122=ifelse(anes20$discuss022==1,1,0)

anes20$posted029=ifelse(anes20$V202029>0,anes20$V202029,NA)
anes20$posted129=ifelse(anes20$posted029==1,1,0)


anes20$OVERALLINDEX1=((anes20$talk109+anes20$attend113+anes20$attend114+anes20$wear115+
                         anes20$work116+anes20$give117+anes20$give119+anes20$contact130+anes20$contact134+
                         anes20$contact136+anes20$give138+anes20$contact140+anes20$joined125+anes20$signed126+
                         anes20$bought142+anes20$discuss122+anes20$posted129)/17)


anes20$SYSTEMICINDEX1=((anes20$talk109+anes20$attend113+anes20$attend114+anes20$wear115+
                          anes20$work116+anes20$give117+anes20$give119+anes20$contact130+
                          anes20$contact134+anes20$contact136+anes20$give138+anes20$contact140)/12)


anes20$NONSYSTEMICINDEX1=((anes20$joined125+anes20$signed126+anes20$bought142+anes20$discuss122
                           +anes20$posted129)/5)



anes20$MASONINDEX1=((anes20$talk109+anes20$wear115+anes20$give117+anes20$joined125+anes20$work116)/5)


##ftdifference
anes20$FT_dem=ifelse(anes20$V201156>=0, anes20$V201156, NA)
anes20$FT_rep=ifelse(anes20$V201157>=0, anes20$V201157, NA)
anes20$ftdifference=ifelse(anes20$ind==0,abs(anes20$FT_dem-anes20$FT_rep),NA)
hist(anes20$ftdifference)


table(anes20$V201452)

#religiosity
anes20$religscale=ifelse(anes20$V201452==2,0,NA )
table(anes20$religscale)
table(anes20$V201453)
anes20$religscale=ifelse(anes20$V201453==5,0,anes20$religscale)
table(anes20$religscale)
anes20$religscale=ifelse(anes20$V201453==1,0.80, anes20$religscale)
anes20$religscale=ifelse(anes20$V201453==2,0.60, anes20$religscale)
anes20$religscale=ifelse(anes20$V201453==3,0.40, anes20$religscale)
anes20$religscale=ifelse(anes20$V201453==4,0.20, anes20$religscale)
anes20$religscale=ifelse(anes20$V201454==1,0.80, anes20$religscale)
anes20$religscale=ifelse(anes20$V201454==2,1.00, anes20$religscale)
table(anes20$religscale)

##born again 

table(anes20$V201456)
anes20$bornagain=ifelse(anes20$V201456==1,1,0)
table(anes20$bornagain)

##2016
#religiosity
anes16$religscale=ifelse(anes16$V161244==2,0,NA )
table(anes16$religscale)
table(anes16$V161245)
anes16$religscale=ifelse(anes16$V161245==5,0,anes16$religscale)
table(anes16$religscale)
anes16$religscale=ifelse(anes16$V161245==1,0.80, anes16$religscale)
anes16$religscale=ifelse(anes16$V161245==2,0.60, anes16$religscale)
anes16$religscale=ifelse(anes16$V161245==3,0.40, anes16$religscale)
anes16$religscale=ifelse(anes16$V161245==4,0.20, anes16$religscale)
anes16$religscale=ifelse(anes16$V161245a==1,0.80, anes16$religscale)
anes16$religscale=ifelse(anes16$V161245a==2,1.00, anes16$religscale)
table(anes16$religscale)

##born again 

table(anes16$V161263)
anes16$bornagain=ifelse(anes16$V161263==1,1,0)
table(anes16$bornagain)

##support for democracy-- higher values show less support for democracy

#V202440: Satisfaction with how democracy works in the US
#V201367: How important to democracy that branches check and balance each other
#V201372x: How helpful or harmful if the president didn’t need to worry about Congress or the courts?

anes20$dem1=NULL
anes20$dem1=ifelse(anes20$V202440==1,0,NA)
anes20$dem1=ifelse(anes20$V202440==2,0.333,anes20$dem1)
anes20$dem1=ifelse(anes20$V202440==4,0.666,anes20$dem1)
anes20$dem1=ifelse(anes20$V202440==5,1,anes20$dem1)
table(anes20$dem1)

anes20$dem2=ifelse(anes20$V201367==1,0.2,0)
anes20$dem2=ifelse(anes20$V201367==2,0.4,anes20$dem2)
anes20$dem2=ifelse(anes20$V201367==3,0.6,anes20$dem2)
anes20$dem2=ifelse(anes20$V201367==4,0.8,anes20$dem2)
anes20$dem2=ifelse(anes20$V201367==5,1,anes20$dem2)
table(anes20$dem2)

anes20$dem3=ifelse(anes20$V201372x==7,0.14,0)
anes20$dem3=ifelse(anes20$V201372x==6,0.28,anes20$dem3)
anes20$dem3=ifelse(anes20$V201372x==5,0.42,anes20$dem3)
anes20$dem3=ifelse(anes20$V201372x==4,0.57,anes20$dem3)
anes20$dem3=ifelse(anes20$V201372x==3,0.71,anes20$dem3)
anes20$dem3=ifelse(anes20$V201372x==2,0.85,anes20$dem3)
anes20$dem3=ifelse(anes20$V201372x==1,1,anes20$dem3)
table(anes20$dem3)

anes20$demsupport=NULL
anes20$demsupport=(anes20$dem1+anes20$dem2+anes20$dem3)/3

####Repeat for 2016####
### Dependent variables:
#(thru line:)
##How much do you feel it is justified for people to use violence to pursue 
##their political goals in this country?

anes16$vio_justscale=NULL
anes16$vio_justscale=ifelse(anes16$V161344>0&anes16$V161344<6, anes16$V161344, NA)
anes16$vio_justy=ifelse(anes16$V161344>1,1,0)
anes16$vio_justy[anes16$V161344<0]=NA
table(anes16$vio_justscale)
table(anes16$vio_justy)


## breakdown into binary
#(0 if they answer 1 (not okay with violence) and 1 if they answer 2-5 (okay with violene to some extent))

anes16$viojustbi=NULL
anes16$viojustbi=ifelse(anes16$vio_justscale>1&anes16$vio_justscale,1,0)
table(anes16$viojustbi)

##perceptions of protestors - wasn't asked


##what is the best way to deal with urban unrest?  - wasn't asked


##"In your view, how often do the following things occur in this country’s elections:
## Votes are counted fairly?"
## responses--> 
##-9. Refused
##-8. Don’t know
##-7. No post-election data, deleted due to incomplete interview
##-6. No post-election interview
##-5. Interview breakoff (sufficient partial IW)
## 1. All of the time
## 2. Most of the time
## 3. About half of the time
## 4. Some of the time
## 5. Never


#(1 for values )

anes16$FAIRELEC=NULL


anes16$fairelec=ifelse(anes16$V162219>0&anes16$V162219<8,anes16$V162219,NA)

table(anes16$fairelec)

#make it binary 

anes16$fairelecbi=ifelse(anes16$fairelec>3,1,0)
table(anes16$fairelecbi)

###"On the whole, are you very satisfied, fairly satisfied, not very
### satisfied, or not at all satisfied with the way democracy works in
### the United States?"

#-9. Refused
#-8. Don’t know
#-7. No post-election data, deleted due to incomplete interview
#-6. No post-election interview
#-5. Interview breakoff (sufficient partial IW)
# 1. Very satisfied
# 2. Fairly satisfied
# 4. Not very satisfied
# 5. Not at all satisfied

anes16$demsat=ifelse(anes16$V162290>0&anes16$V162290<6,anes16$V162290,NA)

table(anes16$demsat)

#make it binary

anes16$demsatbi=ifelse(anes16$demsat>3,1,0)

table(anes16$demsatbi)

##Would you prefer a government official who compromises to get 
##things done, or who sticks to their principles no matter what?"

#-9. Refused
#-8. Don’t know
#1. Compromises to get things done
#2. Sticks to their principles no matter what

anes16$compprinc=ifelse(anes16$V161171>0,anes16$V161171,NA)

table(anes16$compprinc)

#Are those who are more sorted more or less willing to compromise?
#Dependent Variable: Willingness to compromise
#Independent Variable: Party id sorting_r
#Needed: code DV - higher value means less willingness to compromise 

anes16$comp1=ifelse(anes16$V162259>0,anes16$V162259,NA) 
table(anes16$comp1)


anes16$comp2=ifelse(anes16$V161171>0,anes16$V161171,NA)
table(anes16$comp2)


#recoding comp1 to be/1 and going the correct direction (lower value means they think compromise is good, hogher value means it is bad)

anes16$comp1.1=ifelse(anes16$comp1==5,0.20,NA)
anes16$comp1.1=ifelse(anes16$comp1==4,0.40,anes16$comp1.1)
anes16$comp1.1=ifelse(anes16$comp1==3,0.60,anes16$comp1.1)
anes16$comp1.1=ifelse(anes16$comp1==2,0.80,anes16$comp1.1)
anes16$comp1.1=ifelse(anes16$comp1==1,1,anes16$comp1.1)


#recoding comp2 to be 0 and 1 instead of 1 and 2
anes16$comp2.1=ifelse(anes16$comp2==1,0,NA)
anes16$comp2.1=ifelse(anes16$comp2==2,1,anes16$comp2.1)


##variables for political involvement indexes

#V162018a: Joined protest, march, rally, or demonstration in the last 12 months

anes16$join2=ifelse(anes16$V162018a>0,anes16$V162018a, NA)
anes16$joinprotest=ifelse(anes16$join2==1,1,0)


#V162195: Worked with others to deal with issue in community in last 12 months

anes16$community=ifelse(anes16$V162195>0, anes16$V162195, NA)
anes16$commiss=ifelse(anes16$community==1,1,0)

#V162198: Contact any federal elected official in last 12 months
anes16$contact=ifelse(anes16$V162198>=1, anes16$V162198, NA)
anes16$conoff=ifelse(anes16$contact==1,1,0)

#We would like to find out about some of the things people do to help a party or a candidate 
#win an election. During the campaign, did you talk to any people and try to show them why
#they should vote for or against one of the parties or candidates?
anes16$talk009=ifelse(anes16$V162010>0,anes16$V162010,NA)
anes16$talk109=ifelse(anes16$talk009==1,1,0)

#Did you go to any political meetings, rallies, speeches, dinners, or things like that 
# in support of a particular candidate?
anes16$attend014=ifelse(anes16$V162011>0,anes16$V162011,NA)
anes16$attend114=ifelse(anes16$attend014==1,1,0)

#Did you wear a campaign button, put a campaign sticker on your car, or place a sign 
#in your window or in front of your house?
anes16$wear015=ifelse(anes16$V162012>0,anes16$V162012,NA)
anes16$wear115=ifelse(anes16$wear015==1,1,0)

#Did you do any other work for one of the parties or candidates?
anes16$work016=ifelse(anes16$V162013>0,anes16$V162013,NA)
anes16$work116=ifelse(anes16$work016==1,1,0)

#During an election year people are often asked to make a contribution to support 
#campaigns. Did you give money to an individual candidate running for public office?
anes16$give017=ifelse(anes16$V162014>0,anes16$V162014,NA)
anes16$give117=ifelse(anes16$give017==1,1,0)

#Did you give money to a political party during this election year?
anes16$give019=ifelse(anes16$V162016>0,anes16$V162016,NA)
anes16$give119=ifelse(anes16$give019==1,1,0)

#During the past 12 months, have you contacted or tried to contact a member of the 
#U.S. Senate or U.S. House of Representatives, or have you not done this in the past 12 months?
anes16$contact030=ifelse(anes16$V162019>0,anes16$V162019,NA)
anes16$contact130=ifelse(anes16$contact030==1,1,0)

#In the past twelve months, have you contacted a federal elected official, 
#such as a member of Congress or the President, or someone on the staff of such an official?
anes16$contact034=ifelse(anes16$V162198>0,anes16$V162198,NA)
anes16$contact134=ifelse(anes16$contact034==1,1,0)

#And what about a non-elected official in a federal government agency? 
# Have you contacted such a person in the past twelvemonths?
anes16$contact036=ifelse(anes16$V162200>0,anes16$V162200,NA)
anes16$contact136=ifelse(anes16$contact036==1,1,0)

#What about an elected official on the state or local level, such as a governor, 
# mayor, or a member of the state legislature or city council, or someone on the 
# staff of such an elected official? Have you contacted such a person in the past twelve months?
anes16$give038=ifelse(anes16$V162202>0,anes16$V162202,NA)
anes16$give138=ifelse(anes16$give038==1,1,0)

#And what about a non-elected official in a state or local government agency? 
#Have you contacted such a person in the past twelve months?
anes16$contact040=ifelse(anes16$V162204>0,anes16$V162204,NA)
anes16$contact140=ifelse(anes16$contact040==1,1,0)

##non systemic

#During the past 12 months, have you joined in a protest march,rally, or demonstration, 
# or have you not done this in the past 12 months?
anes16$joined025=ifelse(anes16$V162018a>0,anes16$V162018a,NA)
anes16$joined125=ifelse(anes16$joined025==1,1,0)

#During the past 12 months, have you signed a petition on the Internet or on paper 
#about a political or social issue, or have you not done this in the past 12 months?
anes16$signed026=ifelse(anes16$V162018b>0,anes16$V162018b,NA)
anes16$signed126=ifelse(anes16$signed026==1,1,0)

anes16$bought042=ifelse(anes16$V162141>0,anes16$V162141,NA)
anes16$bought142=ifelse(anes16$bought042==1,0,1)
#(0==never done before 1== consolodated 2-5 scale of have done before)

anes16$discuss022=ifelse(anes16$V162174>0,anes16$V162174,NA)
anes16$discuss122=ifelse(anes16$discuss022==1,1,0)



anes16$OVERALLINDEX1=((anes16$talk109+anes16$attend114+anes16$wear115+
                         anes16$work116+anes16$give117+anes16$give119+anes16$contact130+anes16$contact134+
                         anes16$contact136+anes16$give138+anes16$contact140+anes16$joined125+anes16$signed126+
                         anes16$bought142+anes16$discuss122)/15)


anes16$SYSTEMICINDEX1=((anes16$talk109+anes16$attend114+anes16$wear115+
                          anes16$work116+anes16$give117+anes16$give119+anes16$contact130+
                          anes16$contact134+anes16$contact136+anes16$give138+anes16$contact140)/11)


anes16$NONSYSTEMICINDEX1=((anes16$joined125+anes16$signed126+anes16$bought142+anes16$discuss122)/4)


anes16$MASONINDEX1=((anes16$talk109+anes16$wear115+anes16$give117+anes16$joined125+anes16$work116)/5)


##ftdifference

anes16$FT_dem=ifelse(anes16$V161095>=0, anes16$V161095, NA)
anes16$FT_rep=ifelse(anes16$V161096>=0, anes16$V161096, NA)
anes16$ftdifference=ifelse(anes16$ind==0,abs(anes16$FT_dem-anes16$FT_rep),NA)
hist(anes16$ftdifference)

##support for democracy-- higher values show less support for democracy

#V162290: Satisfaction with how democracy works in the US
#V201367: How important to democracy that branches check and balance each other - NOT ASKED
#V201372x: How helpful or harmful if the president didn’t need to worry about Congress or the courts? - NOT ASKED

anes16$dem1=NULL
anes16$dem1=ifelse(anes16$V162290==1,0,NA)
anes16$dem1=ifelse(anes16$V162290==2,0.333,anes16$dem1)
anes16$dem1=ifelse(anes16$V162290==4,0.666,anes16$dem1)
anes16$dem1=ifelse(anes16$V162290==5,1,anes16$dem1)
table(anes16$dem1)


####Actual merging####
#To do this, let's first make a separate data object only for the panel folks
panel=anes20[anes20$V200003==2,]
#This variable can be used to do the merging: V160001_orig
#Weighting variable for the panel data is V200011a for pre-election and V200011b for post
panel_combined=merge(panel, anes16, by.x="V160001_orig", by.y="V160001_orig")

#.x variables refer to 2020; .y variables refer to 2016

####Weights for panel:####
#Details on this package can be found here:
#       https://stats.idre.ucla.edu/r/seminars/survey-data-analysis-with-r/
#panel_w       <- svydesign(id      = ~V200010c,
#                            strata  = ~V200010d,
#                            weights = ~V200011a,
#                            nest    = T,
#                            data    = panel_combined)

#This is set for the pre-election variables; if post-election ones are used,
#replace V200010a with V200010b
#panel_wp       <- svydesign(id      = ~V200010c,
#                             strata  = ~V200010d,
#                             weights = ~V200011b,
#                             nest    = T,
#                             data    = panel_combined[!is.na(panel_combined$V200011b),])

#Weight for RR and AUTH
#panel_wp2       <- svydesign(id      = ~V200010c,
#                              strata  = ~V200010d,
#                              weights = ~V200011b,
#                              nest    = T,
#                              data    = panel_combined[!is.na(panel_combined$RR.x)&!is.na(panel_combined$AUTH.x),])


####Create some new variables for the panel data####
panel_combined$sorting_change=panel_combined$sorting_r.x-panel_combined$sorting_r.y
hist(panel_combined$sorting_change, main="Change in party-ideology sorting",
     xlab="Change in sorting, 2020 - 2016")
summary(panel_combined$sorting_change)
cor.test(panel_combined$sorting_r.x, panel_combined$sorting_r.y)
panel_combined$sorting_increase=ifelse(panel_combined$sorting_change>0,1,0)
panel_combined$sorting_decrease=ifelse(panel_combined$sorting_change<0,1,0)
prop.table(table(panel_combined$sorting_increase))
prop.table(table(panel_combined$sorting_decrease))
prop.table(table(panel_combined$sorting_decrease, panel_combined$sorting_increase))

panel_combined$RR_change=(panel_combined$RR.x-panel_combined$RR.y)/4
hist(panel_combined$RR_change, main="Change in Racial Resentment",
     xlab="Change in Racial Resentment, 2020 - 2016")
summary(panel_combined$RR_change)
cor.test(panel_combined$RR.x, panel_combined$RR.y)
panel_combined$RR_increase=ifelse(panel_combined$RR_change>0,1,0)
panel_combined$RR_decrease=ifelse(panel_combined$RR_change<0,1,0)
prop.table(table(panel_combined$RR_decrease, panel_combined$RR_increase))


panel_combined$AUTH_change=(panel_combined$AUTH.x-panel_combined$AUTH.y)/4
hist(panel_combined$AUTH_change, main="Change in Authoritarianism",
     xlab="Change in Authoritarianism, 2020 - 2016", breaks=8)
summary(panel_combined$AUTH_change)
cor.test(panel_combined$AUTH.x, panel_combined$AUTH.y)
panel_combined$AUTH_increase=ifelse(panel_combined$AUTH_change>0,1,0)
panel_combined$AUTH_decrease=ifelse(panel_combined$AUTH_change<0,1,0)
prop.table(table(panel_combined$AUTH_decrease, panel_combined$AUTH_increase))


####binary variable for the panel####
anes20$panel=ifelse(anes20$V200003==2, 1,0)


####Save the cleaned data####
save.image("Sorting - 2020 ANES - updated.RData")

