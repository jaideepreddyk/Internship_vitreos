df<-read.csv('top20df.csv',header = T)
summary(df)
str(df)

for(i in (1:nrow(df))){
  if (df$prob_new[i]>0.20){
    df$prob_new[i]='risk'
  }
  else{
    df$prob_new[i]='no risk'
  }
}


head(df)

str(df$prob_new)
summary(df$prob_new)
df$prob_new<-as.factor(df$prob_new)

summary(df$prob_new)

############### 20 break histogram #############
(0.901822-0.000824)/20

seq(2)
breaks=integer(21)
breaks[1]=0.000824
breaks

for (i in seq(21)){
  breaks[i+1]=breaks[i]+0.0450499
}

range(df$prob_new)

x11()
hist(df$prob_new,breaks = c(breaks),col = 'skyblue3')

####### converting to log scale #########
str(df)
summary(df$prob_new)

#### Hospital.Admissions_ndc_index_FY ###
library(arules)
tempdf<-df
hist(log(tempdf$Hospital.Admissions_ndc_index_FY+1)) 
tempdf$Hospital.Admissions_ndc_index_FY<-df$Hospital.Admissions_ndc_index_FY
tempdf$Hospital.Admissions_ndc_index_FY<-log(tempdf$Hospital.Admissions_ndc_index_FY+1)
summary(tempdf$Hospital.Admissions_ndc_index_FY)
tempdf$Hospital.Admissions_ndc_index_FY<-cut(tempdf$Hospital.Admissions_ndc_index_FY,c(0,0.2,0.4,0.6,0.8,1.0,2) , include.lowest=TRUE)

exp(tempdf$Hospital.Admissions_ndc_index_FY)


##### age ######
hist(tempdf$mo12_Age)
summary(df$mo12_Age)
tempdf$mo12_Age<-cut(df$mo12_Age,c(0,20,40,60,80,100,120))

### mo12_Hospital.Admissions_PCP_index ###
tempdf$mo12_Hospital.Admissions_PCP_index<-df$mo12_Hospital.Admissions_PCP_index

for (i in (1:nrow(df))){
  if(tempdf$mo12_Hospital.Admissions_PCP_index[i]>=0){
    x<-as.numeric(tempdf$mo12_Hospital.Admissions_PCP_index[i])+1
    tempdf$mo12_Hospital.Admissions_PCP_index[i]<-log(x)}
  else{
    tempdf$mo12_Hospital.Admissions_PCP_index[i]<--1
  }
}

summary(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0)])
hist(df$mo12_Hospital.Admissions_PCP_index)

hist(log(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0)]),breaks = 20)

hist(log(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0)]+1))

hist(log(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0)]))

hist(as.numeric(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0)]>0.01))

hist(scale(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0)],center = T))
summary(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0)])

quantile(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0)], probs = seq(0, 1, by= 0.1))

hist(tempdf$mo12_Hospital.Admissions_PCP_index)

for (i in (1:nrow(df))){
  if(tempdf$mo12_Hospital.Admissions_PCP_index[i]>0){
    tempdf$mo12_Hospital.Admissions_PCP_index[i]<-log(tempdf$mo12_Hospital.Admissions_PCP_index[i])}
}

sum(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index==0)])
hist(df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>=0 & df$mo12_Hospital.Admissions_PCP_index<0.1)])

sum(df$mo12_Hospital.Admissions_PCP_index==0)
nrow(df)
sum(df$mo12_Hospital.Admissions_PCP_index==-1)
sum(df$mo12_Hospital.Admissions_PCP_index==-9)

tempdf$mo12_Hospital.Admissions_PCP_index[which(tempdf$mo12_Hospital.Admissions_PCP_index>0)]<-discretize(tempdf$mo12_Hospital.Admissions_PCP_index[which(tempdf$mo12_Hospital.Admissions_PCP_index>0)],method="interval",categories = 5)
summary(tempdf$mo12_Hospital.Admissions_PCP_index[which(tempdf$mo12_Hospital.Admissions_PCP_index>0)])

df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index<0 & df$mo12_Hospital.Admissions_PCP_index>-2)]

##### final discretization
tempdf$mo12_Hospital.Admissions_PCP_index<-df$mo12_Hospital.Admissions_PCP_index
summary(tempdf$mo12_Hospital.Admissions_PCP_index)
tempdf$mo12_Hospital.Admissions_PCP_index<-cut(tempdf$mo12_Hospital.Admissions_PCP_index,c(-9,-1,0,0.1,0.2,0.3,0.4,0.5),include.lowest = T)
tempdf$mo12_Hospital.Admissions_PCP_index

hist(log(30*df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>0)]))
hist(log(100*df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>0)]))
hist(log(1000*df$mo12_Hospital.Admissions_PCP_index[which(df$mo12_Hospital.Admissions_PCP_index>0)]))





summary(df$mo12_Hospital.Admissions_PCP_index)

##### mo12_Socio.Economic.Risk ####

hist(df$mo12_Socio.Economic.Risk)
summary(tempdf$mo12_Socio.Economic.Risk)
tempdf$mo12_Socio.Economic.Risk<-discretize(tempdf$mo12_Socio.Economic.Risk,method="interval",categories = 5)

#### Hospital.Admissions_icd_index_FY ######
 
hist(tempdf$Hospital.Admissions_icd_index_FY)
hist(log(tempdf$Hospital.Admissions_icd_index_FY))
hist(log(tempdf$Hospital.Admissions_icd_index_FY+1))

summary(tempdf$Hospital.Admissions_icd_index_FY)
summary(log(tempdf$Hospital.Admissions_icd_index_FY))

tempdf$Hospital.Admissions_icd_index_FY<-log(tempdf$Hospital.Admissions_icd_index_FY+1)
tempdf$Hospital.Admissions_icd_index_FY<-discretize(tempdf$Hospital.Admissions_icd_index_FY,method = 'interval',categories = 5)

#### mo12_ER.Visits_PCP_index #####

hist(tempdf$mo12_ER.Visits_PCP_index,breaks=20)
head(tempdf$mo12_ER.Visits_PCP_index)
hist(log(tempdf$mo12_ER.Visits_PCP_index))
summary(tempdf$mo12_ER.Visits_PCP_index)

hist(log(tempdf$mo12_ER.Visits_PCP_index[which(tempdf$mo12_ER.Visits_PCP_index>=0)]))
length(which(df==-1 | df==-9))

tempdf$mo12_ER.Visits_PCP_index<-df$mo12_ER.Visits_PCP_index
tempdf$mo12_ER.Visits_PCP_index<-cut(tempdf$mo12_ER.Visits_PCP_index,c(-9,-1,0,0.03,1,2,3),include.lowest = T)

hist(log(10*df$mo12_ER.Visits_PCP_index[which(df$mo12_ER.Visits_PCP_index>0)]))



#### Average_Monthly_Cost_ndc_index_FY ####

hist(tempdf$Average_Monthly_Cost_ndc_index_FY)
summary(tempdf$Average_Monthly_Cost_ndc_index_FY)
hist(log(tempdf$Average_Monthly_Cost_ndc_index_FY+1),breaks=20)

tempdf$Average_Monthly_Cost_ndc_index_FY<-cut(tempdf$Average_Monthly_Cost_ndc_index_FY,c(0,1000,5000,10000,15000,20000),include.lowest = T)

####  ER.Events_icd_index_FY #####
hist(tempdf$ER.Events_icd_index_FY)
summary(tempdf$ER.Events_icd_index_FY)
hist(log(tempdf$ER.Events_icd_index_FY+1),breaks=20)

tempdf$ER.Events_icd_index_FY<-cut(tempdf$ER.Events_icd_index_FY,c(0,1,5,10,15,22),include.lowest = T)

#### mo12_Access.To.Care.Risk ####
hist(tempdf$mo12_Access.To.Care.Risk)
hist(log(tempdf$mo12_Access.To.Care.Risk))

summary(tempdf$mo12_Access.To.Care.Risk)
summary(log(tempdf$mo12_Access.To.Care.Risk))

# tempdf$mo12_Access.To.Care.Risk<-log(tempdf$mo12_Access.To.Care.Risk)
tempdf$mo12_Access.To.Care.Risk<-cut(tempdf$mo12_Access.To.Care.Risk,c(0,20,40,60,80,100))

#### mon_enroll ####
hist(tempdf$Mon_Enroll)
summary(tempdf$Mon_Enroll)

tempdf$Mon_Enroll<-cut(tempdf$Mon_Enroll,c(0,10,20,30,40,50,60,70))

#### mo12_Months.since.Lipid.Panel ####

hist(tempdf$mo12_Months.since.Lipid.Panel,breaks=20)
summary(tempdf$mo12_Months.since.Lipid.Panel)


tempdf$mo12_Months.since.Lipid.Panel<-cut(tempdf$mo12_Months.since.Lipid.Panel,c(0,10,20,30,40,50,60,70))

#### Hospital.Admissions_icd_index_qt4 ####

hist(tempdf$Hospital.Admissions_icd_index_qt4)
hist(log(tempdf$Hospital.Admissions_icd_index_qt4))
hist(log(tempdf$Hospital.Admissions_icd_index_qt4+1))

summary(log(tempdf$Hospital.Admissions_icd_index_qt4+1))
tempdf$Hospital.Admissions_icd_index_qt4<-df$Hospital.Admissions_icd_index_qt4
tempdf$Hospital.Admissions_icd_index_qt4<-cut(tempdf$Hospital.Admissions_icd_index_qt4,c(0,1,2,4,10),include.lowest = T)

#### exp_mv_avg_Gaps.In.Care.Count ####
hist(tempdf$exp_mv_avg_Gaps.In.Care.Count)
hist(log(tempdf$exp_mv_avg_Gaps.In.Care.Count+1))

tempdf$exp_mv_avg_Gaps.In.Care.Count<-log(tempdf$exp_mv_avg_Gaps.In.Care.Count+1)
tempdf$exp_mv_avg_Gaps.In.Care.Count<-df$exp_mv_avg_Gaps.In.Care.Count
tempdf$exp_mv_avg_Gaps.In.Care.Count<-cut(tempdf$exp_mv_avg_Gaps.In.Care.Count,c(0,5,10,15,20))

#### mv_avg_index_Avg.HCC.Risk ###
hist(tempdf$mv_avg_index_Avg.HCC.Risk)
hist(log(tempdf$mv_avg_index_Avg.HCC.Risk+1))
summary(log(tempdf$mv_avg_index_Avg.HCC.Risk+1))

tempdf$mv_avg_index_Avg.HCC.Risk<-cut(tempdf$mv_avg_index_Avg.HCC.Risk,c(-0.5,-0.4,-0.2,0,0.2,0.4,0.6,0.7))

#### mo9_Avg.HCC.Risk ####

hist(tempdf$mo9_Avg.HCC.Risk)
hist(log(tempdf$mo9_Avg.HCC.Risk+1))

summary(log(tempdf$mo9_Avg.HCC.Risk+1))

tempdf$mo9_Avg.HCC.Risk<-log(tempdf$mo9_Avg.HCC.Risk+1)
tempdf$mo9_Avg.HCC.Risk<-cut(tempdf$mo9_Avg.HCC.Risk,c(0,0.5,1,1.5,2,2.5),include.lowest = T)

#### mo12_Avg.HCC.Risk ####

hist(tempdf$mo12_Avg.HCC.Risk)

hist(log(tempdf$mo12_Avg.HCC.Risk+1))
summary(log(tempdf$mo12_Avg.HCC.Risk+1))

tempdf$mo12_Avg.HCC.Risk<-log(tempdf$mo12_Avg.HCC.Risk+1)
tempdf$mo12_Avg.HCC.Risk<-cut(tempdf$mo12_Avg.HCC.Risk,c(0,0.5,1,1.5,2,2.5),include.lowest = T)

#### mo12_Months.Since.Flu.Shot ####
hist(tempdf$mo12_Months.Since.Flu.Shot)
tempdf$mo12_Months.Since.Flu.Shot<-discretize(tempdf$mo12_Months.Since.Flu.Shot,method = "interval",categories = 5)

#### mv_avg_index_Gaps.In.Care.Count ####
hist(tempdf$mv_avg_index_Gaps.In.Care.Count)
hist(log(tempdf$mv_avg_index_Gaps.In.Care.Count+1))
summary(tempdf$mv_avg_index_Gaps.In.Care.Count)

tempdf$mv_avg_index_Gaps.In.Care.Count<-cut(tempdf$mv_avg_index_Gaps.In.Care.Count,c(-3,-2,-1,0,1,2,3))

#### simple_mv_avg_mtl_Cost  ####

hist(df$simple_mv_avg_mtl_Cost)
hist(log(tempdf$simple_mv_avg_mtl_Cost+1))  

summary(df$simple_mv_avg_mtl_Cost)
hist(scale(tempdf$simple_mv_avg_mtl_Cost,center = T))
plot(df$simple_mv_avg_mtl_Cost,df$prob_new)

tempdf$simple_mv_avg_mtl_Cost<-df$simple_mv_avg_mtl_Cost
tempdf$simple_mv_avg_mtl_Cost<-cut(tempdf$simple_mv_avg_mtl_Cost,c(-2000,0,1000,45000))

#### Average_Monthly_Cost_icd_index_qt1 ####
hist(tempdf$Average_Monthly_Cost_icd_index_qt1)
summary(tempdf$Average_Monthly_Cost_icd_index_qt1)
hist(log(tempdf$Average_Monthly_Cost_icd_index_qt1+1))

tempdf$Average_Monthly_Cost_icd_index_qt1<-log(tempdf$Average_Monthly_Cost_icd_index_qt1+1)
tempdf$Average_Monthly_Cost_icd_index_qt1<-cut(tempdf$Average_Monthly_Cost_icd_index_qt1,c(0,1,4,8,10,12),include.lowest = T)

########## RULE MINING ##########
trans<-as(tempdf, "transactions")

rules<-apriori(trans,parameter = list(supp = 0.005, conf = 0.75, target = "rules",minlen=3),appearance = list(rhs=c("prob_new=risk"),default='lhs'))
top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift,5))
inspect(rules)


