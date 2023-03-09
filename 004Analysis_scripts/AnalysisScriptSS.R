## Header---------------------------
##
## Script name: Analysis of Shrub Data Script
##
## Purpose of script: regressing remotely sensed GEE metrics with field data on shrub severity and mortality
##
## Author: Hannah Fertel
##
## Date Created: 2022-01-11
##
## Copyright (c) Hannah Fertel, 2022
## Email: hmfertel@berkeley.edu

## ---------------------------

#Packages
library(tidyverse)
library(lme4)

####RDNR for LNU Plots####
rdnbr_data<-read.csv("002Data/LNUDNBR.csv")
sev_data<-read.csv("002Data/AGPlots_sev.csv")


#need to get average by plot for severity metric 
sev_data1<-sev_data %>% 
  group_by(PlotID_old) %>% 
  summarise(Diam=mean(Mean_diam)) %>% 
  rename(PlotID=PlotID_old) %>% 
  mutate(site=substr(PlotID,start=1, stop=3))#getting site variable

rdnbr_data$PlotID=rdnbr_data$Transect
rdnbr_join<-left_join(sev_data1, rdnbr_data,by="PlotID")

rdnbr_join<- rdnbr_join %>% 
  rename(rdnbr=MEAN)


model1<-lm(rdnbr_join$rdnbr~scale(rdnbr_join$Diam))
summary(model1)

par(mfrow = c(2, 2))
plot(model1)
#not meeting assumptions....

#regress diam on rdnbr
model2<-lm(rdnbr_join$Diam~scale(rdnbr_join$rdnbr))


summary(model2)
par(mfrow = c(2, 2))
plot(model2)


#add site to dataframe
#add component that's number of fires to joined dataframe
#add year of last fire to joined dataframe

LNUfireHist<-read.csv("002Data/FireHistory_LNU.csv")
LNUfireHist<-LNUfireHist %>% 
  rename(PlotID=Transect)

rdnbr_join1<-left_join(rdnbr_join,LNUfireHist, by="PlotID")



#model that's rdnbr regressed on number of fires 
#recovery regressed on number of fires?

#severity metric that was used on the ground IS correlated with rdnbr in above model
#and does seem to show relationship between intensity and remote indices 

library(lme4)


#Do hierarchical model as well including site and/or # of fires as random variable? or include as variable



plot(rdnbr_join$RASTERVALU,rdnbr_join$Diam)


#####Hopland RDNBR Data####
#reading in Hopland data
sev_hopland<- read.csv("002Data/Hopland2018sev.csv")
rdnbr_hopland<-read.csv("002Data/HoplandDNBR.csv")

#reading in other plot and transect info
transplotinfo<-read.csv("002Data/TransectInfo.csv")
treatment<-read.csv("002Data/Hopland_Treatment_Info.csv")


#creating key for each transect with treatment, year, and plot id
treatment2<-treatment %>% 
  dplyr::select(Location,Treatment,TreatmentYear,Plot) %>% 
  rename(PlotID=Plot)
  

transectkey<-transplotinfo %>% 
  dplyr::select(PlotID,TransectID)
transectkey<-left_join(transectkey,treatment2)


#ok now processing severity data
sev_hopland1<-sev_hopland %>% 
  mutate(Avesev=rowMeans(dplyr::select(sev_hopland,
                                 starts_with('min')), na.rm = TRUE)) %>% 
  na.omit(min_0_1) %>% 
  dplyr::select(c(PlotID, TransectID,Avesev) )


#join severity data to rdbnr data by transect ID
rdnbr_hopland$TransectID<-rdnbr_hopland$Transect
sev_hopland2<-left_join(x=sev_hopland1,y=rdnbr_hopland, by="TransectID")


#joining transect key to severity data to add to model
sev_hopland2<-left_join(x=sev_hopland2,y=transectkey, by="TransectID")


#modeling sev and rdnbr?
model3<-lm(sev_hopland2$Avesev~scale(sev_hopland2$MEAN))
summary(model3)

#####SEVERITY MODEL LINEAR MODEL####
#####combining both Hopland and LNU transects? Or do the different methods make this not feasible
#scale both independently, then combine and have site as random, num fires included?
#methods are so different, that scaling then combining is what makes sense to me, or keeping separate
#make them have same colums
#add treament column with "fire" to LNU, add numprev disturbances to Hopland

combo1<-sev_hopland2 %>% 
  dplyr::select(TransectID,Avesev,MEAN,Treatment,TreatmentYear,Location) %>% 
  rename(Rdnbr=MEAN,Diam=Avesev,Year=TreatmentYear,site=Location) %>% 
  mutate(Rdnbrscale=scale(Rdnbr),DiamScale=scale(Diam),studyarea="Hopland") %>% 
  mutate(NumPrevFires=ifelse(Treatment=="Control",0,1))

combo2<-rdnbr_join1 %>% 
  dplyr::select(PlotID,Diam,rdnbr,Year,NumPrevFires,site) %>% 
  rename(Rdnbr=rdnbr, TransectID=PlotID) %>% 
  mutate(Rdnbrscale=scale(Rdnbr),DiamScale=scale(Diam),studyarea="LNU") %>% 
  mutate(Treatment="Fire")
  
#rbind
combo3<-rbind(combo1,combo2)

#generate time since fire variable 
#replace NAs 
combo4<-combo3 %>% 
  mutate(Year=ifelse(is.na(Year),0,Year)) %>% 
  mutate(TSF=ifelse(studyarea=="Hopland",2018,2020)) %>% 
  mutate(TSF=TSF-Year) %>% 
  mutate(TSF=ifelse(Year==0,65,TSF))

#read in NDVI data--need to run spatial script first

ndvi<-transects1$Mean
transects<-transects1$Transect
ndvi<-as.data.frame(ndvi)
ndvi<-cbind(ndvi,transects)
ndvi<-ndvi %>% 
  rename(TransectID=transects)
#read in all metrics
allmetrics<-read_rds("002Data/metrics_Transects.rds") %>% 
  rename(TransectID=Transect) %>% 
  dplyr::select(-geometry)

combo4<-left_join(combo4,allmetrics)


#goal is to generate model of severity metric taking into account other variables
#add scaled values per site that are for RBR and dnbr
#ok super unclear on why I'm getting different values...

#assumptions to test first?

library(lme4)
library(car)

#correlations among variables
num<-select_if(combo4,is.numeric)
num<-na.omit(num)

cor(num)
#ndvi highly correlated to num prev fires...
#normality of responses?
hist((combo4$DiamScale))
hist(scale(combo4$rDNBR))
#consider transforming the data??
  

#linear models, simple

model4<-lm(DiamScale~Rdnbrscale+studyarea,data=combo4)
summary(model4)
#new rdnbr stuff
model4<-lm(DiamScale~scale(Rdnbr)+studyarea,data=combo4)
summary(model4)
model5<-lm(DiamScale~Rdnbrscale+site+ NumPrevFires ,data=combo4)
summary(model5)
model5.1<-model5<-lm(DiamScale~Rdnbrscale+site+TSF+NumPrevFires ,data=combo4)
summary(model5.1)
model5.2<-model5<-lm(DiamScale~Rdnbrscale+ NumPrevFires+TSF ,data=combo4)
summary(model5.2)
par(mfrow = c(2, 2))
plot(model5.1)

#mixed model
model6<-lmer(DiamScale~log(rDNBR)+ NumPrevFires+ TSF+(1|site),data=combo4)
summary(model6)
car::Anova(model6,type=3)#extract pvalues
#interaction term
model6.1<-lmer(DiamScale~log(rDNBR)*TSF+ NumPrevFires+(1|site),data=combo4)
summary(model6.1)
car::Anova(model6.1,type=3)#extract pvalues

parameters::p_value(model6)#extract pvalues
par(mfrow = c(2, 2))
plot(model6)
AIC(model6)
#other metrics
model6.1<-lmer(DiamScale~scale(DNBR)+ NumPrevFires+ TSF+(1|site),data=combo4)
summary(model6.1)
car::Anova(model6.1,type=3)
model6.2<-lmer(DiamScale~scale(RBR)+ NumPrevFires+ TSF+(1|site),data=combo4)
summary(model6.2)
car::Anova(model6.2,type=3)
#in general with all the models what we're seeing is that time since fires has the strongest signal
#very important variable, confirming what previous studies have found

#######Severity Model-Bayesian##### 
library(brms) ##*** 
library(tidybayes)
library(bayesplot)
library(rstan)
library(MCMCglmm)
library(ape)
library(emmeans)

mod3 <- brm(data = combo4, 
            family = gaussian,
            DiamScale ~ 1 + scale(rDNBR) +  
              TSF+(1|site),
            iter = 3000,
            warmup = 1000, cores = 4, chains = 4,
            seed = 14)
#save(mod3, file = "models/mod3_treedensity.rda")
#load("models/mod3_treedensity.rda")

prior_summary(mod3)
summary(mod3)
pp_check(mod3) ## posterior predictive check
plot(mod3) ### look at the fit of each of the model parameters
conditional_effects(mod3)

#

#categorical variable for TSF?

#####SEM Severity######
#need conceptual model of the relationships, and to look at distribution of data 
hist(combo4$DiamScale)

plot(combo4$DiamScale~combo4$TSF)
plot(combo4$DiamScale~combo4$NumPrevFires)


#Lavaan
library(lavaan)


k_mod <- "
  DiamScale ~ rDNBR+ NumPrevFires+ TSF
  rDNBR ~~ TSF"


k_fit_lavaan <- sem(k_mod, data = combo4)
summary(k_fit_lavaan)


#brms
#######CART#####
library(party)
library(rpart)


#ctree
tree1<-ctree(DiamScale~scale(RBR)+ NumPrevFires+ TSF, data=combo4)
plot(tree1)

#####ALTERNATIVE APPROACH--THRESHOLDS
#do I need to think about using clustering algorithims to find thresholds in rdnbr scale for on the ground metrics?
#carry those through to if they carry predictions for recovery?
#work backwards from recovery??


####RECOVERY MODEL LINEAR MODEL####
#linkage with rdnbr value and percentage shrub cover along transect?
#read in point line and continuous cover
contcvr<-read.csv("002Data/Cont_Shrub_Cvr_LNU22.csv")

#cleaning continuous cover data and will get %cover of shrub...
#want to drop NAs (blank lines), get length of each segment
#then will group by species/NS cover for plot and get totals

contcvr2<-contcvr %>% 
  drop_na(Start) %>% 
  rename(Transect=ï..TransectID) %>% 
  mutate(length=abs(End-Start)) %>%#getting length of each segment
  group_by(Transect,Spec) %>% 
  summarise(totallength=sum(length)) %>% 
  ungroup() %>% 
  group_by(Transect) %>% 
  mutate(transleng=50) %>% 
  ungroup() %>% 
  mutate(perc=totallength/transleng)
#qua005 issue
qua005<-contcvr %>% 
  rename(TransectID=ï..TransectID)%>%
  filter(TransectID=="QUA005") %>% 
  mutate(leng=End-Start) %>% 
  filter(Spec=="NS")

S_NS<-contcvr2 %>% 
  filter(Spec=="NS") %>% 
  mutate(S_leng=50-totallength, S_perc=1-perc)

#join rdnbr values by transect
S_NS2<-left_join(x=S_NS,y = rdnbr_data,by="Transect")

par(mfrow = c(2, 2))


##Hopland Cover Data##
#read in Hopland cover data
hoplandcover<-read.csv("002Data/HoplandShrubCover2021.csv")

hoplandcover<-hoplandcover %>% 
  filter(SurveyYear==2021) %>% 
  dplyr::select(ID,Transect,Plant, Count, SumPercent,AvePercent) %>% 
  mutate(TransectID=Transect)

#join to existing Hopland dataset
Hoplandjoin<-left_join(sev_hopland1,hoplandcover,"TransectID")

#just shrub vs. no shrub?
HoplandSNS<-Hoplandjoin %>% 
  filter(Plant=="No Shrub") %>% 
  mutate(shrub=1-SumPercent)

#####Combination cover by percentage for Hopland and LNU####
#best way to incorporate species into the equation??
#need to assess if this makes sense given that one is three years post fire and one is two years?

#bring both in as S/NS 
CombocoverH<-HoplandSNS %>% 
  dplyr::select(TransectID,Avesev,shrub) %>% 
  mutate(NS=1-shrub) %>% 
  rename(S=shrub)

CombocoverL<-S_NS %>% 
  dplyr::select(Transect,S_perc) %>% 
  mutate(NS=1-S_perc,Avesev=100) %>% 
  rename(S=S_perc,TransectID=Transect)
combocover<-rbind(CombocoverH,CombocoverL)

#left join by transect information
combocover1<-left_join(combocover,combo4,by="TransectID")
hist(combocover1$S)

#big issue here is finding the right distribution for this data 
#perhaps beta distribution is the best...
#alternatively can do binomial model, successes as counts/100? 

#LNU cover model
LNUcover<-combocover1 %>% 
  filter(studyarea=="LNU")
hist(LNUcover$S)
hist(log(LNUcover$S)) #decently normal like this
modelcoverL<-lmer(log(S)~scale(rDNBR)+ NumPrevFires+ TSF+(1|site),data=LNUcover)
summary(modelcoverL)
car::Anova(modelcoverL,type=3)

#Hopland cover model
Hopcover<-combocover1 %>% 
  filter(studyarea=="Hopland")
hist(Hopcover$S) #more normal in raw form
hist(log(Hopcover$S))
modelcoverH<-lmer(S~scale(rDNBR)+ NumPrevFires+ TSF+(1|site),data=Hopcover)
summary(modelcoverH)
car::Anova(modelcoverH,type=3)


#cover model, both
#add variable for years since fire
combocover1<-combocover1 %>% 
  mutate(years= as.factor(ifelse(studyarea=="Hopland",3,2)))
modelcover<-lmer(S~scale(rDNBR)+ NumPrevFires+ TSF+years+(1|site),data=combocover1)
summary(modelcover)
car::Anova(modelcover,type=3)



######Bayesian Cover Model#####
#bayesian model? 
library(brms) ##*** 
library(tidybayes)
library(bayesplot)
library(rstan)
library(MCMCglmm)
library(ape)
library(emmeans)

mod3 <- brm(data = combocover1, 
            family = gaussian,
            S ~ 1 + scale(rDNBR) +  
              TSF+ NumPrevFires+years+ (1|site),
            iter = 3000,
            warmup = 1000, cores = 4, chains = 4,
            seed = 14)


prior_summary(mod3)
summary(mod3)
pp_check(mod3)
## posterior predictive check
plot(mod3) ### look at the fit of each of the model parameters


#I think it needs to be a beta model--need to nail down how to format this
#beta binomial?

betamod1 <- brm(S ~ 1 + scale(rDNBR) +  
                  TSF+ NumPrevFires+years+ (1|site), data=combocover1, family="beta",
                iter = 3000,
                warmup = 1000, cores = 4, chains = 4,
                seed = 14)

prior_summary(betamod1)
summary(betamod1)

pp_check(betamod1)
plot(betamod1)

#interpreting outputs from this??

######Beta model by seeding strategy#####
#need percent cover by each species type for each transcript# 





#######CART Recovery#####
library(party)
library(rpart) 

#split into low, moderate and high cover and then regress using the trees...


#ctree
tree1<-ctree(S~scale(rDNBR)+ NumPrevFires+TSF, data=Hopcover)
plot(tree1)


#####By Species continuous cover####


######point line cover and type and severity#####
#need to clean the point line data
#get total tally per transect, divide by total to get percentages?
#sum up all species/shrub hits to get broad types --break down with species too?

ptline<-read.csv("002Data/ptline_Cvr_LNU22.csv")

ptline2<-ptline %>% 
  drop_na(Total) %>%
  rename(Transect=ï..TransectID) %>% 
  group_by(Transect) %>% 
  add_tally(Total) %>% 
  filter(Total<99) %>% 
  ungroup()

ptline3<-ptline2 %>% 
  group_by(Transect, Cover.Type) %>% 
  summarise(Total=Total, n=n) %>% 
  summarise(Total=sum(Total),n=n) %>% 
  unique() %>% 
  mutate(perc=round(Total/n,digits=2)) %>% 
  ungroup()

unique(ptline3$Cover.Type)

#join with species codes to breakdown by type (i.e. Grass)

Speccodes<-read.csv("002Data/SpeciesCodes_LNU.csv")

ptline3<-ptline3 %>% 
  mutate(CODE=Cover.Type) %>% 
  left_join(y=Speccodes, by="CODE")
#group just by type
ptline4<-ptline3 %>% 
  group_by(Transect, TYPE) %>% 
  summarise(perc=sum(perc)) %>% 
  ungroup()

##benefit of this is using count data models...


######Binomial Model Cover#####