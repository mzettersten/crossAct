#load packages
library(here)
#set data path #version 0.1
data_path <- here::here("data","Crossact_data.csv")
source('summarizeData.R') #helper functions, from R cookbook (http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/)
library(tidyverse) #version 1.2.1
library(cowplot) #version 1.0.0
theme_set(theme_cowplot())
library(lme4) #version 1.1-21
library(car) #version 3.0-3
library(AICcmodavg) #version 2.2-2
library(ggstance) #version 0.3.3

#### Read in Data ####
d <- read.csv(data_path)

#### Demographics and Exclusions ####
#count exclusions
exclusions <-  d %>%
  group_by(subject,experiment_name) %>%
  summarize(Exclude=Exclude[1]) %>%
  ungroup()  %>%
  group_by(experiment_name) %>%
  summarize(N=n(),num_exclusions=sum(Exclude!="N"))

exclusions

#demographics (exclusions filtered)
demographics <- d %>%
  filter(Exclude=="N") %>%
  group_by(subject,age_group,experiment_name) %>%
  summarize(Gender=Gender[1],Age=Age[1],L1_english=L1_english[1],languages_besides_english_yn=languages_besides_english_yn[1],L1=L1[1],L1percent=L1percent[1]) %>%
  ungroup() %>%
  group_by(age_group,experiment_name)  %>%
  summarize(
    N=n(),
    gender_f=sum(Gender=="female"),
    mean_age=round(mean(Age,na.rm=T),2),
    sd_age=round(sd(Age,na.rm=T),2),
    min_age=round(min(Age,na.rm=T),2),
    max_age=round(max(Age,na.rm=T),2),
    native_english=ifelse(is.na(sum(L1_english=="English")),sum(L1=="English",na.rm=T),sum(L1_english=="English",na.rm=T)),
    languages_besides_english=ifelse(age_group[1]=="adults",NA,sum(languages_besides_english_yn=="Yes",na.rm=T)),
    monolingual=ifelse(age_group[1]=="adults",NA,sum(L1percent>=90,na.rm=T))
  )
demographics 

#remove excluded participants
d <- d %>%
  filter(Exclude=="N")

#### By-Subject Summaries ####
#Summarize sampling and test behavior by subject
#used in later analyses and plotting

#selections by subject
subj_selection <- d %>%
  filter(trialType=="selection") %>%
  group_by(experiment_name,ambiguity_condition,subject) %>%
  summarize(
    N=n(),
    prop_ambig_selection=sum(selectionType!="low")/N,
    num_ambig_selection=sum(selectionType!="low"))

#test performance by subject
subj_test <- d %>%
  filter(trialType=="test") %>%
  group_by(experiment_name,ambiguity_condition,subject) %>%
  summarize(
    N=n(),
    accuracy=mean(isRight,na.rm=T))

#test performance split by item type (ambiguous vs. non-ambiguous)
subj_test_item <- d %>%
  filter(trialType=="test") %>%
  group_by(experiment_name,ambiguity_condition,subject,targetType,targetIsAmbiguous,targetIsAmbiguousYN) %>%
  summarize(
    N=n(),
    accuracy=mean(isRight,na.rm=T))

#### Experiment 1 - Sampling ####

## descriptives
subj_summary_1 <-  subj_selection %>%
  filter(experiment_name=="Experiment 1") %>%
  group_by(ambiguity_condition) %>%
  summarize(
    N=n(),
    prop_ambiguous=mean(prop_ambig_selection),
    ci_ambiguous=qt(0.975, N-1)*sd(prop_ambig_selection,na.rm=T)/sqrt(N),
    prop_ambiguous_lower_ci=prop_ambiguous-ci_ambiguous,
    prop_ambiguous_upper_ci=prop_ambiguous+ci_ambiguous,
  )
subj_summary_1

## logistic mixed-effects model
m <- glmer(isAmbiguous~(1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment 1"&trialType=="selection"&ambiguity_condition=="ambiguous"),family=binomial, glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)
confint(m,method="Wald")[3,]

## non-parametric test (wilcoxon signed rank test)
wilcox.test(filter(subj_selection, experiment_name=="Experiment 1"&ambiguity_condition=="ambiguous")$prop_ambig_selection,mu=1/2, conf.int=T, conf.level=0.95)

## plot
#create data frame with model predictions of logistic mixed-effects model
m <- glmer(isAmbiguous~(1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment 1"&trialType=="selection"&ambiguity_condition=="ambiguous"),family=binomial, glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
model_pred <- data.frame(ambiguity_condition="ambiguous")
pY <- predictSE(m,model_pred, type="response")
model_pred <- model_pred %>%
  mutate(
    experiment_name= "Experiment 1",
    prop_ambiguous = pY$fit,
    prop_ambiguous_lower_ci = pY$fit - 1.96*pY$se.fit,
    prop_ambiguous_upper_ci = pY$fit + 1.96*pY$se.fit)

#create plot
p_exp1_sampling <- ggplot(subset(model_pred,ambiguity_condition=="ambiguous"),aes(x=experiment_name,y=prop_ambiguous,color=ambiguity_condition,fill=ambiguity_condition))+
  geom_bar(stat="identity",size=2.5,fill="white",width=0.5)+
  geom_dotplot(data=subset(subj_selection,experiment_name=="Experiment 1"&ambiguity_condition=="ambiguous"), aes(y=prop_ambig_selection),binaxis="y",stackdir="center",alpha=0.5,dotsize=0.6)+
  geom_errorbar(aes(ymin=prop_ambiguous_lower_ci,ymax=prop_ambiguous_upper_ci),width=0,size=1.2)+
  ylab("Probability of \nAmbiguous Selection")+
  geom_hline(yintercept=0.5,linetype="dotted")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),limits=c(0,1.08))+
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1")+
  theme_classic(base_size=24)+
  theme(legend.position="none")+
  scale_x_discrete(name="Experiment 1")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
p_exp1_sampling


#### Experiment 1 - Test ####

## descriptives
subj_summary_test_1 <-  subj_test %>%
  filter(experiment_name=="Experiment 1") %>%
  group_by(experiment_name,ambiguity_condition) %>%
  summarize(
    N=n(),
    prop_correct=mean(accuracy,na.rm=T),
    ci=qt(0.975, N-1)*sd(accuracy,na.rm=T)/sqrt(N),
    prop_correct_lower_ci=prop_correct-ci,
    prop_correct_upper_ci=prop_correct+ci
  )
subj_summary_test_1 

## split by item, within-subjects corrected CIs
subj_summary_test_item_1 <-  summarySEwithin(
  filter(
    subj_test_item,
    experiment_name=="Experiment 1"),
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("targetIsAmbiguousYN"),
  idvar="subject") %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci
  ) %>%
  select(-accuracy_norm,-sd,-se,-ci)
subj_summary_test_item_1

## testing overall accuracy against chance
d$offset.125 <- 1/8
m <- glmer(isRight~offset(logit(offset.125))+(1|subject)+(1|targetImage),data=filter(d,experiment_name=="Experiment 1"&trialType=="test"),family=binomial,glmerControl(optimizer="bobyqa"))
summary(m)

## testing difference between items
d$targetIsAmbiguousC <- ifelse(!is.na(d$targetIsAmbiguous) & d$targetIsAmbiguous==1,0.5,
                                        ifelse(!is.na(d$targetIsAmbiguous) & d$targetIsAmbiguous==0,-0.5,NA))
m <- glmer(isRight~1+targetIsAmbiguousC+(1+targetIsAmbiguousC|subject)+(1|targetImage),data=filter(d,(experiment_name=="Experiment 1")&trialType=="test"),family=binomial,glmerControl(optimizer="bobyqa"))
summary(m)
confint(m, method="Wald")

## by-item test plot
ggplot(subj_summary_test_item_1,aes(x=targetIsAmbiguousYN,y=accuracy,color=targetIsAmbiguousYN,fill=targetIsAmbiguousYN))+
  geom_bar(stat="identity",size=1.5,position=position_dodge(.53),width=0.5,alpha=0)+
  geom_point(data=filter(subj_test_item,experiment_name=="Experiment 1"),aes(fill=targetIsAmbiguousYN),alpha=0.5,position=position_jitterdodge(dodge.width=0.53,jitter.width=0.1,jitter.height=0.02))+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0.0,size=0.8,position=position_dodge(.53))+
  scale_color_manual(name="Item Type",
                     limits=c("no","yes"),
                     labels=c("disambiguated","ambiguous"),
                     values=c("#4DAF4A","#E41A1C"))+
  scale_fill_manual(name="Item Type",
                    limits=c("no","yes"),
                    labels=c("disambiguated","ambiguous"),
                    values=c("#4DAF4A","#E41A1C"))+
  scale_x_discrete(name = "Item Type",
                   limits=c("no","yes"),
                   labels=c("disambiguated","fully\nambiguous"))+
  ylab("Test Accuracy")+
  geom_hline(yintercept=1/8,linetype="dashed",size=1.1)+
  theme_classic(base_size=16)+
  theme(legend.position="none", axis.text.x=element_text(size=15))
ggsave("../figures/exp1_test_by_item.pdf",width=11, height=7,dpi=600)
ggsave("../figures/exp1_test_by_item.png",width=11, height=7,dpi=600)
#ggsave("../figures/exp1_test_by_item.tiff",width=11, height=7)

## Relationship between sampling and test accuracy
#join sampling and test
subj_selection <- subj_selection %>%
  left_join(subj_test,by=c("subject","experiment_name","ambiguity_condition"))

#join sampling and test by item
subj_selection_item <- subj_selection %>%
  rename(overall_test_accuracy=accuracy) %>%
  left_join(subj_test_item,by=c("subject","experiment_name","ambiguity_condition"))

#correlation between preference for sampling ambiguous items and overall test performance
cor.test(subset(subj_selection,ambiguity_condition=="ambiguous"&experiment_name=="Experiment 1")$accuracy,subset(subj_selection,ambiguity_condition=="ambiguous"&experiment_name=="Experiment 1")$prop_ambig_selection)

##plot
p_exp1_sampling_test <- ggplot(filter(subj_selection,experiment_name=="Experiment 1"),aes(prop_ambig_selection,accuracy, color=ambiguity_condition))+
  geom_violin(aes(group=prop_ambig_selection),draw_quantiles=c(0.5))+
  geom_dotplot(aes(group=prop_ambig_selection,fill=ambiguity_condition),alpha=0.6,binaxis="y",stackdir="center",dotsize=0.8)+
  scale_color_manual(limits=c("ambiguous"),
                     values=c("#E41A1C"))+
  geom_smooth(method="lm",color="black",fill="#4B0082",alpha=0.3)+
  theme_classic()+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),limits=c(0,1.08))+
  theme_classic(base_size=24)+
  ylab("Test Accuracy")+
  theme(legend.position="none")+
  xlab("Probability of \nAmbiguous Selection")

##combine into grid with sampling preference plot
plot_grid(p_exp1_sampling,p_exp1_sampling_test,labels=c("A","B"),label_size=24,align="h")
ggsave("../figures/exp1_sampling_test.pdf",width=11, height=6.95,dpi=600)
ggsave("../figures/exp1_sampling_test.png",width=11, height=6.95,dpi=600)
#ggsave("../figures/exp1_sampling_test.tiff",width=11, height=6.95)

#### Experiment 2 - Sampling ####

#descriptives
subj_summary_2 <-  subj_selection %>%
  filter(experiment_name=="Experiment 2") %>%
  group_by(ambiguity_condition) %>%
  summarize(
    N=n(),
    prop_ambiguous=mean(prop_ambig_selection),
    ci_ambiguous=qt(0.975, N-1)*sd(prop_ambig_selection,na.rm=T)/sqrt(N),
    prop_ambiguous_lower_ci=prop_ambiguous-ci_ambiguous,
    prop_ambiguous_upper_ci=prop_ambiguous+ci_ambiguous,
  )
subj_summary_2

##logistic mixed-effects model
d$offset.33 <- 1/3
m <- glmer(isAmbiguous~offset(logit(offset.33))+ (1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment 2"&trialType=="selection"),family=binomial, glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)
confint(m,method="Wald")

##non-parametric test (Wilcoxon)
wilcox.test(filter(subj_selection,experiment_name=="Experiment 2")$prop_ambig_selection,mu=1/3, conf.int=T, conf.level=0.95)

##plot
ggplot(filter(subj_selection,experiment_name=="Experiment 2"),aes(x=num_ambig_selection,fill=as.factor(num_ambig_selection),color=as.factor(num_ambig_selection)))+
  scale_fill_brewer(palette="Set1",direction=-1)+
  scale_color_brewer(palette="Set1",direction=-1)+
  geom_bar(stat="count",size=1.5,alpha=0.2,width=0.5)+
  geom_vline(xintercept=4/3,linetype="dashed")+
  theme(axis.title = element_text(size=20),
        axis.text  = element_text(size=16),
        legend.position="none")+
  ylab("Number of subjects")+
  xlab("Number of ambiguous selections")+
  scale_x_continuous(breaks=c(0,1,2,3,4), limits=c(-0.5,4))+
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,20))

#save plot
ggsave("../figures/exp2_sampling.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp2_sampling.png",width=9,height=6,dpi=600)
#ggsave("../figures/exp2_sampling.tiff",width=9,height=6)

#relationship with age
#predict ambiguous selections from age
m <- glmer(isAmbiguous~Age+(1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment 2"&trialType=="selection"),family=binomial,glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)
confint(m, method="Wald")

#plot model predictions
pX <- data.frame(Age=seq(min(subset(d,experiment_name=="Experiment 2"&trialType=="selection")$Age,na.rm=T),max(subset(d,experiment_name=="Experiment 2"&trialType=="selection")$Age,na.rm=T),by=0.1))
pY <- predictSE(m,pX,re.form=NA,type="response")
pX$isAmbiguous <- pY$fit
pX$YLower <- pY$fit-pY$se.fit
pX$YUpper <- pY$fit+pY$se.fit
ggplot(pX,aes(Age,isAmbiguous))+
  geom_violinh(data=subset(d,experiment_name=="Experiment 2"&trialType=="selection"),aes(y=isAmbiguous,group=isAmbiguous),scale="count",width=0.1, trim=F)+
  geom_jitter(data=subset(d,experiment_name=="Experiment 2"&trialType=="selection"),aes(y=isAmbiguous,group=isAmbiguous),height=0.01)+
  geom_smooth(aes(ymin=YLower,ymax=YUpper),stat="identity",color="#E41A1C",fill="#E41A1C")+
  geom_hline(yintercept=1/3,linetype="dotted")+
  theme_classic(base_size=16)+
  xlab("Age (in years)")+
  scale_x_continuous(breaks=c(3,4,5,6,7,8))+
  ylab("Proportion of ambiguous selections")+
  theme(axis.title = element_text(size=20),
        axis.text  = element_text(size=16))

#save plot
ggsave("../figures/exp2_sampling_age.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp2_sampling_age.png",width=9,height=6,dpi=600)
#ggsave("../figures/exp2_sampling_age.tiff",width=9,height=6)

#### Experiment 2 - Test ####

##descriptives
#overall
subj_summary_test_2 <-  subj_test %>%
  filter(experiment_name=="Experiment 2") %>%
  group_by(experiment_name,ambiguity_condition) %>%
  summarize(
    N=n(),
    prop_correct=mean(accuracy,na.rm=T),
    ci=qt(0.975, N-1)*sd(accuracy,na.rm=T)/sqrt(N),
    prop_correct_lower_ci=prop_correct-ci,
    prop_correct_upper_ci=prop_correct+ci
  )
subj_summary_test_2

#by item type
subj_summary_item_2 <- summarySEwithin(
  filter(
    subj_test_item,
    (experiment_name=="Experiment 2")),
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("targetIsAmbiguousYN"),
  idvar="subject") %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci
  )
subj_summary_item_2

## testing overall accuracy against chance
d$offset.17 <- 1/6
m <- glmer(isRight~offset(logit(offset.17))+(1|subject)+(1|targetImage),data=filter(d,experiment_name=="Experiment 2"&trialType=="test"),family=binomial,glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)

## accuracy by item type
##logistic mixed=effects model
m <- glmer(isRight~1+targetIsAmbiguousC+(1+targetIsAmbiguousC|subject)+(1|targetImage),data=subset(d,trialType=="test"&experiment_name=="Experiment 2"),family=binomial,glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)
confint(m, method="Wald")

## create plot
ggplot(subj_summary_item_2,aes(x=targetIsAmbiguousYN,y=accuracy,color=targetIsAmbiguousYN,fill=targetIsAmbiguousYN))+
  geom_bar(stat="identity",size=1.5,position=position_dodge(.53),width=0.5,alpha=0)+
  geom_dotplot(data=filter(subj_test_item,experiment_name=="Experiment 2"),alpha=0.6,binaxis="y",stackdir="center",dotsize=0.8)+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0.0,size=0.6,position=position_dodge(.53))+
  scale_color_manual(name="Item Type",
                     limits=c("no","yes"),
                     labels=c("disambiguated","ambiguous"),
                     values=c("#4DAF4A","#E41A1C"))+
  scale_fill_manual(name="Item Type",
                    limits=c("no","yes"),
                    labels=c("disambiguated","ambiguous"),
                    values=c("#4DAF4A","#E41A1C"))+
  scale_x_discrete(name = "Item Type",
                   limits=c("no","yes"),
                   labels=c("disambiguated","ambiguous"))+
  ylab("Test Accuracy")+
  geom_hline(yintercept=1/6,linetype="dashed",size=1.1)+
  theme_classic(base_size=16)+
  theme(legend.position="none", axis.text.x=element_text(size=15))
ggsave("../figures/exp2_test_item.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp2_test_item.png",width=9,height=6,dpi=600)
#ggsave("../figures/exp2_test_item.tiff",width=9,height=6)

## Tendency to select ambiguous items at test
# Which items (ambiguous vs. disambiguated) do participants choose on each test trial type?
d$testChoiceType <- ifelse(d$trialType=="test"&(as.character(d$choiceImage)==d$High1|as.character(d$choiceImage)==d$High2),"ambiguous",
                           ifelse(d$trialType=="test","disambiguated",NA))

#summarize choice tendency by participant
subj_test_choiceType_2 <- d %>%
  filter(trialType=="test"&experiment_name  == "Experiment 2") %>%
  group_by(subject,experiment_name,ambiguity_condition,targetType,targetIsAmbiguous,targetIsAmbiguousYN) %>%
  summarize(
    N=n(),
    ambiguous_choice=mean(testChoiceType=="ambiguous",na.rm=T))

#summarize across participants
subj_summary_choiceType_2 <- summarySEwithin(
  subj_test_choiceType_2,
  "ambiguous_choice",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("targetIsAmbiguousYN"),
  idvar="subject") %>%
  mutate(
    lower_ci = ambiguous_choice - ci,
    upper_ci = ambiguous_choice + ci
  )
subj_summary_choiceType_2

##plot
ggplot(subj_summary_choiceType_2,aes(x=targetIsAmbiguousYN,y=ambiguous_choice,color=targetIsAmbiguousYN,fill=targetIsAmbiguousYN))+
  geom_bar(stat="identity",size=1.5,position=position_dodge(.53),width=0.5,alpha=0)+
  geom_dotplot(data=subj_test_choiceType_2,alpha=0.6,binaxis="y",stackdir="center",dotsize=0.8)+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0.0,size=0.6,position=position_dodge(.53))+
  scale_color_manual(name="Item Type",
                     limits=c("no","yes"),
                     labels=c("disambiguated","ambiguous"),
                     values=c("#4DAF4A","#E41A1C"))+
  scale_fill_manual(name="Item Type",
                    limits=c("no","yes"),
                    labels=c("disambiguated","ambiguous"),
                    values=c("#4DAF4A","#E41A1C"))+
  scale_x_discrete(name = "Item Type",
                   limits=c("no","yes"),
                   labels=c("disambiguated","ambiguous"))+
  ylab("Proportion Ambiguous Items Selected")+
  geom_hline(yintercept=1/3,linetype="dashed",size=1.1)+
  theme_classic(base_size=16)+
  theme(legend.position="none", axis.text.x=element_text(size=15))
ggsave("../figures/exp2_test_choiceType.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp2_test_choiceType.png",width=9,height=6,dpi=600)
#ggsave("../figures/exp2_test_choiceType.tiff",width=9,height=6)


##Relationship between sampling selections and test performance
#summarize test accuracy by choice
subj_test_choice_2 <- d %>%
  filter(trialType=="test"&experiment_name  %in% c("Experiment 2")) %>%
  group_by(subject,experiment_name,ambiguity_condition,chosen) %>%
  summarize(
    N=n(),
    accuracy=mean(isRight,na.rm=T)) 

subj_summary_choice_2 <- summarySEwithin(
  subj_test_choice_2,
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("chosen"),
  idvar="subject",
  na.rm=T) %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci,
    chosen_factor=ifelse(chosen==0,"NOT SAMPLED","SAMPLED")
  )
subj_summary_choice_2 

#summarize test accuracy by item and chosen items
subj_test_item_choice_2 <- d %>%
  filter(trialType=="test"&experiment_name  %in% c("Experiment 2")) %>%
  group_by(subject,experiment_name,ambiguity_condition,chosen,targetType,targetIsAmbiguous,targetIsAmbiguousYN) %>%
  summarize(
    N=n(),
    accuracy=mean(isRight,na.rm=T))

subj_summary_item_choice_2 <- summarySEwithin(
  subj_test_item_choice_2,
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("chosen","targetIsAmbiguousYN"),
  idvar="subject",
  na.rm=T) %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci,
    chosen_factor=ifelse(chosen==0,"NOT SAMPLED","SAMPLED")
  )
subj_summary_item_choice_2

ggplot(subj_summary_item_choice_2,aes(x=targetIsAmbiguousYN,y=accuracy,color=targetIsAmbiguousYN,fill=targetIsAmbiguousYN))+
  geom_bar(stat="identity",size=1.5,position=position_dodge(.53),width=0.5,alpha=0)+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0.0,size=0.6,position=position_dodge(.53))+
  scale_color_manual(name="Item Type",
                     limits=c("no","yes"),
                     labels=c("disambiguated","ambiguous"),
                     values=c("#4DAF4A","#E41A1C"))+
  scale_fill_manual(name="Item Type",
                    limits=c("no","yes"),
                    labels=c("disambiguated","ambiguous"),
                    values=c("#4DAF4A","#E41A1C"))+
  scale_x_discrete(name = "Item Type",
                   limits=c("no","yes"),
                   labels=c("disambiguated","ambiguous"))+
  ylab("Test Accuracy")+
  geom_hline(yintercept=1/4,linetype="dashed",size=1.1)+
  theme_classic(base_size=16)+
  theme(legend.position="none", axis.text.x=element_text(size=15))+
  facet_wrap(~chosen_factor)
ggsave("../figures/exp2_test_item_sampled.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp2_test_item_sampled.png",width=9,height=6,dpi=600)
#ggsave("../figures/exp2_test_item_sampled.tiff",width=9,height=6)

## logistic mixed-effects model
d$chosenC <- ifelse(!is.na(d$chosen)&d$chosen==0,-0.5,
            ifelse(!is.na(d$chosen)&d$chosen==1,0.5,NA))

m <- glmer(isRight~targetIsAmbiguousC*chosenC+(1+targetIsAmbiguousC|subject)+(1|targetImage),data=subset(d,trialType=="test"&experiment_name=="Experiment 2"),family=binomial,glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)
confint(m, method="Wald")

## preference for sampling ambiguous items and test performance
cor.test(subset(subj_selection,ambiguity_condition=="ambiguous"&experiment_name=="Experiment 2")$accuracy,subset(subj_selection,ambiguity_condition=="ambiguous"&experiment_name=="Experiment 2")$prop_ambig_selection)
##plot
p_exp2_sampling_test <- ggplot(filter(subj_selection,experiment_name=="Experiment 2"),aes(prop_ambig_selection,accuracy, color=ambiguity_condition))+
  geom_violin(aes(group=prop_ambig_selection),draw_quantiles=c(0.5))+
  geom_dotplot(aes(group=prop_ambig_selection,fill=ambiguity_condition),alpha=0.6,binaxis="y",stackdir="center",dotsize=0.8)+
  scale_color_manual(limits=c("ambiguous"),
                     values=c("#E41A1C"))+
  geom_smooth(method="lm",color="black",fill="#4B0082",alpha=0.3)+
  theme_classic()+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),limits=c(0,1.08))+
  theme_classic(base_size=24)+
  ylab("Test Accuracy")+
  theme(legend.position="none")+
  xlab("Probability of \nAmbiguous Selection")
ggsave("../figures/exp2_test_sampling_preference.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp2_test_sampling_preference.png",width=9,height=6,dpi=600)

#### Experiment 3 - Sampling ####

#descriptives
subj_summary_3 <-  subj_selection %>%
  filter(experiment_name=="Experiment 3") %>%
  group_by(ambiguity_condition) %>%
  summarize(
    N=n(),
    prop_ambiguous=mean(prop_ambig_selection),
    ci_ambiguous=qt(0.975, N-1)*sd(prop_ambig_selection,na.rm=T)/sqrt(N),
    prop_ambiguous_lower_ci=prop_ambiguous-ci_ambiguous,
    prop_ambiguous_upper_ci=prop_ambiguous+ci_ambiguous,
  )
subj_summary_3

##logistic mixed-effects model
m=glmer(isAmbiguous ~ 1+(1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment 3"&trialType=="selection"),family=binomial,glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)
confint(m, method="Wald")

##non-parametric test (Wilcoxon)
wilcox.test(filter(subj_selection,experiment_name=="Experiment 3")$prop_ambig_selection,mu=1/2, conf.int=T, conf.level=0.95)

##plot
p1 <- ggplot(filter(subj_selection,experiment_name=="Experiment 3"),aes(x=num_ambig_selection,fill=as.factor(num_ambig_selection),color=as.factor(num_ambig_selection)))+
  scale_fill_brewer(palette="Set1",direction=-1)+
  scale_color_brewer(palette="Set1",direction=-1)+
  geom_bar(stat="count",size=1.5,alpha=0.2,width=0.5)+
  #geom_vline(xintercept=1,linetype="dashed")+
  theme(axis.title = element_text(size=20),
        axis.text  = element_text(size=16),
        legend.position="none")+
  ylab("Number of subjects")+
  xlab("Number of ambiguous selections")+
  #geom_density(aes(group=1, y=..count../1.3))+
  scale_x_continuous(breaks=c(0,1,2))+
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30))
ggsave("../figures/exp3_sampling.pdf",width=7,height=6,dpi=600)
ggsave("../figures/exp3_sampling.png",width=7,height=6,dpi=600)
#ggsave("../figures/exp3_sampling.tiff",width=7,height=6)

## Relationship to age
m <- glmer(isAmbiguous~Age+(1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment 3"&trialType=="selection"),family=binomial)
summary(m)
confint(m, method="Wald")

pX <- data.frame(Age=seq(min(subset(d,experiment_name=="Experiment 3"&trialType=="selection")$Age,na.rm=T),max(subset(d,experiment_name=="Experiment 3"&trialType=="selection")$Age,na.rm=T),by=0.1))
pY <- predictSE(m,pX,re.form=NA,type="response")
pX$isAmbiguous <- pY$fit
pX$YLower <- pY$fit-pY$se.fit
pX$YUpper <- pY$fit+pY$se.fit
p2 <- ggplot(pX,aes(Age,isAmbiguous))+
  geom_violinh(data=subset(d,experiment_name=="Experiment 3"&trialType=="selection"),aes(y=isAmbiguous,group=isAmbiguous),scale="count",width=0.1, trim=F)+
  geom_jitter(data=subset(d,experiment_name=="Experiment 3"&trialType=="selection"),aes(y=isAmbiguous,group=isAmbiguous),height=0.01)+
  geom_smooth(aes(ymin=YLower,ymax=YUpper),stat="identity",color="#E41A1C",fill="#E41A1C")+
  geom_hline(yintercept=0.5,linetype="dotted")+
  theme_classic(base_size=16)+
  xlab("Age (in years)")+
  scale_x_continuous(breaks=c(3,4,5,6,7,8))+
  ylab("Proportion of ambiguous selections")+
  theme(axis.title = element_text(size=20),
        axis.text  = element_text(size=16))
ggsave("../figures/exp3_sampling_age.pdf",width=9, height=6,dpi=600)
ggsave("../figures/exp3_sampling_age.png",width=9, height=6,dpi=600)
#ggsave("../figures/exp3_sampling_age.tiff",width=9, height=6)

plot_grid(p1,p2,labels=c("A","B"),label_size=24,align="h")
ggsave("../figures/exp3_sampling_age_hist.pdf",width=11, height=6.95,dpi=600)
ggsave("../figures/exp3_sampling_age_hist.png",width=11, height=6.95,dpi=600)
#ggsave("../figures/exp3_sampling_age_hist.tiff",width=11, height=6.95)

####Experiment 3 - Test####

## descriptives
subj_summary_test_3 <-  subj_test %>%
  filter(experiment_name=="Experiment 3") %>%
  group_by(experiment_name,ambiguity_condition) %>%
  summarize(
    N=n(),
    prop_correct=mean(accuracy,na.rm=T),
    ci=qt(0.975, N-1)*sd(accuracy,na.rm=T)/sqrt(N),
    prop_correct_lower_ci=prop_correct-ci,
    prop_correct_upper_ci=prop_correct+ci)
subj_summary_test_3 

##plot by item type
#by item type
subj_summary_item_3 <- summarySEwithin(
  filter(
    subj_test_item,
    (experiment_name=="Experiment 3")),
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("targetIsAmbiguousYN"),
  idvar="subject") %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci
  )
subj_summary_item_3 

ggplot(subj_summary_item_3,aes(x=targetIsAmbiguousYN,y=accuracy,color=targetIsAmbiguousYN,fill=targetIsAmbiguousYN))+
  geom_bar(stat="identity",size=1.5,position=position_dodge(.53),width=0.5,alpha=0)+
  geom_dotplot(data=filter(subj_test_item,experiment_name=="Experiment 3"),alpha=0.6,binaxis="y",stackdir="center",dotsize=0.8)+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0.0,size=0.6,position=position_dodge(.53))+
  scale_color_manual(name="Item Type",
                     limits=c("no","yes"),
                     labels=c("mutual exclusivity","ambiguous"),
                     values=c("#4DAF4A","#E41A1C"))+
  scale_fill_manual(name="Item Type",
                    limits=c("no","yes"),
                    labels=c("mutual exclusivity","ambiguous"),
                    values=c("#4DAF4A","#E41A1C"))+
  scale_x_discrete(name = "Item Type",
                   limits=c("no","yes"),
                   labels=c("mutual exclusivity","ambiguous"))+
  ylab("Test Accuracy")+
  geom_hline(yintercept=1/4,linetype="dashed",size=1.1)+
  theme_classic(base_size=16)+
  theme(legend.position="none", axis.text.x=element_text(size=15))
ggsave("../figures/exp3_test_item.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp3_test_item.png",width=9,height=6,dpi=600)
#ggsave("../figures/exp3_test_item.tiff",width=9,height=6)

##logistic mixed-effects model
d$offset.25 <- 1/4
#overall
m <- glmer(isRight~offset(logit(offset.25))+(1|subject)+(1|targetImage),data=filter(d,experiment_name=="Experiment 3"&trialType=="test"),family=binomial)
summary(m)

#by item type
m <- glmer(isRight~1+targetIsAmbiguousC+(1+targetIsAmbiguousC|subject)+(1|targetImage),data=filter(d,experiment_name=="Experiment 3"&trialType=="test"),family=binomial)
summary(m)
confint(m,method="Wald")

##Test Choices
#summarize test accuracy by chosen items
subj_test_choice_3 <- d %>%
  filter(trialType=="test"&experiment_name  %in% c("Experiment 3")) %>%
  group_by(subject,experiment_name,ambiguity_condition,chosen) %>%
  summarize(
    N=n(),
    accuracy=mean(isRight,na.rm=T))

subj_summary_choice_3 <- summarySEwithin(
  subj_test_choice_3,
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("chosen"),
  idvar="subject",
  na.rm=T) %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci,
    chosen_factor=ifelse(chosen==0,"NOT SAMPLED","SAMPLED")
  )
subj_summary_choice_3

#summarize test accuracy by item and chosen items
subj_test_item_choice_3 <- d %>%
  filter(trialType=="test"&experiment_name  %in% c("Experiment 3")) %>%
  group_by(subject,experiment_name,ambiguity_condition,chosen,targetType,targetIsAmbiguous,targetIsAmbiguousYN) %>%
  summarize(
    N=n(),
    accuracy=mean(isRight,na.rm=T))

subj_summary_item_choice_3 <- summarySEwithin(
  subj_test_item_choice_3,
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("chosen","targetIsAmbiguousYN"),
  idvar="subject",
  na.rm=T) %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci,
    chosen_factor=ifelse(chosen==0,"NOT SAMPLED","SAMPLED")
  )
subj_summary_item_choice_3

ggplot(subj_summary_item_choice_3,aes(x=targetIsAmbiguousYN,y=accuracy,color=targetIsAmbiguousYN,fill=targetIsAmbiguousYN))+
  geom_bar(stat="identity",size=1.5,position=position_dodge(.53),width=0.5,alpha=0)+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0.0,size=0.6,position=position_dodge(.53))+
  scale_color_manual(name="Item Type",
                     limits=c("no","yes"),
                     labels=c("mutual exclusivity","ambiguous"),
                     values=c("#4DAF4A","#E41A1C"))+
  scale_fill_manual(name="Item Type",
                    limits=c("no","yes"),
                    labels=c("mutual exclusivity","ambiguous"),
                    values=c("#4DAF4A","#E41A1C"))+
  scale_x_discrete(name = "Item Type",
                   limits=c("no","yes"),
                   labels=c("mutual exclusivity","ambiguous"))+
  ylab("Test Accuracy")+
  geom_hline(yintercept=1/4,linetype="dashed",size=1.1)+
  theme_classic(base_size=16)+
  theme(legend.position="none", axis.text.x=element_text(size=15))+
  facet_wrap(~chosen_factor)
ggsave("../figures/exp3_test_item_sampled.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp3_test_item_sampled.png",width=9,height=6,dpi=600)
#ggsave("../figures/exp3_test_item_sampled.tiff",width=9,height=6)

m <- glmer(isRight~targetIsAmbiguousC*chosenC+(1+targetIsAmbiguousC|subject)+(1|targetImage),data=subset(d,trialType=="test"&experiment_name=="Experiment 3"),family=binomial,glmerControl(optimizer="bobyqa"))
summary(m)
confint(m, method="Wald")

##relationship between sampling preference for ambiguous items and test accuracy
cor.test(subset(subj_selection,ambiguity_condition=="ambiguous_me"&experiment_name=="Experiment 3")$accuracy,subset(subj_selection,ambiguity_condition=="ambiguous_me"&experiment_name=="Experiment 3")$prop_ambig_selection)

##plot
p_exp3_sampling_test <- ggplot(filter(subj_selection,experiment_name=="Experiment 3"),aes(prop_ambig_selection,accuracy, color=ambiguity_condition))+
  geom_violin(aes(group=prop_ambig_selection),draw_quantiles=c(0.5))+
  geom_dotplot(aes(group=prop_ambig_selection,fill=ambiguity_condition),alpha=0.6,binaxis="y",stackdir="center",dotsize=0.8)+
  scale_color_manual(limits=c("ambiguous_me"),
                     values=c("#E41A1C"))+
  geom_smooth(method="lm",color="black",fill="#4B0082",alpha=0.3)+
  theme_classic()+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),limits=c(0,1.08))+
  theme_classic(base_size=24)+
  ylab("Test Accuracy")+
  theme(legend.position="none")+
  xlab("Probability of \nAmbiguous Selection")
ggsave("../figures/exp3_test_sampling_preference.pdf",width=9,height=6,dpi=600)
ggsave("../figures/exp3_test_sampling_preference.png",width=9,height=6,dpi=600)

#### Experiment S1 - Sampling ####

##descriptives
subj_summary_s1 <-  subj_selection %>%
  filter(experiment_name=="Experiment S1") %>%
  group_by(ambiguity_condition) %>%
  summarize(
    N=n(),
    prop_ambiguous=mean(prop_ambig_selection),
    ci_ambiguous=qt(0.975, N-1)*sd(prop_ambig_selection,na.rm=T)/sqrt(N),
    prop_ambiguous_lower_ci=prop_ambiguous-ci_ambiguous,
    prop_ambiguous_upper_ci=prop_ambiguous+ci_ambiguous,
  )
subj_summary_s1

## logistic mixed-effects model
## condition comparison
d$conditionC <- ifelse(d$ambiguity_condition=="ambiguous",0.5, 
                       ifelse(d$ambiguity_condition=="partially ambiguous",-0.5,NA))

m <- glmer(isAmbiguous~conditionC+(1|subject)+(1|choiceImage),data=subset(d,(experiment_name=="Experiment S1")&trialType=="selection"),family=binomial,glmerControl(check.conv.singular="ignore"))
summary(m)
confint(m,method="Wald")

## Fully Ambiguous Condition
d$conditionFull <- ifelse(d$ambiguity_condition=="ambiguous",0,
                          ifelse(d$ambiguity_condition=="partially ambiguous",-1,NA))

m <- glmer(isAmbiguous~conditionFull+(1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment S1"&trialType=="selection"),family=binomial,glmerControl(check.conv.singular="ignore"))
summary(m)
confint(m,method="Wald")

## Partially Ambiguous Condition
d$conditionPartial <- ifelse(d$ambiguity_condition=="ambiguous",1,
                             ifelse(d$ambiguity_condition=="partially ambiguous",0,NA))

m <- glmer(isAmbiguous~conditionPartial+(1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment S1"&trialType=="selection"),family=binomial,glmerControl(check.conv.singular="ignore"))
summary(m)
confint(m,method="Wald")

##non-parametric test

##condition comparison
wilcox.test(subset(subj_selection , ambiguity_condition=="ambiguous"&experiment_name=="Experiment S1")$prop_ambig_selection,
            subset(subj_selection, ambiguity_condition=="partially ambiguous"&experiment_name=="Experiment S1")$prop_ambig_selection,
            conf.int=T, conf.level=0.95)

##Fully Ambiguous Condition
wilcox.test(subset(subj_selection, experiment_name=="Experiment S1"&ambiguity_condition=="ambiguous")$prop_ambig_selection,mu=1/2, conf.int=T, conf.level=0.95)
##Partially Ambiguous Condition
wilcox.test(subset(subj_selection, experiment_name=="Experiment S1"&ambiguity_condition=="partially ambiguous")$prop_ambig_selection,mu=1/2, conf.int=T, conf.level=0.95)

## Plot
m <- glmer(isAmbiguous~conditionFull+(1|subject)+(1|choiceImage),data=subset(d,experiment_name=="Experiment S1"&trialType=="selection"),family=binomial, glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
model_pred <- data.frame(conditionFull=c(-1,0),ambiguity_condition=c("partially ambiguous","ambiguous"))
pY <- predictSE(m,model_pred, type="response")
model_pred <- model_pred %>%
  mutate(
    prop_ambiguous = pY$fit,
    prop_ambiguous_lower_ci = pY$fit - 1.96*pY$se.fit,
    prop_ambiguous_upper_ci = pY$fit + 1.96*pY$se.fit)
p_expS1_sampling <- ggplot(model_pred,aes(x=ambiguity_condition,y=prop_ambiguous,color=ambiguity_condition,fill=ambiguity_condition))+
  geom_bar(stat="identity",size=2.5,fill="white",width=0.5)+
  geom_dotplot(data=subset(subj_selection,experiment_name=="Experiment S1"), aes(y=prop_ambig_selection),binaxis="y",stackdir="center",alpha=0.5,dotsize=0.6)+
  geom_errorbar(aes(ymin=prop_ambiguous_lower_ci,ymax=prop_ambiguous_upper_ci),width=0,size=1.2)+
  ylab("Probability of \nAmbiguous Selection")+
  geom_hline(yintercept=0.5,linetype="dotted")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),limits=c(0,1.08))+
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1")+
  theme_classic(base_size=24)+
  theme(legend.position="none")+
  scale_x_discrete(name="Condition")
ggsave("../figures/expS1_sampling.pdf",width=11, height=6.95,dpi=600)
ggsave("../figures/expS1_sampling.png",width=11, height=6.95,dpi=600)
#ggsave("../figures/expS1_sampling.tiff",width=11, height=6.95)

#### Experiment S1 - Test ####

## descriptives

#test performance split by test half (only relevant for Experiment S1)
subj_test_half <- d %>%
  filter(trialType=="test"&experiment_name=="Experiment S1") %>%
  group_by(subject,experiment_name,ambiguity_condition, testHalf) %>%
  summarize(
    N=n(),
    accuracy=mean(isRight,na.rm=T))
subj_test_half_item <- d %>%
  filter(trialType=="test"&experiment_name=="Experiment S1") %>%
  group_by(subject,experiment_name,ambiguity_condition, testHalf,targetType,targetIsAmbiguous,targetIsAmbiguousYN) %>%
  summarize(
    N=n(),
    accuracy=mean(isRight,na.rm=T))

#summarize
subj_summary_test_s1 <-  summarySEwithin(
  subj_test_half,
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("testHalf"),
  idvar="subject") %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci,
    testHalf_name = paste("Test Block",testHalf,sep=" ")
  )
subj_summary_test_s1 

#summarize by item
subj_summary_test_item_s1 <-  summarySEwithin(
  subj_test_half_item,
  "accuracy",
  betweenvars=c("experiment_name","ambiguity_condition"),
  withinvars=c("testHalf","targetIsAmbiguousYN"),
  idvar="subject") %>%
  mutate(
    lower_ci = accuracy - ci,
    upper_ci = accuracy + ci,
    testHalf_name = paste("Test Block",testHalf,sep=" ")
  )
subj_summary_test_item_s1

## Logistic mixed-effects model
## Effects of Test Half and Item Type
d$testHalfC <- ifelse(!is.na(d$testHalf) & d$testHalf==2,0.5,
                      ifelse(!is.na(d$testHalf) & d$testHalf==1,-0.5,NA))
#three-way interaction
m <- glmer(isRight~targetIsAmbiguousC*conditionC*testHalfC+(1+targetIsAmbiguousC*testHalfC|subject)+(1|targetImage),data=filter(d,(experiment_name=="Experiment S1")&trialType=="test"&ambiguity_condition!="non ambiguous"),family=binomial,glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)
confint(m, method="Wald")

#test specifically test half increase for ambiguous items in the Fully Ambiguous condition
d$targetIsAmbiguous_ambiguous <- ifelse(!is.na(d$targetIsAmbiguous) & d$targetIsAmbiguous==1,0,
                                        ifelse(!is.na(d$targetIsAmbiguous) & d$targetIsAmbiguous==0,-1,NA))
m <- glmer(isRight~targetIsAmbiguous_ambiguous*conditionFull*testHalfC+(1+targetIsAmbiguous_ambiguous*testHalfC|subject)+(1|targetImage),data=filter(d,(experiment_name=="Experiment S1")&trialType=="test"&ambiguity_condition!="non ambiguous"),family=binomial,glmerControl(optimizer="bobyqa",check.conv.singular="ignore"))
summary(m)
confint(m, method="Wald")

## Plot
p_expS1_fulltest <- ggplot(filter(subj_summary_test_item_s1,ambiguity_condition=="ambiguous"),aes(x=targetIsAmbiguousYN,y=accuracy,color=targetIsAmbiguousYN,fill=targetIsAmbiguousYN))+
  geom_bar(stat="identity",size=1.5,position=position_dodge(.53),width=0.5,alpha=0)+
  geom_point(data=filter(subj_test_half_item,experiment_name=="Experiment S1"&ambiguity_condition=="ambiguous"),aes(fill=targetIsAmbiguousYN),alpha=0.5,position=position_jitterdodge(dodge.width=0.53,jitter.width=0.1,jitter.height=0.02))+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0.0,size=0.8,position=position_dodge(.53))+
  scale_color_manual(name="Item Type",
                     limits=c("no","yes"),
                     labels=c("disambiguated","ambiguous"),
                     values=c("#4DAF4A","#E41A1C"))+
  scale_fill_manual(name="Item Type",
                    limits=c("no","yes"),
                    labels=c("disambiguated","ambiguous"),
                    values=c("#4DAF4A","#E41A1C"))+
  scale_x_discrete(name = "Item Type\n\nFully Ambiguous Condition",
                   limits=c("no","yes"),
                   labels=c("disambiguated","fully\nambiguous"))+
  ylab("Test Accuracy")+
  geom_hline(yintercept=1/8,linetype="dashed",size=1.1)+
  theme_classic(base_size=16)+
  theme(legend.position="none", axis.text.x=element_text(size=15))+
  facet_wrap(~testHalf_name)

p_expS1_partialtest <- ggplot(filter(subj_summary_test_item_s1, ambiguity_condition=="partially ambiguous"),aes(x=targetIsAmbiguousYN,y=accuracy,color=targetIsAmbiguousYN,fill=targetIsAmbiguousYN))+
  geom_bar(stat="identity",size=1.5,position=position_dodge(.53),width=0.5,alpha=0)+
  geom_point(data=filter(subj_test_half_item,experiment_name=="Experiment S1"&ambiguity_condition=="partially ambiguous"),aes(fill=targetIsAmbiguousYN),alpha=0.5,position=position_jitterdodge(dodge.width=0.53,jitter.width=0.1,jitter.height=0.02))+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0.0,size=0.8,position=position_dodge(.53))+
  scale_color_manual(name="Item Type",
                     limits=c("no","yes"),
                     labels=c("disambiguated","partially\nambiguous"),
                     values=c("#4DAF4A","#377EB8"))+
  scale_fill_manual(name="Item Type",
                    limits=c("no","yes"),
                    labels=c("disambiguated","partially\nambiguous"),
                    values=c("#4DAF4A","#377EB8"))+
  scale_x_discrete(name = "Item Type\n\nPartially Ambiguous Condition",
                   limits=c("no","yes"),
                   labels=c("disambiguated","partially\nambiguous"))+
  ylab("Test Accuracy")+
  geom_hline(yintercept=1/8,linetype="dashed",size=1.1)+
  theme_classic(base_size=16)+
  theme(legend.position="none", axis.text.x=element_text(size=15))+
  facet_wrap(~testHalf_name)
plot_grid(p_expS1_fulltest,p_expS1_partialtest, labels=c("A","B"),rel_widths=c(1,1),label_size=24,nrow=1)
ggsave("../figures/expS1_adults_test.pdf",width=13, height=8.21,dpi=600)
ggsave("../figures/expS1_adults_test.png",width=13, height=8.21,dpi=600)
#ggsave("../figures/expS1_adults_test.tiff",width=13, height=8.21)

#correlations Experiment S1: proportion ambiguous items selected and test accuracy
subj_test_half <- subj_test_half %>% 
  left_join(select(subj_selection,subject,prop_ambig_selection)) %>%
  mutate(
    testHalf_name = paste("Test Block",testHalf,sep=" "))
#Fully Ambiguous - test block 1
cor.test(subset(subj_test_half,ambiguity_condition=="ambiguous"&testHalf==1)$accuracy,subset(subj_test_half,ambiguity_condition=="ambiguous"&testHalf==1)$prop_ambig_selection)
#Fully Ambiguous - test block 2
cor.test(subset(subj_test_half,ambiguity_condition=="ambiguous"&testHalf==2)$accuracy,subset(subj_test_half,ambiguity_condition=="ambiguous"&testHalf==2)$prop_ambig_selection)
#Partially Ambiguous - test block 1
cor.test(subset(subj_test_half,ambiguity_condition=="partially ambiguous"&testHalf==1)$accuracy,subset(subj_test_half,ambiguity_condition=="partially ambiguous"&testHalf==1)$prop_ambig_selection)
#Partially Ambiguous - test block 2
cor.test(subset(subj_test_half,ambiguity_condition=="partially ambiguous"&testHalf==2)$accuracy,subset(subj_test_half,ambiguity_condition=="partially ambiguous"&testHalf==2)$prop_ambig_selection)

#Experiment S1 - sampling-test
pS1_fullsampling_test <- ggplot(filter(subj_test_half,ambiguity_condition=="ambiguous"),aes(prop_ambig_selection,accuracy, color=ambiguity_condition))+
  geom_violin(aes(group=prop_ambig_selection),draw_quantiles=c(0.5))+
  scale_color_manual(limits=c("ambiguous"),
                     labels=c("fully ambiguous"),
                     values=c("#E41A1C"))+
  #geom_point(position=position_jitter(width=.05,height=.0))+
  geom_smooth(method="lm",color="black",fill="#4B0082",alpha=0.3)+
  theme_classic()+
  #ylim(0,1.08)+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),limits=c(0,1.08))+
  theme_classic(base_size=16)+
  ylab("Test Accuracy")+
  theme(legend.position="none")+
  xlab("Proportion of Ambiguous Selections\n\nFully Ambiguous Condition")+
  facet_wrap(~testHalf_name)
pS1_partialsampling_test <- ggplot(filter(subj_test_half,ambiguity_condition=="partially ambiguous"),aes(prop_ambig_selection,accuracy, color=ambiguity_condition))+
  geom_violin(aes(group=prop_ambig_selection),draw_quantiles=c(0.5))+
  scale_color_manual(limits=c("partially ambiguous"),
                     labels=c("partially ambiguous"),
                     values=c("#377EB8"))+
  #geom_point(position=position_jitter(width=.05,height=.0))+
  geom_smooth(method="lm",color="black",fill="#4B0082",alpha=0.3)+
  theme_classic()+
  #ylim(0,1.08)+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),limits=c(0,1.08))+
  theme_classic(base_size=16)+
  ylab("Test Accuracy")+
  theme(legend.position="none")+
  xlab("Proportion of Ambiguous Selections\n\nPartially Ambiguous Condition")+
  facet_wrap(~testHalf_name)
plot_grid(pS1_fullsampling_test,pS1_partialsampling_test, labels=c("A","B"),rel_widths=c(1,1),label_size=24,nrow=1)
ggsave("../figures/expS1_sampling_test.pdf",width=11, height=6.95,dpi=600)
ggsave("../figures/expS1_sampling_test.png",width=11, height=6.95,dpi=600)
#ggsave("../figures/expS1_sampling_test.tiff",width=11, height=6.95)

#increase in accuracy block 1 to 2
subj_test_half_wide <- subj_test_half %>%
  select(-testHalf_name) %>%
  pivot_wider(names_from=testHalf,values_from=accuracy,names_prefix="test_block_") %>%
  mutate(accuracy_increase=test_block_2-test_block_1)

ggplot(subj_test_half_wide,aes(prop_ambig_selection,accuracy_increase))+
  geom_jitter()+
  geom_smooth(method="lm")+
  facet_wrap(~ambiguity_condition)

#Fully Ambiguous - accuracy increase
cor.test(subset(subj_test_half_wide,ambiguity_condition=="ambiguous"&experiment_name=="Experiment S1")$accuracy_increase,subset(subj_test_half_wide,ambiguity_condition=="ambiguous"&experiment_name=="Experiment S1")$prop_ambig_selection)
#Partially Ambiguous - increase
cor.test(subset(subj_test_half_wide,ambiguity_condition=="partially ambiguous"&experiment_name=="Experiment S1")$accuracy_increase,subset(subj_test_half_wide,ambiguity_condition=="partially ambiguous"&experiment_name=="Experiment S1")$prop_ambig_selection)

#by item
subj_test_half_item <- subj_test_half_item %>% 
  left_join(select(subj_selection,subject,prop_ambig_selection))

subj_test_half_item_wide <- subj_test_half_item %>%
  pivot_wider(names_from=testHalf,values_from=accuracy,names_prefix="test_block_") %>%
  mutate(accuracy_increase=test_block_2-test_block_1)
#inspect plot
ggplot(subj_test_half_item_wide,aes(prop_ambig_selection,accuracy_increase))+
  geom_jitter()+
  geom_smooth(method="lm")+
  facet_wrap(~ambiguity_condition+targetIsAmbiguousYN)
