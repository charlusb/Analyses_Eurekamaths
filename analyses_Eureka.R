library(afex)
library(emmeans)
library(ppcor)
library(car)
library(ggplot2)
library(reshape2)


parts <- read.table("participants.csv", sep=";", header=T, dec=",")
parts <- subset(parts, included==1 & number_lessons!=0)

tests <- read.table("tests.csv", sep=";", header=T, dec=",")
tests <- subset(tests, included==1 & number_lessons!=0)
tests$test_condition <- relevel(as.factor(tests$test_condition), ref="Test1_non_circles")

confidence <- read.table("confidence_ratings.csv", sep=";", header=T, dec=",")
confidence <- subset(confidence, included==1 & number_lessons!=0)
conf_mean <- aggregate(confidence~participant+number_lessons+math_edu+included, data=confidence, FUN=mean)

insight <- read.table("insight_moments.csv", sep = ";", header=T)
insight_bin <- read.table("insight_binary.csv", sep=";", header=T)
insight_bin <- subset(insight_bin, included==1 & number_lessons!=0)


############################################## Effect of number of lessons on performance ########################################################################

lessons_perf <- mixed(acc~number_lessons*test_condition+math_edu*test_condition+(1|participant), data=tests,family=binomial,check_contrasts=FALSE,method="LRT")
lessons_perf

### Exploring the interaction between test condition and number of lessons: Linear trends by number of lessons in each test condition 

perf_trends <- emtrends(lessons_perf,var="number_lessons",specs=c("test_condition"))
summary(perf_trends,infer=TRUE,null=0, adjust="holm")




###############################################  Effect of number of lessons on Insight reports ###################################################################

insight_lessons <- glm(insight~number_lessons+math_edu, data=insight_bin,family=binomial)
Anova(insight_lessons, type="III")


################################### Relation between insight report and performance ####################################################

#### data frames merging data about insight, confidence and performance 
conf_perf <- merge(tests, conf_mean)
insight_perf <- merge(tests,insight_bin)
confmeaninsight <- merge(conf_perf, insight_bin)


perf_ins <- mixed(acc~insight*test_condition+ (1|participant), data=insight_perf,family=binomial,check_contrasts=FALSE,method="LRT")
perf_ins


### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on accuracy by test condition

contrast(emmeans(perf_ins,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"),adjust="holm")
confint(contrast(emmeans(perf_ins,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"),adjust="holm"))


###############with covariates for Math edu  and Number of lessons  ####################################################################

perf_ins_cov <- mixed(acc~test_condition*number_lessons+test_condition*insight+math_edu*test_condition +(1|participant), data=insight_perf,family=binomial,check_contrasts=F,method="LRT")
perf_ins_cov

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_ins_cov,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"), adjust="holm")
confint(contrast(emmeans(perf_ins_cov,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"), adjust="holm"))


################################### Relation between insight experiences and confspective judgments of confidence #################################################


################ Correlation tests ######################################################################################################

confcol<-dcast(participant+included+number_lessons+math_edu~measurement,value.var="confidence",data=confidence)

### one separate df for pos3 confidence comparisons, as one data is missing for pos3.
confinsight <- merge(confcol,insight_bin)
confinsight2=subset(confinsight, Pos3!="NA")                                                                                                                                                

### simple correlations between each confidence measure and insight
### not the same nb of df : one missing data point for conf 3
cor.test(~Pos1+Pos2,data=confinsight, method="spearman")
cor.test(~Pos1+Pos3,data=confinsight2, method="spearman")
cor.test(~Pos2+Pos3,data=confinsight2, method="spearman") 
cor.test(~Pos1+insight,data=confinsight, method="spearman") 
cor.test(~Pos2+insight,data=confinsight, method="spearman")
cor.test(~Pos3+insight,data=confinsight2, method="spearman")

### p values corrected with holm 
cory <- cbind(
cor.test(~Pos1+Pos2,data=confinsight, method="spearman"),
cor.test(~Pos1+Pos3,data=confinsight2, method="spearman"),
cor.test(~Pos2+Pos3,data=confinsight2, method="spearman"), 
cor.test(~Pos1+insight,data=confinsight, method="spearman"), 
cor.test(~Pos2+insight,data=confinsight, method="spearman"),
cor.test(~Pos3+insight,data=confinsight2, method="spearman"))
format(p.adjust(cory[3,], method="holm"),scientific=F)   


### Correlations with math_edu and number of lessons as covariates
pcor.test(confinsight$Pos1, confinsight$Pos2,list( confinsight$math_edu, confinsight$number_lessons), method="spearman")
pcor.test(confinsight2$Pos1, confinsight2$Pos3,list( confinsight2$math_edu, confinsight2$number_lessons), method="spearman")
pcor.test(confinsight2$Pos2, confinsight2$Pos3,list( confinsight2$math_edu, confinsight2$number_lessons), method="spearman")
pcor.test(confinsight$Pos1, confinsight$insight,list( confinsight$math_edu, confinsight$number_lessons), method="spearman")
pcor.test(confinsight$Pos2, confinsight$insight,list( confinsight$math_edu, confinsight$number_lessons), method="spearman")
pcor.test(confinsight2$Pos3, confinsight2$insight,list( confinsight2$math_edu, confinsight2$number_lessons), method="spearman")


#### p values corrected with holm
cori <- cbind(
pcor.test(confinsight$Pos1, confinsight$Pos2,list( confinsight$math_edu, confinsight$number_lessons), method="spearman")[,2],
pcor.test(confinsight2$Pos1, confinsight2$Pos3,list( confinsight2$math_edu, confinsight2$number_lessons), method="spearman")[,2],
pcor.test(confinsight2$Pos2, confinsight2$Pos3,list( confinsight2$math_edu, confinsight2$number_lessons), method="spearman")[,2],
pcor.test(confinsight$Pos1, confinsight$insight,list( confinsight$math_edu, confinsight$number_lessons), method="spearman")[,2],
pcor.test(confinsight$Pos2, confinsight$insight,list( confinsight$math_edu, confinsight$number_lessons), method="spearman")[,2],
pcor.test(confinsight2$Pos3, confinsight2$insight,list( confinsight2$math_edu, confinsight2$number_lessons), method="spearman")[,2])
format(p.adjust(cori, method="holm"), scientific=F)


########################## relation between confidence, insight and performance #####################################

########################### simple model #################################################################################

perf_confs <- mixed(acc~test_condition*insight + confidence*test_condition+(1|participant), data=confmeaninsight,family=binomial,check_contrasts=F,method="LRT")
perf_confs

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_confs,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each test condition 

conf_trends <- emtrends(perf_confs,var="confidence",specs=c("test_condition"))
summary(conf_trends,infer=TRUE,null=0, adjust="holm")



############ with covariates for Math edu  and Number of lessons #########################################################

perf_confs_cov <- mixed(acc~test_condition*number_lessons+test_condition*insight+math_edu*test_condition + confidence*test_condition+(1|participant), data=confmeaninsight,family=binomial,
check_contrasts=F,method="LRT")
perf_confs_cov

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_confs_cov,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs_cov,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each  test condition 

conf_trends_cov <- emtrends(perf_confs_cov,var="confidence",specs=c("test_condition"))
summary(conf_trends_cov,infer=T,null=0, adjust="holm")


################ supplementary analyses #############################################################################

############### repartitions of insights by position in the experiment ###########################################################

table(subset(insight, response==1)$position)

############## Number of lessons effect on questions about parallel lines in test3 #################################

parallel <- read.table("parallel_perf.csv", sep=";", header=T)
parallel <- subset(parallel, number_lessons !=0 & included==1)

#### pourcentage of correct answers
para <- aggregate(acc~participant+number_lessons+math_edu, data=parallel, subset=included==1, FUN=mean)
mean(para$acc)
### 45.5% correct

#### effect of number of lessons on parallel's answers
paral_lessons <- glmer(acc~number_lessons+math_edu+(1|participant), data=parallel, family=binomial)
summary(paral_lessons)
confint(paral_lessons, level=0.95)

###############  Number of lessons effect on confidence #############################################################

contrasts(confidence$measurement) = "contr.sum"
conf_lessons <- mixed(confidence~number_lessons*measurement + math_edu*measurement+(1|participant), data=confidence,check_contrasts=F)
conf_lessons

slopes_conf <- emtrends(conf_lessons,var="number_lessons",specs=c("measurement"))
summary(slopes_conf,infer=T,null=0, adjust="holm")


######################################################################################################################
########### Figures  #################################################################################################
######################################################################################################################

####### esthetic functions

mycolors=function(ggobject){

ggobject+
scale_color_manual(values = c("#00AFBB", "#E7B800", "#CC0066", "#FF6666", "#9999FF", "#D16103","#FFDB6D" ))

}

themetiny=function(ggobject){

ggobject+
theme(plot.title = element_text(face="bold",size=14,hjust = 0.5))+
theme(axis.text=element_text(size=9,face="bold"),                                                                                                                                                 
axis.title=element_text(size=10,face="bold"),
legend.text=element_text(size=10))
}


###### Effect of number of lessons on performance  ###############
####### predictions of model + individual performance corrected for math edu 

## No need to set level of math edu here, emmeans already sets at mean level in  model
predictions <- data.frame(emmeans(lessons_perf,specs=c("number_lessons","test_condition"), cov.reduce=F, type="response"))

predictions$test_condition= factor(predictions$test_condition, levels=c('Test1_non_circles','Test1_great_circles','Test1_small_circles','Test2_nonstraight_nonplanar',
'Test2_straight_planar','Test2_nonstraight_planar','Test2_straight_nonplanar', 'Test3_sphere', 'Test3_other_surfaces'), order=T )
predictions=predictions[order(predictions$test_condition),]

all_plot=aggregate(acc~participant+number_lessons+test_condition+math_edu, data=tests, FUN=mean)
all_plot$acclogit=logit(all_plot$acc, adjust=0.01)
all_plot$acclogitcor=all_plot$acclogit-((all_plot$math_edu-3.9)*(summary(lessons_perf)$coefficients[11,1]))
all_plot$coeffniv[all_plot$test_condition=="Test1_non_circles"]=0
all_plot$coeffniv[all_plot$test_condition=="Test1_great_circles"]=summary(lessons_perf)$coefficients[20,1]
all_plot$coeffniv[all_plot$test_condition=="Test1_small_circles"]=summary(lessons_perf)$coefficients[21,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_nonstraight_nonplanar"]=summary(lessons_perf)$coefficients[22,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_nonstraight_planar"]=summary(lessons_perf)$coefficients[23,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_straight_nonplanar"]=summary(lessons_perf)$coefficients[24,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_straight_planar"]=summary(lessons_perf)$coefficients[25,1]
all_plot$coeffniv[all_plot$test_condition=="Test3_sphere"]=summary(lessons_perf)$coefficients[27,1]
all_plot$coeffniv[all_plot$test_condition=="Test3_other_surfaces"]=summary(lessons_perf)$coefficients[26,1]
all_plot$acclogitcor=all_plot$acclogitcor-((all_plot$math_edu-3.9)*all_plot$coeffniv)
all_plot$acclogitcor=exp(all_plot$acclogitcor)/(1+exp(all_plot$acclogitcor))


all_plot$test_condition= factor(all_plot$test_condition, levels=c('Test1_non_circles','Test1_great_circles','Test1_small_circles','Test2_nonstraight_nonplanar',
'Test2_straight_planar','Test2_nonstraight_planar','Test2_straight_nonplanar','Test3_sphere', 'Test3_other_surfaces'), order=T )
all_plot=all_plot[order(all_plot$test_condition),]

condtest1 = c("Test1_non_circles","Test1_small_circles","Test1_great_circles")
perf_test1 <- ggplot(aes(x=number_lessons,y=prob),data=subset(predictions,test_condition%in%condtest1))+
  geom_point(data=subset(all_plot,test_condition%in%condtest1),aes(y=acclogitcor),  color="grey50",alpha=1/5, size=1.5,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons)),size=0.8)+
  geom_line(size=0.5)+
  facet_grid(. ~ test_condition)+
  geom_point(size=1.5,aes(color=factor(number_lessons)))+
  theme_classic()+
  theme(aspect.ratio=2.5)+
  scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))
perf_test1 <- perf_test1 + ggtitle("Effect of the number of lessons on accuracy") + xlab("Number of lessons")+ylab("Accuracy")
perf_test1 <- perf_test1 + guides(color=guide_legend("Number of lessons"))
perf_test1 <-mycolors(perf_test1)
perf_test1 <-themetiny(perf_test1)
dev.new(width=10,height=4)
perf_test1

ggsave("number_lessons_on_perf_test1.png")

condtest2 = c("Test2_nonstraight_nonplanar","Test2_straight_planar","Test2_nonstraight_planar","Test2_straight_nonplanar")
perf_test2 <- ggplot(aes(x=number_lessons,y=prob),data=subset(predictions,test_condition%in%condtest2))+
  geom_point(data=subset(all_plot,test_condition%in%condtest2),aes(y=acclogitcor),  color="grey50",alpha=1/5, size=1.5,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons)),size=0.8)+
  geom_line(size=0.5)+
  facet_grid( . ~ test_condition)+
  geom_point(size=1.5,aes(color=factor(number_lessons)))+
  theme_classic()+
  theme(aspect.ratio=2.5)+
  scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))
perf_test2 <- perf_test2 + ggtitle("Effect of number of lessons on accuracy") + xlab("Number of lessons")+ylab("Accuracy")
perf_test2 <- perf_test2 + guides(color=guide_legend("Number of lessons"))
perf_test2 <-mycolors(perf_test2)
perf_test2 <-themetiny(perf_test2)
dev.new(width=10,height=4)
perf_test2

ggsave("number_lessons_on_perf_test2.png")

condtest3 = c("Test3_sphere","Test3_other_surfaces")
perf_test3 <- ggplot(aes(x=number_lessons,y=prob),data=subset(predictions,test_condition%in%condtest3))+
  geom_point(data=subset(all_plot,test_condition%in%condtest3),aes(y=acclogitcor),  color="grey50",alpha=1/5, size=1.5,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons)),size=0.8)+
  geom_line(size=0.5)+
  facet_grid( . ~ test_condition)+
  geom_point(size=1.5,aes(color=factor(number_lessons)))+
  theme_classic()+
  theme(aspect.ratio=2.5)+
  scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))+
  scale_y_continuous( limits=c(0,1))
  
perf_test3 <- perf_test3 + ggtitle("Effect of number of lessons on accuracy") + xlab("Number of lessons")+ylab("Accuracy")
perf_test3 <- perf_test3 + guides(color=guide_legend("Number of lessons"))
perf_test3 <-mycolors(perf_test3)
perf_test3 <-themetiny(perf_test3)
dev.new(width=10,height=4)
perf_test3

ggsave("number_lessons_on_perf_test3.png")

###########################################################################################################################
##### Number of lessons effect on insight #################################################################################
####### predictions of model + individual responses corrected for math edu 

ins_plot <- aggregate(insight~participant+number_lessons+math_edu, data=insight_bin, FUN=mean)                                                                       
ins_plot$inslogit <- logit(ins_plot$insight, adjust=0.01)
ins_plot$inscor <- ins_plot$inslogit-((ins_plot$math_edu-3.9)*summary(insight_lessons)$coefficients[3,1])
ins_plot$inscor <- exp(ins_plot$inscor)/(1+exp(ins_plot$inscor))

## No need to set level of math edu here, emmeans already set at mean level in  model                                                                                                                   
insight <- emmeans(insight_lessons,specs="number_lessons", cov.reduce=F,type="response")
predict_insight <- data.frame(summary(insight))


ins <- ggplot(aes(x=number_lessons,y=prob),data=predict_insight)+
  geom_point(data=ins_plot,aes(y=inscor),color="grey50",alpha=1/5, size=3,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons)),size=0.8)+
  geom_line(size=0.8)+
  scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))+     
  geom_point(size=3,aes(color=factor(number_lessons)))+
  theme_classic()
ins <- ins + ggtitle("Effect of number of lessons on insight report") + xlab("Number of lessons")+ylab("% insight reports")
ins <- ins + guides(color=guide_legend("Number of lessons"))
ins <- ins + theme(plot.title = element_text(face="bold",size=15,hjust = 0.5))
ins <- themetiny(ins)
ins <-mycolors(ins)
dev.new(width=7,height=4)
ins

ggsave("Numb_lessons_eff_on_Insight.png")


############################################################################################################################
##### Insight relation with accuracy in Test2 condition straight  non planar  lines, predictions + individual performance
##### corrected for math edu, number of lessons  and confidence

ins_plot=aggregate(acc~participant+insight+number_lessons+test_condition+math_edu+confidence, data=subset(confmeaninsight, test_condition=="Test2_straight_nonplanar"), FUN=mean)
ins_plot$acclogit=logit(ins_plot$acc, adjust=0.01)
ins_plot$coeffniv[ins_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[34,1]
ins_plot$acclogitcor=ins_plot$acclogit-((ins_plot$math_edu-3.9)*(summary(perf_confs_cov)$coefficients[12,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$math_edu-3.9)*ins_plot$coeffniv)
ins_plot$coeffcond[ins_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[18,1]
ins_plot$acclogitcor=ins_plot$acclogit-((ins_plot$number_lessons-4)*(summary(perf_confs_cov)$coefficients[10,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$number_lessons-4)*ins_plot$coeffcond)
ins_plot$coeffconf[ins_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[42,1]
ins_plot$acclogitcor=ins_plot$acclogit-((ins_plot$confidence-7.8)*(summary(perf_confs_cov)$coefficients[13,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$confidence-7.8)*ins_plot$coeffconf)
ins_plot$acclogitcor=exp(ins_plot$acclogitcor)/(1+exp(ins_plot$acclogitcor))


### default values of emmeans : math_edu 3.9, confidence 7.5 and number lessons at 0, resetting them as mean mevels
ins <-summary( emmeans(perf_confs_cov, specs=c("insight", "test_condition"), at=list("math_edu"=3.9, "confidence"=7.8, "number_lessons"=4),cov.reduce=F), type="response")
predict_ins <- data.frame(ins)
predict_ins=subset(predict_ins, test_condition=="Test2_straight_nonplanar")


insac <- ggplot(aes(x=insight,y=prob),data=predict_ins)+
  geom_point(data=ins_plot,aes(y=acclogitcor, x=insight),size=2, alpha=1/5,position=position_jitter(w=0.05,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.4,width=.2)+
  facet_wrap(~test_condition, ncol=1)+
  theme_classic()+
  geom_point(size=3)+
    scale_x_discrete(limits=c(0,1), labels=c("No insight", "insight"))
  ## scale_x_discrete(labels=c("0"="No insight", "1"="Insight"))
insac <-themetiny(insac)
insac <- insac  + xlab("Insight report")+ylab("Accuracy")
dev.new(width=7*0.65,height=4*0.65)
insac

ggsave("insight_Test2_straight_nonplanar.jpg")



#####  Confidence relation with accuracy in condition Test2 non planar straigth lines, predictions + individual performance with same corrections as figure above

conf_plot=aggregate(acc~participant+confidence+number_lessons+test_condition+math_edu+insight, data=subset(confmeaninsight, test_condition=="Test2_straight_nonplanar"), FUN=mean)
conf_plot$acclogit=logit(conf_plot$acc, adjust=0.01)
conf_plot$coeffniv[conf_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[34,1]
conf_plot$acclogitcor=conf_plot$acclogit-((conf_plot$math_edu-3.9)*(summary(perf_confs_cov)$coefficients[12,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$math_edu-3.9)*conf_plot$coeffniv)
conf_plot$coeffcond[conf_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[18,1]
conf_plot$acclogitcor=conf_plot$acclogit-((conf_plot$number_lessons-4)*(summary(perf_confs_cov)$coefficients[10,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$number_lessons-4)*conf_plot$coeffcond)
conf_plot$coeffins[conf_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[26,1]
conf_plot$acclogitcor=conf_plot$acclogit-((conf_plot$insight-0.61)*(summary(perf_confs_cov)$coefficients[11,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$insight-0.61)*conf_plot$coeffins)
conf_plot$acclogitcor=exp(conf_plot$acclogitcor)/(1+exp(conf_plot$acclogitcor))

### default values of emmeans : math_edu 3.9, insight 0.5 and number lessons at 0, resetting them as mean mevels
conf <-summary( emmeans(perf_confs_cov, specs=c("confidence", "test_condition"), at=list("math_edu"=3.9, "insight"=0.61, "number_lessons"=4),cov.reduce=F), type="response")
predict_conf <- data.frame(conf)
predict_conf=subset(predict_conf, test_condition=="Test2_straight_nonplanar")


confac <- ggplot(aes(x=confidence, y=prob),data=predict_conf)+
	geom_point(data=conf_plot,aes(y=acclogitcor, x=confidence),alpha=1/5, size=2,position=position_jitter(w=0.05,h=0))+
	facet_wrap(~test_condition, ncol=1)+
	geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.8)+
	geom_line()+
	theme_classic()+
	geom_point(size=3)
confac <- mycolors(confac)
confac <-themetiny(confac)
confac <- confac +  xlab("Mean confidence rating")+ylab("Accuracy")
confac <- confac + guides(color=guide_legend("Insight"))   
dev.new(width=7*0.65,height=4*0.65)
confac
     
ggsave("conf_Test2_straight_nonplanar.jpg") 

