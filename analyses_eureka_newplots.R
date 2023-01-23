###################################################################

###### Januray 19th, 2023 FINAL VERSION of Eureka paper analyses

###################################################################

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

confidence <- read.table("confidence_ratings.csv", sep=";", header=T, dec=",")
confidence <- subset(confidence, included==1 & number_lessons!=0)
conf_mean <- aggregate(confidence~participant+number_lessons+math_edu+included, data=confidence, FUN=mean)

insight <- read.table("insight_moments.csv", sep = ";", header=T)
insight <- subset(insight, included==1 & number_lessons!=0)
insight_bin <- read.table("insight_binary.csv", sep=";", header=T)
insight_bin <- subset(insight_bin, included==1 & number_lessons!=0)
insight_bin$insight <- as.factor(insight_bin$insight)

###### insight during teaching phase
insight_teach <- read.table("insight_lessons.csv", sep=";", header=T)
insight_teach <- subset(insight_teach, included==1 & number_lessons!=0)


#### insights during learning sessions+tests (everything except pre tests)
insights_post <- subset(insight, included==1 & number_lessons!=0)
insights_post <- subset(insight, !position%in%c("straight_lines_on_spheres1", "planar_geometry" ))
insights_post <- aggregate(response~participant+included+number_lessons+math_edu+lessons, data=insights_post, FUN=max)
insights_post$insight <- insights_post$response
insights_post$response <- NULL




################################################ centering numerical variables

tests$number_lessons_n <- scale(tests$number_lessons, center=TRUE, scale=FALSE)
tests$math_edu_n <- scale(tests$math_edu, center=TRUE, scale=FALSE)

insight_bin$number_lessons_n <- scale(insight_bin$number_lessons, center=TRUE, scale=FALSE)
insight_bin$math_edu_n <- scale(insight_bin$math_edu, center=TRUE, scale=FALSE)  

confidence$number_lessons_n <- scale(confidence$number_lessons, center=TRUE, scale=FALSE)
confidence$math_edu_n <- scale(confidence$math_edu, center=TRUE, scale=FALSE)
confidence$confidence_n <- scale(confidence$confidence, center=TRUE, scale=FALSE)
conf_mean$number_lessons_n <- scale(conf_mean$number_lessons, center=TRUE, scale=FALSE)
conf_mean$confidence_n <- scale(conf_mean$confidence, center=T, scale=F)
conf_mean$math_edu_n <- scale(conf_mean$math_edu, center=TRUE, scale=FALSE)  


############################################## Effect of number of lessons on performance ########################################################################

lessons_perf <- mixed(acc~number_lessons_n*test_condition+math_edu_n*test_condition+(1|participant), data=tests,family=binomial,check_contrasts=T, method="LRT")
lessons_perf


### Exploring the interaction between test condition and number of lessons: Linear trends by number of lessons in each test condition 

perf_trends <- emtrends(lessons_perf,var="number_lessons_n",specs=c("test_condition"))
summary(perf_trends,infer=TRUE,null=0, adjust="holm")


###############################################  Effect of number of lessons on Insight reports ###################################################################
insight_lessons <- glm(insight~number_lessons_n+math_edu_n, data=insight_bin,family=binomial)
Anova(insight_lessons, type="III")


################################### Relation between insight report and performance ####################################################

#### data frames merging data about insight, confidence and performance
conf_perf <- merge(tests, conf_mean)
insight_perf <- merge(tests,insight_bin)
insight_perf$insight <- as.factor(insight_perf$insight)
confmeaninsight <- merge(conf_perf, insight_bin)
confmeaninsight$insight <- as.factor(confmeaninsight$insight)

perf_ins <- mixed(acc~insight*test_condition+ (1|participant), data=insight_perf,family=binomial,check_contrasts=TRUE,method="LRT")
perf_ins


### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on accuracy by test condition

contrast(emmeans(perf_ins,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"),adjust="holm")
confint(contrast(emmeans(perf_ins,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"),adjust="holm"))


###############with covariates for Math edu  and Number of lessons  ####################################################################

perf_ins_cov <- mixed(acc~test_condition*number_lessons_n+test_condition*insight+math_edu_n*test_condition +(1|participant), data=insight_perf,family=binomial,check_contrasts=T,method="LRT")
summary(perf_ins_cov)

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_ins_cov,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"), adjust="holm")
confint(contrast(emmeans(perf_ins_cov,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"), adjust="holm"))


################################### Relation between insight experiences and confspective judgments of confidence #################################################


################ Correlation tests ######################################################################################################

confcol<-dcast(participant+included+number_lessons_n+math_edu_n~measurement,value.var="confidence",data=confidence_n)

### one separate df for pos3 confidence comparisons, as one data is missing for pos3.
confinsight <- merge(confcol,insight_bin)
confinsight2=subset(confinsight, Pos3!="NA")                                                                                                                                                
confinsight$insight <- as.numeric(as.character(confinsight$insight))
confinsight2$insight <- as.numeric(as.character(confinsight2$insight))

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
pcor.test(confinsight$Pos1, confinsight$Pos2,list( confinsight$math_edu_n, confinsight$number_lessons), method="spearman")
pcor.test(confinsight2$Pos1, confinsight2$Pos3,list( confinsight2$math_edu_n, confinsight2$number_lessons_n), method="spearman")
pcor.test(confinsight2$Pos2, confinsight2$Pos3,list( confinsight2$math_edu_n, confinsight2$number_lessons_n), method="spearman")
pcor.test(confinsight$Pos1, confinsight$insight,list( confinsight$math_edu_n, confinsight$number_lessons_n), method="spearman")
pcor.test(confinsight$Pos2, confinsight$insight,list( confinsight$math_edu_n, confinsight$number_lessons_n), method="spearman")
pcor.test(confinsight2$Pos3, confinsight2$insight,list( confinsight2$math_edu_n, confinsight2$number_lessons_n), method="spearman")


#### p values corrected with holm
cori <- cbind(
pcor.test(confinsight$Pos1, confinsight$Pos2,list( confinsight$math_edu_n, confinsight$number_lessons_n), method="spearman")[,2],
pcor.test(confinsight2$Pos1, confinsight2$Pos3,list( confinsight2$math_edu_n, confinsight2$number_lessons_n), method="spearman")[,2],
pcor.test(confinsight2$Pos2, confinsight2$Pos3,list( confinsight2$math_edu_n, confinsight2$number_lessons_n), method="spearman")[,2],
pcor.test(confinsight$Pos1, confinsight$insight,list( confinsight$math_edu_n, confinsight$number_lessons_n), method="spearman")[,2],
pcor.test(confinsight$Pos2, confinsight$insight,list( confinsight$math_edu_n, confinsight$number_lessons_n), method="spearman")[,2],
pcor.test(confinsight2$Pos3, confinsight2$insight,list( confinsight2$math_edu_n, confinsight2$number_lessons_n), method="spearman")[,2])
format(p.adjust(cori, method="holm"), scientific=F)


########################## relation between confidence, insight and performance #####################################

########################### simple model #################################################################################

perf_confs <- mixed(acc~test_condition*insight + confidence_n*test_condition+(1|participant), data=confmeaninsight,family=binomial,check_contrasts=T,method="LRT")
summary(perf_confs)

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_confs,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each test condition 

conf_trends <- emtrends(perf_confs,var="confidence_n",specs=c("test_condition"))
summary(conf_trends,infer=TRUE,null=0, adjust="holm")



############ With covariates for Math edu  and Number of lessons #########################################################

perf_confs_cov <- mixed(acc~test_condition*number_lessons_n+test_condition*insight+math_edu_n*test_condition + confidence_n*test_condition+(1|participant), data=confmeaninsight,family=binomial,
check_contrasts=T,method="LRT")
summary(perf_confs_cov)

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_confs_cov,~insight|test_condition,cov.reduce=F, rg.limit=10080), "revpairwise",simple=list("insight"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs_cov,~insight|test_condition,cov.reduce=F, rg.limit=10080), "revpairwise",simple=list("insight"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each  test condition 

conf_trends_cov <- emtrends(perf_confs_cov,var="confidence_n",specs=c("test_condition"))
summary(conf_trends_cov,infer=T,null=0, adjust="holm")


################ Exploratory analyses #############################################################################

############### repartitions of insights by position in the experiment ###########################################################

table(subset(insight, response==1)$position)

############## Number of lessons effect on questions about parallel lines in test3 #################################

parallel <- read.table("parallel_perf.csv", sep=";", header=T)
parallel <- subset(parallel, number_lessons !=0 & included==1)

#### pourcentage of correct answers
para <- aggregate(acc~participant+number_lessons_n+math_edu_n, data=parallel, subset=included==1, FUN=mean)
mean(para$acc)
### 45.5% correct

#### effect of number of lessons on parallel's answers
paral_lessons <- glmer(acc~number_lessons_n+math_edu_n+(1|participant), data=parallel, family=binomial)
summary(paral_lessons)
confint(paral_lessons, level=0.95)

###############  Number of lessons effect on confidence #############################################################

contrasts(confidence$measurement) = "contr.sum"
conf_lessons <- mixed(confidence~number_lessons_n*measurement + math_edu_n*measurement+(1|participant), data=confidence_n,check_contrasts=F)
conf_lessons

slopes_conf <- emtrends(conf_lessons,var="number_lessons",specs=c("measurement"))
summary(slopes_conf,infer=T,null=0, adjust="holm")


######################################################################################################################
########### Figures  #################################################################################################
######################################################################################################################

####### esthetic functions

mycolors=function(ggobject){

ggobject+
scale_color_manual(values = c("#9AE39A", "#E7B800", "#CC0066", "#FF6666", "#9999FF", "#D16103","#FFDB6D" ))

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
lessons_perf_plot <- mixed(acc~number_lessons_n*test_condition+math_edu_n*test_condition+(1|participant), data=tests,family=binomial,check_contrasts=F, method="LRT")

## No need to set level of math edu here, emmeans already sets at mean level in  model
predictions <- data.frame(emmeans(lessons_perf_plot,specs=c("number_lessons_n","test_condition"), cov.reduce=F, type="response"))

predictions$test_condition= factor(predictions$test_condition, levels=c('Test1_non_circles','Test1_great_circles','Test1_small_circles','Test2_nonstraight_nonplanar',
'Test2_straight_planar','Test2_nonstraight_planar','Test2_straight_nonplanar', 'Test3_sphere', 'Test3_other_surfaces'), order=T )
predictions=predictions[order(predictions$test_condition),]

all_plot=aggregate(acc~participant+number_lessons_n+test_condition+math_edu_n, data=tests, FUN=mean)
all_plot$acclogit=logit(all_plot$acc, adjust=0.01)
all_plot$acclogitcor=all_plot$acclogit-((all_plot$math_edu_n)*(summary(lessons_perf_plot)$coefficients[11,1]))
all_plot$coeffniv[all_plot$test_condition=="Test1_great_circles"]=0
all_plot$coeffniv[all_plot$test_condition=="Test1_non_circles"]=summary(lessons_perf_plot)$coefficients[20,1]
all_plot$coeffniv[all_plot$test_condition=="Test1_small_circles"]=summary(lessons_perf_plot)$coefficients[21,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_nonstraight_nonplanar"]=summary(lessons_perf_plot)$coefficients[22,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_nonstraight_planar"]=summary(lessons_perf_plot)$coefficients[23,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_straight_nonplanar"]=summary(lessons_perf_plot)$coefficients[24,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_straight_planar"]=summary(lessons_perf_plot)$coefficients[25,1]
all_plot$coeffniv[all_plot$test_condition=="Test3_sphere"]=summary(lessons_perf_plot)$coefficients[27,1]
all_plot$coeffniv[all_plot$test_condition=="Test3_other_surfaces"]=summary(lessons_perf_plot)$coefficients[26,1]
all_plot$acclogitcor=all_plot$acclogitcor-((all_plot$math_edu_n)*all_plot$coeffniv)
all_plot$acclogitcor=exp(all_plot$acclogitcor)/(1+exp(all_plot$acclogitcor))


all_plot$test_condition= factor(all_plot$test_condition, levels=c('Test1_non_circles','Test1_great_circles','Test1_small_circles','Test2_nonstraight_nonplanar',
'Test2_straight_planar','Test2_nonstraight_planar','Test2_straight_nonplanar','Test3_sphere', 'Test3_other_surfaces'), order=T )
all_plot=all_plot[order(all_plot$test_condition),]

condtest1 = c("Test1_non_circles","Test1_small_circles","Test1_great_circles")
perf_test1 <- ggplot(aes(x=number_lessons_n,y=prob),data=subset(predictions,test_condition%in%condtest1))+
  geom_point(data=subset(all_plot,test_condition%in%condtest1),aes(y=acclogitcor),  color="grey50",alpha=1/5, size=2,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons_n)),size=0.8)+
  geom_line(size=0.5)+
  facet_grid(. ~ test_condition)+
  geom_point(size=2,aes(color=factor(number_lessons_n)))+
  theme_classic()+
  theme(aspect.ratio=2.5)
  ## scale_x_discrete(breaks=c(1,3,5,7),limits=c("-4","", "-1", "","1", ""))
perf_test1 <- perf_test1 + ggtitle("Effect of the number of lessons on accuracy") + xlab("Number of lessons")+ylab("Accuracy")
perf_test1 <- perf_test1 + guides(color=guide_legend("Number of lessons"))
perf_test1 <-mycolors(perf_test1)
perf_test1 <-themetiny(perf_test1)
dev.new(width=10,height=4)
perf_test1

ggsave("number_lessons_on_perf_test1_newplot.pdf")

condtest2 = c("Test2_nonstraight_nonplanar","Test2_straight_planar","Test2_nonstraight_planar","Test2_straight_nonplanar")
perf_test2 <- ggplot(aes(x=number_lessons_n,y=prob),data=subset(predictions,test_condition%in%condtest2))+
  geom_point(data=subset(all_plot,test_condition%in%condtest2),aes(y=acclogitcor),  color="grey50",alpha=1/5, size=2,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons_n)),size=0.8)+
  geom_line(size=0.5)+
  facet_grid( . ~ test_condition)+
  geom_point(size=2,aes(color=factor(number_lessons_n)))+
  theme_classic()+
  theme(aspect.ratio=2.5)
perf_test2 <- perf_test2 + ggtitle("Effect of number of lessons on accuracy") + xlab("Number of lessons")+ylab("Accuracy")
perf_test2 <- perf_test2 + guides(color=guide_legend("Number of lessons"))
perf_test2 <-mycolors(perf_test2)
perf_test2 <-themetiny(perf_test2)
dev.new(width=10,height=4)
perf_test2

ggsave("number_lessons_on_perf_test2.pdf")

condtest3 = c("Test3_sphere","Test3_other_surfaces")
perf_test3 <- ggplot(aes(x=number_lessons_n,y=prob),data=subset(predictions,test_condition%in%condtest3))+
  geom_point(data=subset(all_plot,test_condition%in%condtest3),aes(y=acclogitcor),  color="grey50",alpha=1/5, size=2,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons_n)),size=0.8)+
  geom_line(size=0.5)+
  facet_grid( . ~ test_condition)+
  geom_point(size=2,aes(color=factor(number_lessons_n)))+
  theme_classic()+
  theme(aspect.ratio=2.5)

  
perf_test3 <- perf_test3 + ggtitle("Effect of number of lessons on accuracy") + xlab("Number of lessons")+ylab("Accuracy")
perf_test3 <- perf_test3 + guides(color=guide_legend("Number of lessons"))
perf_test3 <-mycolors(perf_test3)
perf_test3 <-themetiny(perf_test3)
dev.new(width=10,height=4)
perf_test3

ggsave("number_lessons_on_perf_test3.pdf")

###########################################################################################################################
##### Number of lessons effect on insight #################################################################################
####### predictions of model + individual responses corrected for math edu

insight_bin$insight=as.numeric(as.character(insight_bin$insight))
ins_cond <- aggregate(insight~participant+number_lessons_n+math_edu_n, data=insight_bin, FUN=mean)                                                                       
ins_cond$inslogit <- logit(ins_cond$insight, adjust=0.01)
ins_cond$inscor <- ins_cond$inslogit-((ins_cond$math_edu_n)*(summary(insight_lessons)$coefficients[3,1]))
ins_cond$inscor <- exp(ins_cond$inscor)/(1+exp(ins_cond$inscor))

insight <- emmeans(insight_lessons,specs="number_lessons_n", cov.reduce=F,type="response")
predict_insight <- data.frame(summary(insight))


ins <- ggplot(aes(x=number_lessons_n,y=prob),data=predict_insight)+
  geom_point(data=ins_cond,aes(y=inscor),color="grey50",alpha=1/5, size=3,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons_n)),size=0.8)+
  geom_line(size=0.8)+
  ## scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))+     
  geom_point(size=3,aes(color=factor(number_lessons_n)))+
  theme_classic()
ins <- ins + ggtitle("Effect of number of lessons on insight report") + xlab("Number of lessons")+ylab("% insight reports")
ins <- ins + guides(color=guide_legend("Number of lessons"))
ins <- ins + theme(plot.title = element_text(face="bold",size=15,hjust = 0.5))
ins <- themetiny(ins)
ins <-mycolors(ins)
dev.new(width=7,height=4)
ins

ggsave("Numb_lessons_eff_on_Insight.pdf")


############################################################################################################################
##### Insight relation with accuracy in Test2 condition straight  non planar  lines, predictions + individual performance
##### corrected for math edu, number of lessons  and confidence

##### model with original contrasts for plots
perf_confs_cov_plot <- mixed(acc~test_condition*number_lessons_n+test_condition*insight+math_edu_n*test_condition + confidence_n*test_condition+(1|participant), data=confmeaninsight,family=binomial,
check_contrasts=F,method="LRT")


ins_plot=aggregate(acc~participant+insight+number_lessons_n+test_condition+math_edu_n+confidence_n, data=subset(confmeaninsight, test_condition=="Test2_straight_nonplanar"), FUN=mean)
ins_plot$acclogit=logit(ins_plot$acc, adjust=0.01)

### correction for maths edu + interaction
ins_plot$acclogitcor=ins_plot$acclogit-((ins_plot$math_edu_n)*(summary(perf_confs_cov_plot)$coefficients[12,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$math_edu_n)*(summary(perf_confs_cov_plot)$coefficients[34,1]))

##### correction for number of lesson
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$number_lessons_n)*(summary(perf_confs_cov_plot)$coefficients[10,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$number_lessons_n)*(summary(perf_confs_cov_plot)$coefficients[18,1]))

### correction for confidence
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$confidence_n)*(summary(perf_confs_cov_plot)$coefficients[13,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$confidence_n)*(summary(perf_confs_cov_plot)$coefficients[42,2]))

ins_plot$acclogitcor=exp(ins_plot$acclogitcor)/(1+exp(ins_plot$acclogitcor))

ins <-summary( emmeans(perf_confs_cov_plot, specs=c("insight", "test_condition"), rg.limit=10080, cov.reduce=F), type="response")
predict_ins <- data.frame(ins)
predict_ins=subset(predict_ins, test_condition=="Test2_straight_nonplanar")


insac <- ggplot(aes(x=insight,y=prob),data=predict_ins)+
  geom_point(data=ins_plot,aes(y=acclogitcor, x=insight),size=2, alpha=1/3, position=position_jitter(w=0.05,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.4,width=.2)+
  facet_wrap(~test_condition, ncol=1)+
  theme_classic()+
  geom_point(size=3)
insac <-themetiny(insac)
insac <- insac  + xlab("Insight report")+ylab("Accuracy")
dev.new(width=7*0.65,height=4*0.65)
insac

ggsave("insight_Test2_straight_nonplanar.pdf")



#####  Confidence relation with accuracy in condition Test2 non planar straigth lines, predictions + individual performance with same corrections as figure above

conf_plot=aggregate(acc~participant+confidence_n+number_lessons_n+test_condition+math_edu_n+insight, data=subset(confmeaninsight, test_condition=="Test2_straight_nonplanar"), FUN=mean)
conf_plot$insight=as.numeric(as.character(conf_plot$insight))

conf_plot$acclogit=logit(conf_plot$acc, adjust=0.01)

### correction for maths edu + interaction
conf_plot$acclogitcor=conf_plot$acclogit-((conf_plot$math_edu_n)*(summary(perf_confs_cov_plot)$coefficients[12,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$math_edu_n)*(summary(perf_confs_cov_plot)$coefficients[34,1]))

##### correction for number of lesson
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$number_lessons_n)*(summary(perf_confs_cov_plot)$coefficients[10,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$number_lessons_n)*(summary(perf_confs_cov_plot)$coefficients[18,1]))

#### correction for insight 
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$insight)*(summary(perf_confs_cov_plot)$coefficients[11,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$insight)*(summary(perf_confs_cov_plot)$coefficients[26,1]))
conf_plot$acclogitcor=exp(conf_plot$acclogitcor)/(1+exp(conf_plot$acclogitcor))

### default values of emmeans : math_edu 3.9, insight 0.5 and number lessons at 0, resetting them as mean mevels
conf <-summary( emmeans(perf_confs_cov, specs=c("confidence_n", "test_condition"), rg.limit=10080, cov.reduce=F), type="response")
predict_conf <- data.frame(conf)
predict_conf=subset(predict_conf, test_condition=="Test2_straight_nonplanar")


confac <- ggplot(aes(x=confidence_n, y=prob),data=predict_conf)+
	geom_point(data=conf_plot,aes(y=acclogitcor, x=confidence_n),alpha=1/3, size=2,position=position_jitter(w=0.05,h=0))+
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
     
ggsave("conf_Test2_straight_nonplanar.pdf") 

###############################################################################

### Supplementary analyses : analyses according previous insight (measure of insight for each test: insight before, yes/no)

################################### Relation between insight report and performance ####################################################

#### data frames merging data about insight, confidence and performance
### 
insight_pre=subset(insight, select="participant", insight$position%in%c("planar_geometry", "straight_lines_on_spheres1") & insight$response==1)
insight_pre=unique(insight_pre)

### parts with insight before lessons
insight_lessons=subset(insight, select="participant",insight$position=="lessons" & insight$response==1)

#### before test 2
insight_test2=subset(insight, select="participant", insight$position%in%c("straight_lines_on_spheres2", "definition2") & insight$response==1)
insight_test2 <- unique(insight_test2)

### before test 3
insight_test3=subset(insight, select="participant", insight$position=="straight_lines_on_various_surfaces" & insight$response==1)


###### insights inédits avant lessons, test2 puis test3
insight_lessons$participant[!insight_lessons$participant%in%insight_pre$participant]
insight_test2$participant[!insight_test2$participant%in%insight_lessons$participant]
insight_test3$participant[!insight_test3$participant%in%insight_test2$participant]

#####
prev_ins=tests
prev_ins$insight_bef=0
prev_ins$insight_bef[prev_ins$participant%in%insight_pre$participant] <- 1
prev_ins$insight_bef[prev_ins$participant%in%insight_lessons$participant] <- 1 
prev_ins$insight_bef[(prev_ins$test=="Test2" | prev_ins$test=="Test3") & prev_ins$participant%in%insight_test2$participant] <- 1
## prev_ins$insight_bef[prev_ins$test=="Test3" & prev_ins$participant%in%insight_test2$participant] <- 1
prev_ins$insight_bef[prev_ins$test=="Test3" & prev_ins$participant%in%insight_test3$participant] <- 1
###### normalization
prev_ins$number_lessons_n <- scale(prev_ins$number_lessons, center=TRUE, scale=FALSE)
prev_ins$math_edu_n <- scale(prev_ins$math_edu, center=TRUE, scale=FALSE)          


#### normalization
conf_mean$number_lessons_n <- scale(conf_mean$number_lessons, center=TRUE, scale=FALSE)
conf_mean$confidence_n <- scale(conf_mean$confidence, center=T, scale=F)
conf_mean$math_edu_n <- scale(conf_mean$math_edu, center=TRUE, scale=FALSE)
conf_perf <- merge(tests, conf_mean)
confmeaninsight_bef <- conf_perf
confmeaninsight_bef$insight_bef <- 0
confmeaninsight_bef$insight_bef[confmeaninsight_bef$participant%in%insight_pre$participant] <- 1
confmeaninsight_bef$insight_bef[confmeaninsight_bef$participant%in%insight_lessons$participant] <- 1 
confmeaninsight_bef$insight_bef[(confmeaninsight_bef$test=="Test2" | confmeaninsight_bef$test=="Test3") & confmeaninsight_bef$participant%in%insight_test2$participant] <- 1
confmeaninsight_bef$insight_bef[confmeaninsight_bef$test=="Test3" & confmeaninsight_bef$participant%in%insight_test3$participant] <- 1


###########################################
#### effect of numb of lessons on insight


insight_lessons_bef <- glm(insight_bef~number_lessons_n+math_edu_n, data=prev_ins,family=binomial)
Anova(insight_lessons_bef, type="III")



perf_ins_bef <- mixed(acc~insight_bef*test_condition+ (1|participant), data=prev_ins,family=binomial,check_contrasts=T,method="LRT")
perf_ins_bef


### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on accuracy by test condition

contrast(emmeans(perf_ins_bef,~insight_bef|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight_bef"),adjust="holm")
confint(contrast(emmeans(perf_ins_bef,~insight_bef|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight_bef"),adjust="holm"))


###############with covariates for Math edu  and Number of lessons  ####################################################################

perf_ins_cov_bef <- mixed(acc~test_condition*number_lessons_n+test_condition*insight_bef+math_edu_n*test_condition +(1|participant), data=prev_ins,family=binomial,check_contrasts=T,method="LRT")
perf_ins_cov_bef

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_ins_cov_bef,~insight_bef|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight_bef"), adjust="holm")
confint(contrast(emmeans(perf_ins_cov_bef,~insight_bef|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight_bef"), adjust="holm"))


################################### Relation between insight experiences and confspective judgments of confidence #################################################


################ Correlation tests ######################################################################################################
#### data frames merging data about insight, confidence and performance

confidence$number_lessons_n <- scale(confidence$number_lessons, center=TRUE, scale=FALSE)
confidence$math_edu_n <- scale(confidence$math_edu, center=TRUE, scale=FALSE)
confidence$confidence_n <- scale(confidence$confidence, center=TRUE, scale=FALSE)

confcol<-dcast(participant+included+number_lessons_n+math_edu_n~measurement,value.var="confidence_n",data=confidence)

### one separate df for pos3 confidence comparisons, as one data is missing for pos3.
confinsight_bef <- merge(confcol,prev_ins)
confinsight2_bef=subset(confinsight, Pos3!="NA")                                                                                                                                                

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

perf_confs_bef <- mixed(acc~test_condition*insight_bef + confidence_n*test_condition+(1|participant), data=confmeaninsight_bef,family=binomial,check_contrasts=T,method="LRT")
perf_confs_bef

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_confs_bef,~insight_bef|test_condition,cov.reduce=F), "revpairwise",simple=list("insight_bef"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs_bef,~insight_bef|test_condition,cov.reduce=F), "revpairwise",simple=list("insight_bef"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each test condition 

conf_trends_bef <- emtrends(perf_confs_bef,var="confidence_n",specs=c("test_condition"))
summary(conf_trends_bef,infer=TRUE,null=0, adjust="holm")



############ with covariates for Math edu  and Number of lessons #########################################################

perf_confs_cov_bef <- mixed(acc~test_condition*number_lessons_n+test_condition*insight_bef+math_edu_n*test_condition + confidence_n*test_condition+(1|participant),
data=confmeaninsight_bef,family=binomial, check_contrasts=T,method="LRT")
perf_confs_cov_bef

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_confs_cov_bef,~insight_bef|test_condition,cov.reduce=F, rg.limit=10080), "revpairwise",simple=list("insight_bef"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs_cov_bef,~insight_bef|test_condition,cov.reduce=F, rg.limit=10080), "revpairwise",simple=list("insight_bef"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each  test condition 

conf_trends_cov_bef <- emtrends(perf_confs_cov_bef,var="confidence",specs=c("test_condition"))
summary(conf_trends_cov_bef,infer=T,null=0, adjust="holm")




