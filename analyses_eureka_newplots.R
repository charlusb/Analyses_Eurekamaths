###################################################################

###### Januray 23th, 2023 FINAL VERSION of Eureka paper analyses

###################################################################

library(afex)
library(emmeans)
library(ppcor)
library(car)
library(ggplot2)
library(reshape2)


parts <- read.table("participants.csv", sep=";", header=T, dec=",")
parts <- subset(parts, included==1 & number_lessons!=0) # use this to exclude the participants who did not receive any lessons (main analyses)
# parts <- subset(parts, included==1) # use this instead to include the participants who did not receive any lessons (supplementary analyses)

tests <- read.table("tests.csv", sep=";", header=T, dec=",")
tests <- subset(tests, participant%in%parts$participant)

confidence <- read.table("confidence_ratings.csv", sep=";", header=T, dec=",")
confidence <- subset(confidence, participant%in%parts$participant)
conf_mean <- aggregate(confidence~participant+number_lessons+math_edu+included, data=confidence, FUN=mean)

eureka <- read.table("eureka_moments.csv", sep = ";", header=T)
eureka <- subset(eureka, participant%in%parts$participant)
eureka_bin <- read.table("eureka_binary.csv", sep=";", header=T)
eureka_bin <- subset(eureka_bin, participant%in%parts$participant)

###### eurekas during teaching phase
eureka_teach <- read.table("eureka_lessons.csv", sep=";", header=T)
eureka_teach <- subset(eureka_teach, participant%in%parts$participant)


#### eurekas during learning sessions+tests (everything except pre tests)
eureka_post <- subset(eureka, participant%in%parts$participant)
eureka_post <- subset(insight, !position%in%c("straight_lines_on_spheres1", "planar_geometry" ))
eureka_post <- aggregate(response~participant+included+number_lessons+math_edu+lessons, data=eureka_post, FUN=max)
eureka_post$eureka <- eureka_post$response
eureka_post$response <- NULL




################################################ centering numerical variables on 0

tests$number_lessons_n <- scale(tests$number_lessons, center=TRUE, scale=FALSE)
tests$math_edu_n <- scale(tests$math_edu, center=TRUE, scale=FALSE)

eureka_bin$number_lessons_n <- scale(eureka_bin$number_lessons, center=TRUE, scale=FALSE)
eureka_bin$math_edu_n <- scale(eureka_bin$math_edu, center=TRUE, scale=FALSE)  

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


###############################################  Effect of number of lessons on Eureka reports ###################################################################
eureka_lessons <- glm(eureka~number_lessons_n+math_edu_n, data=eureka_bin,family=binomial)
Anova(eureka_lessons, type="III")


################################### Relation between eureka report and performance ####################################################

#### data frames merging data about eureka, confidence and performance
conf_perf <- merge(tests, conf_mean)
eureka_perf <- merge(tests,eureka_bin)
eureka_perf$eureka <- as.factor(eureka_perf$eureka)
confmeaneureka <- merge(conf_perf, eureka_bin)
confmeaneureka$eureka <- as.factor(confmeaneureka$eureka)

perf_eur <- mixed(acc~eureka*test_condition+ (1|participant), data=eureka_perf,family=binomial,check_contrasts=TRUE,method="LRT")
perf_eur


### Effect of eureka (estimated contrast between participants who did vs. did not report an eureka) on accuracy by test condition

contrast(emmeans(perf_eur,~eureka|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka"),adjust="holm")
confint(contrast(emmeans(perf_eur,~eureka|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka"),adjust="holm"))


###############with covariates for Math edu  and Number of lessons  ####################################################################

perf_eur_cov <- mixed(acc~test_condition*number_lessons_n+test_condition*eureka+math_edu_n*test_condition +(1|participant), data=eureka_perf,family=binomial,check_contrasts=T,method="LRT")
summary(perf_eur_cov)

### Effect of eureka (estimated contrast between participants who did vs. did not report an eureka) on  accuracy by test condition

contrast(emmeans(perf_eur_cov,~eureka|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka"), adjust="holm")
confint(contrast(emmeans(perf_eur_cov,~eureka|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka"), adjust="holm"))


################################### Relation between eureka experiences and confspective judgments of confidence #################################################


################ Correlation tests ######################################################################################################

confcol<-dcast(participant+included+number_lessons_n+math_edu_n~measurement,value.var="confidence",data=confidence_n)

### one separate df for pos3 confidence comparisons, as one data is missing for pos3.
confeureka <- merge(confcol,eureka_bin)
confeureka2=subset(confeureka, Pos3!="NA")                                                                                                                                                
confeureka$eureka <- as.numeric(as.character(confeureka$eureka))
confeureka2$eureka <- as.numeric(as.character(confeureka2$eureka))

### simple correlations between each confidence measure and eureka
### not the same nb of df : one missing data point for conf 3
cor.test(~Pos1+Pos2,data=confeureka, method="spearman")
cor.test(~Pos1+Pos3,data=confeureka2, method="spearman")
cor.test(~Pos2+Pos3,data=confeureka2, method="spearman") 
cor.test(~Pos1+eureka,data=confeureka, method="spearman") 
cor.test(~Pos2+eureka,data=confeureka, method="spearman")
cor.test(~Pos3+eureka,data=confeureka2, method="spearman")

### p values corrected with holm 
cory <- cbind(
cor.test(~Pos1+Pos2,data=confeureka, method="spearman"),
cor.test(~Pos1+Pos3,data=confeureka2, method="spearman"),
cor.test(~Pos2+Pos3,data=confeureka2, method="spearman"), 
cor.test(~Pos1+eureka,data=confeureka, method="spearman"), 
cor.test(~Pos2+eureka,data=confeureka, method="spearman"),
cor.test(~Pos3+eureka,data=confeureka2, method="spearman"))
format(p.adjust(cory[3,], method="holm"),scientific=F)   


### Correlations with math_edu and number of lessons as covariates
pcor.test(confeureka$Pos1, confeureka$Pos2,list( confeureka$math_edu_n, confeureka$number_lessons), method="spearman")
pcor.test(confeureka2$Pos1, confeureka2$Pos3,list( confeureka2$math_edu_n, confeureka2$number_lessons_n), method="spearman")
pcor.test(confeureka2$Pos2, confeureka2$Pos3,list( confeureka2$math_edu_n, confeureka2$number_lessons_n), method="spearman")
pcor.test(confeureka$Pos1, confeureka$eureka,list( confeureka$math_edu_n, confeureka$number_lessons_n), method="spearman")
pcor.test(confeureka$Pos2, confeureka$eureka,list( confeureka$math_edu_n, confeureka$number_lessons_n), method="spearman")
pcor.test(confeureka2$Pos3, confeureka2$eureka,list( confeureka2$math_edu_n, confeureka2$number_lessons_n), method="spearman")


#### p values corrected with holm
cori <- cbind(
pcor.test(confeureka$Pos1, confeureka$Pos2,list( confeureka$math_edu_n, confeureka$number_lessons_n), method="spearman")[,2],
pcor.test(confeureka2$Pos1, confeureka2$Pos3,list( confeureka2$math_edu_n, confeureka2$number_lessons_n), method="spearman")[,2],
pcor.test(confeureka2$Pos2, confeureka2$Pos3,list( confeureka2$math_edu_n, confeureka2$number_lessons_n), method="spearman")[,2],
pcor.test(confeureka$Pos1, confeureka$eureka,list( confeureka$math_edu_n, confeureka$number_lessons_n), method="spearman")[,2],
pcor.test(confeureka$Pos2, confeureka$eureka,list( confeureka$math_edu_n, confeureka$number_lessons_n), method="spearman")[,2],
pcor.test(confeureka2$Pos3, confeureka2$eureka,list( confeureka2$math_edu_n, confeureka2$number_lessons_n), method="spearman")[,2])
format(p.adjust(cori, method="holm"), scientific=F)


########################## relation between confidence, eureka and performance #####################################

########################### simple model #################################################################################

perf_confs <- mixed(acc~test_condition*eureka + confidence_n*test_condition+(1|participant), data=confmeaneureka,family=binomial,check_contrasts=T,method="LRT")
summary(perf_confs)

### Effect of eureka (estimated contrast between participants who did vs. did not report an eureka) on  accuracy by test condition

contrast(emmeans(perf_confs,~eureka|test_condition,cov.reduce=F), "revpairwise",simple=list("eureka"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs,~eureka|test_condition,cov.reduce=F), "revpairwise",simple=list("eureka"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each test condition 

conf_trends <- emtrends(perf_confs,var="confidence_n",specs=c("test_condition"))
summary(conf_trends,infer=TRUE,null=0, adjust="holm")



############ With covariates for Math edu  and Number of lessons #########################################################

perf_confs_cov <- mixed(acc~test_condition*number_lessons_n+test_condition*eureka+math_edu_n*test_condition + confidence_n*test_condition+(1|participant), data=confmeaneureka,family=binomial,
check_contrasts=T,method="LRT")
summary(perf_confs_cov)

### Effect of eureka (estimated contrast between participants who did vs. did not report an eureka) on  accuracy by test condition

contrast(emmeans(perf_confs_cov,~eureka|test_condition,cov.reduce=F, rg.limit=10080), "revpairwise",simple=list("eureka"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs_cov,~eureka|test_condition,cov.reduce=F, rg.limit=10080), "revpairwise",simple=list("eureka"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each  test condition 

conf_trends_cov <- emtrends(perf_confs_cov,var="confidence_n",specs=c("test_condition"))
summary(conf_trends_cov,infer=T,null=0, adjust="holm")


%%%%
################ supplementary analyses #############################################################################
############### repartitions of Eurekas by stage in the experiment ###########################################################

table(subset(eureka,response==1)$position)
### DANS LA VERSION PRECEDENTE, EUREKA N'ETAIT PAS RESTREINT AUX SUJETS INCLUS. VERIFIER QUE LES POURCENTAGES
### DONNES DANS L'ARTICLE CORRESPONDENT AUX SUJETS INCLUS

############## Number of lessons effect on questions about parallel lines in test3 #################################

parallel <- read.table("parallel_perf.csv", sep=";", header=T)
parallel <- subset(parallel, number_lessons !=0 & included==1)

### A METTRE A JOUR EN FONCTION DE CE QU'ON A MIS DANS L'ARTICLE
### (l'analyse "simple" par % réponses correctes me semble plus simple à décrire à priori)

### average accuracy
para <- aggregate(acc~participant+number_lessons+math_edu, data=parallel, subset=included==1, FUN=mean)
mean(para$acc)
### 45.5% correct

### Effect of number of lessons
### UTILISER MIXED PAR SOUCI DE COHERENCE AVEC LES ANALYSES PRINCIPALES
paral_lessons <- glmer(acc~number_lessons+math_edu+(1|participant), data=parallel, family=binomial)
summary(paral_lessons)
para_trends <- summary(emmeans(paral_lessons,~number_lessons, cov.reduce=F), infer=T)

### SUPPRIMER CE QUI SUIT
#### counting as correct only those who give correct answers to both 
para <- aggregate(acc~participant+number_lessons+math_edu+included, data=parallel, FUN=min)

###### mean accuracy ###
nrow(subset(para, acc==1)/56

## et donc supprimer ceci
paral_lessons <- glm(acc~number_lessons+math_edu, data=parallel, family=binomial)
summary(paral_lessons)
para_trends <- summary(emmeans(paral_lessons,~number_lessons, cov.reduce=F), infer=T)


### SUPPRIMER CETTE PARTIE? ON N'EN PARLE PAS DU TOUT DANS LE TEXTE
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
##### Number of lessons effect on eureka #################################################################################
####### predictions of model + individual responses corrected for math edu

eureka_bin$eureka=as.numeric(as.character(eureka_bin$eureka))
eur_cond <- aggregate(eureka~participant+number_lessons_n+math_edu_n, data=eureka_bin, FUN=mean)                                                                       
eur_cond$eurlogit <- logit(eur_cond$eureka, adjust=0.01)
eur_cond$eurcor <- eur_cond$eurlogit-((eur_cond$math_edu_n)*(summary(eureka_lessons)$coefficients[3,1]))
eur_cond$eurcor <- exp(eur_cond$eurcor)/(1+exp(eur_cond$eurcor))

eureka <- emmeans(eureka_lessons,specs="number_lessons_n", cov.reduce=F,type="response")
predict_eureka <- data.frame(summary(eureka))


eur <- ggplot(aes(x=number_lessons_n,y=prob),data=predict_eureka)+
  geom_point(data=eur_cond,aes(y=eurcor),color="grey50",alpha=1/5, size=3,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons_n)),size=0.8)+
  geom_line(size=0.8)+
  ## scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))+     
  geom_point(size=3,aes(color=factor(number_lessons_n)))+
  theme_classic()
eur <- eur + ggtitle("Effect of number of lessons on eureka report") + xlab("Number of lessons")+ylab("% eureka reports")
eur <- eur + guides(color=guide_legend("Number of lessons"))
eur <- eur + theme(plot.title = element_text(face="bold",size=15,hjust = 0.5))
eur <- themetiny(eur)
eur <-mycolors(eur)
dev.new(width=7,height=4)
eur

ggsave("Numb_lessons_eff_on_Eureka.pdf")


############################################################################################################################
##### Eureka relation with accuracy in Test2 condition straight  non planar  lines, predictions + individual performance
##### corrected for math edu, number of lessons  and confidence

##### model with original contrasts for plots
perf_confs_cov_plot <- mixed(acc~test_condition*number_lessons_n+test_condition*eureka+math_edu_n*test_condition + confidence_n*test_condition+(1|participant), data=confmeaneureka,family=binomial,
check_contrasts=F,method="LRT")


eur_plot=aggregate(acc~participant+eureka+number_lessons_n+test_condition+math_edu_n+confidence_n, data=subset(confmeaneureka, test_condition=="Test2_straight_nonplanar"), FUN=mean)
eur_plot$acclogit=logit(eur_plot$acc, adjust=0.01)

### correction for maths edu + interaction
eur_plot$acclogitcor=eur_plot$acclogit-((eur_plot$math_edu_n)*(summary(perf_confs_cov_plot)$coefficients[12,1]))
eur_plot$acclogitcor=eur_plot$acclogitcor-((eur_plot$math_edu_n)*(summary(perf_confs_cov_plot)$coefficients[34,1]))

##### correction for number of lesson
eur_plot$acclogitcor=eur_plot$acclogitcor-((eur_plot$number_lessons_n)*(summary(perf_confs_cov_plot)$coefficients[10,1]))
eur_plot$acclogitcor=eur_plot$acclogitcor-((eur_plot$number_lessons_n)*(summary(perf_confs_cov_plot)$coefficients[18,1]))

### correction for confidence
eur_plot$acclogitcor=eur_plot$acclogitcor-((eur_plot$confidence_n)*(summary(perf_confs_cov_plot)$coefficients[13,1]))
eur_plot$acclogitcor=eur_plot$acclogitcor-((eur_plot$confidence_n)*(summary(perf_confs_cov_plot)$coefficients[42,2]))

eur_plot$acclogitcor=exp(eur_plot$acclogitcor)/(1+exp(eur_plot$acclogitcor))

eur <-summary( emmeans(perf_confs_cov_plot, specs=c("eureka", "test_condition"), rg.limit=10080, cov.reduce=F), type="response")
predict_eur <- data.frame(eur)
predict_eur=subset(predict_eur, test_condition=="Test2_straight_nonplanar")


eurac <- ggplot(aes(x=eureka,y=prob),data=predict_eur)+
  geom_point(data=eur_plot,aes(y=acclogitcor, x=eureka),size=2, alpha=1/3, position=position_jitter(w=0.05,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.4,width=.2)+
  facet_wrap(~test_condition, ncol=1)+
  theme_classic()+
  geom_point(size=3)
eurac <-themetiny(eurac)
eurac <- eurac  + xlab("Eureka report")+ylab("Accuracy")
dev.new(width=7*0.65,height=4*0.65)
eurac

ggsave("eureka_Test2_straight_nonplanar.pdf")



#####  Confidence relation with accuracy in condition Test2 non planar straigth lines, predictions + individual performance with same corrections as figure above

conf_plot=aggregate(acc~participant+confidence_n+number_lessons_n+test_condition+math_edu_n+eureka, data=subset(confmeaneureka, test_condition=="Test2_straight_nonplanar"), FUN=mean)
conf_plot$eureka=as.numeric(as.character(conf_plot$eureka))

conf_plot$acclogit=logit(conf_plot$acc, adjust=0.01)

### correction for maths edu + interaction
conf_plot$acclogitcor=conf_plot$acclogit-((conf_plot$math_edu_n)*(summary(perf_confs_cov_plot)$coefficients[12,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$math_edu_n)*(summary(perf_confs_cov_plot)$coefficients[34,1]))

##### correction for number of lesson
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$number_lessons_n)*(summary(perf_confs_cov_plot)$coefficients[10,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$number_lessons_n)*(summary(perf_confs_cov_plot)$coefficients[18,1]))

#### correction for eureka 
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$eureka)*(summary(perf_confs_cov_plot)$coefficients[11,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$eureka)*(summary(perf_confs_cov_plot)$coefficients[26,1]))
conf_plot$acclogitcor=exp(conf_plot$acclogitcor)/(1+exp(conf_plot$acclogitcor))

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
confac <- confac + guides(color=guide_legend("Eureka"))   
dev.new(width=7*0.65,height=4*0.65)
confac
     
ggsave("conf_Test2_straight_nonplanar.pdf") 

###############################################################################

### Supplementary analyses : analyses according previous eureka (measure of eureka for each test: eureka before, yes/no)

################################### Relation between eureka report and performance ####################################################

#### data frames merging data about eureka, confidence and performance
### 
eureka_pre=subset(eureka, select="participant", eureka$position%in%c("planar_geometry", "straight_lines_on_spheres1") & eureka$response==1)
eureka_pre=unique(eureka_pre)

### parts with eureka before lessons
eureka_lessons=subset(eureka, select="participant",eureka$position=="lessons" & eureka$response==1)

#### before test 2
eureka_test2=subset(eureka, select="participant", eureka$position%in%c("straight_lines_on_spheres2", "definition2") & eureka$response==1)
eureka_test2 <- unique(eureka_test2)

### before test 3
eureka_test3=subset(eureka, select="participant", eureka$position=="straight_lines_on_various_surfaces" & eureka$response==1)


###### eurekas inédits avant lessons, test2 puis test3
eureka_lessons$participant[!eureka_lessons$participant%in%eureka_pre$participant]
eureka_test2$participant[!eureka_test2$participant%in%eureka_lessons$participant]
eureka_test3$participant[!eureka_test3$participant%in%eureka_test2$participant]

#####
prev_eur=tests
prev_eur$eureka_bef=0
prev_eur$eureka_bef[prev_eur$participant%in%eureka_pre$participant] <- 1
prev_eur$eureka_bef[prev_eur$participant%in%eureka_lessons$participant] <- 1 
prev_eur$eureka_bef[(prev_eur$test=="Test2" | prev_eur$test=="Test3") & prev_eur$participant%in%eureka_test2$participant] <- 1
## prev_eur$eureka_bef[prev_eur$test=="Test3" & prev_eur$participant%in%eureka_test2$participant] <- 1
prev_eur$eureka_bef[prev_eur$test=="Test3" & prev_eur$participant%in%eureka_test3$participant] <- 1
###### normalization
prev_eur$number_lessons_n <- scale(prev_eur$number_lessons, center=TRUE, scale=FALSE)
prev_eur$math_edu_n <- scale(prev_eur$math_edu, center=TRUE, scale=FALSE)          


#### normalization
conf_mean$number_lessons_n <- scale(conf_mean$number_lessons, center=TRUE, scale=FALSE)
conf_mean$confidence_n <- scale(conf_mean$confidence, center=T, scale=F)
conf_mean$math_edu_n <- scale(conf_mean$math_edu, center=TRUE, scale=FALSE)
conf_perf <- merge(tests, conf_mean)
confmeaneureka_bef <- conf_perf
confmeaneureka_bef$eureka_bef <- 0
confmeaneureka_bef$eureka_bef[confmeaneureka_bef$participant%in%eureka_pre$participant] <- 1
confmeaneureka_bef$eureka_bef[confmeaneureka_bef$participant%in%eureka_lessons$participant] <- 1 
confmeaneureka_bef$eureka_bef[(confmeaneureka_bef$test=="Test2" | confmeaneureka_bef$test=="Test3") & confmeaneureka_bef$participant%in%eureka_test2$participant] <- 1
confmeaneureka_bef$eureka_bef[confmeaneureka_bef$test=="Test3" & confmeaneureka_bef$participant%in%eureka_test3$participant] <- 1


###########################################
#### effect of numb of lessons on eureka


eureka_lessons_bef <- glm(eureka_bef~number_lessons_n+math_edu_n, data=prev_eur,family=binomial)
Anova(eureka_lessons_bef, type="III")



perf_eur_bef <- mixed(acc~eureka_bef*test_condition+ (1|participant), data=prev_eur,family=binomial,check_contrasts=T,method="LRT")
perf_eur_bef


### Effect of eureka (estimated contrast between participants who did vs. did not report an eureka) on accuracy by test condition

contrast(emmeans(perf_eur_bef,~eureka_bef|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka_bef"),adjust="holm")
confint(contrast(emmeans(perf_eur_bef,~eureka_bef|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka_bef"),adjust="holm"))


###############with covariates for Math edu  and Number of lessons  ####################################################################

perf_eur_cov_bef <- mixed(acc~test_condition*number_lessons_n+test_condition*eureka_bef+math_edu_n*test_condition +(1|participant), data=prev_eur,family=binomial,check_contrasts=T,method="LRT")
perf_eur_cov_bef

### Effect of eureka (estimated contrast between participants who did vs. did not report an eureka) on  accuracy by test condition

contrast(emmeans(perf_eur_cov_bef,~eureka_bef|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka_bef"), adjust="holm")
confint(contrast(emmeans(perf_eur_cov_bef,~eureka_bef|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka_bef"), adjust="holm"))


################################### Relation between eureka experiences and confspective judgments of confidence #################################################


################ Correlation tests ######################################################################################################
#### data frames merging data about eureka, confidence and performance

confidence$number_lessons_n <- scale(confidence$number_lessons, center=TRUE, scale=FALSE)
confidence$math_edu_n <- scale(confidence$math_edu, center=TRUE, scale=FALSE)
confidence$confidence_n <- scale(confidence$confidence, center=TRUE, scale=FALSE)

confcol<-dcast(participant+included+number_lessons_n+math_edu_n~measurement,value.var="confidence_n",data=confidence)

### one separate df for pos3 confidence comparisons, as one data is missing for pos3.
confeureka_bef <- merge(confcol,prev_eur)
confeureka2_bef=subset(confeureka, Pos3!="NA")                                                                                                                                                

### simple correlations between each confidence measure and eureka
### not the same nb of df : one missing data point for conf 3
cor.test(~Pos1+Pos2,data=confeureka, method="spearman")
cor.test(~Pos1+Pos3,data=confeureka2, method="spearman")
cor.test(~Pos2+Pos3,data=confeureka2, method="spearman") 
cor.test(~Pos1+eureka,data=confeureka, method="spearman") 
cor.test(~Pos2+eureka,data=confeureka, method="spearman")
cor.test(~Pos3+eureka,data=confeureka2, method="spearman")

### p values corrected with holm 
cory <- cbind(
cor.test(~Pos1+Pos2,data=confeureka, method="spearman"),
cor.test(~Pos1+Pos3,data=confeureka2, method="spearman"),
cor.test(~Pos2+Pos3,data=confeureka2, method="spearman"), 
cor.test(~Pos1+eureka,data=confeureka, method="spearman"), 
cor.test(~Pos2+eureka,data=confeureka, method="spearman"),
cor.test(~Pos3+eureka,data=confeureka2, method="spearman"))
format(p.adjust(cory[3,], method="holm"),scientific=F)   


### Correlations with math_edu and number of lessons as covariates
pcor.test(confeureka$Pos1, confeureka$Pos2,list( confeureka$math_edu, confeureka$number_lessons), method="spearman")
pcor.test(confeureka2$Pos1, confeureka2$Pos3,list( confeureka2$math_edu, confeureka2$number_lessons), method="spearman")
pcor.test(confeureka2$Pos2, confeureka2$Pos3,list( confeureka2$math_edu, confeureka2$number_lessons), method="spearman")
pcor.test(confeureka$Pos1, confeureka$eureka,list( confeureka$math_edu, confeureka$number_lessons), method="spearman")
pcor.test(confeureka$Pos2, confeureka$eureka,list( confeureka$math_edu, confeureka$number_lessons), method="spearman")
pcor.test(confeureka2$Pos3, confeureka2$eureka,list( confeureka2$math_edu, confeureka2$number_lessons), method="spearman")


#### p values corrected with holm
cori <- cbind(
pcor.test(confeureka$Pos1, confeureka$Pos2,list( confeureka$math_edu, confeureka$number_lessons), method="spearman")[,2],
pcor.test(confeureka2$Pos1, confeureka2$Pos3,list( confeureka2$math_edu, confeureka2$number_lessons), method="spearman")[,2],
pcor.test(confeureka2$Pos2, confeureka2$Pos3,list( confeureka2$math_edu, confeureka2$number_lessons), method="spearman")[,2],
pcor.test(confeureka$Pos1, confeureka$eureka,list( confeureka$math_edu, confeureka$number_lessons), method="spearman")[,2],
pcor.test(confeureka$Pos2, confeureka$eureka,list( confeureka$math_edu, confeureka$number_lessons), method="spearman")[,2],
pcor.test(confeureka2$Pos3, confeureka2$eureka,list( confeureka2$math_edu, confeureka2$number_lessons), method="spearman")[,2])
format(p.adjust(cori, method="holm"), scientific=F)


########################## relation between confidence, eureka and performance #####################################

########################### simple model #################################################################################

perf_confs_bef <- mixed(acc~test_condition*eureka_bef + confidence_n*test_condition+(1|participant), data=confmeaneureka_bef,family=binomial,check_contrasts=T,method="LRT")
perf_confs_bef

### Effect of eureka (estimated contrast between participants who did vs. did not report an eureka) on  accuracy by test condition

contrast(emmeans(perf_confs_bef,~eureka_bef|test_condition,cov.reduce=F), "revpairwise",simple=list("eureka_bef"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs_bef,~eureka_bef|test_condition,cov.reduce=F), "revpairwise",simple=list("eureka_bef"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each test condition 

conf_trends_bef <- emtrends(perf_confs_bef,var="confidence_n",specs=c("test_condition"))
summary(conf_trends_bef,infer=TRUE,null=0, adjust="holm")



############ with covariates for Math edu  and Number of lessons #########################################################

perf_confs_cov_bef <- mixed(acc~test_condition*number_lessons_n+test_condition*eureka_bef+math_edu_n*test_condition + confidence_n*test_condition+(1|participant),
data=confmeaneureka_bef,family=binomial, check_contrasts=T,method="LRT")
perf_confs_cov_bef

### Effect of eureka (estimated contrast between participants who did vs. did not report an eureka) on  accuracy by test condition

contrast(emmeans(perf_confs_cov_bef,~eureka_bef|test_condition,cov.reduce=F, rg.limit=10080), "revpairwise",simple=list("eureka_bef"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs_cov_bef,~eureka_bef|test_condition,cov.reduce=F, rg.limit=10080), "revpairwise",simple=list("eureka_bef"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each  test condition 

conf_trends_cov_bef <- emtrends(perf_confs_cov_bef,var="confidence",specs=c("test_condition"))
summary(conf_trends_cov_bef,infer=T,null=0, adjust="holm")




