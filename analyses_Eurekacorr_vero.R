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
tests$test_condition <- relevel(as.factor(tests$test_condition), ref="Test1_non_circles")

confidence <- read.table("confidence_ratings.csv", sep=";", header=T, dec=",")
confidence <- subset(confidence, participant%in%parts$participant)
conf_mean <- aggregate(confidence~participant+number_lessons+math_edu+included, data=confidence, FUN=mean)

Eureka <- read.table("Eureka_moments.csv", sep = ";", header=T)
Eureka <- subset(Eureka, participan%in%parts$participant)
Eureka_bin <- read.table("Eureka_binary.csv", sep=";", header=T)
Eureka_bin <- subset(Eureka_bin, participant%in%parts$participant)


############################################## Effect of number of lessons on performance ########################################################################

lessons_perf <- mixed(acc~number_lessons*test_condition+math_edu*test_condition+(1|participant), data=tests,family=binomial,check_contrasts=FALSE,method="LRT")
lessons_perf

### Exploring the interaction between test condition and number of lessons: Linear trends by number of lessons in each test condition 

perf_trends <- emtrends(lessons_perf,var="number_lessons",specs=c("test_condition"))
summary(perf_trends,infer=TRUE,null=0, adjust="holm")




###############################################  Effect of number of lessons on Eureka reports ###################################################################

Eureka_lessons <- glm(eureka~number_lessons+math_edu, data=Eureka_bin,family=binomial)
Anova(Eureka_lessons, type="III")


################################### Relation between Eureka report and performance ####################################################

#### data frames merging data about Eureka, confidence and performance 
conf_perf <- merge(tests, conf_mean)
Eureka_perf <- merge(tests,Eureka_bin)
confmeanEureka <- merge(conf_perf, Eureka_bin)


perf_eureka <- mixed(acc~eureka*test_condition+ (1|participant), data=Eureka_perf,family=binomial,check_contrasts=FALSE,method="LRT")
perf_eureka


### Effect of Eureka (estimated contrast between participants who did vs. did not report a Eureka) on accuracy by test condition

contrast(emmeans(perf_eureka,~eureka|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka"),adjust="holm")
confint(contrast(emmeans(perf_eureka,~eureka|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka"),adjust="holm"))


###############with covariates for Math edu  and Number of lessons  ####################################################################

perf_eureka_cov <- mixed(acc~test_condition*number_lessons+test_condition*eureka+math_edu*test_condition +(1|participant), data=Eureka_perf,family=binomial,check_contrasts=F,method="LRT")
perf_eureka_cov

### Effect of Eureka (estimated contrast between participants who did vs. did not report a Eureka) on  accuracy by test condition

contrast(emmeans(perf_eureka_cov,~eureka|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka"), adjust="holm")
confint(contrast(emmeans(perf_eureka_cov,~eureka|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("eureka"), adjust="holm"))


################################### Relation between Eureka experiences and judgments of confidence #################################################


################ Correlation tests ######################################################################################################

confcol<-dcast(participant+included+number_lessons+math_edu~measurement,value.var="confidence",data=confidence)

### one separate df for pos3 confidence comparisons, as one data is missing for pos3.
confEureka <- merge(confcol,Eureka_bin)
confEureka2=subset(confEureka, Pos3!="NA")                                                                                                                                                

### simple correlations between each confidence measure and Eureka
### not the same nb of df : one missing data point for conf 3
cor.test(~Pos1+Pos2,data=confEureka, method="spearman")
cor.test(~Pos1+Pos3,data=confEureka2, method="spearman")
cor.test(~Pos2+Pos3,data=confEureka2, method="spearman") 
cor.test(~Pos1+eureka,data=confEureka, method="spearman") 
cor.test(~Pos2+eureka,data=confEureka, method="spearman")
cor.test(~Pos3+eureka,data=confEureka2, method="spearman")

### p values corrected with holm 
cory <- cbind(
cor.test(~Pos1+Pos2,data=confEureka, method="spearman"),
cor.test(~Pos1+Pos3,data=confEureka2, method="spearman"),
cor.test(~Pos2+Pos3,data=confEureka2, method="spearman"), 
cor.test(~Pos1+eureka,data=confEureka, method="spearman"), 
cor.test(~Pos2+eureka,data=confEureka, method="spearman"),
cor.test(~Pos3+eureka,data=confEureka2, method="spearman"))
format(p.adjust(cory[3,], method="holm"),scientific=F)   


### Correlations with math_edu and number of lessons as covariates
pcor.test(confEureka$Pos1, confEureka$Pos2,list( confEureka$math_edu, confEureka$number_lessons), method="spearman")
pcor.test(confEureka2$Pos1, confEureka2$Pos3,list( confEureka2$math_edu, confEureka2$number_lessons), method="spearman")
pcor.test(confEureka2$Pos2, confEureka2$Pos3,list( confEureka2$math_edu, confEureka2$number_lessons), method="spearman")
pcor.test(confEureka$Pos1, confEureka$eureka,list( confEureka$math_edu, confEureka$number_lessons), method="spearman")
pcor.test(confEureka$Pos2, confEureka$eureka,list( confEureka$math_edu, confEureka$number_lessons), method="spearman")
pcor.test(confEureka2$Pos3, confEureka2$eureka,list( confEureka2$math_edu, confEureka2$number_lessons), method="spearman")


#### p values corrected with holm
cori <- cbind(
pcor.test(confEureka$Pos1, confEureka$Pos2,list( confEureka$math_edu, confEureka$number_lessons), method="spearman")[,2],
pcor.test(confEureka2$Pos1, confEureka2$Pos3,list( confEureka2$math_edu, confEureka2$number_lessons), method="spearman")[,2],
pcor.test(confEureka2$Pos2, confEureka2$Pos3,list( confEureka2$math_edu, confEureka2$number_lessons), method="spearman")[,2],
pcor.test(confEureka$Pos1, confEureka$eureka,list( confEureka$math_edu, confEureka$number_lessons), method="spearman")[,2],
pcor.test(confEureka$Pos2, confEureka$eureka,list( confEureka$math_edu, confEureka$number_lessons), method="spearman")[,2],
pcor.test(confEureka2$Pos3, confEureka2$eureka,list( confEureka2$math_edu, confEureka2$number_lessons), method="spearman")[,2])
format(p.adjust(cori, method="holm"), scientific=F)


########################## relation between confidence, Eureka and performance #####################################

########################### simple model #################################################################################

perf_confs <- mixed(acc~test_condition*eureka + confidence*test_condition+(1|participant), data=confmeanEureka,family=binomial,check_contrasts=F,method="LRT")
perf_confs

### Effect of Eureka (estimated contrast between participants who did vs. did not report a Eureka) on  accuracy by test condition

contrast(emmeans(perf_confs,~eureka|test_condition,cov.reduce=F), "revpairwise",simple=list("eureka"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs,~eureka|test_condition,cov.reduce=F), "revpairwise",simple=list("eureka"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each test condition 

conf_trends <- emtrends(perf_confs,var="confidence",specs=c("test_condition"))
summary(conf_trends,infer=TRUE,null=0, adjust="holm")



############ with covariates for Math edu  and Number of lessons #########################################################

perf_confs_cov <- mixed(acc~test_condition*number_lessons+test_condition*eureka+math_edu*test_condition + confidence*test_condition+(1|participant), data=confmeanEureka,family=binomial,
check_contrasts=F,method="LRT")
perf_confs_cov

### Effect of Eureka (estimated contrast between participants who did vs. did not report a Eureka) on  accuracy by test condition

contrast(emmeans(perf_confs_cov,~eureka|test_condition,cov.reduce=F), "revpairwise",simple=list("eureka"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_confs_cov,~eureka|test_condition,cov.reduce=F), "revpairwise",simple=list("eureka"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each  test condition 

conf_trends_cov <- emtrends(perf_confs_cov,var="confidence",specs=c("test_condition"))
summary(conf_trends_cov,infer=TRUE,null=0, adjust="holm")


################ supplementary analyses #############################################################################

############### repartitions of Eurekas by stage in the experiment ###########################################################

table(subset(Eureka,response==1)$position)
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
scale_color_manual(values = c("#00AFBB", "#E7B800", "#CC0066", "#FF6666", "#9999FF", "#D16103","#FFDB6D" ))

}

themetiny=function(ggobject){

ggobject+
theme(plot.title = element_text(face="bold",size=15,hjust = 0.5))+
theme(axis.text=element_text(size=6,face="bold"),                                                                                                                                                 
axis.title=element_text(size=8,face="bold"),
legend.text=element_text(size=10))
}


###### Effect of number of lessons on performance  ###############
####### predictions of model + individual performance corrected for math edu 

## No need to set level of math edu here, emmeans already sets at mean level in  model
predictions <- data.frame(emmeans(lessons_perf,specs=c("number_lessons","test_condition"), cov.reduce=F, type="response"))

predictions$test_condition= factor(predictions$test_condition, levels=c('Test1_non_circles','Test1_great_circles','Test1_small_circles','Test2_nonstraight_nonplanar',
'Test2_straight_planar','Test2_nonstraight_planar','Test2_straight_nonplanar','Test3_other_surfaces', 'Test3_sphere'), order=T )
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
perf_test1 <- perf_test1 + ggtitle("Effect of the number of lessons on performance") + xlab("Number of lessons")+ylab("Accuracy")
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
perf_test2 <- perf_test2 + ggtitle("Effect of the number of lessons on performance") + xlab("Number of lessons")+ylab("Accuracy")
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
  scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))
perf_test3 <- perf_test3 + ggtitle("Effect of the number of lessons on performance") + xlab("Number of lessons")+ylab("Accuracy")
perf_test3 <- perf_test3 + guides(color=guide_legend("Number of lessons"))
perf_test3 <-mycolors(perf_test3)
perf_test3 <-themetiny(perf_test3)
dev.new(width=10,height=4)
perf_test3

ggsave("number_lessons_on_perf_test3.png")

###########################################################################################################################
##### Number of lessons effect on Eureka #######################################################################
####### predictions of model + individual responses corrected for math edu 

eureka_plot <- aggregate(eureka~participant+number_lessons+math_edu, data=Eureka_bin, FUN=mean)                                                                       
eureka_plot$eurlogit <- logit(eureka_plot$eureka, adjust=0.01)
eureka_plot$eurcor <- eureka_plot$eurlogit-((eureka_plot$math_edu-3.9)*summary(Eureka_lessons)$coefficients[3,1])
eureka_plot$eurcor <- exp(eureka_plot$eurcor)/(1+exp(eureka_plot$ercor))

## No need to set level of math edu here, emmeans already set at mean level in  model                                                                                                                   
emm_Eureka <- emmeans(Eureka_lessons,specs="number_lessons", cov.reduce=F,type="response")
predict_Eureka <- data.frame(summary(emm_Eureka))


eur <- ggplot(aes(x=number_lessons,y=prob),data=predict_Eureka)+
  geom_point(data=eureka_plot,aes(y=eurcor),color="grey50",alpha=1/5, size=3,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons)),size=0.8)+
  geom_line(size=0.8)+
  scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))+     
  geom_point(size=3,aes(color=factor(number_lessons)))+
  theme_classic()
eur <- eur + ggtitle("Number of lessons effect on Eureka reports") + xlab("Number of lessons")+ylab("% Eureka reports")
eur <- eur + guides(color=guide_legend("Number of lessons"))
eur <- eur + theme(plot.title = element_text(face="bold",size=18,hjust = 0.5))
eur <- themetiny(eur)
eur <- mycolors(eur)
dev.new(width=7,height=4)
eur

ggsave("Numb_lessons_eff_on_Eureka.png")


############################################################################################################################
##### Eureka relation with accuracy in Test2 condition straight  non planar lines, predictions + individual performance
##### corrected for math edu, number of lessons  and confidence

### SUR CES PARTIES AVEC CORRECTIONS, POUR QUE LES VALEURS N'AIENT PAS L'AIR ARBITRAIRES, CALCULER LES MOYENNES
###  ET UTILISER CES VARIABLES DANS LE CODE
### EX: ICI meanedu <- mean(eureka_plot$math_edu) 
### ET DANS LE CODE CI-DESSOUS: eureka_plot$acclogitcor=eureka_plot$acclogit-((eureka_plot$math_edu-meanedu)*(summary(perf_confs_cov)$coefficients[12,1]))


eureka_plot=aggregate(acc~participant+eureka+number_lessons+test_condition+math_edu+confidence, data=subset(confmeanEureka, test_condition=="Test2_straight_nonplanar"), FUN=mean)
eureka_plot$acclogit=logit(eureka_plot$acc, adjust=0.01)
eureka_plot$coeffniv[eureka_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[34,1]
eureka_plot$acclogitcor=eureka_plot$acclogit-((eureka_plot$math_edu-3.9)*(summary(perf_confs_cov)$coefficients[12,1]))
eureka_plot$acclogitcor=eureka_plot$acclogitcor-((eureka_plot$math_edu-3.9)*eureka_plot$coeffniv)
eureka_plot$coeffcond[eureka_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[18,1]
eureka_plot$acclogitcor=eureka_plot$acclogit-((eureka_plot$number_lessons-4)*(summary(perf_confs_cov)$coefficients[10,1]))
eureka_plot$acclogitcor=eureka_plot$acclogitcor-((eureka_plot$number_lessons-4)*eureka_plot$coeffcond)
eureka_plot$coeffconf[eureka_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[42,1]
eureka_plot$acclogitcor=eureka_plot$acclogit-((eureka_plot$confidence-7.8)*(summary(perf_confs_cov)$coefficients[13,1]))
eureka_plot$acclogitcor=eureka_plot$acclogitcor-((eureka_plot$confidence-7.8)*eureka_plot$coeffconf)
eureka_plot$acclogitcor=exp(eureka_plot$acclogitcor)/(1+exp(eureka_plot$acclogitcor))


### default values of emmeans : math_edu 3.9, confidence 7.5 and number lessons at 0, resetting them as mean mevels
emm_Eureka <-summary( emmeans(perf_confs_cov, specs=c("eureka", "test_condition"), at=list("math_edu"=3.9, "confidence"=7.8, "number_lessons"=4),cov.reduce=F), type="response")
predict_Eureka <- data.frame(emm_Eureka)
predict_Eureka=subset(predict_Eureka, test_condition=="Test2_straight_nonplanar")


eurac <- ggplot(aes(x=eureka,y=prob),data=predict_Eureka)+
  geom_point(data=Eureka_plot,aes(y=acclogitcor, x=eureka),size=2, alpha=1/5,position=position_jitter(w=0.05,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.4,width=.2)+
  facet_wrap(~test_condition, ncol=1)+
  theme_classic()+
  geom_point(size=3)+
  scale_x_discrete(labels=c("No Eureka", "Eureka"))
eurac <-themetiny(eurac)
eurac <- eurac + ggtitle("Eureka relation with straight non planar lines") + xlab("Eureka")+ylab("Accuracy")
dev.new(width=7,height=4)
eurac

ggsave("Eureka_on_nonplanar_straight_lines.jpg")



#####  Confidence relation with accuracy in condition Test2 non planar straigth lines, predictions + individual performance with same corrections as figure above

conf_plot=aggregate(acc~participant+confidence+number_lessons+test_condition+math_edu+eureka, data=subset(confmeanEureka, test_condition=="Test2_straight_nonplanar"), FUN=mean)
conf_plot$acclogit=logit(conf_plot$acc, adjust=0.01)
conf_plot$coeffniv[conf_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[34,1]
conf_plot$acclogitcor=conf_plot$acclogit-((conf_plot$math_edu-3.9)*(summary(perf_confs_cov)$coefficients[12,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$math_edu-3.9)*conf_plot$coeffniv)
conf_plot$coeffcond[conf_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[18,1]
conf_plot$acclogitcor=conf_plot$acclogit-((conf_plot$number_lessons-4)*(summary(perf_confs_cov)$coefficients[10,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$number_lessons-4)*conf_plot$coeffcond)
conf_plot$coeffeur[conf_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_confs_cov)$coefficients[26,1]
conf_plot$acclogitcor=conf_plot$acclogit-((conf_plot$eureka-0.61)*(summary(perf_confs_cov)$coefficients[11,1]))
conf_plot$acclogitcor=conf_plot$acclogitcor-((conf_plot$eureka-0.61)*conf_plot$coeffeur)
conf_plot$acclogitcor=exp(conf_plot$acclogitcor)/(1+exp(conf_plot$acclogitcor))

### default values of emmeans : math_edu 3.9, eureka 0.5 and number lessons at 0, resetting them as mean mevels
conf <-summary( emmeans(perf_confs_cov, specs=c("confidence", "test_condition"), at=list("math_edu"=3.9, "eureka"=0.61, "number_lessons"=4),cov.reduce=F), type="response")
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
confac <- confac + ggtitle("Confidence in understanding relation with straight non planar lines") + xlab("Mean confidence")+ylab("Accuracy")
confac <- confac + guides(color=guide_legend("Eureka"))   
dev.new(width=7,height=4)
confac
     
ggsave("conf_Test2_straight_nonplanar.jpg") 
