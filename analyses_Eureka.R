
library(afex)
library(emmeans)
library(ppcor)
library(car)
library(ggplot2)


parts <- read.table("participants.csv", sep=";", header=T, dec=",")
parts <- subset(parts, included==1 & number_lessons!=0)

tests <- read.table("tests.csv", sep=";", header=T, dec=",")
tests <- subset(tests, included==1 & number_lessons!=0)
tests$test_condition <- relevel(as.factor(tests$test_condition), ref="Test1_non_circles")

introspection <- read.table("introspection_rates.csv", sep=";", header=T, dec=",")
introspection <- subset(introspection, included==1 & number_lessons!=0)
intro_mean <- aggregate(introspection~participant+number_lessons+math_edu+included, data=introspection, FUN=mean)

insight <- read.table("insight_moments.csv", sep = ";", header=T)
insight_bin <- read.table("insight_binary.csv", sep=";", header=T)
insight_bin <- subset(insight_bin, included==1 & number_lessons!=0)


############################################## Effect of number of lessons on performance ########################################################################

lessons_perf <- mixed(acc~number_lessons*test_condition+math_edu*test_condition+(1|participant), data=tests,family=binomial,check_contrasts=FALSE,method="LRT")

### Exploring the interaction between test condition and number of lessons: Linear trends by number of lessons in each test condition 

perf_trends <- emtrends(lessons_perf,var="number_lessons",specs=c("test_condition"))
summary(perf_trends,infer=TRUE,null=0, adjust="holm")




###############################################  Effect of number of lessons on Insight reports ###################################################################

insight_lessons <- glm(insight~number_lessons+math_edu, data=insight_bin,family=binomial)
Anova(insight_lessons, type="III")

################################### Relation between insight report and performance ####################################################

intro_perf <- merge(tests, intro_mean)
insight_perf <- merge(tests,insight_bin)
intromeaninsight <- merge(intro_perf, insight_bin)

perf_ins <- mixed(acc~insight*test_condition+ (1|participant), data=insight_perf,family=binomial,check_contrasts=FALSE,method="LRT")

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on accuracy by test condition

contrast(emmeans(perf_ins,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"),adjust="holm")
confint(contrast(emmeans(perf_ins,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"),adjust="holm"))


###############with covariates for Math edu  and Number of lessons  ####################################################################

perf_ins_cov <- mixed(acc~test_condition*number_lessons+test_condition*insight+math_edu*test_condition +(1|participant), data=insight_perf,family=binomial,check_contrasts=F,method="LRT")

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_ins_cov,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"), adjust="holm")
confint(contrast(emmeans(perf_ins_cov,~insight|test_condition,cov.reduce=F), "revpairwise", combine=T, simple=list("insight"), adjust="holm"))




################################### Relation between insight experiences and introspective judgments of confidence #################################################


################ Correlation tests ######################################################################################################

introcol<-dcast(participant+included+number_lessons+math_edu~position,value.var="introspection",data=introspection)

### one separate df for pos3 confidence comparisons, as one data is missing for pos3. Removing the missing data only for Pos3 comparisons (otherwise it deletes the entire part)
introinsight <- merge(introcol,insight_bin)
introinsight2=subset(introinsight, Pos3!="NA")                                                                                                                                                

### simple correlations between each introspection measure and insight
### not the same nb of df : one missing data point for intro 3
cor.test(~Pos1+Pos2,data=introinsight, method="spearman")
cor.test(~Pos1+Pos3,data=introinsight2, method="spearman")
cor.test(~Pos2+Pos3,data=introinsight2, method="spearman") 
cor.test(~Pos1+insight,data=introinsight, method="spearman") 
cor.test(~Pos2+insight,data=introinsight, method="spearman")
cor.test(~Pos3+insight,data=introinsight2, method="spearman")

### p values corrected with holm 
cory <- cbind(
cor.test(~Pos1+Pos2,data=introinsight, method="spearman"),
cor.test(~Pos1+Pos3,data=introinsight2, method="spearman"),
cor.test(~Pos2+Pos3,data=introinsight2, method="spearman"), 
cor.test(~Pos1+insight,data=introinsight, method="spearman"), 
cor.test(~Pos2+insight,data=introinsight, method="spearman"),
cor.test(~Pos3+insight,data=introinsight2, method="spearman"))
format(p.adjust(cory[3,], method="holm"),scientific=F)   


### Correlations with math_edu and number of lessons as covariates
pcor.test(introinsight$Pos1, introinsight$Pos2,list( introinsight$math_edu, introinsight$number_lessons), method="spearman")
pcor.test(introinsight2$Pos1, introinsight2$Pos3,list( introinsight2$math_edu, introinsight2$number_lessons), method="spearman")
pcor.test(introinsight2$Pos2, introinsight2$Pos3,list( introinsight2$math_edu, introinsight2$number_lessons), method="spearman")
pcor.test(introinsight$Pos1, introinsight$insight,list( introinsight$math_edu, introinsight$number_lessons), method="spearman")
pcor.test(introinsight$Pos2, introinsight$insight,list( introinsight$math_edu, introinsight$number_lessons), method="spearman")
pcor.test(introinsight2$Pos3, introinsight2$insight,list( introinsight2$math_edu, introinsight2$number_lessons), method="spearman")


#### p values corrected with holm
cori <- cbind(
pcor.test(introinsight$Pos1, introinsight$Pos2,list( introinsight$math_edu, introinsight$number_lessons), method="spearman")[,2],
pcor.test(introinsight2$Pos1, introinsight2$Pos3,list( introinsight2$math_edu, introinsight2$number_lessons), method="spearman")[,2],
pcor.test(introinsight2$Pos2, introinsight2$Pos3,list( introinsight2$math_edu, introinsight2$number_lessons), method="spearman")[,2],
pcor.test(introinsight$Pos1, introinsight$insight,list( introinsight$math_edu, introinsight$number_lessons), method="spearman")[,2],
pcor.test(introinsight$Pos2, introinsight$insight,list( introinsight$math_edu, introinsight$number_lessons), method="spearman")[,2],
pcor.test(introinsight2$Pos3, introinsight2$insight,list( introinsight2$math_edu, introinsight2$number_lessons), method="spearman")[,2])
format(p.adjust(cori, method="holm"), scientific=F)


########################## relations between introspections measures and performance #####################################

########################### simple model #################################################################################

perf_intros <- mixed(acc~test_condition*insight + introspection*test_condition+(1|participant), data=intromeaninsight,family=binomial,check_contrasts=F,method="LRT")

### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_intros,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_intros,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each test condition 

intro_trends <- emtrends(perf_intros,var="introspection",specs=c("test_condition"))
summary(intro_trends,infer=TRUE,null=0, adjust="holm")



############ with covariates for Math edu  and Number of lessons #########################################################
perf_intros_cov <- mixed(acc~test_condition*number_lessons+test_condition*insight+math_edu*test_condition + introspection*test_condition+(1|participant), data=intromeaninsight,family=binomial,
check_contrasts=F,method="LRT")


### Effect of insight (estimated contrast between participants who did vs. did not report an insight) on  accuracy by test condition

contrast(emmeans(perf_intros_cov,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm")
confint(contrast(emmeans(perf_intros_cov,~insight|test_condition,cov.reduce=F), "revpairwise",simple=list("insight"), combine=T, adjust="holm"))

### Effect of confidence (linear trends) on accuracy in each  test condition 

intro_trends_cov <- emtrends(perf_intros_cov,var="introspection",specs=c("test_condition"))
summary(intro_trends_cov,infer=TRUE,null=0, adjust="holm")


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
####### predictions of model plus raw datas corrected for math edu 

## No need to set level of math edu here, emmeans already set at mean level in  model
predictions <- data.frame(emmeans(lessons_perf,specs=c("number_lessons","test_condition"), cov.reduce=F, type="response"))

predictions$test_condition= factor(predictions$test_condition, levels=c('Test1_non_circles','Test1_great_circles','Test1_small_circles','Test2_nonstraight_nonplanar',
'Test2_straight_planar','Test2_nonstraight_planar','Test2_straight_nonplanar','Test3_other_surfaces', 'Test3_sphere'), order=T )
predictions=predictions[order(predictions$test_condition),]

all_plot=aggregate(acc~participant+number_lessons+test_condition+math_edu, data=tests, FUN=mean)
all_plot$acclogit=logit(all_plot$acc, adjust=0.01)
all_plot$acclogitcor=all_plot$acclogit-((all_plot$math_edu-3.9)*(summary(lessons_perf)$coefficients[11,1]))
all_plot$coeffniv[all_plot$test_condition=="Test1_great_circles"]=summary(lessons_perf)$coefficients[20,1]
all_plot$coeffniv[all_plot$test_condition=="Test1_small_circles"]=summary(lessons_perf)$coefficients[21,1]
all_plot$coeffniv[all_plot$test_condition=="Test1_non_circles"]=3.9
all_plot$coeffniv[all_plot$test_condition=="Test2_nonstraight_nonplanar"]=summary(lessons_perf)$coefficients[22,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_nonstraight_planar"]=summary(lessons_perf)$coefficients[23,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_straight_nonplanar"]=summary(lessons_perf)$coefficients[24,1]
all_plot$coeffniv[all_plot$test_condition=="Test2_straight_planar"]=summary(lessons_perf)$coefficients[25,1]
all_plot$coeffniv[all_plot$test_condition=="Test3_sphere"]=summary(lessons_perf)$coefficients[27,1]
all_plot$coeffniv[all_plot$test_condition=="Test3_other_surfaces"]=summary(lessons_perf)$coefficients[26,1]
all_plot$acclogitcor=all_plot$acclogitcor-((all_plot$math_edu-3.9)*all_plot$coeffniv)
all_plot$acclogitcor=exp(all_plot$acclogitcor)/(1+exp(all_plot$acclogitcor))


all_plot$test_condition= factor(all_plot$test_condition, levels=c('Test1_non_circles','Test1_great_circles','Test1_small_circles','Test2_nonstraight_nonplanar',
'Test2_straight_planar','Test2_nonstraight_planar','Test2_straight_nonplanar','Test3_other_surfaces', 'Test3_sphere'), order=T )
all_plot=all_plot[order(all_plot$test_condition),]


perf <- ggplot(aes(x=number_lessons,y=prob),data=predictions)+
  geom_point(data=all_plot,aes(y=acclogitcor),  color="grey50",alpha=1/5, size=1.5,position=position_jitter(w=0.3,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,color=factor(number_lessons)),size=0.8)+
  geom_line(size=0.5)+
  facet_wrap( ~ test_condition)+
  geom_point(size=1.5,aes(color=factor(number_lessons)))+
  theme_classic()+
  scale_x_discrete(breaks=c(1,3,5,7),limits=c("1","", "3", "","5", "","7"))
perf <- perf + ggtitle("Effect of the number of lessons on performance") + xlab("Number of lessons")+ylab("Accuracy")
perf <- perf + guides(color=guide_legend("Number of lessons"))
perf <-mycolors(perf)
perf <-themetiny(perf)
dev.new(width=8,height=10)
perf

ggsave("number_lessons_on_perf.png")

###########################################################################################################################
##### Number of lessons effect on insight #################################################################################
####### predictions of model plus raw datas corrected for math edu 

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
ins <- ins + ggtitle("Number of lessons effect on insight reports") + xlab("Number of lessons")+ylab("% insight reports")
ins <- ins + guides(color=guide_legend("Number of lessons"))
ins <- ins + theme(plot.title = element_text(face="bold",size=18,hjust = 0.5))
ins <- themetiny(ins)
ins <-mycolors(ins)
dev.new(width=7,height=4)
ins

ggsave("Numb_lessons_eff_on_Insight.png")


############################################################################################################################
##### Insight relation with accuracy in Test2 condition straight  non planar  lines, predictions and corrected raw datas
##### corrected for math edu, number of lessons  and introspection, taking mean level of each factor for correction


ins_plot=aggregate(acc~participant+insight+number_lessons+test_condition+math_edu+introspection, data=subset(intromeaninsight, test_condition=="Test2_straight_nonplanar"), FUN=mean)
ins_plot$acclogit=logit(ins_plot$acc, adjust=0.01)
ins_plot$coeffniv[ins_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_intros_cov)$coefficients[34,1]
ins_plot$acclogitcor=ins_plot$acclogit-((ins_plot$math_edu-3.9)*(summary(perf_intros_cov)$coefficients[12,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$math_edu-3.9)*ins_plot$coeffniv)
ins_plot$coeffcond[ins_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_intros_cov)$coefficients[18,1]
ins_plot$acclogitcor=ins_plot$acclogit-(ins_plot$number_lessons-4*(summary(perf_intros_cov)$coefficients[10,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-(ins_plot$number_lessons-4*ins_plot$coeffcond)
ins_plot$coeffintro[ins_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_intros_cov)$coefficients[42,1]
ins_plot$acclogitcor=ins_plot$acclogit-((ins_plot$introspection-7.8)*(summary(perf_intros_cov)$coefficients[13,1]))
ins_plot$acclogitcor=ins_plot$acclogitcor-((ins_plot$introspection-7.8)*ins_plot$coeffintro)
ins_plot$acclogitcor=exp(ins_plot$acclogitcor)/(1+exp(ins_plot$acclogitcor))


### default values of emmeans : math_edu 3.9, introspection 7.5 and number lessons at 0, resetting them as mean mevels
ins <-summary( emmeans(perf_intros_cov, specs=c("insight", "test_condition"), at=list("math_edu"=3.9, "introspection"=7.8, "number_lessons"=4),cov.reduce=F), type="response")
predict_ins <- data.frame(ins)
predict_ins=subset(predict_ins, test_condition=="Test2_straight_nonplanar")


insac <- ggplot(aes(x=insight,y=prob),data=predict_ins)+
  geom_point(data=ins_plot,aes(y=acclogitcor, x=insight),size=2, alpha=1/5,position=position_jitter(w=0.05,h=0))+
  geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.4,width=.2)+
  facet_wrap(~test_condition, ncol=1)+
  theme_classic()+
  geom_point(size=3)+
  scale_x_discrete(limits=c(0,1))
insac <-themetiny(insac)
insac <- insac + ggtitle("Insight on Straight non planar lines") + xlab("Insight")+ylab("Accuracy")
dev.new(width=7,height=4)
insac

ggsave("insight_on_nonplanar_straight_lines.jpg")



#####  Introspection relation with accuracy in condition Test2 non planar straigth lines, predictions and corrected raw datas, same corrections than insight figure

intro_plot=aggregate(acc~participant+introspection+number_lessons+test_condition+math_edu+insight, data=subset(intromeaninsight, test_condition=="Test2_straight_nonplanar"), FUN=mean)
intro_plot$acclogit=logit(intro_plot$acc, adjust=0.01)
intro_plot$coeffniv[intro_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_intros_cov)$coefficients[34,1]
intro_plot$acclogitcor=intro_plot$acclogit-((intro_plot$math_edu-3.9)*(summary(perf_intros_cov)$coefficients[12,1]))
intro_plot$acclogitcor=intro_plot$acclogitcor-((intro_plot$math_edu-3.9)*intro_plot$coeffniv)
intro_plot$coeffcond[intro_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_intros_cov)$coefficients[18,1]
intro_plot$acclogitcor=intro_plot$acclogit-(intro_plot$number_lessons-4*(summary(perf_intros_cov)$coefficients[10,1]))
intro_plot$acclogitcor=intro_plot$acclogitcor-(intro_plot$number_lessons-4*intro_plot$coeffcond)
intro_plot$coeffins[intro_plot$test_condition=="Test2_straight_nonplanar"]=summary(perf_intros_cov)$coefficients[26,1]
intro_plot$acclogitcor=intro_plot$acclogit-((intro_plot$insight-0.61)*(summary(perf_intros_cov)$coefficients[11,1]))
intro_plot$acclogitcor=intro_plot$acclogitcor-((intro_plot$insight-0.61)*intro_plot$coeffins)
intro_plot$acclogitcor=exp(intro_plot$acclogitcor)/(1+exp(intro_plot$acclogitcor))

### default values of emmeans : math_edu 3.9, insight 0.5 and number lessons at 0, resetting them as mean mevels
intro <-summary( emmeans(perf_intros_cov, specs=c("introspection", "test_condition"), at=list("math_edu"=3.9, "insight"=0.61, "number_lessons"=4),cov.reduce=F), type="response")
predict_intro <- data.frame(intro)
predict_intro=subset(predict_intro, test_condition=="Test2_straight_nonplanar")


introac <- ggplot(aes(x=introspection, y=prob),data=predict_intro)+
	geom_point(data=intro_plot,aes(y=acclogitcor, x=introspection),alpha=1/5, size=2,position=position_jitter(w=0.05,h=0))+
	facet_wrap(~test_condition, ncol=1)+
	geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL),size=0.8)+
	geom_line()+
	theme_classic()+
	geom_point(size=3)
introac <- mycolors(introac)
introac <-themetiny(introac)
introac <- introac + ggtitle("Introspection on Straight non planar lines") + xlab("Mean Introspection")+ylab("Accuracy")
introac <- introac + guides(color=guide_legend("Insight"))   
dev.new(width=7,height=4)
introac
     
ggsave("intro_difOui.jpg") 

