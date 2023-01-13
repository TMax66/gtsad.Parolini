library(readxl)
library(dplyr)
library(gplots)
library(ggplot2)
library(tidyr)      
library(scales)     
library(ggthemes)
library(DescTools)
library(rstanarm)
library(bayesplot)
library(coda)
library(loo)
library(DataExplorer)
library(emmeans)
library(ggeffects)
library(rethinking)
library(tidyverse)
library(ggridges)
library(RColorBrewer)

#DATI####
df <- read_excel("df.xlsx")
cols <- c("specie", "HEV","tgondi","governorate", "area", "sex", "age_class", "year", "sector")

deer_b<-filter(df, specie=="DEER")
deer_b<-select(deer_b, c("tgondi","age_class","sex","area","year"))
deer_b<-deer_b[complete.cases(deer_b),]

deer_b$tgondi<-as.numeric(as.factor(deer_b$tgondi))
deer_b$tgondi<-ifelse(deer_b$tgondi==1, 0, deer_b$tgondi-1)
deer_b$age_class<-as.numeric(as.factor(deer_b$age_class))
deer_b$sex<-as.numeric(as.factor(deer_b$sex))
deer_b$area<-as.numeric(as.factor(deer_b$area))
deer_b$year<-as.numeric(as.factor(deer_b$year))

data_list<-list(
  tgondi=as.integer(deer_b$tgondi), 
  age_class=as.integer(deer_b$age_class), 
  sex=as.integer(deer_b$sex), 
  area=as.integer(deer_b$area),
  year=as.integer(deer_b$year)
)

area<-deer_b %>% 
  group_by(area) %>% 
  summarise(prev=mean(tgondi)) %>% 
  arrange(prev) %>% 
  mutate(area=factor(area,unique(area))) %>%
  ggplot(aes(x=area, y=prev))+geom_bar(stat = "identity", width = 0.3, fill="firebrick4")+
  coord_flip()

age<-deer_b %>% 
  group_by(age_class) %>% 
  summarise(prev=mean(tgondi)) %>% 
  arrange(prev) %>% 
  mutate(age_class=factor(age_class,unique(age_class))) %>%
  ggplot(aes(x=age_class, y=prev))+geom_bar(stat = "identity", width = 0.3, fill="firebrick4")+
  coord_flip()

anno<-deer_b %>% 
  group_by(year) %>% 
  summarise(prev=mean(tgondi)) %>% 
  arrange(prev) %>% 
  mutate(year=factor(year,unique(year))) %>%
  ggplot(aes(x=year, y=prev))+geom_bar(stat = "identity", width = 0.3, fill="firebrick4")+
  coord_flip()


library(patchwork)
area+age+anno


### DAG####
library(dagitty)

Toxo<-dagitty( "dag{ 
            Area<-Year->Age
             Area->Age
             Area->T<-Age
                     }")


plot(graphLayout(Toxo))

impliedConditionalIndependencies(Toxo)    


for( n in names(Toxo) ){
  for( m in children(Toxo,n) ){
    a <- adjustmentSets( Toxo, n, m, effect="direct" )
    if( length(a) > 0 ){
      cat("The coefficient on ",n,"->",m,
          " is identifiable controlling for:\n",sep="")
      print( a, prefix=" * " )
    }
  }
}

for( n in names(Toxo) ){
  for( m in setdiff( descendants( Toxo, n ), n ) ){
    a <- adjustmentSets( Toxo, n, m )
    if( length(a) > 0 ){
      cat("The total effect of ",n," on ",m,
          " is identifiable controlling for:\n",sep="")
      print( a, prefix=" * " )
    }
  }
}


#PRIOR PREDICTIVE CHECK####

#modello nullo..####
m0.1<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a,
    a~dnorm(0,1)), 
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)
set.seed(999)
prior1<-extract.prior(m0.1)
prior1$a<-inv_logit(prior1$a)

m0.2<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a,
    a~dnorm(0,5)), 
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)
 
set.seed(999)
prior2<-extract.prior(m0.2)

prior2$a<-inv_logit(prior2$a)
dens(prior2$a,adj=0.1)


m0.3<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a,
    a~dnorm(0,2.5)), 
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)

set.seed(999)
prior3<-extract.prior(m0.3)

prior3$a<-inv_logit(prior3$a)
 
dens(prior3$a,adj=0.1)


m0.4<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a,
    a~dnorm(0,1.5)), 
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)

 
set.seed(999)
prior4<-extract.prior(m0.4)

prior4$a<-inv_logit(prior4$a)

dens(prior4$a,adj=0.1)


png("priors.png")
par(mfrow=c(2,2))
dens(prior2$a,adj=0.1, main="norm(0,5)")
dens(prior3$a,adj=0.1, main="norm(0,2.5)")
dens(prior4$a,adj=0.1, main="norm(0,1.5)")
dens(prior1$a,adj=0.1, main="norm(0,1)")
dev.off()


#modello con predittore####
m1.1<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a+b[area],
    a~dnorm(0,1.5), 
    b[area]~dnorm(0,1)),
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000, chains=4,log_lik=TRUE)

set.seed(999)
prior5<-extract.prior(m1.1)

p1<-sapply(1:4, function(k) inv_logit(prior5$a+prior5$b[,k]))

dens(abs(p1[,1]-p1[,2]), adj=0.1, main="norm(0,1")

mean(abs(p1[,1]-p1[,2]))

m1.2<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a+b[area],
    a~dnorm(0,1.5), 
  b[area]~dnorm(0,1.5)),
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)

set.seed(999)
prior6<-extract.prior(m1.2)

p2<-sapply(1:4, function(k) inv_logit(prior6$a+prior6$b[,k]))

dens(abs(p2[,1]-p2[,2]), adj=0.1)

mean(abs(p2[,1]-p2[,2]))

m1.3<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a+b[area],
    a~dnorm(0,1.5), 
    b[area]~dnorm(0,2)),
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)

set.seed(999)
prior7<-extract.prior(m1.3)

p3<-sapply(1:4, function(k) inv_logit(prior7$a+prior7$b[,k]))

dens(abs(p3[,1]-p3[,2]), adj=0.1)

mean(abs(p3[,1]-p3[,2]))


png("priors2.png")
par(mfrow=c(2,2))
dens(abs(p1[,1]-p1[,2]), adj=0.1, main="norm(0,1")
dens(abs(p2[,1]-p2[,2]), adj=0.1, main="norm(0,1.5")
dens(abs(p3[,1]-p3[,2]), adj=0.1, main="norm(0,2)")
dev.off()


#MODEL SELECTION####

#full model-M1####

M1<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a+b[area]+b2[age_class]+b3[year],
    a~dnorm(0,1.5), 
    b[area]~dnorm(0,2),
    b2[age_class]~dnorm(0,1.5),
    b3[year]~dnorm(0,1.5)
    ),
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)




#model area random-M2####

M2a<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a_bar+ z[area]*sigma_a+ #area intercept
    b2[age_class]+b3[year],
    b2[age_class]~dnorm(0,1.5),
    b3[year]~dnorm(0,1.5),
    z[area]~dnorm(0, 1), 
    a_bar~dnorm(0,1.5),
    sigma_a~dexp(1),
    
    gq> vector[area]:a <<- a_bar+z*sigma_a
  ),
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)






# labsM2<-c( "Valley1", "Valley2", "Valley3", "Valley4",
#             "Age_class 1", "Age_class 2", "Age_class 3",
#             "2017","2018", "sValley1", "sValley2", "sValley3", "sValley4",
#             "a_bar","sigma_a","iValley1", "iValley2", "iValley3", "iValley4"
# )

# plot(precis(M2,dept=2),labels=labsM2)


##model area e anno random-M3####

M3<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a_bar+ z[area]*sigma_a+#area intercept
      x[year]*sigma_g+#year intercept
      b2[age_class], #fixed effect
    #priors fixed effect
    b2[age_class]~dnorm(0,1.5),
    
    #standardizide priors intercepts
    z[area]~dnorm(0,1), 
    x[year]~dnorm(0,1),
    
    #hyper-priors
    a_bar~dnorm(0,1.5),
    sigma_a~dexp(1),
    sigma_g~dexp(1),
    
    #vettori stime intercette inv-standardized
    gq>vector[area]:a <<- a_bar+z*sigma_a,
    gq>vector[year]:g <<- x*sigma_g
    
  ),
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)




# M3<-ulam(
#   alist(
#     tgondi~dbinom(1,p),
#     logit(p)<-a_bar+ z[area]*sigma_a+#area intercept
#               x[year]*sigma_g+#year intercept
#               b[area]+b2[age_class], #fixed effect
#     #priors fixed effect
#     b[area]~dnorm(0,2),
#     b2[age_class]~dnorm(0,1.5),
#     
#     #standardizide priors intercepts
#     z[area]~dnorm(0,1), 
#     x[year]~dnorm(0,1),
#     
#     #hyper-priors
#     a_bar~dnorm(0,1.5),
#     sigma_a~dexp(1),
#     sigma_g~dexp(1),
#     
#     #vettori stime intercette inv-standardized
#     gq>vector[area]:a <<- a_bar+z*sigma_a,
#     gq>vector[year]:g <<- x*sigma_g
#     
#   ),
#   data=data_list,control=list(adapt_delta=0.99), 
#   iter = 20000,  chains=4,log_lik=TRUE)
# print(xtable(T), type="html", comment=FALSE) 



M4<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<-a_bar+x[year]*sigma_g+#year intercept
      b1[area]+
      b2[age_class], #fixed effect
    #priors fixed effect
     b1[area]~dnorm(0,1.5),
     b2[age_class]~dnorm(0,1.5),
    #standardizide priors intercepts
    x[year]~dnorm(0,1),
    
    #hyper-priors
    a_bar~dnorm(0,1.5),
    sigma_g~dexp(1),
    
    #vettori stime intercette inv-standardized
    gq>vector[year]:g <<- x*sigma_g
  ),
  data=data_list,control=list(adapt_delta=0.99), 
  iter = 20000,  chains=4,log_lik=TRUE)

print(xtable(T2), type="html", comment=FALSE) 















# labsM3<-c( "Valley1", "Valley2", "Valley3", "Valley4",
#            "Age_class 1", "Age_class 2", "Age_class 3",
#           "sValley1", "sValley2", "sValley3", "sValley4",
#           "s2017","s2018",
#            "a_bar","sigma_a", "sigma_g","i2017","i2018",
#           "iValley1", "iValley2", "iValley3", "iValley4"
# )
# 
# precis(M3, depth = 2, pars=c("b","b2"),labels=labsM3)
# plot(precis(M3,dept=2, pars=c("b","b2")),labels=labsM3)



#confronto tra modelli WAIC e PSIS####
T<-compare(M1,M2,M3)  
T2<-compare(M4, M3,M2, M1)
 



postM3<-extract.samples(M3)
post2.1<-data.frame(post2.1)
post2.1<-post2.1%>%
  select(c(2:8,14:17)) #selezione delle posterior di interesse
names(post2.1)<-labs2.1
post2.1 %>%  
  pivot_longer(cols=1:11,names_to = "factor", values_to = "logit") %>% 
  mutate("prevalence"=inv_logit(logit))%>% 
  ggplot(aes(x = prevalence, y=factor,fill = stat(x))) + geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
  theme(legend.position="none", text = element_text(size=10))+
  labs(x="Posterior Seroprevalence estimate of T.gondii in Deer",y="")












area<-deer_b %>% 
  group_by(area) %>% 
  summarise(prev=mean(tgondi)) %>% 
  arrange(prev) %>% 
  mutate(area=factor(area,unique(area))) %>%
  ggplot(aes(x=area, y=prev))+geom_bar(stat = "identity", width = 0.3, fill="firebrick4")+
  coord_flip()

age<-deer_b %>% 
  group_by(age_class) %>% 
  summarise(prev=mean(tgondi)) %>% 
  arrange(prev) %>% 
  mutate(age_class=factor(age_class,unique(age_class))) %>%
  ggplot(aes(x=age_class, y=prev))+geom_bar(stat = "identity", width = 0.3, fill="firebrick4")+
  coord_flip()


library(patchwork)

area+age

#   m2.1<-ulam(
#   alist(
#     tgondi~dbinom(1,p),
#     logit(p)<- a_bar+x[area]*sigma_a + b1[age_class] + b2[sex] + b4[year], #standardized area intercepts (pg424)
#     a_bar~dnorm(0,1),
#     b1[age_class]~dnorm(0,1),
#     b2[sex]~dnorm(0,1),
#     b4[year]~dnorm(0,1),
#     x[area]~dnorm(0,1.5),
#     sigma_a~dexp(1),
#     gq>vector[area]:a<<-a_bar+x*sigma_a #calcolo nuovo vettore delle stime per le intercette
#   ), data=dat_list2.1, control=list(adapt_delta=0.99), iter = 20000,  chains=4,log_lik=TRUE)
# 
# #results
# #traceplot_ulam(m2.1)
# #pairs(m2.1)
# par(mfrow=c(1,1))
# labs2.1<-c("Age_class 1", "Age_class 2", "Age_class 3", "Female", "Male","2017","2018", "Valley1", "Valley2", "Valley3", "Valley4")
# precis(m2.1, depth = 2)
# 
# post2.1<-extract.samples(m2.1)
# post2.1<-data.frame(post2.1)
# post2.1<-post2.1%>%
#   select(c(2:8,14:17)) #selezione delle posterior di interesse
# names(post2.1)<-labs2.1
# post2.1 %>%  
#   pivot_longer(cols=1:11,names_to = "factor", values_to = "logit") %>% 
#   mutate("prevalence"=inv_logit(logit))%>% 
#   ggplot(aes(x = prevalence, y=factor,fill = stat(x))) + geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
#   theme(legend.position="none", text = element_text(size=10))+
#   labs(x="Posterior Seroprevalence estimate of T.gondii in Deer",y="")
# 
# ftable(deer_b$tgondi, deer_b$year)
# 
# diffs2.1<-list(
#   ageClass3_Vs_AgeClass2=inv_logit(post2.1$`Age_class 3`)-inv_logit(post2.1$`Age_class 2`),
#   ageClass3_Vs_AgeClass1=inv_logit(post2.1$`Age_class 3`)-inv_logit(post2.1$`Age_class 1`),
#   ageClass2_Vs_AgeClass1=inv_logit(post2.1$`Age_class 2`)-inv_logit(post2.1$`Age_class 1`),
#   Valley4_Vs_Valley3=inv_logit(post2.1$Valley4)-inv_logit(post2.1$Valley3),
#   Valley4_Vs_Valley2=inv_logit(post2.1$Valley4)-inv_logit(post2.1$Valley2),
#   Valley4_Vs_Valley1=inv_logit(post2.1$Valley4)-inv_logit(post2.1$Valley1),
#   Valley2_Vs_Valley3=inv_logit(post2.1$Valley2)-inv_logit(post2.1$Valley3),
#   Valley2_Vs_Valley1=inv_logit(post2.1$Valley2)-inv_logit(post2.1$Valley1),
#   Valley3_Vs_Valley1=inv_logit(post2.1$Valley3)-inv_logit(post2.1$Valley1),
#   Female_Vs_Male=inv_logit(post2.1$Female)-inv_logit(post2.1$Male),
#   y2017_Vs_y2018=inv_logit(post2.1$`2017`)-inv_logit(post2.1$`2018`))
# par(mfrow=c(3,4))
# dens(diffs2.1$ageClass3_Vs_AgeClass2, main = "AGE_CLASS (3Vs2)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$ageClass3_Vs_AgeClass1, main = "AGE_CLASS (3Vs1)", xlab="Value")  +abline(v=0, lty=3)
# dens(diffs2.1$ageClass2_Vs_AgeClass1, main = "AGE_CLASS (2Vs1)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$Valley4_Vs_Valley3, main = "VALLEY (4Vs3)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$Valley4_Vs_Valley2, main = "VALLEY (4Vs2)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$Valley4_Vs_Valley1, main = "VALLEY (4Vs1)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$Valley2_Vs_Valley3, main = "VALLEY (2Vs3)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$Valley2_Vs_Valley1, main = "VALLEY (2Vs1)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$Valley3_Vs_Valley1, main = "VALLEY (3Vs1)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$Female_Vs_Male, main = "SEX (FemaleVsMale)", xlab="Value")+abline(v=0, lty=3)
# dens(diffs2.1$y2017_Vs_y2018, main = "YEAR (2017Vs2018)", xlab="Value")+abline(v=0, lty=3)
