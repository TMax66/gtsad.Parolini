librerie()
library(DataExplorer)
library(rstanarm)
library(odbc)
library(DBI)
library(openxlsx)
library(readxl)
library(crosstable)
library(flextable)
library(rstanarm)
library(ggeffects)
library(sjPlot)
library(bayesplot)
library(bayestestR)
library(knitr)
library(kableExtra)
library(gt)
library(binom)
library(ggridges)
library(rstan)
library(rethinking)


library(here)
library(tidyverse)
library(readxl)

# con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it",
#                       Database = "IZSLER", Port = 1433)
# 
# source(here("R", "sql.R"))

# importo i dati dalla cartella 

dt <- read_excel(here("data", "raw", "dati.xlsx"))# da query bobj

 
siero <- dt %>% 
  mutate(ID = paste0(anno, nconf, ncamp)) %>% 
  filter(prova == "Brucella abortus/melitensis/suis: anticorpi", 
         !is.na(esito) ) %>% 
  mutate(sieroP = as.integer(factor(esito))-1)





cinghiale <- siero %>% 
  filter(specie == "CINGHIALE")


cinghiale <- cinghiale %>% 
  filter(pr %in% c("BS", "CR"),
         !anno %in% c(2017, 2022)) %>% 
  mutate(age_class = case_when(age == "Cl 0: 0-12 mesi \\ 0-1 anni"~"Juveniles", 
                               age == "Cl 1: 13-24 mesi \\ 1-2 anni"~"Yearlings", 
                               age == "Cl 2: > 25 mesi \\ > 2 anni"~"Adults", 
                               is.na(age)~"Unknows")) %>% 
  select(sieroP, pr, age_class, anno) 
  



dati_cingh<-cinghiale[complete.cases(cinghiale),]

data_list<-list(
  brucellosi = dati_cingh$sieroP,
  year = as.integer(as.numeric(as.factor(dati_cingh$anno))),
  province = as.integer(as.numeric(as.factor(dati_cingh$pr))),
  age = as.integer(as.numeric(as.factor(dati_cingh$age_class)))
)

# m0<-ulam(
#   alist(
#     brucellosi~dbinom(1, p),
#     logit(p)<-a[year]+ b[year]*province,
#     a[year]~dnorm(0,1.5),
#     b[year]~dnorm(0,1)), 
#   data=data_list,control=list(adapt_delta=0.99),
#   iter = 100, chains=2,log_lik=TRUE)
# 
# 
# 
# m1<-ulam(
#   alist(
#     brucellosi~dbinom(1, p),
#     logit(p)<-a[year]+ b[year]*province + bA[age]*age,
#     a[year]~dnorm(0,1.5),
#     b[year]~dnorm(0,1), 
#     bA[age]~dnorm(0,1)),
#   data=data_list,control=list(adapt_delta=0.99),
#   iter = 100, chains=2,log_lik=TRUE)
# 
# 
# 
# m2<-ulam(
#   alist(
#     brucellosi~dbinom(1, p),
#     logit(p)<-a[year]+ b[year]*province + bA[year]*age,
#     a[year]~dnorm(0,1.5),
#     b[year]~dnorm(0,1), 
#     bA[year]~dnorm(0,1)),
#   data=data_list,control=list(adapt_delta=0.99),
#   iter = 100, chains=2,log_lik=TRUE)
# 
# 
# 
# stancode(m2)




library(rstan)

options(mc.cores = parallel::detectCores())


fit1 <- stan(here("R", "stanmodel.stan"), iter = 200, chains = 2, data = data_list)

extr <- extract(fit1)

a <- data.frame(extr[["a"]])
b <- data.frame(extr[["b"]])
bA <- data.frame(extr[["bA"]])
 

df <- a %>% 
  pivot_longer(cols = 1:4,names_to = "anno", values_to = "year") %>% 
  
 bind_cols(
    b %>% pivot_longer(cols = 1:4,names_to = "prov", values_to = "province")   
  ) %>% 
 
  bind_cols(
    bA %>% pivot_longer(cols = 1:4,names_to = "age", values_to = "age_class") ) %>% 
  select(-prov, -age) %>% 
  arrange(anno)




dati <- df %>% group_by(anno) %>% View()
  mutate("BSAd" = year+province+age_class, 
         "BSjuv" = year+province+2*age_class, 
         "BSunknow" = year+province+3*age_class, 
         "BSYerl"= year+province+4*age_class, 
         "CRAd" = year+2*province+age_class, 
         "CRjuv" = year+2*province+2*age_class, 
         "CRunknow" = year+2*province+3*age_class, 
         "CRYerl"= year+2*province+4*age_class, 
         
         anno = case_when(anno == "X1"~"2018", 
                   anno == "X2"~"2019",
                   anno == "X3"~"2020",
                   anno == "X4"~"2021")) 


a<- dati %>% 
  select(anno, BSAd) %>% 
  mutate("province" = "BS", 
         "age_class" = "Adult") %>% 
  rename( "estimate" = BSAd) %>% 
  
  bind_rows(
    
    dati %>% 
      select(anno, BSjuv) %>% 
      mutate("province" = "BS", 
             "age_class" = "Juveniles") %>% 
      rename( "estimate" = BSjuv)
    
  ) %>% 
  
  bind_rows( 
    
    dati %>% 
      select(anno, BSunknow) %>% 
      mutate("province" = "BS", 
             "age_class" = "Unknows") %>% 
      rename( "estimate" = BSunknow)
    
    
    ) %>% 
  
  bind_rows(
    
    dati %>% 
      select(anno, BSYerl) %>% 
      mutate("province" = "BS", 
             "age_class" = "Yearlings") %>% 
      rename( "estimate" = BSYerl)
    
  ) %>% 
  
  bind_rows(
    dati %>% 
      select(anno, CRAd) %>% 
      mutate("province" = "CR", 
             "age_class" = "Adult") %>% 
      rename( "estimate" = CRAd)
    
  ) %>% 
  
  bind_rows(
    dati %>% 
      select(anno, CRjuv) %>% 
      mutate("province" = "CR", 
             "age_class" = "Juveniles") %>% 
      rename( "estimate" = CRjuv)
    
  ) %>% 
  
  bind_rows(
    dati %>% 
      select(anno, CRunknow) %>% 
      mutate("province" = "CR", 
             "age_class" = "Unknows") %>% 
      rename( "estimate" = CRunknow)
    
  ) %>% 

  bind_rows(
    dati %>% 
      select(anno, CRYerl) %>% 
      mutate("province" = "CR", 
             "age_class" = "Yearlings") %>% 
      rename( "estimate" = CRYerl)
    
  )  


a %>% 
  #filter(province == "BS") %>% 
ggplot()+
  aes(x = 100*invlogit(estimate), 
      y = age_class, fill = stat(x))+
  geom_density_ridges_gradient(scale = 4, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Prevalence")+ 
  facet_wrap(anno~province, ncol = 2)



  

a %>%
  group_by(anno, province, age_class) %>%
  summarise("median" = median(invlogit(estimate)*100),
            q5 = quantile(invlogit(estimate)*100, 0.025),
            q90= quantile(invlogit(estimate)*100, 0.97))%>%
  flextable()




post %>% 
    pivot_longer(cols = 1:11, names_to = "Province", values_to = "Prevalence") %>%
    ggplot(aes(x = Prevalence, y=Province,fill = stat(x))) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
    # scale_fill_brewer(palette = 2) +
    labs(x="Posterior Bayesian Estimated Uncertainty Probability of Brucella suis in Wild Boars",y="")





cinghiale %>%

  group_by(anno, age_class,pr ) %>% 
  count() %>% 
  pivot_wider(names_from = "anno", values_from = "n", values_fill = 0) %>% View()
  flextable()



siero %>% filter(specie == "Ci") %>%
crosstable(
           cols = c(pr),
           by = c(esito, anno, specie),
           showNA = "ifany") %>%
  as_flextable()



bsuis<-cinghiale %>%
  group_by(anno, pr, esito) %>%
  count() %>%
  pivot_wider(names_from = "esito", values_from = "n", values_fill = 0) %>%
  arrange(anno, pr) %>%
  mutate(Nr = P+N) %>%
  filter(pr != "PR")



options(digits=2)
Rhpd <- binom.bayes(
  x = bsuis$P, n = bsuis$Nr, type = "central", conf.level = 0.95, tol = 1e-9, prior.shape1 = 1, prior.shape2 = 1)
tb<- cbind(  bsuis[, 1], bsuis[, 2], Rhpd)

z <- tb %>% mutate(mean = 100*mean,
              lower = 100*lower,
              upper = 100*upper) %>%
  select("Year" = anno, "Province" = pr, -method, "B.suis-Positive"=x, "Nr. of sample"=n, "Prevalence"=mean,
         "inf-HPD"=lower, "sup-HPD"=upper)


#%>%
#  gt()
  # kable("latex", booktabs = T,
  #       caption = "Bayesian posterior estimate of Brucella suis, seroprevalence with 95% of High Density Probability Interval") %>%
  # kable_styling()


# zplot <- binom.bayes.densityplot(tb)
# zplot+facet_grid(pr~anno, scales = "free")+xlab("Estimation")









plot(Rhpd)

mod0 <- stan_glm(sieroP~0 + pr, data = cinghiale, binomial(link = "logit"),
                 cores = 8)


post <- as.data.frame(invlogit(as.matrix(mod0)))




post %>%
  mutate(prBSTrue= prBS*0.995+ (1-prBS)*(1-0.996),
         prCRTrue= prCR*0.995+ (1-prCR)*(1-0.996)) %>%
  ggplot()+
  aes(x = prBS)+
  geom_density()+
  geom_density(aes(x = prBSTrue), color = "red")+
  geom_density(aes(x = prCR))+
  geom_density(aes(x = prCRTrue), color = "red")


mod_inter <- stan_glm(sieroP~0 + pr*anno, data = cinghiale, binomial(link = "logit"),
                              cores = 8, chains = 2, iter = 200 )

rstan::get_stanmodel(mod_inter$stanfit)

post_inter <- as.data.frame(invlogit(as.matrix(mod_inter)))

post %>%
  pivot_longer(cols = 1:11, names_to = "Province", values_to = "Prevalence") %>%
  ggplot(aes(x = Prevalence, y=Province,fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
  # scale_fill_brewer(palette = 2) +
  labs(x="Posterior Bayesian Estimated Uncertainty Probability of Brucella suis in Wild Boars",y="")



posterior <- as.matrix(mod0)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")


ggplot()+
  aes(posterior,
      x = )+
  geom_density()






saveRDS(mod0,file= "mod0.RDS")



# prev <- stan_glm(sieroP~0+pr*anno, data = cinghiale, binomial(link = "logit"),
#                  cores = 8)
# saveRDS(prev, "prevalenze.RDS")



posterior <- as.matrix(prev)

post <- as.data.frame(invlogit(as.matrix(prev)))

post <- post %>% select(17:36)



post %>%
  pivot_longer(cols = 1:20, names_to = "Province_Year", values_to = "Prevalence") %>%
  ggplot(aes(x = prevalence, y=factor,fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
  # scale_fill_brewer(palette = 2) +
  labs(x="Posterior Bayesian Estimated Uncertainty Probability of T.gondii in Wild Boars",y="")



### beta density approach

p <- rbeta(1000000, shape1 = 0+1, shape2 = 2-0+1)

ggplot()+
  aes(p)+
  geom_density()



#### rethinking approach

cinghiale <- cinghiale %>%
  filter(pr %in% c("BS", "CR")) %>%
  mutate(age_class = case_when(age == "Cl 0: 0-12 mesi \\ 0-1 anni"~"Juveniles",
                               age == "Cl 1: 13-24 mesi \\ 1-2 anni"~"Yearlings",
                               age == "Cl 2: > 25 mesi \\ > 2 anni"~"Adults")) %>%
  select(sieroP, pr, age_class, anno)



cinghiale<-cinghiale[complete.cases(cinghiale),]

data_list<-list(
  brucellosi = cinghiale$sieroP,
  year = as.integer(as.numeric(cinghiale$anno)),
  province = as.integer(as.numeric(as.factor(cinghiale$pr)))
)

m0<-ulam(
  alist(
    brucellosi~dbinom(1, p),
    logit(p)<-a[year]+ b[year]*province + b[year]*prelievo,
    a[year]~dnorm(0,1.5),
    b[year]~dnorm(0,1)),
  data=data_list,control=list(adapt_delta=0.99),
  iter = 200, chains=2,log_lik=TRUE)


stancode(m0)

remove.packages("rethinking")
library(rstan)

options(mc.cores = parallel::detectCores())


fit1 <- stan("stanmodel.stan", iter = 200, chains = 4, data = data_list)





pa <- extract(fit, "a")[[1]]
pt <- extract(fit1, "a")[[1]]

pint<-extract

dt <- data.frame("AP" = pa, "TP" = pt)

dt %>% ggplot()+
  geom_density(aes(x = invlogit(AP)), col= "blue")+
  geom_density(aes(x = invlogit(TP)), col = "red")



p = 0.05537226

p*0.8+(1-p)*(1-0.90)




x <- cinghiale %>%
  filter(anno == 2017, pr== "CR")
