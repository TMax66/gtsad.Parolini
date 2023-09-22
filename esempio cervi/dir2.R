library(readxl)
#library(bmixture)
library("tidyverse")
library("hrbrthemes")
library(mc2d)


slv <- read_excel("C:/Users/mcrotta4/Desktop/collab/lavoro selvatici/Book2rev.xlsx")
slv<-as.data.frame(slv)
slv$Status<-as.factor(ifelse(slv$hev=="NEG" & slv$tgondii=="POS", "PTG",
                             ifelse(slv$hev=="POS" & slv$tgondii=="NEG", "PHEV", 
                                    ifelse(slv$hev=="POS" & slv$tgondii=="POS", "PTGHEV", "PNEG"))))
ftable(slv$Status)
slv$specie<-ifelse(slv$specie=="Wild boar", "Wild boars", slv$specie)
slv$specie<-ifelse(slv$specie=="Roe", "Roe deer", slv$specie)
ftable(slv$specie, slv$area)
slv$specie<-as.factor(slv$specie)
slv$year<-as.factor(slv$year)
slv$hev<-as.factor(slv$hev)
slv$tgondii<-as.factor(slv$tgondii)
slv$area<-as.factor(ifelse(slv$area=="Prealpi Bergamasche"|slv$area=="prealpi bergamasche","pre-Alpine","Alpine"))
slv$class_age<-NULL
slv$sex<-NULL
colnames(slv)<-c("Species","hev","tgondii","Year","Area","Status")

slv<-slv[complete.cases(slv), ]
ftable(slv$Species, slv$Area)

df <- slv %>%
  filter(Area %in% c("pre-Alpine", "Alpine")) %>%
  group_by(Species, Area, Status) %>%
  summarise(counts = n())

df
ftable(slv$Species,slv$hev,slv$Area)
ftable(slv$Species,slv$tgondii,slv$Area)
ftable(slv$Species)
ggplot(df, aes(x = Species, y = counts, fill=Status, label=counts))+
  theme_bw()+
  scale_fill_manual(values=c("grey","goldenrod4","navyblue","darkred"))+
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) + facet_wrap(vars(Area))+
  geom_text(position = position_dodge2(width = 0.9, preserve ="single"), angle = 0, vjust=-0.3, hjust=0.7,
            color="black", size=5)+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=16),
        strip.text.x = element_text(size = 16, face="bold"),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.title.x  = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        panel.spacing = unit(1.5, "lines"),
        legend.position = "bottom") 

colnames(df)[4]<-"Counts"
df %>% 
  ggplot(aes(x=Status,y=Counts, label=Counts))+ 
  geom_segment( aes(x=Status, xend=Status, y=0, yend=Counts), color="black")+
  geom_point(fill="pink",shape=21, size=12)+
  geom_text(color="black", size=5)+
  coord_flip()+ facet_grid(Species~Area)+
  theme(strip.text.x = element_text(size = 14, face = "bold"))+
  theme(strip.text.y = element_text(size = 14, face = "bold"))+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(panel.spacing.x = unit(1,"lines"))+
  theme(panel.spacing.y = unit(1,"lines"))+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_text(size = 14))

theme_bw()



ftable(slv$Species, slv$Area, slv$tgondii)
mouflonTG_A<-data.frame("P"=rbeta(100000, shape1 = 3+1, shape2 = 18-3+1), "Area"="Alps", "specie"="Mouflon")
mouflonTG_P<-data.frame("P"=rbeta(100000, shape1 = 9+1, shape2 = 32-9+1), "Area"="Pre-Alps", "specie"="Mouflon")
reddeerTG_A<-data.frame("P"=rbeta(100000, shape1 = 9+1, shape2 = 59-9+1), "Area"="Alps", "specie"="Red deer")
reddeerTG_P<-data.frame("P"=rbeta(100000, shape1 = 10+1, shape2 = 37-10+1), "Area"="Pre-Alps", "specie"="Red deer")
roedeerTG_A<-data.frame("P"=rbeta(100000, shape1 = 10+1, shape2 = 65-10+1), "Area"="Alps", "specie"="Roe deer")
roedeerTG_P<-data.frame("P"=rbeta(100000, shape1 = 120+1, shape2 = 258-120+1), "Area"="Pre-Alps", "specie"="Roe deer")
wildboarTG_P<-data.frame("P"=rbeta(100000, shape1 = 164+1, shape2 = 331-164+1), "Area"="Pre-Alps", "specie"="Wild boar")
chamoisTG_A<-data.frame("P"=rbeta(100000, shape1 = 4+1, shape2 = 100-4+1), "Area"="Alps", "specie"="Chamois")

df2<-rbind(mouflonTG_A, mouflonTG_P, reddeerTG_A, reddeerTG_P, roedeerTG_A, roedeerTG_P, wildboarTG_P, chamoisTG_A)
df2$Pathogen<-"TGondii"
contrast1mouflons_tg<-data.frame("contrasts"=mouflonTG_P$P-mouflonTG_A$P, Species="Mouflons", path="TG")
contrast1reddeer_tg<-data.frame("contrasts"=reddeerTG_P$P-reddeerTG_A$P, Species="Red deer", path="TG")
contrast1roedeer_tg<-data.frame("contrasts"=roedeerTG_P$P-roedeerTG_A$P, Species="Roe deer", path="TG")


#hev
ftable(slv$Species, slv$Area, slv$hev)
mouflonHEV_A<-data.frame("P"=rbeta(100000, shape1 = 1+1, shape2 = 18-1+1), "Area"="Alps", "specie"="Mouflon")
mouflonHEV_P<-data.frame("P"=rbeta(100000, shape1 = 0+1, shape2 = 32-0+1), "Area"="Pre-Alps", "specie"="Mouflon")
reddeerHEV_A<-data.frame("P"=rbeta(100000, shape1 = 0+1, shape2 = 59-0+1), "Area"="Alps", "specie"="Red deer")
reddeerHEV_P<-data.frame("P"=rbeta(100000, shape1 = 0+1, shape2 = 37-0+1), "Area"="Pre-Alps", "specie"="Red deer")
roedeerHEV_A<-data.frame("P"=rbeta(100000, shape1 = 0+1, shape2 = 65-0+1), "Area"="Alps", "specie"="Roe deer")
roedeerHEV_P<-data.frame("P"=rbeta(100000, shape1 = 1+1, shape2 = 163-1+1), "Area"="Pre-Alps", "specie"="Roe deer")
wildboarHEV_P<-data.frame("P"=rbeta(100000, shape1 = 50+1, shape2 = 326-50+1), "Area"="Pre-Alps", "specie"="Wild boar")
chamoisHEV_A<-data.frame("P"=rbeta(100000, shape1 = 5+1, shape2 = 97-5+1), "Area"="Alps", "specie"="Chamois")

df3<-rbind(mouflonHEV_A, mouflonHEV_P, reddeerHEV_A, reddeerHEV_P, roedeerHEV_A, roedeerHEV_P, wildboarHEV_P, chamoisHEV_A)
df3$Pathogen<-"HEV"

contrast1mouflons_hev<-data.frame("contrasts"=mouflonHEV_P$P-mouflonHEV_A$P, Species="Mouflons", path="HEV")
contrast1reddeer_hev<-data.frame("contrasts"=reddeerHEV_P$P-reddeerHEV_A$P, Species="Red deer", path="HEV")
contrast1roedeer_hev<-data.frame("contrasts"=roedeerHEV_P$P-roedeerHEV_A$P, Species="Roe deer", path="HEV")
contrasts1<-rbind(contrast1mouflons_tg,contrast1mouflons_hev,contrast1reddeer_tg,contrast1reddeer_hev,contrast1roedeer_tg,contrast1roedeer_hev)

ggplot(contrasts1, aes(x = contrasts, fill=Species))+
  geom_density(alpha=0.2, size=0.7)+
  #geom_histogram(position='identity', alpha = 0.5, binwidth = 0.01, color="black", fill="black")+
  scale_x_continuous(name="Contrast estimate")+
  theme_bw()+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=16),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        strip.text.y = element_text(size = 16, face="bold"),
        strip.text.x = element_text(size = 16, face="bold"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))+
  scale_fill_manual(values=c("red","blue","black"))+
  facet_grid(path ~.)+
  theme(legend.title = element_blank())



ggplot(df2, aes(x = P, fill=Area))+
  geom_density(alpha=0.4, size=0.7)+
  theme_bw()+
  scale_fill_manual(values = c("navy", "darkgreen"))+
  scale_color_manual(values = c("navy", "darkgreen"))+
  scale_x_continuous(breaks = seq(0,1,0.1), name = "Seroprevalence") +
  facet_wrap(vars(specie))+
  ggtitle(expression(italic("T.gondii")))+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=16),
        strip.text.x = element_text(size = 16, face="bold"),
        strip.text.y = NULL,
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        legend.position = "bottom")

ggplot(df3, aes(x = P, fill=Area))+
  geom_density(alpha=0.4, size=0.7)+
  theme_bw()+
  scale_fill_manual(values = c("navy", "darkgreen"))+
  scale_color_manual(values = c("navy", "darkgreen"))+
  scale_x_continuous(breaks = seq(0,1,0.1), name = "Seroprevalence") +
  facet_wrap(vars(specie))+
  ggtitle(expression(italic("HEV")))+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=16),
        strip.text.x = element_text(size = 16, face="bold"),
        strip.text.y = NULL,
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        legend.position = "bottom")



#contrasts dirichlet area
#red deer Pre-Alps
trials<-100000
alphapa<-c(28,11,1,1)
xpa <- rdirichlet(trials, alphapa)
alphav<-c(51,10,1,1)
xv <- rdirichlet(trials, alphav)
delta.dirichletPNEG<-  data.frame("Contrast"= (xpa[,1] - xv[,1]), "Status"="NEG")
delta.dirichletPTG<-   data.frame("Contrast"=(xpa[,2] - xv[,2]), "Status"="PTG")
delta.dirichletPHEV<-  data.frame("Contrast"=(xpa[,3] - xv[,3]), "Status"="PHEV")
delta.dirichletPTGHEV<-data.frame("Contrast"=(xpa[,4] - xv[,4]), "Status"="PTGHEV")



contrastsdeer<-rbind(delta.dirichletPNEG,delta.dirichletPTG,delta.dirichletPHEV,delta.dirichletPTGHEV)
str(contrastsdeer)
contrastsdeer$specie<-"Red deer"


#roe
alphaparoep<-c(79,85,1,2)
xparoe <- rdirichlet(trials, alphaparoep)
alphavroe<-c(56,11,1,1)
xvroe <- rdirichlet(trials, alphavroe)
delta.dirichletPNEGroe<-  data.frame("Contrast"= (xparoe[,1] - xvroe[,1]), "Status"="NEG")
delta.dirichletPTGroe<-   data.frame("Contrast"=(xparoe[,2] - xvroe[,2]), "Status"="PTG")
delta.dirichletPHEVroe<-  data.frame("Contrast"=(xparoe[,3] - xvroe[,3]), "Status"="PHEV")
delta.dirichletPTGHEVroe<-data.frame("Contrast"=(xparoe[,4] - xvroe[,4]), "Status"="PTGHEV")
contrastsroe<-rbind(delta.dirichletPNEGroe, delta.dirichletPTGroe,delta.dirichletPHEVroe, delta.dirichletPTGHEVroe)
contrastsroe$specie<-"Roe"

#mouflons
alphamu<-c(24,10,1,1)
xmu <- rdirichlet(trials, alphamu)
alphavmu<-c(15,4,2,1)
xvmu <- rdirichlet(trials, alphavmu)
delta.dirichletPNEGmu<-  data.frame("Contrast"= (xmu[,1] - xvmu[,1]), "Status"="NEG")
delta.dirichletPTGmu<-   data.frame("Contrast"=(xmu[,2] - xvmu[,2]), "Status"="PTG")
delta.dirichletPHEVmu<-  data.frame("Contrast"=(xmu[,3] - xvmu[,3]), "Status"="PHEV")
delta.dirichletPTGHEVmu<-data.frame("Contrast"=(xmu[,4] - xvmu[,4]), "Status"="PTGHEV")

contrastsmu<-rbind(delta.dirichletPNEGmu, delta.dirichletPTGmu,delta.dirichletPHEVmu, delta.dirichletPTGHEVmu)
contrastsmu$specie<-"Mouflons"


mg<-rbind(contrastsdeer,contrastsroe, contrastsmu)
colnames(mg)[3]<-"Species"
ggplot(mg, aes(x = Contrast, fill=Species))+
  geom_density(alpha=0.2, size=0.7)+
  #geom_histogram(position='identity', alpha = 0.5, binwidth = 0.01, color="black", fill="black")+
  scale_x_continuous(name="Contrasts")+
  theme_bw()+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=16),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        strip.text.y = element_text(size = 16, face="bold"),
        strip.text.x = element_text(size = 16, face="bold"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))+
  scale_fill_manual(values=c("red","blue","black"))+
  facet_grid(Status ~.)+
  theme(legend.title = element_blank())



