library(tidyverse)
library(readxl)
library(lemon)
library(ggh4x)
library(patchwork)
library(cowplot)
library(lemon)
library(truncnorm)
library(ggridges)

Kd <- read_excel('Kd_ff.xlsx') 
  
PW <- Kd %>% 
  pivot_longer(cols=c(2:6),
               names_to='metal',
               values_to='Kd') %>% 
  mutate(TRM=case_when(
    metal=='Cd'~0.95,
    metal=='Cu'~680,
    metal=='Ni'~140,
    metal=='Zn'~320,
    metal=='Pb'~35,
  )) %>% 
  mutate(PW=TRM/Kd*1000,
         Replicate=as.factor(Replicate)) %>% 
  filter(metal!='Zn' &metal!='Pb') %>% 
  filter(!(Treatment=='Coarse-size'&Replicate=='2')) %>% 
  mutate(Treatment=factor(Treatment,
                          levels=c('Fine-size',
                                   'Medium-size',
                                   'Coarse-size')))

###summarise different size & different replicate
PW_t <- PW %>% 
  select('Time2',Treatment,Replicate,metal,PW) %>% 
  group_by(Time2,metal) %>% 
  summarise(mean_PW=mean(PW),sd=sd(PW))


ggplot(PW_t, aes(x=Time2, y=mean_PW,color=metal)) +
           geom_point(size=3,show.legend=F) +
           geom_line(linetype="dashed",show.legend=F) +
  geom_errorbar(aes(x=Time2,ymin=mean_PW-sd,ymax=mean_PW+sd,color=metal),inherit.aes=F,
                width=0.1,show.legend=F)+
           theme_bw() +
  ylab(expression("Mean porewater concentration (" * mu * "g L"^-1 * ")"))+
           facet_wrap(~metal,scales='free')


##Monte carlo simulate
set.seed(123) 

generate_positive_rnorm <- function(n, mean, sd) {
  values <- rnorm(n, mean, sd)  
  while (any(values < 0)) {  
    values[values < 0] <- rnorm(sum(values < 0), mean, sd)
  }
  return(values)
}

generate_range_rnorm <- function(n, mean, sd) {
  values <- rnorm(n, mean, sd)  
  while (any(values < mean-sd | values > mean+sd)) {  
    values[values < mean-sd | values > mean+sd] <- rnorm(sum(values < mean-sd | values > mean+sd), mean, sd)
  }
  return(values)
}


PW_simulate <- PW_t %>% 
  group_by(Time2,metal,mean_PW,sd) %>% 
  mutate(x=1) %>% 
  nest() %>% 
  mutate(simulate_pw=map2(.x=mean_PW,.y=sd,~generate_range_rnorm(n=10000,mean=.x,sd=.y))) %>% 
  unnest(cols=simulate_pw) %>% 
  select(-data)

PW_simulate_mean <- PW_simulate %>% 
  group_by(Time2,metal) %>% 
  summarise(mean_PW_simulate=mean(simulate_pw),sd=sd(simulate_pw))

ggplot(PW_simulate_mean, aes(x=Time2, y=mean_PW_simulate,color=metal)) +
  geom_violin(data=PW_simulate,aes(x=Time2,y=simulate_pw,group=Time2),show.legend=F)+
  geom_point(size=3,show.legend=F) +
  geom_line(linetype="dashed",show.legend=F) +
  geom_errorbar(aes(x=Time2,ymin=mean_PW_simulate-sd,ymax=mean_PW_simulate+sd,color=metal),inherit.aes=F,
                width=0.1,show.legend=F)+
  theme_bw() +
  ylab(expression("Mean porewater concentration (" * mu * "g L"^-1 * ")"))+
  facet_wrap(~metal,scales='free')


##folder cal
PW_fold <- PW_simulate %>%
  ungroup() %>% 
  select(1,2,5) %>% 
  pivot_wider(id_cols=metal,names_from=Time2,values_from=simulate_pw) %>% 
  unnest(cols=everything() ) %>% 
  mutate(phase1=`7.01`/`0.02`,
         phase2=`15.01`/`0.02`,
         phase3=`23`/`0.02`,
         phase4=`31.03`/`0.02`) %>% 
  select(1,7:10) %>% 
  pivot_longer(cols=c(2:5),names_to='phase',values_to='fold')

PW_fold_mean <- PW_fold %>% 
  group_by(metal,phase) %>% 
  summarise(mean_fold=mean(fold),sd=sd(fold))

##PW fold fig
ggplot(PW_fold_mean)+
  #geom_violin(data=PW_fold,aes(x=phase,y=fold,group=phase))+
  geom_errorbar(aes(x=phase,ymin=mean_fold-sd,ymax=mean_fold+sd,color=metal),
                width=0.1,show.legend=F)+
  geom_point(aes(x=phase,y=mean_fold,color=metal),size=3,show.legend=F)+
  facet_wrap(~metal,scales='free')+
  theme_classic()



##trunc distribution
n_total <- 30000


metals <- rep(c("Cd", "Cu", "Ni"), each=n_total / 3)


phases <- c("phase1", "phase2", "phase3", "phase4")
probs <- c(0.433, 0.499, 0.064, 0.004)




phase_values <- sample(phases, size=n_total, replace=TRUE, prob=probs)


dfs <- data.frame(metal=metals, phase=phase_values) %>% 
  left_join(PW_t %>% mutate(phase=case_when(
    Time2==0.02~'phase0',
    Time2==7.01~'phase1',
    abs(Time2-15.0)<1e-1~'phase2',
    Time2==23~'phase3',
    abs(Time2-31.0)<1e-1~'phase4'
  )),by=c('metal','phase')) %>% 
  left_join(PW_t %>% mutate(phase=case_when(
    Time2==0.02~'phase0'
  )) %>% filter(Time2==0.02),by=c('metal')) %>% 
  select(1,2,4,5,7,8) %>% 
  mutate(phasePW=rtruncnorm(n=1,a=mean_PW.x-sd.x,b=mean_PW.x+sd.x,mean=mean_PW.x,sd=sd.x),
         bgPW=rtruncnorm(n=1,a=mean_PW.y-sd.y,b=mean_PW.y+sd.y,mean=mean_PW.y,sd=sd.y),
         fold=phasePW/bgPW)



p1 <- ggplot(dfs, aes(x=fold,y=reorder(metal,fold),fill=metal)) +
  geom_density_ridges(position=position_nudge(y=0.1),
                      scale=0.3,rel_min_height=0.01,alpha=0.3,
                      aes(x=fold,y=reorder(metal,fold),color=metal,fill=metal),show.legend=F)+
  geom_boxplot(width=0.1, position=position_nudge(x=0.2),
               outlier.shape=NA,
               alpha=0.3,show.legend=F)+
  scale_color_manual(values=c("#6592B0","#DDA569","#2A4866") )+
  scale_fill_manual(values=c("#6592B0","#DDA569","#2A4866") )+
  scale_x_continuous(limits=c(0,14))+
  theme_classic()+
  labs(x='Fold uncertainty in PW Conc.',
       y='　')

p1

prop.table(table(df$Phase))


##shear
##probabilities : Role of Sediment Resuspension in the Remobilization of Particulate-Phase Metals from Coastal Sediments
probabilities <- c(5655, 707, 538, 110, 50, 18, 3, 10) / 7091
intervals <- list(
  c(0, 0.11),
  c(0.11, 0.14),
  c(0.14, 0.18),
  c(0.18, 0.23),
  c(0.23, 0.28),
  c(0.28, 0.33),
  c(0.33, 0.35),
  c(0.35, 0.417)
)


category <- sample(1:8, size=10000, replace=TRUE, prob=probabilities)


values <- numeric(10000)
for (i in 1:8) {
  idx <- which(category == i)
  n <- length(idx)
  if (n > 0) {
    values[idx] <- runif(n, min=intervals[[i]][1], max=intervals[[i]][2])
  }
}

breaks <- c(0, 0.11, 0.14, 0.18, 0.23, 0.28, 0.33, 0.35, 0.417)

values_cat <- cut(values, breaks=breaks, include.lowest=FALSE, right=TRUE,
                  labels=c("(0, 0.11]", "(0.11, 0.14]", "(0.14, 0.18]", "(0.18, 0.23]", 
                             "(0.23, 0.28]", "(0.28, 0.33]", "(0.33, 0.35]", "(0.35, 0.417]"))


simulate_stress <- data.frame(stress=values,cat=values_cat) %>% 
  mutate(index=row_number())

##shear prob
breaks_lab <- c(0, 0.06,0.16,0.28,0.5)
simulate_stress$phase <- cut(simulate_stress$stress, breaks=breaks_lab, include.lowest=TRUE, right=FALSE,
                             labels=c("phase1","phase2","phase3","phase4"))
p_text <- simulate_stress %>% 
  group_by(phase) %>% 
  summarise(n=n(),p=n/10000) %>% 
  mutate(x=c(0.03,0.11,0.22,0.37),
         y=c(7.5,7.5,7.5,7.5)) %>% 
  mutate(p=c('43%','50%','6.4%','0.6%'))


p2 <- ggplot(simulate_stress)+
  geom_errorbar(aes(x=index,ymin=0,ymax=stress,color=cat),width=0,show.legend=F)+
  scale_color_manual(values=c('gray80','gray70','gray60','gray50',
                                'gray40','gray30','gray20','gray10'))+
  theme_bw()+
  labs(x='Shear stress event index',
       y='Shear stress (Pa)')+
  theme_classic()
p2
##probability pic
p3 <- ggplot(simulate_stress)+
  geom_density(aes(x=stress))+
  geom_area(stat="density", aes(x=stress), 
            fill="#F7F7FF") +
  geom_vline(xintercept=c(0.06, 0.16, 0.28), linetype="dashed", color="#827777")+
  #stat_density(aes(x=stress))+
  theme_classic()+
  scale_x_continuous(expand=c(0,0),limits=c(0,0.45),
                     labels=c(0,0.1,0.2,0.3,0.4))+
  scale_y_continuous(expand=c(0,0),limits=c(0,8.5))+
  geom_text(data=data.frame(x=c(0.03,0.11,0.22,0.37),
                            y=c(8.2,8.2,8.2,8.2),
                            label=c('bold(I)','bold(II)','bold(III)','bold(IV)')),
            aes(x=x,y=y,label=label),
            parse=T,size=5)+
  geom_text(data=p_text,
            aes(x=x,y=y,label=p),size=3)+
  labs(x='Shear stress (Pa)',
       y='Probability density')
p3


dfs_mean <- dfs %>% 
  group_by(metal,phase.x) %>% 
  summarise(mean_fold=mean(fold),sd=sd(fold))



##PW folder
n_total <- 10000
metals <- rep(c("Cd", "Cu", "Ni"), each=4*n_total)
phases <- c("phase1", "phase2", "phase3", "phase4")


dfs_2 <- data.frame(metal=metals, phase=phases) %>%  
  left_join(PW_t %>% mutate(phase=case_when(
    Time2==0.02~'phase0',
    Time2==7.01~'phase1',
    abs(Time2-15.0)<1e-1~'phase2',
    Time2==23~'phase3',
    abs(Time2-31.0)<1e-1~'phase4'
  )),by=c('metal','phase')) %>% 
  left_join(PW_t %>% mutate(phase=case_when(
    Time2==0.02~'phase0'
  )) %>% filter(Time2==0.02),by=c('metal')) %>% 
  select(1,2,4,5,7,8) %>% 
  mutate(phasePW=rtruncnorm(n=1,a=mean_PW.x-sd.x,b=mean_PW.x+sd.x,mean=mean_PW.x,sd=sd.x),
         bgPW=rtruncnorm(n=1,a=mean_PW.y-sd.y,b=mean_PW.y+sd.y,mean=mean_PW.y,sd=sd.y),
         fold=phasePW/bgPW) %>% 
  mutate(metal=factor(metal,levels=c('Cu','Ni','Cd')))




dfs_mean_2 <- dfs_2 %>% 
  group_by(metal,phase.x) %>% 
  summarise(mean_fold=mean(fold),sd=sd(fold))


p4 <- ggplot()+
  geom_boxplot(data=dfs_2,aes(x=phase.x,y=fold,color=metal),width=0.5,
               position=position_dodge(width=1),outliers=F,show.legend=F)+
  geom_vline(aes(xintercept=c(1.5,2.5,3.5)),linetype='dashed')+
  geom_text(data=data.frame(x=c(1,2,3,4),
                            y=c(13,13,13,13),
                            text=c('bold("Phase I")','bold("Phase II")','bold("Phase III")','bold("Phase IV")')),
            aes(x=x,y=y,label=text),parse=T)+
  scale_color_manual(values=c('#DBA060','#1A3B5B','#6190AE'))+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  labs(x='Phase')


plot_grid(p4,p2,p3,p1,align='hv',
          labels=c('a','b','c','d'))

ggsave('Figure HHW.png',width = 851/90,height = 528/90,dpi=900)
