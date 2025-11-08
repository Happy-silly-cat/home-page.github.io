library(tidyverse)
library(readxl)
library(patchwork)
library(ggh4x)
library(lemon)
library(cowplot)
library(deSolve)
library(ggpp)

death <- read_excel('S_new_a&b.xlsx')%>% 
  rename(`time_d`=`...1`) %>% 
  pivot_longer(cols = c(2:4),values_to = 'rate') %>% 
  mutate(group=case_when(
    name=="Module1.S_0" ~ '0',
    name=="Module1.S_400" ~ '400',
    name=="Module1.S_800" ~ '800',
  ))%>% mutate(group=case_when(
    group==0~'Control',
    group==400~'Low-Cu treatment',
    group==800~'High-Cu treatment'
  ))%>% 
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment')))


DGT_mean <- read.csv('DGT_mean.csv')
  


set.seed(123) 


n <- 1000
a_vals   <- rnorm(n, mean = 0.2342,  sd = 0.0044)
b_vals   <- rnorm(n, mean = 0.6813,  sd = 0.0040)
CIT_vals <- rnorm(n, mean = 38.34, sd = 1.23)
kk_vals  <- rnorm(n, mean = 7.891,  sd = 0.388)
ke_vals <- rnorm(n,mean=0.0527,sd=0.0044)


DGT_t0 <- approxfun( x = DGT_mean[,1],
                     y = DGT_mean[,2],
                     rule = 2)
DGT_t400 <- approxfun( x = DGT_mean[,1],
                       y = DGT_mean[,3],
                       rule = 2)

DGT_t800 <- approxfun( x = DGT_mean[,1],
                       y = DGT_mean[,4],
                       rule = 2)

TK <- function (t,y, parameters) {
  a <- parameters[1]
  b <- parameters[2]
  ke <- parameters[3]
  CIT <- parameters[4]
  kk <- parameters[5]
  choice <- parameters[6]
  Cint_1 <- y[1]
  if(choice==1){
    dCint_1 <-  a*DGT_t0(t)^b - (ke) * Cint_1
    dHt <- kk/1000*max(Cint_1-CIT,0)
    list(c(dCint_1,dHt))
  }
  else if(choice==2){
    dCint_1 <-  a*DGT_t400(t)^b - (ke) * Cint_1
    dHt <- kk/1000*max(Cint_1-CIT,0)
    list(c(dCint_1,dHt))
  }
  else{
    dCint_1 <-  a*DGT_t800(t)^b - (ke) * Cint_1
    dHt <- kk/1000*max(Cint_1-CIT,0)
    list(c(dCint_1,dHt))
  }
}
results_list <- c()


for(j in 1:3){
  for(i in 1:n){
    times=seq(0,10,by=0.1)
    params <- c(a = a_vals[i],
                b = b_vals[i],
                ke= ke_vals[i],
                CIT = CIT_vals[i],
                kk = kk_vals[i],
                choice=j)
    temp <- ode(func = TK, 
                times = times,
                y = c(Cint_1 = 6.64456,Ht=0), 
                parms = params)
    temp[,3] <- exp(-temp[,3])
    df <- as.data.frame(temp) %>% mutate(batch=i) %>% 
      mutate(group=case_when(
        j==1~'Control',
        j==2~'Low-Cu treatment',
        j==3~'High-Cu treatment'
      ))
    #print(df)
    results_list <- rbind(df,results_list)
  }
}





sed_death <- read.csv('sed_death.csv') %>% 
  filter(group!=1200)

sed_mean_death <- read.csv('sed_mean_death.csv') %>% 
  select(3:6) %>% 
  filter(group!=1200) %>% 
  rbind(data.frame(Time_d=unique(.$Time_d),
                   group='0',
                   mean_rate=0,
                   sd=0)) %>% 
  rbind(data.frame(Time_d=unique(.$Time_d),
                   group='400',
                   mean_rate=0,
                   sd=0)) %>% 
  mutate(group=factor(group,levels=c('0','400','800'))) %>% 
  arrange(Time_d,group)


results_list_ribbon <- results_list %>% 
  filter(group=='High-Cu treatment') %>% 
  group_by(time) %>% 
  summarise(min_Ht=min(Ht),max_Ht=max(Ht))

pd_1 <- ggplot()+
  geom_errorbar(data=sed_mean_death %>% filter(group==800),
                aes(x=Time_d,ymin=(1-mean_rate)-sd,ymax=(1-mean_rate)+sd,),show.legend = F,
                color='black',
                width=0)+
  geom_point(data=sed_mean_death %>% filter(group==800),
             aes(x=Time_d,y=(1-mean_rate),fill=group),shape=21,size=3,show.legend = F,
             color='black')+
  geom_line(data=results_list %>% filter(group=='High-Cu treatment'),
            aes(x=time,y=Ht,group=batch),alpha=0.01,color='grey50')+
  geom_line(data=death %>% filter(group=='High-Cu treatment'),
            aes(x=time_d,y=rate),color='#CF4A3F')+
  
  theme_classic()+
  scale_fill_manual(values = c('#EB5432'))+
  scale_y_continuous(limits = c(0,1.1),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c('0','25','50','75','100'))+
  scale_x_continuous(breaks = c(0,2,4,6,8,10))+
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.tag.position = c(0.09,0.92))+
  labs(x='',
       y='Survival (%)')+
  ggtitle(label = "Sed-High-Cu Toxicity")+
  annotate(geom='text',x=4.8,y=0.495,label='}',size=2,color='gray50')+
  annotate(geom='text',x=5.3,y=0.495,label='–10%',size=2,color='gray50')
pd_1





bind_death <- sed_mean_death %>%filter(group==800) %>%  left_join(death %>% filter(group=='High-Cu treatment'),
                                                                  by=c('Time_d'='time_d')) %>% 
  mutate(mean_rate=1-mean_rate) %>% 
  mutate(percent=rate-mean_rate)

obj_fun <- function(b, x, y, k) {
  sum((y - (k*x + b))^2)
}

res <- optim(par = 0, fn = obj_fun, x = bind_death$rate, y =bind_death$mean_rate , k = 1)
res$par 


pd_1_s <- ggplot(bind_death)+
  geom_point(aes(x=rate,y=mean_rate),shape=21,size=1.5,show.legend = F,
             color='gray50',fill='gray90')+
  theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(color='gray30',size=6),
        axis.ticks = element_line(color='gray30'),
        axis.text = element_text(color='gray30',size=6),
        plot.background = element_blank())+
  labs(x='Predicted (%)',
       y='Measured (%)')+
  scale_y_continuous(limits = c(0,1.1),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c('0','25','50','75','100'),expand=c(0,0))+
  scale_x_continuous(limits = c(0,1.1),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c('0','25','50','75','100'),expand=c(0,0))+
  geom_line(data=data.frame(x=c(0,1.1),y=c(0,1.1)),aes(x=x,y=y),linetype='solid')+
  geom_line(data=data.frame(x=c(0.1048413,1.1),y=c(0,1.1-0.1048413)),aes(x=x,y=y),
            linetype='dashed',color='black')


pd_1_s

pd_1+inset_element(pd_1_s,left=0.02,bottom=0.02,right=0.5,top=0.5)



death_NSE <- sed_mean_death %>% filter(group==800) %>% mutate(mean_rate=1-mean_rate) %>% 
  left_join(death %>% filter(group=='High-Cu treatment'),by=c('Time_d'='time_d')) %>% 
  mutate(upper=(mean_rate-rate)^2,
         lower=((mean_rate-mean((sed_mean_death %>% filter(group==800)%>% mutate(mean_rate=1-mean_rate))$mean_rate))^2))

NSE <- 1-sum(death_NSE$upper)/sum(death_NSE$lower)
NSE
##NSE=0.7370519



##CIT
CIT_l <- c(25,30,35,40)
results_list_CIT <- c()
for(i in CIT_l){
  times=seq(0,10,by=0.1)
  params <- c(a = 0.2342,
              b = 0.6813,
              ke= 0.0527,
              CIT = i,
              kk = 7.891,
              choice=3)
  temp <- ode(func = TK, 
              times = times,
              y = c(Cint_1 = 6.64456,Ht=0), 
              parms = params)
  temp[,3] <- exp(-temp[,3])
  df <- as.data.frame(temp) %>% mutate(batch=factor(i))
  #print(df)
  results_list_CIT <- rbind(df,results_list_CIT)
}


##ke
ke_l <- c(0.01,0.03,0.05,0.08)
results_list_ke_l <- c()
for(i in ke_l){
  times=seq(0,10,by=0.1)
  params <- c(a = 0.2342,
              b = 0.6813,
              ke= i,
              CIT = 38.34,
              kk = 7.891,
              choice=3)
  temp <- ode(func = TK, 
              times = times,
              y = c(Cint_1 = 6.64456,Ht=0), 
              parms = params)
  temp[,3] <- exp(-temp[,3])
  df <- as.data.frame(temp) %>% mutate(batch=factor(i))
  #print(df)
  results_list_ke_l <- rbind(df,results_list_ke_l)
}


##kk
kk_l <- c(6,10,12,14)
results_list_kk_l <- c()
for(i in kk_l){
  times=seq(0,10,by=0.1)
  params <- c(a = 0.2342,
              b = 0.6813,
              ke= 0.0527,
              CIT = 38.34,
              kk = i,
              choice=3)
  temp <- ode(func = TK, 
              times = times,
              y = c(Cint_1 = 6.64456,Ht=0), 
              parms = params)
  temp[,3] <- exp(-temp[,3])
  df <- as.data.frame(temp) %>% mutate(batch=factor(i))
  #print(df)
  results_list_kk_l <- rbind(df,results_list_kk_l)
}




cal_NSE <- function(data,flag){
  if(flag==0){
    return(sum((data$mean_rate-data$Ht)^2))
  }
  else{
    return(sum((data$mean_rate-data$mean)^2))
  }
}

##cal NSE
##pred:results_list_params
##obs:sed_mean_death
results_list_CIT
sed_mean_death

NSE_CIT <- sed_mean_death %>% 
  filter(group==800) %>% 
  mutate(mean_rate=1-mean_rate) %>% 
  select(1,3) %>% 
  left_join(results_list_CIT,c('Time_d'='time')) %>% 
  select(1,2,4,5) %>% 
  group_by(batch) %>% 
  nest() %>% 
  mutate(mean=map(.x=data,~mean(.x$mean_rate))) %>% 
  unnest() %>% 
  group_by(batch) %>% 
  nest() %>% 
  mutate(upp=as.numeric(map(.x=data,~cal_NSE(data=.x,flag=0))))%>% 
  mutate(low=as.numeric(map(.x=data,~cal_NSE(data=.x,flag=1)))) %>% 
  mutate(NSE=1-upp/low)

##NSE-max
times=seq(0,10,by=0.1)
extra_p=c(0.2342,0.6813,0.0527,7.891,3)
req_cw <- function(CIT,extra_p){
  temp <- ode(func = TK, 
              times = times,
              y = c(Cint_1 = 6.64456,Ht=0), 
              parms = c(a = extra_p[1],b=extra_p[2],
                        ke=extra_p[3],
                        CIT=CIT,kk=extra_p[4],choice=extra_p[5]))
  temp[,3] <- exp(-temp[,3])
  temp <- as.data.frame(temp)
  temp <- sed_mean_death%>% 
    filter(group==800) %>% 
    mutate(mean_rate=1-mean_rate) %>% 
    select(1,3) %>% 
    left_join(temp,c('Time_d'='time'))

  mean_meas <- mean(temp$mean_rate)
  low <- sum((temp$mean_rate-mean_meas)^2)
  upp <- sum((temp$mean_rate-temp$Ht)^2)
  NSE_r=(-upp/low)^2
  print(NSE_r)
  return(NSE_r)
}

Cw_r=optim(par=38,fn=req_cw,method='L-BFGS-B',extra_p=extra_p,lower=25)
Cw_r
1-sqrt(Cw_r$value)
##CIT=28.05333
##NSE=0.9740585


##NSE-ke
results_list_ke_l
sed_mean_death

NSE_ke <- sed_mean_death %>% 
  filter(group==800) %>% 
  mutate(mean_rate=1-mean_rate) %>% 
  select(1,3) %>% 
  left_join(results_list_ke_l,c('Time_d'='time')) %>% 
  select(1,2,4,5) %>% 
  group_by(batch) %>% 
  nest() %>% 
  mutate(mean=map(.x=data,~mean(.x$mean_rate))) %>% 
  unnest() %>% 
  group_by(batch) %>% 
  nest() %>% 
  mutate(upp=as.numeric(map(.x=data,~cal_NSE(data=.x,flag=0))))%>% 
  mutate(low=as.numeric(map(.x=data,~cal_NSE(data=.x,flag=1)))) %>% 
  mutate(NSE=1-upp/low)

##NES-kk
results_list_kk_l
sed_mean_death

NSE_kk <- sed_mean_death %>% 
  filter(group==800) %>% 
  mutate(mean_rate=1-mean_rate) %>% 
  select(1,3) %>% 
  left_join(results_list_kk_l,c('Time_d'='time')) %>% 
  select(1,2,4,5) %>% 
  group_by(batch) %>% 
  nest() %>% 
  mutate(mean=map(.x=data,~mean(.x$mean_rate))) %>% 
  unnest() %>% 
  group_by(batch) %>% 
  nest() %>% 
  mutate(upp=as.numeric(map(.x=data,~cal_NSE(data=.x,flag=0))))%>% 
  mutate(low=as.numeric(map(.x=data,~cal_NSE(data=.x,flag=1)))) %>% 
  mutate(NSE=1-upp/low)



results_list_CIT_f <- results_list_CIT %>% left_join(NSE_CIT,by=c('batch')) %>% 
  select(-5:-7) %>% 
  mutate(batch=paste(batch,' ','(',round(NSE,2),')',sep='')) %>% 
  mutate(batch=factor(batch),batch=factor(batch,levels=rev(levels(batch))))


results_list_ke_l_f <- results_list_ke_l %>% left_join(NSE_ke,by=c('batch')) %>% 
  select(-5:-7) %>% 
  mutate(batch=paste(batch,' ','(',format(round(NSE, 2), nsmall = 2),')',sep='')) %>% 
  mutate(batch=factor(batch),batch=factor(batch,levels=rev(levels(batch))))

results_list_kk_l_f <- results_list_kk_l %>% left_join(NSE_kk,by=c('batch')) %>% 
  select(-5:-7) %>% 
  mutate(batch=paste(batch,' ','(',round(NSE,2),')',sep='')) %>% 
  mutate(batch=factor(batch),batch=factor(batch,levels=c('6 (0.59)',
                                                         '10 (0.87)',
                                                         '12 (0.91)',
                                                         '14 (0.92)')))


df_2 <- data.frame(x1 = 7.8, y1 = 0.85, x2 = 4.3, y2 = 0.65)
label_2 <- data.frame(x=2.8,y=0.6,label='Decreasing~italic(C)[IT]')

pd_2 <- ggplot()+
  geom_point(data=sed_mean_death %>% filter(group==800),
             aes(x=Time_d,y=(1-mean_rate),fill=group),shape=21,size=3,show.legend = F,
             color='black')+
  geom_line(data=results_list_CIT_f ,aes(x=time,y=Ht,group=batch,color=batch), size = 0.5)+

  theme_classic()+
  scale_fill_manual(values = c('grey50'))+
  scale_y_continuous(limits = c(0,1.05),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c('0','25','50','75','100'))+
  scale_x_continuous(breaks = c(0,2,4,6,8,10),limits = c(0,10))+
  scale_color_manual(values = c('#3E666B',
                                '#5BB0BB',
                                '#85C7DC',
                                '#CA8E2E'))+
  geom_segment(
    data = df_2,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
    colour = "black",
    size = 0.5
  )+
  geom_text(data=label_2,aes(x=x,y=y,label=label),parse=T)+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.25,0.25),
        legend.key = element_rect(fill='transparent'),
        legend.background = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.title = element_text(hjust=1,size=8,vjust=-2),
        legend.key.spacing.y = unit(-1,'mm'),
        plot.tag.position = c(0.04,0.92))+
  labs(x='',
       y='',
       color=~italic(C)[IT]~(NSE))+
  ggtitle(~Varying~italic(C)[IT])


pd_2


df_3<- data.frame(x1 = 7.8, y1 = 0.85, x2 = 4.3, y2 = 0.65)
label_3 <- data.frame(x=2.8,y=0.6,label='Decreasing~italic(k)[e]')



pd_3 <- ggplot()+
  geom_point(data=sed_mean_death %>% filter(group==800),
             aes(x=Time_d,y=(1-mean_rate),fill=group),shape=21,size=3,show.legend = F,
             color='black')+
  geom_line(data=results_list_ke_l_f ,aes(x=time,y=Ht,group=batch,color=batch),size = 0.5)+
  theme_classic()+
  scale_fill_manual(values = c('grey50'))+
  scale_y_continuous(limits = c(0,1.05),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c('0','25','50','75','100'))+
  scale_x_continuous(breaks = c(0,2,4,6,8,10),limits = c(0,10))+
  scale_color_manual(values = c('#3E666B',
                                '#5BB0BB',
                                '#85C7DC',
                                '#CA8E2E'))+
  geom_segment(
    data = df_3,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
    colour = "black",
    size = 0.5
  )+
  geom_text(data=label_3,aes(x=x,y=y,label=label),parse=T)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.25,0.25),
        #axis.title.y = element_blank(),
        legend.key = element_rect(fill='transparent'),
        legend.background = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.title = element_text(hjust=1,size=8,vjust=-2),
        legend.key.spacing.y = unit(-1,'mm'),
        plot.tag.position = c(0.09,0.92))+
  labs(x='Time (d)',
       y='Survival (%)',
       color=~italic(k)[e]~(NSE))+
  ggtitle(~Varying~italic(k)[e])


pd_3

df_4<- data.frame(x1 = 7.8, y1 = 0.85, x2 = 4.3, y2 = 0.65)
label_4 <- data.frame(x=2.8,y=0.6,label='Increasing~italic(k)[m]')


pd_4 <- ggplot()+
  geom_point(data=sed_mean_death %>% filter(group==800),
             aes(x=Time_d,y=(1-mean_rate),fill=group),shape=21,size=3,show.legend = F,
             color='black')+
  geom_line(data=results_list_kk_l_f ,aes(x=time,y=Ht,group=batch,color=batch),size = 0.5)+
  theme_classic()+
  scale_fill_manual(values = c('grey50'))+
  scale_y_continuous(limits = c(0,1.05),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c('0','25','50','75','100'))+
  scale_x_continuous(breaks = c(0,2,4,6,8,10),limits = c(0,10))+
  scale_color_manual(values = c('#3E666B',
                                '#5BB0BB',
                                '#85C7DC',
                                '#CA8E2E'))+
  geom_segment(
    data = df_4,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
    colour = "black",
    size = 0.5
  )+
  geom_text(data=label_4,aes(x=x,y=y,label=label),parse=T)+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.25,0.25),
        legend.key = element_rect(fill='transparent'),
        legend.background = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.title = element_text(hjust=1,size=8,vjust=-2),
        legend.key.spacing.y = unit(-1,'mm'),
        plot.tag.position = c(0.04,0.92))+
  labs(x='Time (d)',
       y='',
       color=~italic(k)[m]~(NSE))+
  ggtitle(~Varying~italic(k)[m])


pd_4





p_sens <- (pd_1+inset_element(pd_1_s,left=0.02,bottom=0.02,right=0.5,top=0.5)+pd_2)/(pd_3+pd_4)+
  plot_annotation(tag_levels = list(c('(a)','','(b)','(c)','(d)')))
p_sens
ggsave('p_sens.png',width = 638/90,height = 573/90,dpi=900)
