library(tidyverse)
library(readxl)
library(patchwork)
library(ggh4x)
library(lemon)
library(cowplot)
library(deSolve)
library(ggpp)
library(gghalves)


##background
group_values <- c(0, 400, 800)
bioa_bgc <- read_excel('clams.xlsx',sheet='Background') %>% 
  summarise(mean=mean(T_Cu63_ug_g,na.rm=T),sd=sd(T_Cu63_ug_g,na.rm=T))%>%
  slice(rep(1, length(group_values))) %>%     # 复制三行
  mutate(
    batch = 0,
    time_d = 0,
    group = group_values,
    mean_Cu = mean,
    sd_Cu = sd
  ) %>%
  select(batch, time_d, group, mean_Cu, sd_Cu) %>% mutate(group=case_when(
    group==0~'Control',
    group==400~'Low-Cu treatment',
    group==800~'High-Cu treatment'
  )) %>% 
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment')))


bioa_bgc_all <- read_excel('clams.xlsx',sheet='Background')

##label-background
water_label <- read_excel('clams.xlsx',sheet='water_label') %>% 
  select('batch','Cu63_ug_g') %>% 
  mutate(Cu_b_bgc=Cu63_ug_g/0.6917) %>% 
  group_by(batch) %>% 
  summarise(mean_Cu_b_bgc=mean(Cu_b_bgc,na.rm = T))


##uptake rate
bioa_non_label <- read_excel('clams.xlsx',sheet='sed-label') %>% 
  filter(flags!=0,group!=1200) %>% 
  select(2,3,4,5,6,12,17) %>% 
  mutate(real_time_d=real_time_d-2) %>% 
  mutate(real_time_d=case_when(
    batch==1~2+real_time_d,
    batch==2~4+real_time_d,
    batch==3~6+real_time_d,
    batch==4~8+real_time_d,
    batch==5~10+real_time_d,
  )) %>% 
  left_join(water_label,by=c('batch')) %>% 
  mutate(T_Cu_ug_g=(T_Cu63_ug_g)-mean_Cu_b_bgc) %>% 
  mutate(group=case_when(
    group==0~'Control',
    group==400~'Low-Cu treatment',
    group==800~'High-Cu treatment'
  ))%>% 
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment'))) %>% 
  mutate(T_Cu_ug_g_rate=T_Cu_ug_g/2) %>% 
  mutate(T_Cu_ug_g=T_Cu_ug_g/0.947) 



##average rate
bioa_non_label_mean_rate <- bioa_non_label %>% 
  group_by(batch,group) %>% 
  summarise(time_d=mean(real_time_d),mean_Cu=mean(T_Cu_ug_g_rate),sd_Cu=sd(T_Cu_ug_g_rate)) %>% 
  rbind(bioa_bgc)

##average accumulation
bioa_non_label_mean <- bioa_non_label %>% 
  group_by(batch,group) %>% 
  summarise(time_d=mean(real_time_d),mean_Cu=mean(T_Cu_ug_g),sd_Cu=sd(T_Cu_ug_g)) %>% 
  rbind(bioa_bgc)


bioa_non_label_bgc <- bioa_non_label %>% 
  group_by(batch) %>% 
  summarise(bgc=mean(mean_Cu_b_bgc))




model_uncer <- function(df){
  simulated_df <- df %>%
    mutate(sim_data = map2(mean_Cu, sd_Cu, ~ rnorm(1000, mean = .x, sd = .y))) %>%
    unnest(sim_data) %>%
    group_by(batch) %>%
    mutate(id = row_number()) %>% 
    ungroup()
  return(simulated_df)
}

bio_non_label_acc <- bioa_non_label_mean %>% 
  group_by(group) %>% 
  nest() %>% 
  mutate(data2=map(.x=data,~model_uncer(df=.x))) %>% 
  unnest(data2) %>% 
  select(-'data')

bio_non_label_plus <- bio_non_label_acc%>% 
  arrange(group, id, batch) %>%  # 确保 batch 从小到大排列
  group_by(group, id) %>%
  mutate(sim_data_acc = cumsum(sim_data)) %>%
  ungroup()

bio_non_label_plus_11d <- bio_non_label_plus %>% 
  filter(batch==5) %>% 
  mutate(time_d=11,batch=6) %>% rbind(bio_non_label_plus)


bio_non_label_plus



label <- c("0"="0 mg/kg",
           "400"="400 mg/kg",
           "800"="800 mg/kg",
           "1200"="1200 mg/kg")

##singular value
sed_acc <- read_excel('clams.xlsx',sheet='sed-acc') %>% 
  filter(flags==1) %>% 
  bind_rows(data.frame(group=1200)) %>% mutate(group=case_when(
    group==0~'Control',
    group==400~'Low-Cu treatment',
    group==800~'High-Cu treatment'
  ))%>% 
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment'))) %>% 
  filter(T_Cu_ug_g<150)


sed_acc_mean <-  sed_acc%>% 
  group_by(group) %>% 
  summarise(mean=mean(T_Cu65_ug_g),
            sd=sd(T_Cu65_ug_g)) %>% 
  filter(!is.na(group))


##batch average
bio_non_label_plus_mean <- bio_non_label_plus %>% 
  group_by(group,batch) %>% 
  summarise(mean_batch=mean(sim_data_acc),
            sd_batch=sd(sim_data_acc))

bio_non_label_plus_mean


scales <- list(
  scale_y_continuous(limits = c(-1,3)),
  scale_y_continuous(limits = c(0,10)),
  scale_y_continuous(limits = c(0,17))
)
p1 <- ggplot()+
  geom_point(data=bioa_non_label,
             aes(x=batch,y=T_Cu_ug_g_rate),alpha=0.7,color='grey80',
             show.legend = F)+
  geom_errorbar(data=bioa_non_label_mean_rate %>% filter(batch!=0),
                aes(x=batch,ymin=mean_Cu-sd_Cu,ymax=mean_Cu+sd_Cu),color='black',
                width=0,show.legend = F)+
  geom_point(data=bioa_non_label_mean_rate %>% filter(batch!=0),
             aes(x=batch,y=mean_Cu),show.legend = F,
             size=4,shape=21,color='black', fill = 'white')+
  geom_point(data=bioa_non_label_mean_rate %>% filter(batch!=0),
            aes(x=batch,y=mean_Cu,fill=as.factor(group),color=as.factor(group)),show.legend = F,
            size=2.5,shape=21,color='black')+
 
  # geom_boxplot(data=bioa_non_label %>% filter(batch!=0),
  #              aes(x=batch,y=T_Cu_ug_g_rate,fill=as.factor(group),group=batch),color='black',show.legend = F,
  #              width = 0.5,alpha=0.5,outliers = F)+
  # geom_col(data=bioa_non_label_mean_rate %>% filter(batch!=0),
  #          aes(x=batch,y=mean_Cu,fill=as.factor(group)),color='black',show.legend = F,
  #          width = 0.5,alpha=0.5)+

  facet_rep_wrap(~as.factor(group),scales = 'free_y',ncol=1,strip.position = 'right')+
  scale_fill_manual(values = c('#A5C3DC','#88A9A2','#EDBE91'))+
  scale_color_manual(values = c('#A5C3DC','#88A9A2','#EDBE91'))+
  xlim(0.7,5.2)+
  labs(x='batch')+
  theme_classic()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_text(vjust=8))+
  labs(y=~'Net Cu accumulation rate '~(mu*g~g^-1~d^-1),
       x='Batch')+
  facetted_pos_scales(y=scales)

p1

##accumulation plot
acc <- read_excel('Cint_MC_new_a&b.xlsx') %>% 
  rename(`time_d`=`...1`) %>% 
  pivot_longer(cols = c(2:4),values_to = 'conc') %>% 
  mutate(group=case_when(
    name=="Module1.Cint_0" ~ '0',
    name=="Module1.Cint_400" ~ '400',
    name=="Module1.Cint_800" ~ '800',
  ))%>% mutate(group=case_when(
    group==0~'Control',
    group==400~'Low-Cu treatment',
    group==800~'High-Cu treatment'
  ))%>% 
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment')))


acc_eli <- read_excel('Cint_eli_a&b.xlsx') %>% 
  rename(`time_d`=`...1`) %>% 
  pivot_longer(cols = c(2:4),values_to = 'conc') %>% 
  mutate(group=case_when(
    name=="Module1.Cint_0_toxicity" ~ '0',
    name=="Module1.Cint_400_toxicity" ~ '400',
    name=="Module1.Cint_800_toxicity" ~ '800',
  ))%>% mutate(group=case_when(
    group==0~'Control',
    group==400~'Low-Cu treatment',
    group==800~'High-Cu treatment'
  ))%>% 
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment')))


scales <- list(
  scale_y_continuous(limits = c(0,30)),
  scale_y_continuous(limits = c(0,100)),
  scale_y_continuous(limits = c(0,150))
)
bind_overfit_local <- acc %>% mutate(type='acc') %>% 
  rbind(acc_eli %>% mutate(type='pred')) %>% 
  mutate(type=factor(type,levels=c('acc','pred')))


p2 <- ggplot()+
  geom_point(data=data.frame(size=c('1','Superimposed')),aes(x=-10,y=-10,size=size))+
  scale_size_manual(values = c(-1,2.5),
                    label=c('','Superimposed'))+
  geom_step(data=bio_non_label_plus_11d,
            aes(x=time_d,y=sim_data_acc,group=id),color='gray90',
            linewidth=0.1)+
  # geom_point(data=bio_non_label_plus,
  #            aes(x=time_d,y=sim_data_acc,color=as.factor(group)),alpha=0.02,show.legend = F)+
  # geom_line(data=bio_non_label_plus,aes(x=time_d,y=sim_data_acc,group=id),color='gray90',
  #           linewidth=0.1,show.legend = F)+
  geom_line(data=bind_overfit_local,aes(x=time_d,y=conc,color=group,linetype=type),
            linewidth=0.8)+
  labs(linetype='')+
  guides(color='none')+
  geom_errorbar(data=bio_non_label_plus_mean,
                aes(x=batch*2,ymin=mean_batch-sd_batch,ymax=mean_batch+sd_batch),width=0,color='black',
                show.legend = F)+
  geom_point(data=bio_non_label_plus_mean,
             aes(x=batch*2,y=mean_batch,fill=as.factor(group)),size=2.5,shape=21,show.legend = F)+
 # geom_segment(data=bio_non_label_plus_mean %>% filter(batch==0),
  #             aes(x = 0, xend = 0, y = 0, yend = mean_batch,color=as.factor(group)),
   #             linewidth = 0.8,show.legend = F)+
  scale_fill_manual(values = c('#A5C3DC','#88A9A2','#EDBE91'))+
  scale_color_manual(values = c('#A5C3DC','#88A9A2','#EDBE91'))+
  scale_linetype_manual(values = c('dotdash','solid'),
                        label=c('Modeled without efflux','Predicted with efflux'))+
  theme_classic()+
  scale_x_continuous(breaks = c(0,2,4,6,8,10),limits =c(0,11))+
  theme(panel.grid = element_blank(),
        plot.margin         = margin(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position= c(0.5,0.97),
        #legend.key.size = unit(7,'mm'),
        legend.background = element_blank(),
        legend.key = element_rect(fill='transparent'),
        #legend.key.spacing = unit(0,'mm'),
        legend.spacing.y = unit(-3.4, "cm"),
        legend.text = element_text(size=8),
        axis.title.x = element_text(vjust=8))+
  xlab('Time (d)')+
  ylab(~'Cumulative tissue Cu concentration '~(mu*g~g^-1))+
  labs(size='')+
  facet_rep_wrap(~group,scales = 'free_y',ncol=1,strip.position = 'right')+
  facetted_pos_scales(y=scales)

p2



##uncertainty
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

##prediction
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


results_list_end <- results_list%>% 
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment'))) %>% 
  filter(time==10)

results_list_end_mean <- results_list_end %>% 
  group_by(group) %>% 
  summarise(mean_batch=mean(Cint_1),sd_batch=sd(Cint_1))


##Significance test
g1 <- results_list_end %>% filter(group=='Control') %>% 
  select(Cint_1) %>% mutate(group='0_test')
s1 <- sed_acc %>% filter(group=='Control' &flags==1) %>% select(T_Cu_ug_g) %>% 
  mutate(group='0_rel') %>% 
  mutate(Cint_1=T_Cu_ug_g) %>% select(2,3)
b1 <- rbind(g1,s1)



shapiro.test(g1$Cint_1)
shapiro.test(s1$Cint_1)
bartlett.test(Cint_1~group,data=b1)
t.test(Cint_1~group,data=b1)




g2 <- results_list_end %>% filter(group=='Low-Cu treatment') %>% 
  select(Cint_1) %>% mutate(group='400_test')
s2 <- sed_acc %>% filter(group=='Low-Cu treatment' &flags==1) %>% select(T_Cu_ug_g) %>% 
  mutate(group='400_rel') %>% 
  mutate(Cint_1=T_Cu_ug_g) %>% select(2,3)
b2 <- rbind(g2,s2)


shapiro.test(g2$Cint_1)
shapiro.test(s2$Cint_1)
bartlett.test(Cint_1~group,data=b2)
t.test(Cint_1~group,data=b2)





g3 <- results_list_end %>% filter(group=='High-Cu treatment') %>% 
  select(Cint_1) %>% mutate(group='800_test')
s3 <- sed_acc %>% filter(group=='High-Cu treatment' &flags==1) %>% select(T_Cu_ug_g) %>% 
  mutate(group='800_rel') %>% 
  mutate(Cint_1=T_Cu_ug_g) %>% select(2,3) %>% 
  filter(Cint_1<150)
b3 <- rbind(g3,s3)
shapiro.test(g3$Cint_1)
shapiro.test(s3$Cint_1)
bartlett.test(Cint_1~group,data=b3)
t.test(Cint_1~group,data=b3)



left_b=data.frame(x=c(1,1,1),
                  yb=c(6.5,35,84),
                  ye=c(12.5+30/15,70+100/15,130),
                  group=c('Control','Low-Cu treatment','High-Cu treatment'))%>%
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment')))


mid_l=data.frame(x=c(1,1,1),
                 xe=c(2,2,2),
                 yb=c(12.5+30/15,70+100/15,130),
                 ye=c(12.5+30/15,70+100/15,130),
                 group=c('Control','Low-Cu treatment','High-Cu treatment'))%>%
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment')))


right_b=data.frame(x=c(2,2,2),
                   yb=c(9.5,56,105),
                   ye=c(12.5+30/15,70+100/15,130),
                   group=c('Control','Low-Cu treatment','High-Cu treatment'))%>%
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment')))


sig_text=data.frame(x=c(1.5,1.5,1.5),
                    y=c(12.5+30/15+30/15,70+100/15+100/15,130+200/45+200/25),
                    label=c('italic(p)<0.01','italic(p)==0.058','italic(p)==0.996'),
                    group=c('Control','Low-Cu treatment','High-Cu treatment'))%>%
  mutate(group=factor(group,levels=c('Control','Low-Cu treatment','High-Cu treatment')))

p3 <- ggplot()+
  geom_half_violin(data=results_list_end,
                   aes(x=1.1,y=Cint_1,color=as.factor(group),fill=as.factor(group)),
                   side='r',width=0.8,alpha=0.25,show.legend = F)+
  geom_point(data=results_list_end,
             aes(x=1,y=Cint_1),color='gray50',alpha=0.01,show.legend = F,
             position=position_jitter(width=0.15))+
  geom_errorbar(data=results_list_end_mean,
                aes(x=1,ymin=mean_batch-sd_batch,ymax=mean_batch+sd_batch), color = "black",
                show.legend = F,width=0)+
  geom_point(data=results_list_end_mean,
             aes(x=1,y=mean_batch,fill=as.factor(group)), shape = 21, color = "black",
             show.legend = F,size=2.5)+
  geom_half_violin(data=sed_acc %>% filter(group!=1200),
                   aes(x=2.1,y=T_Cu_ug_g,color=as.factor(group),fill=as.factor(group)),side='r',
                   width=0.5,show.legend = F,alpha=0.25)+
  geom_point(data=sed_acc %>% filter(group!=1200),aes(x=2,y=T_Cu_ug_g),
             alpha=0.2,show.legend = F,color='gray50',
             position=position_jitter(width=0.01))+
  geom_errorbar(data=sed_acc_mean,aes(x=2,ymin=mean-sd,ymax=mean+sd), color = "black",
                show.legend = F,width=0)+
  geom_point(data=sed_acc_mean %>% filter(group!=1200),aes(x=2,y=mean,fill =as.factor(group)), shape = 21, color = "black",
             show.legend = F,size=2.5)+
  scale_fill_manual(values = c('#A5C3DC','#88A9A2','#EDBE91'))+
  scale_color_manual(values = c('#A5C3DC','#88A9A2','#EDBE91'))+
  scale_x_continuous(limits = c(0.6,2.4),
                     breaks = c(1.0,2.0),
                     labels=c('Predicted','Measured'))+
  geom_segment(data=left_b,aes(x=x,xend=x,y=yb,yend=ye))+
  geom_segment(data=mid_l,aes(x=x,xend=xe,y=yb,yend=ye))+
  geom_segment(data=right_b,aes(x=x,xend=x,y=yb,yend=ye))+
  geom_text(data=sig_text,aes(x=x,y=y,label=label),parse=T,size=3)+
  labs(y=~'Tissue Cu concentration on Day 10 '~(mu*g~g^-1))+
  theme_classic()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=12,face='bold'),
        #strip.text.y.right = element_text(angle=90),
        #strip.text = element_blank(),
        #axis.title.y = element_blank(),
        axis.text.x =  element_text(angle = 30,vjust=1,size=8,hjust=0.8),
        axis.title.x = element_blank())+
  facet_rep_wrap(~group,scales = 'free_y',ncol=1,strip.position = 'right',
             labeller = as_labeller(c(
               'Control'='Sed-Control',
               'Low-Cu treatment'='Sed-Low-Cu',
               'High-Cu treatment'='Sed-High-Cu'
             )),dir='h')+
  facetted_pos_scales(y=scales)

p3


plot_grid(p1,p2,p3,nrow=1,align = 'h',
          rel_widths = c(2,2,1.5),
          labels = c('(a)','(b)','(c)'),
          label_fontface = 'plain',label_x = c(0.01,-0.03,0))

ggsave('acc_boxplot-2.png',width = 512/90,height = 584/90,dpi=900)