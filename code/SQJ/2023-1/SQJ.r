library(readxl)
library(tidyverse)
library(RColorBrewer)
library(glue)
library(deSolve)
library(stringr)
library(FME)
library(readxl)
library(RColorBrewer)
library(ggsci)
library(scales)
library(ggridges)
library(patchwork)
library(ggpubr)
library(ggExtra)
library(ggh4x)

x_factor <- function(data,col,reps=c()){
  if(length(reps)==0){
    #data[col]=as.factor(data[col])
    data[col] <- as.factor(as.vector(data[col])[[1]])
    return(data)
  }
  if('...' %in% reps){
    datalevels <- levels(as.factor(as.vector(data[col])[[1]]))
    otherlevels=c()
    for(i in 1:length(datalevels)){
      if(datalevels[i] %in% reps){
      }
      else{
        otherlevels <- append(otherlevels,datalevels[i])
      }
    }
    alllevels=c()
    for(i in 1:length(reps)){
      if(reps[i]=='...'){
        alllevels <- append(alllevels,otherlevels)
      }
      else{
        alllevels <- append(alllevels,reps[i])
      }
    }
    data[col] <- factor(as.factor(as.vector(data[col])[[1]]),levels = alllevels)
    return(data)
  }
  else{
    data[col] <- factor(as.factor(as.vector(data[col])[[1]]),levels = reps)
    return(data)
  }
}

ref_sea <- read.csv('R.P_tissue metal concentration_µg_g.csv') %>% 
  filter(phase=='pre_labeling'&nominal_time!=0) %>% 
  mutate(IR=(`Ni62_ppb`*0.0363)/(`Ni60_ppb`*0.262)) %>% 
  select(2:7,16,-6)

ref_sea_mean <- ref_sea %>% 
  group_by(tab) %>% 
  summarise(IR_base=mean(IR),IR_base_sd=sd(IR))



ref_sea_fix <- ref_sea %>% 
  slice(-8,-12,-20)

ref_sea_mean_fix <- ref_sea_fix %>% 
  group_by(tab) %>% 
  summarise(IR_base=mean(IR),IR_base_sd=sd(IR))


clams_fix <- read.csv('R.P_tissue metal concentration_µg_g.csv') %>% 
  filter(phase=='TD'|phase=='Bioa',nominal_time!='6h',clear==0) %>% 
  mutate(IR_exp=(`Ni62_ppb`*0.0363)/(`Ni60_ppb`*0.262)) %>% 
  select(2:8,13:14,16) %>% 
  left_join(ref_sea_mean_fix,by='tab')


clams_AR_fix <- clams_fix %>% 
  mutate(M=dw_g*(IR_base*`Ni60_ppb`*0.262-`Ni62_ppb`*0.0363)/(IR_base*0.262-0.0363)) %>% 
  mutate(AR=M/(dw_g*time_h)*1000)

clams_AR_mean_1_fix <- clams_AR_fix %>% 
  group_by(phase,tab,nominal_time,rep) %>% 
  summarise(AR=mean(AR)) %>% 
  mutate(phase=case_when(
    phase=='Bioa'~'1-d Low-Ni sed. exposure',
    (phase=='TD'& nominal_time=='24h')~'1-d High-Ni sed. exposure',
    (phase=='TD'& nominal_time=='72h')~'3-d High-Ni sed. exposure'
  )) %>% 
  mutate(time_d=case_when(
    nominal_time=='24h'~paste(3*as.numeric(tab)-3,'-',3*as.numeric(tab)-2,sep=''),
    nominal_time=='72h'~paste(3*as.numeric(tab)-3,'-',3*as.numeric(tab),sep='')
  )) %>% 
  x_factor('phase',c('1-d Low-Ni sed. exposure',
                     '1-d High-Ni sed. exposure',
                     '3-d High-Ni sed. exposure')) %>% 
  x_factor('time_d',c("0-1","3-4" ,"6-7" ,"9-10","12-13" ,"15-16",
                      "0-3", "3-6" ,"6-9","9-12","12-15","15-18"))

clams_AR_mean_2_fix <- clams_AR_mean_1_fix%>% 
  group_by(phase,tab,time_d,nominal_time) %>%
  summarise(AR_mean=mean(AR),AR_sd=sd(AR),AR_min=min(AR),AR_max=max(AR))




clams_AR_d <- clams_AR_mean_2_fix %>% 
  mutate(AR_d=case_when(nominal_time=='72h'~AR_mean*24/1000,
                        nominal_time=='24h'~AR_mean*24/1000),
         AR_d_sd=AR_sd*24/1000)


### Bioa 24h predict，
n <- 1000
ar1 <- rnorm(n, clams_AR_d$AR_d[1], clams_AR_d$AR_d_sd[1]) 
ar4 <- rnorm(n, clams_AR_d$AR_d[2], clams_AR_d$AR_d_sd[2]) 
ar7 <- rnorm(n, clams_AR_d$AR_d[3], clams_AR_d$AR_d_sd[3]) 
ar10 <- rnorm(n, clams_AR_d$AR_d[4], clams_AR_d$AR_d_sd[4]) 
ar13 <- rnorm(n, clams_AR_d$AR_d[5], clams_AR_d$AR_d_sd[5]) 
ar16 <- rnorm(n, clams_AR_d$AR_d[6], clams_AR_d$AR_d_sd[6]) 

times <- seq(from = 0,
             to = 18,
             by = 0.1)

n_row <- length(times) %>% as.numeric()

df_0 <- as.data.frame(times)

AR <- data.frame(matrix(ncol = n +1, nrow = n_row))
AR$X1 <- times


for (i in 1:n_row) {
  if (AR[i,1] < 3) AR[i,2: (n+1)] = ar1
  else if (AR[i,1] >=3 & AR[i,1] < 6) AR[i,2:(n+1)] = ar4
  else if (AR[i,1] >=6 & AR[i,1] < 9) AR[i,2:(n+1)] = ar7
  else if (AR[i,1] >=9 & AR[i,1] < 12) AR[i,2:(n+1)] = ar10
  else if (AR[i,1] >=12 & AR[i,1] < 15) AR[i,2:(n+1)] = ar13
  else if (AR[i,1] >=15 & AR[i,1] <= 18) AR[i,2:(n+1)] = ar16
}

for(k in 2:1000){
  for(i in 1:5){
    J <- approxfun( x = c(AR[30*i-19,1],AR[30*i-19+20,1]),
                    y = c(AR[30*i-19,k],AR[30*i-19+20,k]),
                    rule = 2)
    for(j in ((30*i-19):(30*i-19+20))){
      AR[j,k]=J(AR[j,1])
    }
  }
}

df_Cint <- data.frame(matrix(ncol = n +1, nrow = n_row))
df_Cint[,1] <- times

j=2
for (j in 2: (n+1)) {
  J <- approxfun( x = AR[,1],
                  y = AR[,j],
                  rule = 2)
  
  TK <- function (t,y, parameters) {
    ke <- parameters[1]
    Cint <- y[1]
    dCint <-  J(t) - ke * Cint
    list(c(dCint))
  }
  
  ke_initial <- 0.02888
  
  temp <- ode(func = TK, 
              times = times,
              y = c(Cint = 2.2802), 
              parms = c(ke = 0.02888)) 
  
  df_Cint[,j] = temp[,2]
  
}

df_Cint_bioa <- df_Cint %>%
  pivot_longer(cols = 2: (n+1),
               names_to = "sample",
               values_to = "Cint",)

df_Cint_bioa <- rename(df_Cint_bioa, time = X1)  


df_Cint_bioa_mean <- df_Cint_bioa %>% 
  group_by(time) %>% 
  summarise(Cint=mean(Cint))



### td24h predict 
ar1 <- rnorm(n, clams_AR_d$AR_d[7], clams_AR_d$AR_d_sd[7]) 
ar4 <- rnorm(n, clams_AR_d$AR_d[8], clams_AR_d$AR_d_sd[8])
ar7 <- rnorm(n,clams_AR_d$AR_d[9], clams_AR_d$AR_d_sd[9])
ar10 <- rnorm(n, clams_AR_d$AR_d[10], clams_AR_d$AR_d_sd[10])
ar13 <- rnorm(n, clams_AR_d$AR_d[11], clams_AR_d$AR_d_sd[11])
ar16 <- rnorm(n, clams_AR_d$AR_d[12], clams_AR_d$AR_d_sd[12])


times <- seq(from = 0,
             to = 18,
             by = 0.1)

n_row <- length(times) %>% as.numeric()


df_0 <- as.data.frame(times)

AR <- data.frame(matrix(ncol = n +1, nrow = n_row))
AR$X1 <- times
for (i in 1:n_row) {
  if (AR[i,1] < 3) AR[i,2: (n+1)] = ar1
  else if (AR[i,1] >=3 & AR[i,1] < 6) AR[i,2:(n+1)] = ar4
  else if (AR[i,1] >=6 & AR[i,1] < 9) AR[i,2:(n+1)] = ar7
  else if (AR[i,1] >=9 & AR[i,1] < 12) AR[i,2:(n+1)] = ar10
  else if (AR[i,1] >=12 & AR[i,1] < 15) AR[i,2:(n+1)] = ar13
  else if (AR[i,1] >=15 & AR[i,1] <= 18) AR[i,2:(n+1)] = ar16
}

for(k in 2:1000){
  for(i in 1:5){
    J <- approxfun( x = c(AR[30*i-19,1],AR[30*i-19+20,1]),
                    y = c(AR[30*i-19,k],AR[30*i-19+20,k]),
                    rule = 2)
    for(j in ((30*i-19):(30*i-19+20))){
      AR[j,k]=J(AR[j,1])
    }
  }
}

df_Cint <- data.frame(matrix(ncol = n +1, nrow = n_row))
df_Cint[,1] <- times

j=2
for (j in 2: (n+1)) {
  J <- approxfun( x = AR[,1],
                  y = AR[,j],
                  rule = 2)
  
  TK <- function (t,y, parameters) {
    ke <- parameters[1]
    Cint <- y[1]
    dCint <-  J(t) - ke * Cint
    list(c(dCint))
  }
  
  ke_initial <- 0.02888
  
  temp <- ode(func = TK, 
              times = times,
              y = c(Cint = 2.7908), 
              parms = c(ke = 0.02888)) 
  
  df_Cint[,j] = temp[,2]
  
}



df_Cint_td_24h <- df_Cint %>%
  pivot_longer(cols = 2: (n+1),
               names_to = "sample",
               values_to = "Cint",)

df_Cint_td_24h <- rename(df_Cint_td_24h, time = X1)  


df_Cint_td_24h_mean <- df_Cint_td_24h %>% 
  group_by(time) %>% 
  summarise(Cint=mean(Cint))




### td72h predict 
ar1 <- rnorm(n,clams_AR_d$AR_d[13], clams_AR_d$AR_d_sd[13]) 
ar4 <- rnorm(n,clams_AR_d$AR_d[14], clams_AR_d$AR_d_sd[14])
ar7 <- rnorm(n,clams_AR_d$AR_d[15], clams_AR_d$AR_d_sd[15])
ar10 <- rnorm(n,clams_AR_d$AR_d[16], clams_AR_d$AR_d_sd[16])

ar13 <- rnorm(n,clams_AR_d$AR_d[17], clams_AR_d$AR_d_sd[17])
ar16 <- rnorm(n,clams_AR_d$AR_d[18], clams_AR_d$AR_d_sd[18])



times <- seq(from = 0,
             to = 18,
             by = 0.1)



n_row <- length(times) %>% as.numeric()

df_0 <- as.data.frame(times)

AR <- data.frame(matrix(ncol = n +1, nrow = n_row))
AR$X1 <- times

for (i in 1:n_row) {
  if (AR[i,1] < 3) AR[i,2: (n+1)] = ar1
  else if (AR[i,1] >=3 & AR[i,1] < 6) AR[i,2:(n+1)] = ar4
  else if (AR[i,1] >=6 & AR[i,1] < 9) AR[i,2:(n+1)] = ar7
  else if (AR[i,1] >=9 & AR[i,1] < 12) AR[i,2:(n+1)] = ar10
  else if (AR[i,1] >=12 & AR[i,1] < 15) AR[i,2:(n+1)] = ar13
  else if (AR[i,1] >=15 & AR[i,1] <= 18) AR[i,2:(n+1)] = ar16
}








df_Cint <- data.frame(matrix(ncol = n +1, nrow = n_row))
df_Cint[,1] <- times

j=2
for (j in 2: (n+1)) {
  J <- approxfun( x = AR[,1],
                  y = AR[,j],
                  rule = 2)
  
  TK <- function (t,y, parameters) {
    ke <- parameters[1]
    Cint <- y[1]
    dCint <-  J(t) - ke * Cint
    list(c(dCint))
  }
  
  ke_initial <- 0.02888
  
  temp <- ode(func = TK, 
              times = times,
              y = c(Cint = 2.7908), 
              parms = c(ke = 0.02888)) 
  
  df_Cint[,j] = temp[,2]
  
}



df_Cint_td_72h <- df_Cint %>%
  pivot_longer(cols = 2: (n+1),
               names_to = "sample",
               values_to = "Cint",)

df_Cint_td_72h <- rename(df_Cint_td_72h, time = X1)  


df_Cint_td_72h_mean <- df_Cint_td_72h %>% 
  group_by(time) %>% 
  summarise(Cint=mean(Cint))

write.csv(df_Cint_bioa,'d_Bioa_24h_predict_Cint.csv')
write.csv(df_Cint_td_24h,'d_TD_24h_predict_Cint.csv')
write.csv(df_Cint_td_72h,'d_TD_72h_predict_Cint.csv')


## draw pic


# d_Bioa_24h <- read.csv("d_Bioa_24h_predict_Cint.csv")
# d_TD_24h <- read.csv("d_TD_24h_predict_Cint.csv")
# d_TD_72h <- read.csv("d_TD_72h_predict_Cint.csv")

d_Bioa_24h <- df_Cint_bioa
d_TD_24h <- df_Cint_td_24h
d_TD_72h <- df_Cint_td_72h



d_non_labeling <- read_excel("R.P sediment exposure3.xlsx", sheet = 5)

names(d_non_labeling)

d_non_labeling_Bioa <- d_non_labeling |> 
  select(2:5,18) |> 
  filter(phase =="Bioa")
d_non_labeling_Bioa$phase <- factor(d_non_labeling_Bioa$phase,
                                    levels = c("Bioa"),
                                    labels = c("1-d Low-Ni sed. exposure"))

d_non_labeling_TD_24h <- d_non_labeling |> 
  select(2:5,18) |> 
  filter(phase =="TD")
d_non_labeling_TD_24h$phase <- factor(d_non_labeling_TD_24h$phase,
                                      levels = c("TD"),
                                      labels = c("1-d High-Ni sed. exposure"))  
d_non_labeling_TD_72h <- d_non_labeling |> 
  select(2:5,18) |> 
  filter(phase =="TD")
d_non_labeling_TD_72h$phase <- factor(d_non_labeling_TD_72h$phase,
                                      levels = c("TD"),
                                      labels = c("3-d High-Ni sed. exposure"))

d_Bioa_24h_1 <- d_Bioa_24h |> 
  mutate(phase = "1-d Low-Ni sed. exposure")
d_TD_24h_1 <- d_TD_24h |> 
  mutate(phase = "1-d High-Ni sed. exposure")
d_TD_72h_1 <- d_TD_72h |> 
  mutate(phase = "3-d High-Ni sed. exposure")


d_measured <- rbind(d_non_labeling_Bioa,d_non_labeling_TD_24h,d_non_labeling_TD_72h)
d_predicted <- rbind(d_Bioa_24h_1,d_TD_24h_1,d_TD_72h_1)

d_measured$phase <- factor(d_measured$phase,
                           levels = c("1-d Low-Ni sed. exposure",
                                      "1-d High-Ni sed. exposure",
                                      "3-d High-Ni sed. exposure"))
d_predicted$phase <- factor(d_predicted$phase,
                            levels = c("1-d Low-Ni sed. exposure",
                                       "1-d High-Ni sed. exposure",
                                       "3-d High-Ni sed. exposure"))

d_measured_mean_1 <- d_measured |> 
  group_by(phase,time_d,tab) |> 
  summarise(mean_rep =mean(Ni_ug_g)) 
d_measured_mean_2 <- d_measured_mean_1 |>  
  group_by(phase,time_d) |> 
  summarise(mean_Ni =mean(mean_rep),
            sd_Ni =sd(mean_rep)) 
#write.csv(d_measured_mean_2,"d_measured_mean_2.csv")

d_predicted_mean <- d_predicted |> 
  group_by(phase,time) |> 
  summarise(mean_Cint =mean(Cint),
            sd_Cint = mean(Cint)) 
d_predicted_mean_1 <- d_predicted_mean |> 
  filter(time %in% c(0,9,18))
#write.csv(d_predicted_mean_1,"d_predicted_mean_1.csv")

#SQG=3*background concentration
d_SQG <- data.frame(SQG=c(6.8406,
                          8.3724,
                          8.3724),
                    phase = c("1-d Low-Ni sed. exposure",
                              "1-d High-Ni sed. exposure",
                              "3-d High-Ni sed. exposure") )
d_SQG$phase <- factor(d_SQG$phase,
                      levels = c("1-d Low-Ni sed. exposure",
                                 "1-d High-Ni sed. exposure",
                                 "3-d High-Ni sed. exposure"))


#facet labels
# Define the position and labels for the facet labels
facet_labels <- data.frame(
  phase = c("1-d Low-Ni sed. exposure",
            "1-d High-Ni sed. exposure",
            "3-d High-Ni sed. exposure"),
  label = c("(a)", "(b)", "(c)"),
  x = c(1, 1, 1),
  y = c(37.5, 75, 75)
)

facet_labels$phase <- factor(facet_labels$phase,
                             levels = c("1-d Low-Ni sed. exposure",
                                        "1-d High-Ni sed. exposure",
                                        "3-d High-Ni sed. exposure"))

scales <- list(
  scale_y_continuous(breaks = c(0,10,20,30,40),
                     limits =  c(0, 40),
                     guide = "axis_minor"),
  scale_y_continuous(breaks = c(0,20,40,60,80),
                     limits = c(0, 80),
                     guide = "axis_minor"),
  scale_y_continuous(breaks = c(0,20,40,60,80),
                     limits =   c(0, 80),
                     guide = "axis_minor"))



p1 <- 
  ggplot()+
  theme_bw()+
  #modeled 1000 curves
   geom_line(data = filter(d_predicted,Cint>0),aes(time,Cint,group =sample, color = phase),
             alpha=0.05,size=0.15,inherit.aes = F)+
  #modeled mean curve   
  geom_line(data = d_predicted_mean,aes(time,mean_Cint,group =phase), color = 'black',
            inherit.aes = F,size =0.85)+
  #individual clam value   
  geom_point(data =filter(d_measured,time_d!=0),aes(time_d, Ni_ug_g),shape =21, 
             size = 1.5, color ="gray50",inherit.aes = F,alpha=0.65,
  )+#color="gray70",color="#C6D7E0"
  geom_point(data =filter(d_measured,time_d==0),aes(time_d, Ni_ug_g, color = phase),shape =21, 
             size = 2,inherit.aes = F)+
  #geom_point(data =filter(d_measured_mean_1,time_d!=0), aes(time_d,mean_rep,fill=phase),shape =21,
  #   alpha=0.65,size=2,inherit.aes = F)+
  geom_point(data =filter(d_measured_mean_1,time_d!=0), aes(time_d,mean_rep, color = phase),shape =21,
             size=2,inherit.aes = F)+
  geom_errorbar(data =d_measured_mean_2,aes(time_d,ymax=mean_Ni+sd_Ni,
                                            ymin=mean_Ni-sd_Ni),
                color = "black",
                width=0,inherit.aes = F)+
  geom_point(data =d_measured_mean_2, aes(time_d,mean_Ni,fill =phase),
             shape =21,size=3,inherit.aes = F)+
  # geom_line(data=full_predict,aes(x=time,y=Cint,group=phase),color='black')+
  geom_hline(data=d_SQG,aes(yintercept=SQG),color = "grey20",linetype="dotdash")+
  facet_wrap(~phase,scales = "free")+
  facetted_pos_scales(y = scales)+
  scale_color_manual(values=c("#F29A76","#5AA4AE","#5AA4AE"))+
  scale_fill_manual(values=c("#F29A76","#5AA4AE","#5AA4AE"))+
  scale_x_continuous(limits = c(0,18),breaks = seq(0,18,3)) +
  # scale_y_continuous(limits = c(0,75),breaks = seq(0,75,10)) +
  theme(panel.grid = element_blank(),
        legend.position = "None",
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(hjust = 0, vjust = 0.5),
        panel.border = element_rect(color='black')) +
  geom_text(data = facet_labels, aes(x = x, y = y, label = label), size = 5)
p1




ggsave("07.predict_Cint_V4.png", width = 741/90, height = 305/90, dpi = 900) 




