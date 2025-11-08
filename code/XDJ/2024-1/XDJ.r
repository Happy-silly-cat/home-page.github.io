
library(ggplot2)
library(latex2exp)
library(ggpp)
library(tidyverse)
library(lemon)
library(cowplot)


##SS prediction, by xwz
ss_predict <- read.csv('ss_predict.csv')

initial_values <- list(
  Ni = 0.279,
  Cu = 20.444,
  Zn = 141.792,
  Cd = 0.955,
  Pb = 0.106
)


initial_values_tri <- list(
  Ni = 0.279*3,
  Cu = 20.444*3,
  Zn = 141.792*3,
  Cd = 0.955*3,
  Pb = 0.106*3
)


ss_time <- c(2,12,24,72,168)
metals_list <- c('Ni','Cu','Zn','Cd','Pb')

ss_predict_lower <- data.frame()
#筛选背景浓度三倍
for(i in metals_list){
  for(j in ss_time){
    ss_predict_lower <- rbind( ss_predict_lower,ss_predict %>% 
                                 filter(metals==i & ss_time==j & Cint<=initial_values_tri[i][1]))
  }
}

ss_predict_lower <- ss_predict_lower |>
  # 重排序
  mutate(metals = fct_relevel(metals, c(
    "Ni",
    "Cu",
    "Zn",
    "Cd",
    "Pb")))

ss_predict_lower_timemax <- ss_predict_lower %>% 
  group_by(metals,ss_time) %>% 
  summarise(time=max(time)) %>% 
  left_join(data.frame(initial_values_tri) %>% pivot_longer(cols = c(1:5),names_to = 'metals'),
            by=c('metals'))


ss_text <- data.frame(initial_values_tri) %>% 
  pivot_longer(cols = c(1:5),names_to = 'metals')%>% 
  filter(metals=='Cu' | metals=='Ni' | metals=='Pb') %>% 
  mutate(labels=TeX(paste('3-fold increase in tissue burden'),output = 'character')) %>% 
  mutate(x=c(31.2,165,37),
         y=c(0.887,65,0.3372))

ss_text <- ss_text|>
  # 重排序
  mutate(metals = fct_relevel(metals, c(
    "Ni",
    "Cu",
    "Pb")))


##背景阴影
ss_predict_time_bound <- ss_predict_lower %>% 
  filter(metals=='Cu' | metals=='Ni' | metals=='Pb') %>% 
  group_by(metals,ss_time) %>% 
  summarise(max_time=max(time)) %>% 
  summarise(min_time=min(max_time),max_time=max(max_time))


ss_predict_bound <- ss_predict_lower %>% 
  filter(metals=='Cu' | metals=='Ni' | metals=='Pb') %>% 
  left_join(ss_predict_time_bound,by=c('metals')) %>% 
  left_join(data.frame(initial_values_tri) %>% pivot_longer(cols = c(1:5),names_to = 'metals')%>% 
              filter(metals=='Cu' | metals=='Ni' | metals=='Pb'),by=c('metals')) %>% 
  left_join(data.frame(initial_values) %>% pivot_longer(cols = c(1:5),names_to = 'metals',values_to = 'bkg')%>% 
              filter(metals=='Cu' | metals=='Ni' | metals=='Pb'),by=c('metals')) %>% 
  group_by(metals,min_time,max_time,value,bkg,time) %>% 
  summarise(min_Cint=min(Cint),max_Cint=max(Cint)) %>% 
  mutate(max_Cint=ifelse(time>min_time,value,max_Cint))

ss_predict_time_bound <- ss_predict_time_bound|>
  # 重排序
  mutate(metals = fct_relevel(metals, c(
    "Ni",
    "Cu",
    "Pb")))

ss_predict_bound$metals <- factor(ss_predict_bound$metals,
                                  levels = c( "Ni",
                                              "Cu",
                                              "Pb"))


##范围描述
ss_time_text <- ss_predict_bound %>%
  ungroup() %>% 
  select(c(1,2,3)) %>% 
  unique() %>% 
  mutate(labels=paste(min_time,' - ',max_time,' d')) %>% 
  mutate(x=c(6.71,1.28,1.55),
         y=c(30,0.4,0.15))
 
ss_time_text <-ss_time_text|>
  # 重排序
  mutate(metals = fct_relevel(metals, c(
    "Ni",
    "Cu",
    "Pb")))

# 先将 ss_time 列转换为带有单位的字符串
ss_predict_lower <- ss_predict_lower %>%
  mutate(ss_time = case_when(
    ss_time == 2 ~ "2 h",
    ss_time == 12 ~ "12 h",
    ss_time == 24 ~ "24 h",
    ss_time == 72 ~ "72 h",
    ss_time == 168 ~ "168 h"
  ))

# 然后将其转换为因子
ss_predict_lower$ss_time <- factor(ss_predict_lower$ss_time, 
                                   levels = c("2 h", "12 h", "24 h", "72 h", "168 h"))

##背景阴影
bkg_rect <- data.frame(metals=c('Cu','Ni','Pb'),
                       xmin=c(-Inf,-Inf,-Inf),
                       xmax=c(Inf,Inf,Inf),
                       ymin=c(20.444*3,0.279*3,0.106*3),
                       ymax=c(Inf,Inf,Inf))

bkg_rect <-bkg_rect|>
  # 重排序
  mutate(metals = fct_relevel(metals, c(
    "Ni",
    "Cu",
    "Pb")))




#创建数据框在每个分面添加时间标签
Labels <- data.frame(
  metals=c('Cu','Ni','Pb'),
  label_s = c(1, 2, 3),
  Species = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  x = c(200,40,50),
  y = c(29,0.4,0.15)
)

Labels_1 <- Labels |>
  mutate(label_s = case_when(
    label_s == 1 ~ "85 – 325 h",
    label_s == 2 ~ "21 – 62 h",
    label_s == 3 ~ "26 – 73 h")) |>
  mutate(label_s = fct_relevel(label_s, c("85 – 325 h", "21 – 62 h", "26 – 73 h")))

Labels_1 <- Labels_1 |>
  # 重排序
  mutate(metals = fct_relevel(metals, c(
    "Ni",
    "Cu",
    "Pb")))


# Plot
p_ss <- ggplot() +
  geom_rect(data=bkg_rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill='1'), fill = "#F6E47F", show.legend = FALSE) +
  geom_segment(data=ss_predict_bound, aes(x=min_time, xend=min_time, y=-Inf, yend=value), color = "grey20",linetype='2121') +
  geom_segment(data=ss_predict_bound, aes(x=max_time, xend=max_time, y=-Inf, yend=value), color = "grey20",linetype='2121') +
  theme_classic() +
  geom_line(data=ss_predict_lower %>% filter(metals=='Cu' | metals=='Ni' | metals=='Pb'),
            aes(x=time, y=Cint, color=as.factor(ss_time), group=ss_time)) +
  facet_wrap(~metals, scales = 'free') +
#  geom_hline(data=data.frame(initial_values_tri) %>% pivot_longer(cols = c(1:5),names_to = 'metals')%>% 
 #              filter(metals=='Cu' | metals=='Ni' | metals=='Pb'),
  #           aes(yintercept=value,group=metals),linetype='solid',color='grey80')+
  labs(color='Suspension pretreatment') +
  scale_color_manual(values = c( "black", "#6F1AB2","#28B532","#5092E1","#A50F15")) +
#  scale_color_manual(values = c("#76C893", "#669BBC","#168AAF","#023E8A", "black")) +
  scale_fill_manual(values = c('lightyellow')) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face='bold', size=12, hjust = 0),
        panel.grid = element_blank(),
        #legend.position = 'bottom',
        legend.direction = "horizontal",
        legend.position = c(0.5,1.2),
        legend.background = element_blank()) +
  geom_text(data=ss_text, aes(x=x, y=y, label=labels), parse=TRUE, size=3.3) +
 # geom_text(data = Labels_1, aes(x = x, y = y, label = label_s), size = 3) +
  #labs(x='Exposure time (h)',y = " ", title = " ")
  labs(x='Exposure time (h)', title = " ", y = NULL)+
 
  geom_segment(data=ss_predict_bound, aes(x=min_time, xend=min_time, y=-Inf, yend=value), color = "grey20",linetype='2121') +
  geom_segment(data=ss_predict_bound, aes(x=max_time, xend=max_time, y=-Inf, yend=value), color = "grey20",linetype='2121')
  
  
      # y=TeX('Tissue metal $concentration\\,(\\mu g\\, g^{–1})$',
     #  y = expression("Tissue metal concentration ("*mu*"g"~"g"^{'–1'}*")"))

p_ss



#读取模型模拟数据
eq_predict <- read.csv("eq_predict.csv")
eq_predict_tot <- readRDS("eq_predict_tot.rds")


##背景阴影：计算每个时间点每个金属的最低值和最高值
eq_predict_ribbon <- eq_predict_tot %>%
  group_by(time, metals) %>%
  summarise(min_Cint = min(Cint), max_Cint = max(Cint), .groups = 'drop')


##竖线
inter <- data.frame()
for(i in metals_list){
  min_time <- (eq_predict_ribbon %>% 
                 filter(metals==i & max_Cint>=initial_values_tri[[i]][1]))[1,1][[1]]
  mid_time <- (eq_predict %>% 
                 filter(metals==i & Cint>=initial_values_tri[[i]][1]))[1,2][[1]]
  max_time <- (eq_predict_ribbon %>% 
                 filter(metals==i & min_Cint>=initial_values_tri[[i]][1]))[1,1][[1]]
  inter <- rbind(inter,
                 data.frame(metals=i,
                            min_time=round(min_time),
                            mid_time=round(mid_time),
                            max_time=round(max_time),
                            tri=initial_values_tri[[i]][1]))  
}

inter <- inter %>% pivot_longer(cols = c(2:4),names_to = 'time')%>%
  mutate(x = c(13, 30, 59, 77, 151, 140, 160, 167, 158,  77, 69, 70,4, 34, 123),
         y = c(0.1, 0.1, 0.1, 2.5, 2.5, 2.5 , 15, 15, 15, 0.15, 0.15, 0.15, 0.035, 0.035, 0.035))


eq_predict$metals <- factor(eq_predict$metals, 
                            levels = c( "Ni",
                                        "Cu",
                                        "Zn",
                                        "Cd",
                                        "Pb"))

eq_predict_ribbon$metals <- factor(eq_predict_ribbon$metals, 
                            levels = c( "Ni",
                                        "Cu",
                                        "Zn",
                                        "Cd",
                                        "Pb"))
# 绘图
p_equali <- ggplot() +
  theme_classic() +
  geom_line(data = filter(eq_predict,
                          metals == "Ni" | metals == "Cu" |metals == "Pb"), 
            aes(x = time, y = Cint, color = metals, group = metals)) +
  # geom_line(data=eq_predict_tot,aes(x=time,y=Cint,group=sample),color='gray50',alpha=0.3)+
  geom_ribbon(data = filter(eq_predict_ribbon,
                            metals == "Ni" | metals == "Cu" | metals == "Pb"), 
              aes(x = time, ymin = min_Cint, ymax = max_Cint, fill = metals), alpha = 0.3) +
  facet_wrap(~metals, scales = 'free')+ 
  labs(x='Equilibration duration (h)',y = " ")+
    #   y=TeX('Tissue metal $concentration\\,(\\mu g\\, g^{-1})$')) +
  scale_color_manual(values = c("#CF7C9A", "#404888", "#117893","#487078","#484840"))+
  scale_fill_manual(values = c("#CF7C9A", "#404888", "#117893","#487078","#484840"))+
  geom_segment(data=inter|>
                 filter(metals == "Ni" | metals == "Cu" | metals == "Pb")|>
                 mutate(metals = fct_relevel(metals, c(
                   "Ni",
                   "Cu",
                   "Pb"))),aes(x=value,xend=value,y=-Inf,yend=tri),na.rm = T,linetype='2121',colour="black")+
  geom_hline(data=data.frame(initial_values_tri) %>% pivot_longer(cols = c(1:5),names_to = 'metals')|>
               filter(metals == "Ni" | metals == "Cu" | metals == "Pb")|>
               mutate(metals = fct_relevel(metals, c(
                 "Ni",
                 "Cu",
                 "Pb"))),aes(yintercept=value,group=metals),linetype='2121',color='#780000')+
  geom_text(data = inter|>
              filter(metals == "Ni" | metals == "Cu" | metals == "Pb")|>
              mutate(metals = fct_relevel(metals, c(
                "Ni",
                "Cu",
                "Pb"))), aes(x = x, y = y,label= value),size=3,colour="black")+
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 12, face='bold',))+
  guides(fill = "none", color = "none")+
  scale_y_continuous(limits = c(0, NA))


p_equali

p_2 <- plot_grid(p_ss, p_equali, 
                   nrow = 2,
                   align = "v",
                   rel_widths = c(1, 1),
                   rel_heights = c(1.15, 1))
p_all <- ggdraw() +
  draw_plot(p_2) +
  draw_label(expression("Tissue metal concentration ("*mu*"g"~"g"^{'–1'}*")"), 
             x = 0, y = 0.5, angle = 90, size = 12,vjust = 0.97, hjust = 0.5)

p_all

ggsave("==ALL_1000_1.png", width = 630/90, height = 520/90, dpi = 900)



