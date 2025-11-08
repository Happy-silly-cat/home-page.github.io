library(patchwork)
library(tidyverse)
library(readxl)
library(ggh4x)
library(broom)
library(lemon)
library(ggpp)
library(dplyr)
library(cowplot)


d <- read_excel("new_metal.xlsx")
names(d)


df <- d %>% 
  select(c(1,2,3)) %>% 
  group_by(metal) %>% 
  summarise(mean = mean(concentration), sd = sd(concentration))



fit <- d %>%
  select(c(1,2,3)) %>% 
  nest(data = -metal) %>% 
  mutate(
    mod = map(data, ~ lm(log10(concentration)~log10(dw_g), 
                         data=.) ), 
    tidied = map(mod, tidy)
  ) %>%
  unnest(tidied)


## linear model
fit_rsq <- d %>%
  select(c(1,2,3)) %>% 
  nest(data = -metal) %>% 
  mutate(
    mod = map(data, ~ lm(log10(concentration)~log10(dw_g), 
                         data=.) ), 
    rsq = map(mod, glance)
  ) %>%
  unnest(rsq)

d_conctext1 <- data.frame(metal=c("Cd113","Pb206",
                                  "Zn68","Ni61"),
                          conc=c("italic(b) ==-0.185", 
                                 "italic(b) ==-0.138", 
                                 "italic(b) ==-0.018", 
                                 "italic(b) ==-0.125"
                                 
                          ))
d_conctext2 <- data.frame(metal=c("Cd113","Pb206",
                                  "Zn68","Ni61"),
                          conc=c("italic(p) < 0.001", 
                                 "italic(p) < 0.001", 
                                 paste("italic(p) == ",
                                       deparse(sprintf("%.3f", 0.710))),
                                 "italic(p) == 0.001"
                          ))

d_conctext3 <- data.frame(metal =c("Cd113","Pb206",
                                   "Zn68","Ni61"),
                          conc = c("bold({}^113*Cd)","bold({}^206*Pb)","bold({}^68*Zn)",
                                   "bold({}^61*Ni)"))


d_conctext4 <- data.frame(metal=c("Cd114","Pb207",
                                  "Zn66","Ni62"),
                          conc=c("italic(b) ==-0.161", 
                                 "italic(b) ==-0.182", 
                                 "italic(b) ==-0.586", 
                                 "italic(b) ==-0.213"
                          ))
d_conctext5 <- data.frame(metal=c("Cd114","Pb207",
                                  "Zn66","Ni62"),
                          conc=c("italic(p) < 0.001", 
                                 "italic(p) < 0.001", 
                                 "italic(p) < 0.001",  
                                 "italic(p) < 0.001"
                          ))
d_conctext6 <- data.frame(metal=c("Cd114","Pb207",
                                  "Zn66","Ni62"),
                          conc = c("bold({}^114*Cd)","bold({}^207*Pb)","bold({}^66*Zn)",
                                   "bold({}^62*Ni)"))





my_col <- c("#000000","#D55E00")
d1 <- d %>% 
  filter(metal %in% c("Cd113","Pb206","Zn68","Ni61"))
d2 <- d %>% 
  filter(metal %in% c("Cd114","Pb207","Zn66","Ni62"))
names(d1)

##Isotopes 1
PA <- 
  ggplot()+
  theme_classic()+
  geom_point(data=d1, aes(dw_g, concentration, fill = metal),shape = 21, 
             color = "transparent",alpha = 0.3,size = 1)+
  geom_point(data=d1, aes(dw_g, concentration, color = metal),shape = 21, 
             fill = "transparent", size = 1)+
  facet_rep_wrap(~metal, scales = "free_y", nrow = 4)+
  scale_x_log10(breaks =  c(0.001,0.01,0.1,1),labels = c("0.001","0.01","0.1","1"))+
  scale_y_log10()+
  annotation_logticks(sides="bl", size = 0.35,
                      short = unit(0.05, "cm"),
                      mid = unit(0.15, "cm"),
                      long = unit(0.25, "cm"))+
  geom_smooth(data = d1,aes(dw_g, concentration, color = metal),
              method = "lm",se = FALSE, linewidth = 0.8)+
  scale_fill_manual(values = c(my_col[1], my_col[1],my_col[1],my_col[1]))+
  scale_color_manual(values = c(my_col[1], my_col[1],my_col[1],my_col[1])) +
  labs(x="Dry weight (g)",
       title = " ",
       y=expression("Newly accumulated isotopes in mussels ("*mu*g~g^"-1"*")")
  )+
  geom_text_npc(data=d_conctext1,
                aes(label=conc), npcx =0.1, npcy = 0.25,
                inherit.aes = FALSE,
                parse = T, color = "black",size=3)+
  geom_text_npc(data=d_conctext2,
                aes(label=conc), npcx =0.1, npcy = 0.08,
                inherit.aes = FALSE,
                parse = T, color = "black",size=3)+
  geom_text_npc(data=d_conctext3,
                aes(label=conc), npcx =0.95, npcy = 0.95,
                inherit.aes = FALSE,
                parse = T, color = "black",size=4, fontface="bold")+
  theme(
    plot.margin = margin(5, 0, 5, 0),
    legend.position = "none",
    strip.background = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    axis.line = element_line(linewidth  = 0.35),
    axis.ticks = element_line(linewidth  = 0.35)
    
  )

PA


## Isotope1 density
PB <- 
  ggplot(d1)+
  geom_smooth(data = d1,aes(dw_g, concentration, color = metal),
              method = "lm",se = FALSE, linewidth = 0.8,color='transparent')+
  facet_rep_wrap(~metal, scales = "free_y", nrow = 4)+
  geom_density(aes(y=concentration),fill='black',color='black',alpha=0.05)+
  geom_boxplot(aes(x=0.1,y=concentration),width=0.2, outlier.shape = NA)+
  scale_y_log10()+
  theme_void()+
  theme(plot.background = element_rect(fill='white',color='white'))+
  theme(legend.position = "none",
        strip.text = element_blank(),plot.margin=margin())

PB

pp <- 
  plot_grid(PA,PB, 
            ncol=2, 
            rel_widths = c(2,0.7),
            label_y = c(1,1),
            align = c("h")
  )
pp



d2_sim <- d2 %>% 
  select(1:3) %>%
  mutate(dw_g=log10(dw_g),concentration=log10(concentration)) %>% 
  group_by(metal) %>% 
  nest() %>% 
  mutate(f=map(.x=data,~lm(concentration~dw_g,data=.))) %>% 
  mutate(data=map2(.x=data,.y=f,~mutate(.data=.x,y_pre=predict(.y,as.data.frame(dw_g))))) %>% 
  unnest(data) %>% 
  mutate(min_y=min(10^concentration,10^y_pre),
         max_y=max(10^concentration,10^y_pre))



PC <- 
  ggplot()+
  theme_classic()+
  geom_point(data=d2, aes(dw_g, concentration, fill = metal),shape = 21, color = "transparent",
             alpha = 0.3,size = 1)+
  geom_point(data=d2, aes(dw_g, concentration, color = metal),shape = 21, 
             fill = "transparent", size = 1)+
  facet_rep_wrap(~metal, scales = "free_y", nrow = 4)+
  scale_x_log10(breaks =  c(0.001,0.01,0.1,1),labels = c("0.001","0.01","0.1","1"))+
  scale_y_log10()+
  annotation_logticks(sides="bl", size = 0.35,
                      short = unit(0.05, "cm"),
                      mid = unit(0.15, "cm"),
                      long = unit(0.25, "cm"))+
  geom_line(data=d2_sim,aes(10^dw_g, 10^y_pre, color = metal),
            linewidth = 0.8)+
  scale_fill_manual(values = c(my_col[2], my_col[2],my_col[2],my_col[2]))+
  scale_color_manual(values = c(my_col[2], my_col[2],my_col[2],my_col[2])) +
  labs(x="Dry weight (g)", 
       title = " ",
       y=NULL
  )+
  geom_text_npc(data=d_conctext4,
                aes(label=conc), npcx =0.1, npcy = 0.25,
                inherit.aes = FALSE,
                parse = T, color = "black",size=3)+
  geom_text_npc(data=d_conctext5,
                aes(label=conc), npcx =0.1, npcy = 0.07,
                inherit.aes = FALSE,
                parse = T, color = "black",size=3)+
  geom_text_npc(data=d_conctext6,
                aes(label=conc), npcx =0.95, npcy = 0.95,
                inherit.aes = FALSE,
                parse = T, color = "black",size=4, fontface="bold")+
  theme(
    plot.margin = margin(5, 0, 5, 0),
    legend.position = "none",
    strip.background = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    axis.line = element_line(linewidth  = 0.35),
    axis.ticks = element_line(linewidth  = 0.35)
    
  )
PC


ask_density <- function(data,min_y,max_y){
  print(data[[1]]$concentration)
  x <- density(data[[1]]$concentration,from=log10(min_y),to=log10(max_y))$x
  y <- density(data[[1]]$concentration,from=log10(min_y),to=log10(max_y))$y
  df <- data.frame(x=x,y=y)
  return(df)
}

d2_den <- d2 %>% 
  select(1:3) %>%
  mutate(dw_g=log10(dw_g),concentration=log10(concentration)) %>% 
  group_by(metal) %>% 
  nest() %>% 
  right_join(d2_sim %>% select(1,6,7),by=c('metal')) %>% 
  unique() %>% 
  mutate(x=pmap(.l=list(data,min_y,max_y),~ask_density(data,min_y,max_y))) %>% 
  select(1,5) %>% 
  unnest(x)


PD <- 
  ggplot(d2)+
  facet_rep_wrap(~metal, scales = "free", nrow = 4)+
  coord_flip()+
  geom_area(data=d2_den,aes(x=10^x,y=y,group=1),color='black',fill=my_col[2],alpha=0.1)+
  geom_boxplot(aes(y=0.1,x=concentration),width=0.2, outlier.shape = NA)+
  scale_x_log10()+
  theme_void()+
  theme(plot.background = element_rect(fill='white',color='white'))+
  theme(legend.position = "none",
        strip.text = element_blank(),plot.margin=margin())

PD


p <- 
  plot_grid(PC,PD, 
            ncol=2, 
            rel_widths = c(2,0.7),
            label_y = c(1,1),
            align = c("h")
  )

p

plot_grid(pp,p, 
          ncol=2, 
          rel_widths = c(1,0.85),
          labels = c("Isotopes 1","Isotopes 2"),
          label_size = 11,
          label_y = c(1.001,1.001),
          label_x = c(0.205,0.12),
          align = c("h")
)

ggsave("new metal v3.png", dpi = 900, width = 458/90, height = 536/90)

