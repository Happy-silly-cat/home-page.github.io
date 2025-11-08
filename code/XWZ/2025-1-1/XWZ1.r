library(readxl)
library(viridis)
library(tidyverse)
library(deSolve)
library(readxl)
library(latex2exp)
library(patchwork)
library(cowplot)
tb <- read_excel('ACR.xlsx',sheet=2) %>% 
  mutate(Conditions=as.factor(Conditions)) %>% 
  mutate(Reference=factor(Reference,levels=c(unique(.$Reference))))

tbs <- tb %>% 
  mutate(species=factor(tb$Species,levels=c(
    "Potamocorbula laevis",'Ruditapes philippinarum',
    "Daphnia magna" ,"Tetrahymena thermophila"
  )))

tbs$species <- factor(tb$Species,levels=c(
  "Potamocorbula laevis",'Ruditapes philippinarum',
  "Daphnia magna" ,"Tetrahymena thermophila"
))



tb_color <- tbs %>% filter(flag==1) %>% 
  mutate(Reference=ifelse(
    Reference=='Chen et al., 2017',1,4
  ))


tb_color_nano <- tbs %>% filter(Contaminant=='Nanoplastics') %>% 
  mutate(Reference=case_when(
    Reference=='Qian et al., 2025'~7,
    Reference=='Zhang et al., 2025'~8,
    Reference=='Wang et al., 2025'~9,
  )) %>% 
  mutate(Conditions=case_when(
    Conditions==30~'5',
    Conditions==60~'8',
    Conditions==80~'10',
    Conditions==100~'15',
    Conditions==120~'20',
    Conditions==170~'25',
    Conditions==200~'30',
  )) %>% 
  filter(Reference==8)



tb_non <- tbs  %>% 
  mutate(ACR=ifelse(Reference%in%c('Chen et al., 2017','Tan et al., 2019',
                                   'Zhang et al., 2025'
                                   ),0.01,ACR))



p <- ggplot(tbs)+
  theme_classic()+
 geom_point(data=tb_color,aes(x=Reference,y=ACR,
                               shape=species,fill=as.numeric(Conditions),
                               color=as.numeric(Conditions)),size=3,
             show.legend = F)+
  geom_point(data=tb_non,aes(x=Reference,y=ACR,shape=species),size=3,color='black',
             position = position_jitter(width = 0),show.legend = F)+
  geom_point(data=tbs %>% mutate(ACR=ifelse(Reference=='This study',26.84,0.01)),aes(x=Reference,y=ACR,shape=species),size=3,
             ,color='grey50')+
  scale_x_discrete(labels=paste('[',seq(1,9,by=1),']',sep=''))+
  geom_segment(x=3.5,xend=3.5,y=-Inf,yend=Inf,linetype='solid', color = "grey50", size = 0.25)+
  geom_segment(x=6.5,xend=6.5,y=-Inf,yend=Inf,linetype='solid', color = "grey50", size = 0.25)+
  scale_y_log10(limit=c(1,300))+
  annotation_logticks(side='l')+
  scale_shape_manual(values = c(21,8,24,23),
                     label=c(~italic('Potamocorbula laevis'),
                             ~italic('Ruditapes philippinarum'),
                             ~italic('Daphnia magna'),
                             ~italic('Tetrahymena thermophila')))+
  scale_color_viridis(option='viridis',direction = -1)+
  scale_fill_viridis(option = "viridis",direction = -1,alpha=0.2)+
  ggnewscale::new_scale_fill()+
  ggnewscale::new_scale_color()+
  geom_point(data=tb_color_nano,aes(x=Reference,y=ACR,
                                    shape=species,fill=as.factor(Conditions),
                                    color=as.factor(Conditions)),show.legend = F,size=3)+
  scale_color_manual(values = c('#000004','#240C4F','#5D126E',
                               '#932667','#C73E4C','#ED6925',
                               '#FCA50A'))+
  scale_fill_manual(values = alpha(c('#000004','#240C4F','#5D126E',
                               '#932667','#C73E4C','#ED6925',
                               '#FCA50A'),0.2))+
  labs(fill='',x='',shape='')+
  guides(shape = guide_legend(nrow = 2)) +
  theme(legend.position = 'top',
        plot.margin=margin(0,0,0,0),
        #legend.background = element_rect(fill='black'),
        legend.margin = margin(10,0,-10,0),
        legend.key.spacing.y = unit(0,'cm'),
        legend.text = element_text(color='grey50'),
        axis.title.y =element_text(vjust=-1.5))+
  geom_text(data=data.frame(x=c(1.75,4.8,8.1),y=c(300,300,300),
                            label=c('Copper','Cadmium','Nanoplastics')),
            aes(x=x,y=y,label=label))


p

tb_ref <- data.frame(x=0,y=seq(0.9,0.1,by=-0.1),
                     ref=paste('[',seq(1,9,by=1),']',unique(tbs$Reference),sep=''))

tb_ref
p2 <- ggplot(tb_ref)+
  theme_classic()+
  xlim(0,1)+
  scale_x_continuous(limits = c(0,1),expand=c(0,0))+
  geom_text(aes(x=x,y=y,label=ref),size=3,color='gray50',hjust=0)+
  geom_text(aes(x=0,y=1.0,label='Reference'), color = "grey50",fontface= "plain",hjust=0)+
  theme(axis.title.y = element_blank(), # 去除x轴标题
        axis.text.y  = element_blank(), # 去除x轴标签
        axis.ticks.y = element_blank(), # 去除x轴ticks
        axis.ticks.length.y = unit(0, "mm"), # ticks去除的关键，ticks的绘图区域调为0
        plot.margin         = margin(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.background = element_blank())



p2

pfix <- plot_grid(p,p2,align = 'hv',rel_widths = c(1,0.3))

pfix


pfix <- plot_grid(p,p2,align = 'hv',rel_widths = c(1,0.5))

pfix




ggsave('ACR_literature_2.png',width = 467/90,height = 299/90,dpi=900)










