library(tidyverse)
library(readxl)

library(geomtextpath)
temp<- read_excel('MXD_kuke.xlsx') %>% 
  filter(tissue=='whole') %>% 
  mutate(temp=treat) %>% 
  select(-treat,-metal) %>% 
  mutate(sal=30) %>% 
  filter(temp!=21)
sal <- read_excel('MXD_kuke.xlsx',sheet=2)%>% 
  filter(tissue=='whole')%>% 
  mutate(sal=treat) %>% 
  select(-treat) %>% 
  mutate(temp=21)

df <- rbind(temp,sal)

fit <- nls(ku ~ exp(a) * exp(-b / (temp+273.15)) - c * sal * exp(-b / (temp+273.15)),
           data = df,
           start = list(a = 1, b = 600, c = 1))
summary(fit)


T_values <- seq(min(df$temp), max(df$temp), length.out = 50) 
sal_values <- seq(min(df$sal), max(df$sal), length.out = 50)  
grid <- expand.grid(temp = T_values, sal = sal_values) 


fitted_values <- predict(fit, newdata = grid)


grid$ku <- fitted_values

surface_data <- data.frame(temp = round(grid$temp,5), sal = grid$sal, ku = fitted_values)



dfs <- df %>% filter(sal==30) %>% 
  mutate(ke=log(ke),temp=1/(temp+273.15))

fit_ke <- lm(ke~temp,data=dfs)


T_values_ke <- seq(min(df$temp), max(df$temp), length.out = 50)
sal_values_ke <- seq(min(df$sal), max(df$sal), length.out = 50) 
grid_ke <- expand.grid(temp = 1/(T_values_ke+273.15), sal = sal_values_ke)



fitted_values_ke <- exp(predict(fit_ke, newdata = grid_ke))


grid_ke$ke <- fitted_values_ke

surface_data_ke <- data.frame(temp = round(1/grid_ke$temp-273.15,5), sal = grid_ke$sal, ke = fitted_values_ke)


surface_data_bcf <- surface_data %>% left_join(surface_data_ke,by=c('temp','sal')) %>% 
  mutate(bcf=ku/ke*1000)


colors <- rev(c('#F8FB9B', '#FCA60C', '#EA632A', '#C33B4F',
            '#8C2369', '#520E6D', '#190E3C', '#000004'))


ggplot(surface_data_bcf, aes(x = sal, y = temp, z = bcf)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  geom_contour_filled(bins = 100, alpha = 1) +
  scale_fill_manual(values =  colorRampPalette(colors)(100)) + 

  geom_textcontour(aes(z = bcf,label = round(..level.., 2)), breaks = c(80,100,120,140,160,180),size = 2.5, padding = unit(0.05, "in"),
                   color='white'
  ) +
  geom_contour(aes(z = bcf), breaks = seq(60,200,by=2),size = 0.01,
                   color='white',alpha=1,linetype='dotted'
  ) +
  labs(x = "Salinity(‰)", y = "Temperature(°C)", fill = "ku", title = ~"BCF(L"~kg^-1*")") +
  theme_classic()+
  theme(legend.position = "none",
        axis.line = element_blank())

http://127.0.0.1:32575/graphics/plot_zoom_png?width=473&height=341
ggsave('MXD.png',width = 473/90,height = 341/90,dpi=900)
