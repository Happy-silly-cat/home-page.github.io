library(tidyverse)
library(readxl)
library(ggfun)
library(cowplot)
library(scales)
library(ggh4x)

my_col <- ggsci::pal_npg()(10)
scales::show_col(my_col)

d_0 <- read_excel("Mussel.xlsx", sheet = 1)
names(d_0)
head(d_0)

d_Label_P <- d_0 |>
  filter(Species == "Perna viridis")|>
  filter(Treatment == "label")
head(d_Label_P)

d_Lab_P <- d_0 |>
  filter(Species == "Perna viridis")|>
  filter(Treatment == "Exposure_labratory")

d_Field_P <- d_0 |>
  filter(Species == "Perna viridis")|>
  filter(Treatment == "exposure_field")

t1 <- d_Label_P[, 3]
t2 <- d_Lab_P[, 3]
t3 <- d_Field_P[, 3]

y1 <- d_Label_P[, 8]
y2 <- d_Lab_P[, 8]
y3 <- d_Field_P[, 8]


# Define an objective function to minimize
objective_function <- function(params) {
  # Extract parameters
  g1 <- params[1]
  g2 <- params[2]
  g3 <- params[3]
  m <- params[4]
  
  # Calculate the fitted values for the common point
  #y_common <- m1 * x1 + b
  
  # Calculate the sum of squared errors for each line
  error1 <- sum((log(y1) - (g1 * (t1-8) + m))^2)
  error2 <- sum((log(y2) - (g2 * (t2-8) + m))^2)
  error3 <- sum((log(y3) - (g3 * (t3-8) + m))^2)
  
  # Return the objective function (sum of squared errors)
  objective <- error1 + error2 + error3
  return(objective)
}

# Optimization (using optim)
initial_params <- c(1, 1, 1, 1)  # Initial parameter guesses
opt_result <- optim(initial_params, fn = objective_function, method = "L-BFGS-B")

# Extract the best-fit parameters
best_params <- opt_result$par
g1 <- best_params[1]
g2 <- best_params[2]
g3 <- best_params[3]
m <- best_params[4]
g1# -0.03673244
g2# -0.02482332
g3# -0.005295113
m#-2.331587

s1 <- exp(m)
s2 <- m-g1*8
s3 <- exp(s2)


W1 <- 0.1303246 * exp(-0.03673244*t1)
W2 <- 0.09714147 * exp(-0.02482332*(t2-8))
W3 <- 0.09714147* exp(-0.005295113*(t3-8))

d4 <- data.frame(t1, W1)
d5 <- data.frame(t2, W2)
d6 <- data.frame(t3, W3)

label_g1 <- data.frame(labels= "italic(g)[1]== -0.037 ~d^{-1}")
label_g2 <- data.frame(labels= "italic(g)[2]== -0.005 ~d^{-1}")
label_g3 <- data.frame(labels= "italic(g)[3]== -0.025 ~d^{-1}")

d_Perna <- d_0|>
  filter(Species == "Perna viridis")

d_Perna_meansd <- d_Perna|>
  group_by(Time_d, Treatment)|>
  summarise(mean_conc = mean(dry_weight_g), sd_conc = sd(dry_weight_g))

p_perna <- ggplot()+
  theme_classic()+
  geom_point(data = filter(d_Perna, Treatment == "label"),
             aes(Time_d, dry_weight_g), color = "#228833",
             alpha = 0.4,size = 0.8, shape = 21,
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Perna, Treatment == "Exposure_labratory",
                           Time_d > 8),
             aes(Time_d, dry_weight_g), color = "grey",
             alpha = 0.4,size = 0.8, shape = 21,
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Perna, Treatment == "exposure_field",
                           Time_d > 8), shape = 21,
             aes(Time_d, dry_weight_g), color = "#000000",
             alpha = 0.4,size = 0.8,
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Perna_meansd, Treatment == "label"),
             aes(Time_d, mean_conc), color = "#228833",
             size = 1.5, 
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Perna_meansd, Treatment == "Exposure_labratory",
                           Time_d > 8), 
             aes(Time_d, mean_conc), color = "grey",
             size = 1.5,
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Perna_meansd, Treatment == "exposure_field",
                           Time_d > 8), 
             aes(Time_d, mean_conc), color = "#000000",
             size = 1.5, 
             position = position_jitter(width = 0.1, height=0))+
  geom_line(data = d4,
              aes(Time_d, Time_d.1), color = "#228833")+
  geom_line(data = d5,
              aes(Time_d, Time_d.1), color = "grey")+
  geom_line(data = d6,
              aes(Time_d, Time_d.1), color = "#000000")+
  labs(y=expression(~"Individual dry weight (g)"), 
       x="Time (d)")+
  theme(strip.background = element_blank())+
  geom_text(x=12.8, y=0.213, data=label_g1, 
            aes(label=labels), parse = T,
            size=2.55,color = "#228833")+
  geom_text(x=12.8, y=0.192, data=label_g2, 
            aes(label=labels), parse = T,
            size=2.55,color = "#000000")+
  geom_text(x=12.81, y=0.17, data=label_g3, 
            aes(label=labels), parse = T,
            size=2.55,color = "grey")+
  annotate(geom="text", label=~italic(Perna~viridis),
           x=-Inf, y=Inf,  hjust = -0.2, vjust = 1,
           size = 3.5)+
  scale_y_continuous(limits = c(0, NA))


p_perna


names(d_0)
head(d_0)

d_Label_M <- d_0 |>
  filter(Species == "Mytella strigata")|>
  filter(Treatment == "label")
head(d_Label_M)

d_Lab_M <- d_0 |>
  filter(Species == "Mytella strigata")|>
  filter(Treatment == "Exposure_labratory")

d_Field_M <- d_0 |>
  filter(Species == "Mytella strigata")|>
  filter(Treatment == "exposure_field")

t4 <- d_Label_M[, 3]
t5 <- d_Lab_M[, 3]
t6 <- d_Field_M[, 3]

y4 <- d_Label_M[, 8]
y5 <- d_Lab_M[, 8]
y6 <- d_Field_M[, 8]


# Define an objective function to minimize
objective_function <- function(params) {
  # Extract parameters
  g4 <- params[1]
  g5 <- params[2]
  g6 <- params[3]
  m <- params[4]
  
  # Calculate the fitted values for the common point
  #y_common <- m1 * x1 + b
  
  # Calculate the sum of squared errors for each line
  error1 <- sum((log(y4) - (g4 * (t4-8) + m))^2)
  error2 <- sum((log(y5) - (g5 * (t5-8) + m))^2)
  error3 <- sum((log(y6) - (g6 * (t6-8) + m))^2)
  
  # Return the objective function (sum of squared errors)
  objective <- error1 + error2 + error3
  return(objective)
}

# Optimization (using optim)
initial_params <- c(1, 1, 1, 1)  # Initial parameter guesses
opt_result <- optim(initial_params, fn = objective_function, method = "L-BFGS-B")

# Extract the best-fit parameters
best_params <- opt_result$par
g4 <- best_params[1]
g5 <- best_params[2]
g6 <- best_params[3]
m <- best_params[4]
g4# -0.005343763
g5# -0.0108207
g6# -0.02118335
m#-2.567868

s4 <- exp(m)
s5 <- m-g4*8
s6 <- exp(s5)


W4 <- 0.08004888 * exp(-0.005343763*t4)
W5 <- 0.0766989 * exp(-0.0108207*(t5-8))
W6 <- 0.0766989* exp(-0.02118335*(t6-8))

d7 <- data.frame(t4, W4)
d8 <- data.frame(t5, W5)
d9 <- data.frame(t6, W6)

label_g4 <- data.frame(labels= "italic(g)[1]== -0.005 ~d^{-1}")
label_g5 <- data.frame(labels= "italic(g)[2]== -0.021 ~d^{-1}")
label_g6 <- data.frame(labels= "italic(g)[3]== -0.011 ~d^{-1}")

d_Mytella<- d_0|>
  filter(Species == "Mytella strigata")

d_Mytella_meansd <- d_Mytella|>
  group_by(Time_d, Treatment)|>
  summarise(mean_conc = mean(dry_weight_g), sd_conc = sd(dry_weight_g))

p_Mytella <- ggplot()+
  theme_classic()+
  geom_point(data = filter(d_Mytella, Treatment == "label"),
             aes(Time_d, dry_weight_g), color = "#228833",
             alpha = 0.4,size = 0.8, shape = 21,
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Mytella, Treatment == "Exposure_labratory",
                           Time_d > 8),
             aes(Time_d, dry_weight_g), color = "grey",
             alpha = 0.4,size = 0.8, shape = 21,
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Mytella, Treatment == "exposure_field",
                           Time_d > 8), shape = 21,
             aes(Time_d, dry_weight_g), color = "#000000",
             alpha = 0.4,size = 0.8,
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Mytella_meansd, Treatment == "label"),
             aes(Time_d, mean_conc), color = "#228833",
             size = 1.5, 
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Mytella_meansd, Treatment == "Exposure_labratory",
                           Time_d > 8), 
             aes(Time_d, mean_conc), color = "grey",
             size = 1.5,
             position = position_jitter(width = 0.1, height=0))+
  geom_point(data = filter(d_Mytella_meansd, Treatment == "exposure_field",
                           Time_d > 8), 
             aes(Time_d, mean_conc), color = "#000000",
             size = 1.5,
             position = position_jitter(width = 0.1, height=0))+
  geom_line(data = d7,
            aes(Time_d, Time_d.1), color = "#228833")+
  geom_line(data = d8,
            aes(Time_d, Time_d.1), color = "grey")+
  geom_line(data = d9,
            aes(Time_d, Time_d.1), color = "#000000")+
  labs(y= NULL, 
       x="Time (d)")+
  theme(strip.background = element_blank())+
  geom_text(x=12.8, y=0.163, data=label_g4, 
            aes(label=labels), parse = T,
            size=2.55,color = "#228833")+
  geom_text(x=12.8, y=0.147, data=label_g5, 
            aes(label=labels), parse = T,
            size=2.55,color = "#000000")+
  geom_text(x=12.81, y=0.132, data=label_g6, 
            aes(label=labels), parse = T,
            size=2.55,color = "grey")+
  annotate(geom="text", label=~italic(Mytella~strigata),
           x=-Inf, y=Inf,  hjust = -0.2, vjust = 1,
           size = 3.5)+
  scale_y_continuous(limits = c(0, NA))


p_Mytella

p_co <- plot_grid(p_perna, p_Mytella,
                  rel_widths = c(1,1))

p_co
ggsave("growth rate_1.png", width = 400/90, height = 200/90, dpi = 900)

ggsave("growth rate_1.pdf", width = 400/90, height = 200/90, dpi = 900)
