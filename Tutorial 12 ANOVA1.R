#Tutorial 12
#Analisis estadisticos
#Juan Carlos MArtinez Montes

##ANOVA



setwd("E:/MAESTRIA-JC/CLASES/Analisis en Lenguaje R/04 Analisis estadisticos")

library(tidyverse)
library(ggplot2)

ranas <- read.csv("frogs_messy_data.csv")

ranas <- ranas %>% rename(frogs_id = ï..Frogspawn.sample.id) 

ranas_tidy <- gather(ranas, temperature, hatching_time, c(2:4)) %>% 
  mutate(temperature = parse_number(temperature)) %>% 
  select(temperature, hatching_time) %>% na.omit()

#necesitamos que sea factor

ranas_tidy$temperature <- as.factor(ranas_tidy$temperature)

ggplot(ranas_tidy, aes(x= hatching_time)) +
  geom_histogram(stat= "count") +
  geom_vline(aes(xintercept = mean(hatching_time)),
             colour = "red", linetype = "dashed", size = 1) +
  labs(x ="\n Tiempo de eclosion (dias)", y = "Frecuencia \n")

ggplot(ranas_tidy, aes(x = temperature, y = hatching_time)) +
  geom_boxplot() + 
  labs(x = "\nTemperatura (°C)", y = "Tiempo eclosion (dias)") +
  theme_bw() +
  scale_fill_manual(values = c("#73BA9B", "#006633", "#022717")) +
  theme(axis.title = element_text(face = "bold"))

#12_02_ANOVA_eclosion

library(ggpubr)
ggqqplot(ranas_tidy$hatching_time)

#12_03_ANOVA_dist
#Ejecutar ANOVA de 1 via simple


ranas_anova <- aov(hatching_time ~ temperature, data = ranas_tidy)
summary(ranas_anova)

#histograma de residuos y grafico Q-Q normal

par(mfrow = c(1,2))
hist(ranas_anova$residuals)
plot(ranas_anova, which = 2)

#12_04_ANOVA_norm
#comunicar resultados

resumen_stats <- ranas_tidy %>% 
  group_by(temperature) %>% 
  summarise(n = n(), 
             average_hatch = mean(hatching_time),
             SD = sd(hatching_time)) %>% 
              mutate(SE = SD / sqrt(n))

ggplot(resumen_stats, aes(x = temperature,
                          y = average_hatch)) +
  geom_bar(stat = "identity") +
  ylab("Tiempo de eclosión (media)") +
  xlab("Temperatura (°C)") +
  scale_color_manual(values = c("#73BA9B", "#006633", "#022717")) +
  theme_classic2() +
  theme(axis.title = element_text(face = "bold"))

#12_05_ANOVA_barplot