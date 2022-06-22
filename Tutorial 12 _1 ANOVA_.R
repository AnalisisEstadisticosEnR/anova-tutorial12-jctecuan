#Tutorial 12.1
#Analisis estadisticos
#Juan Carlos MArtinez Montes

##ANOVA



setwd("E:/MAESTRIA-JC/CLASES/Analisis en Lenguaje R/04 Analisis estadisticos/03 ANOVAs")


library(ggplot2)
library(tidyverse)


diet <- read.csv("stcp-Rdataset-Diet.csv")

#arregflar columnas
#generar columna peso perdido

class(diet$pre.weight)

diet$pre.weight <- as.numeric(diet$pre.weight)

diet$weight.loss <- diet$pre.weight - diet$weight6weeks

#cambiar columnas dieta y genero a factor

diet$Diet <- as.factor(diet$Diet)

diet$diet.type <- recode_factor(diet$Diet, "1" = "A", "2" = "B", "3" = "C")

diet$gender <- as.factor(diet$gender)

diet$Gender <- recode_factor(diet$gender, "0" = "Female", "1" = "Male")

#Graficar datos

boxplot(weight.loss ~ diet.type, data= diet, col = "white",
        ylab ="Perdida de peso (kg)", xlab = "Tipo de dieta")
abline(h = 0, col ="blue")

#T12.1_boxplot_dietas.png

#ANOVA o Kruskal Wallis

#revisar distribucion de los datos

hist(diet$weight.loss, col = "green", main = "")

#T12.1_02_hist_dietas.png

lines(seq(-4, 10, 0.1), length(diet$weight.loss) * dnorm(seq(-4,10,0.1),
                                                         mean(diet$weight.loss),
                                                         sqrt(var(diet$weight.loss))))

shapiro.test(diet$weight.loss)

diet.fisher <- aov(weight.loss ~ diet.type, data = diet)
summary(diet.fisher)

#Revision del modelo

#funcion tapply() permite aplicar cualquier funcion a los datos

mean_group <- tapply(diet$weight.loss, diet$diet.type, mean)
mean_group

median_group <- tapply(diet$weight.loss, diet$diet.type, median)
median_group

#Generar columnas para los residuos de la media y la mediana

diet$resid.mean <- (diet$weight.loss  - mean_group[as.numeric(diet$diet.type)])
diet$resid.median <- (diet$weight.loss  - median_group[as.numeric(diet$diet.type)])

diet[1:10]

#visualizar residuos

par(mfrow = c(1,2), mar = c(4.5, 4.5, 2,0))

#par() permite colocar dos graficos juntos

boxplot(resid.mean ~ diet.type, data = diet, main = "Residual boxplot per group",
        col = "light gray", xlab = "Diet type", ylab = "Residuals")

abline(h = 0, col = "blue")

col_group = rainbow(nlevels(diet$diet.type))

qqnorm(diet$resid.mean, col = col_group[as.numeric(diet$diet.type)])
qqline(diet$resid.mean)
legend("top", legend = levels(diet$diet.type), col = col_group,
       pch = 21, ncol = 3, box.lwd = NA)

#T12.1_03_norm_resid.png

shapiro.test(diet$resid.mean)

#prueba homogeneidad de varianzas

bartlett.test(diet$resid.mean ~ as.numeric(diet$diet.type))

#COMPARACIONES MULTIPLES

#prueba Tukey HSD

plot(TukeyHSD(diet.fisher))

#T12_04_Tuckey.png

t.test(weight.loss ~ diet.type, data = diet[diet$diet.type!="C",], var.equal = TRUE)

#OTRO EJEMPLO

library(tidyverse)
library(ggpubr)
library(rstatix)

#ANOVA de un factor

data("PlantGrowth")
Plantas <- PlantGrowth
head(Plantas)

levels(Plantas)

Plantas$group <- factor(Plantas$group, levels = c("ctrl", "trt1", "trt2"))

#Visualizar

ggboxplot(Plantas, x = "group", y = "weight",
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

#T12.1_04_boxplot_plantas.png

#Checar supuestos

#valores atipicos, pueden verse con graficas o corriendo funcion identify_outliers()

Plantas %>% group_by(group) %>% identify_outliers(weight)

#normalidad

#metodo 1
##1 Analizar residuos del ANOVA

#CREAR MODELO LINEAL

modelo <- lm(weight ~ group, data = Plantas)

#crear grafica tipo QQ de los residuos

ggqqplot(residuals(modelo))

#T12.1_05_norm_residuos_plantas.png

#Shapiro

shapiro_test(residuals(modelo))

##2 metodo 2
## normalidad por grupo

Plantas %>% group_by(group) %>% shapiro_test(weight)

#muestras mayores a 50 n es mejor grafica QQ

ggqqplot(Plantas, x = "weight", facet.by = "group")

#Homogeneidad de la varianza
#Con grafica

plot(modelo, 1)

#T12.1_06__homo_var.png

#Con una prueba

Plantas %>%  levene_test(weight ~ group)

#T12.1_07__homo_var2.png

#En caso de no encontrar homogeneidad de varianzas, se hace prueba de Welch
#welch_anova_test() en el paquete rstatix

#Ahora si, ANOVA

res.aov <- aov(weight ~ group, data = Plantas)
summary(res.aov)

##           Df Sum Sq Mean Sq F value Pr(>F)  
#group        2  3.766  1.8832   4.846 0.0159 *
#Residuals   27 10.492  0.3886   

#Df= nunmero de piezas de informacion independientes utilizadas para calcular la estimacion
#o numero de valores que pueden variar libremente en un conjunto de datos
#Es numero de observaciones - 1 (n -1)

#Sum sq = Suma de la diferencia al cuadrado de cada valor real con respecto al valor previsto

#Mean sq = es en realidad la varianza

#F= Si las medias de los grupos de dato son significativamente diferentes

#################################################################################
##PRUEBA POST-HOC

#COMPARA GRUPOS DE DOS EN DOS, CON UNA PRUEBA DE t

#Tukey y Dunnet aumen homogeneidad de varianza
#Si no hay homogeneidad, se realiza la prueba de Games-Howell

#DUNNETT 

#Se realiza cuando hay un grupo de referencia (Control)

library(DescTools)
DunnettTest(Plantas$weight, Plantas$group)

TukeyHSD(res.aov)
plot(TukeyHSD(res.aov, conf.level = .95), las = 2)

#T12.1_07__tukey_plantas.png

#Calcular media de cada grupo

mean <- aggregate(Plantas$weight, by = list(Plantas$group), mean)
sd <- aggregate(Plantas$weight, by = list(Plantas$group), sd)
resultados <- cbind(mean, sd)

names(resultados) <- c("Tratamiento", "mean_distance","Tratamiento", "sd")

#Visualizacion

aovTest <- Plantas %>% anova_test(weight ~ group)
pwc <- PlantGrowth %>% tukey_hsd(weight ~ group)

#Grafica con valores p

pwc <- pwc %>% add_xy_position(x = "group")

ggboxplot(Plantas, x = "group", y = "weight") +
  stat_pvalue_manual(pwc, hide.ns = T) +
  labs(subtitle = get_test_label(aovTest, detailed = T),
       caption = get_pwc_label(pwc))

#T12.1_08__tukey_boxplot.png