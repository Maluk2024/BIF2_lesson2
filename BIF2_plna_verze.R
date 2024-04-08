install.packages("rvest") 
library(rvest) 
url <- "https://en.wikipedia.org/wiki/List_of_Nobel_laureates" 
wiki <- read_html(url) 
html_text(wiki) 
tables <- html_nodes(wiki, "table") 
tables 
laureates <- html_table(tables[[1]], fill = TRUE) 
laureates 

#jakou datovou strukturu získaná data mají? 

#kolik řadku ma daná struktura? 


#Fitting linear models 
install.packages("ggplot2")
library(ggplot2) 
# závislost mpg (míle na galon) na hmotnosti auta (wt), patrný je trend
ggplot(mtcars, aes(wt, mpg)) + geom_point() 
# lineární model mpg v závislosti na wt
fit <- lm(mpg ~ wt, data = mtcars) 
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + stat_smooth(method = "lm")   # přidání přímky podle lineárního modelu 
plot(fit)
shapiro.test(resid(fit))
# Check model coefficients:
coef(m) 
summary(fit) 

#Koeficienty modelu jsou hledány tak, aby byly minimalizovány odchylky skutečných hodnot od proloženého trendu (model). Nejčastěji se tak děje tzv. metodou nejmenších čtverců, která se snaží minimalizovat součet jednotlivých odchylek umocněných na druhou.

#Predikce
noveWT <- c(3, 4, 5)
predict(fit, newdata = data.frame(wt = noveWT))


#příklad2
library(multcomp)
library(multcompView)
dat <- read.csv("https://raw.githubusercontent.com/ucdavis-bioinformatics-training/2018-September-Bioinformatics-Prerequisites/master/friday/lm_example_data.csv")
head(dat)
str(dat)
oneway.model <- lm(expression ~ treatment, data = dat)
class(oneway.model)
summary(oneway.model)
no.intercept.model <- lm(expression ~ 0 + treatment, data = dat) # '0' means 'no intercept' here
summary(no.intercept.model)

anova<-aov(expression ~ treatment, data = dat)
Summary(anova)

#ONE-WAY ANOVA
library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)
library(moments)

str(chickwts)
df$feed = as.factor(chickwts$feed)

anova <- aov(weight ~ feed, data = chickwts)
qq<-residuals(anova)
shapiro.test(qq)

summary(anova)
tukey <- TukeyHSD(anova)
print(tukey)
cld <- multcompLetters4(anova, tukey)
print(cld)
# table with factors and 3rd quantile
Tk <- group_by(chickwts, feed) %>%
  summarise(mean=mean(weight), quant = quantile(weight, probs = 0.75)) %>%
  arrange(desc(mean))
cld <- as.data.frame.list(cld$feed)
Tk$cld <- cld$Letters
Tk
# boxplot
ggplot(chickwts, aes(feed, weight)) + 
  geom_boxplot()

ggplot(chickwts, aes(feed, weight)) + 
  geom_boxplot() +
  labs(x="Feed Type", y="Weight (g)")

ggplot(chickwts, aes(feed, weight)) + 
  geom_boxplot() +
  labs(x="Feed Type", y="Weight (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(chickwts, aes(feed, weight)) + 
  geom_boxplot() +
  labs(x="Feed Type", y="Weight (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = Tk, aes(x = feed, y = quant, label = cld), size = 3, vjust=-1, hjust =-1)

ggplot(chickwts, aes(feed, weight)) + 
  geom_boxplot(aes(fill = feed), show.legend = FALSE) +
  labs(x="Feed Type", y="Weight (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = Tk, aes(x = feed, y = quant, label = cld), size = 3, vjust=-1, hjust =-1) +
  scale_fill_brewer(palette = "Pastel1")

ggsave("boxplot.png", width = 4, height = 3, dpi = 1000)


#Ukol:je váha piratů závislá na jejich pohlaví?
anova <- aov(weight ~ sex, data = pirates)
qq<-residuals(anova)
shapiro.test(qq)
plot(qq)
summary(anova)
tukey <- TukeyHSD(anova)
print(tukey)
cld <- multcompLetters4(anova, tukey)
print(cld)

#TWO-WAY ANOVA
install.packages("ggpubr")
install.packages("readxl")
library(readxl)
library(ggpubr)
ABA <- read_excel("ABA.xlsx")
View(ABA)                                                                                                             
df2<-ABA
df2

df2$temp = as.factor(df2$temp)
df2$light = as.factor(df2$light)
str(df2)

bxp <- ggboxplot(df2, x = "temp", y = "ABA",  color = "light", palette = c("cadetblue2", "khaki2")) 
bxp

model  <- lm(ABA ~ temp*light, data = df2)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
anova2 <- aov(ABA ~temp*light, data = df2)
summary(anova2)
Tukey2 <- TukeyHSD(anova2)
cld2 <- multcompLetters4(anova2, Tukey2)
cld2


#TWO-WAY ANOVA ukol
#vyzkoušejte si TWO-WAY anovu na datech DW2
