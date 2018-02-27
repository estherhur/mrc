#install packages
#devtools::install_github("jvcasillas/untidydata")
#devtools::install_github("yihui/xaringan")
library(untidydata)
library(xaringan)
library(plot3D)

library(tidyverse)



#2. load language diversity dataset
str(language_diversity)
head(language_diversity)

unique(language_diversity$Measurement)

#Measurment is a bunch of different variables como separalos. Tidy data..Mutate, mutates
#variables

ld <- language_diversity %>%
  filter(., Continent =='Africa')  %>% 
  spread(., Measurement, Value) %>% 
  select(., country = Country, pop = Population, area = Area, lang = Langs) %>% 
  mutate(., logArea = log(area), 
            logPop = log(pop))

# Check normality, transform, plot

hist(log(ld$area))
hist(log(ld$pop))

#in order to see the name of the country

ld %>% 
  ggplot(., aes(x = logArea, y =lang, label = country )) +
  geom_text()

# to see the difference 
ld %>% 
  ggplot(., aes(x = logArea, y =lang, color = logArea )) +
  geom_point()

# Fit model (MRC, 3 PARAGRAMS) include the interaction * 
# all of the possible interactions

my_mod <- lm(lang ~ logPop + logArea, data = ld)
my_int <- lm(lang ~ logPop + logArea + logPop:logArea, data = ld)


my_int <- lm(lang ~ logPop * logArea, data = ld)

#7 converts to a html presentation


summary(my_mod)
summary(my_int)














