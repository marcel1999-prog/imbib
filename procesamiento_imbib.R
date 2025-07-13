install.packages("Rdpack")
install.packages("qpdf", type = "binary")
install.packages(c("magick", "psych", "multcomp", "huito"), dependencies = TRUE)
install.packages("magick", type = "binary")
install.packages("agricolae")
install.packages("dplyr")
install.packages("cowplot")
install.packages("knitr")

library(inti)      # si ger_summary viene de aquí
library(knitr)     # para usar kable()
library(dplyr)     # para usar el pipe %>%


source("https://inkaverse.com/setup.r")
library(tidyverse)
library(agricolae)
library(GerminaR)
library(dplyr)
library(cowplot)

gs <- ("https://docs.google.com/spreadsheets/d/1X5wBN_FrxXZ-XCQjYROYdNjaJcSAwmeBk9zd66utBSI/edit?usp=sharing") %>% 
  as_sheets_id(url)

colnames(fb)

fb <- gs %>% 
  range_read("proces") %>% 
  mutate(across(1:Tratamiento, ~as.factor(.))) %>%
  as.data.frame()

str(fb)

gsm <- ger_summary(SeedN = "seeds"
                   , evalName = "d"
                   , data = fb
)

gsm %>% kable()

# analysis of variance

av <- aov(mgt ~ Tratamiento, data = gsm)
anova(av)

# mean comparison test

mc_mgt <- ger_testcomp(aov = av
                       , comp = c("Tratamiento")
                       , type = "snk")

# data result

mc_mgt$table %>% 
  kable(caption = "Mean germination time comparison")

plot <- mc_mgt$table %>% 
  plot_smr(data = .
           , type = "bar" 
           , x = "Tratamiento"
           , y = "mgt"
           , ylimits = c(0,5, 0.5)
           , sig = "sig"
           , error = "ste"
  )
plot


# analysis of variance

av <- aov(grp ~ Tratamiento, data = gsm)
anova(av)

# mean comparison test

mc_grp <- ger_testcomp(aov = av
                       , comp = c("Tratamiento")
                       , type = "snk")

# data result

mc_grp$table %>% 
  kable(caption = "Mean germination time comparison")

plot <- mc_grp$table %>% 
  plot_smr(data = .
           , type = "bar" 
           , x = "Tratamiento"
           , y = "grp"
           # , ylimits = c(0,3, 0.5)
           , sig = "sig"
           , error = "ste"
  )
plot



git <- ger_intime(Factor = "Tratamiento"
                  , SeedN = "seeds"
                  , evalName = "d"
                  , method = "percentage"
                  , data = fb
)

# data result
git %>% 
  kable(caption = "Cumulative germination by treatment factor")
plot <- git %>% 
  fplot(data = .
        , type = "line"
        , x = "evaluation"
        , y = "mean"
        , group = "Tratamiento"
        , ylimits = c(0, 80, 10)
        , ylab = "Germination ('%')"
        , xlab = "Day"
        , glab = "Dias"
        , color = T
        , error = "ste"
  )+
  scale_x_continuous(limits = c(0, 3))  # <- aquí controlas el eje X
plot

