library(tidyverse)
library(ggthemes)
library(janitor) # for clean_names()
library(survival)
library(survminer)

# Set plot theme
old <- theme_set(theme_tufte() + theme(text = element_text(family = "Menlo"),
                                       legend.position = "bottom"))

# Get Data ----

# Source: Data.World (https://data.world/earino/churn)
dat <- read_csv("https://query.data.world/s/lgpjaaj5nafhawrfrjftyevmygpsle") %>% 
  # convert column names to lower case
  clean_names() %>% 
  # convert churn status to integer
  # here we define 'churn' as event
  mutate(churn = ifelse(churn == "Yes", 1, 0))

# does churn customer differ in account length?
(p <- dat %>% 
    ggplot(aes(accountlength, fill = factor(churn))) + 
    geom_density(alpha = .7) + 
    scale_fill_brewer(palette = "Paired", labels = c(`0` = "NO", `1` = "YES")) +
    labs(x = "Account Length", y = "Density", fill = "Churn"))

# faceted by plan type
p + facet_grid(internationalplan ~ voicemailplan, 
               labeller = labeller(.rows = label_both, .cols = label_both))

# Kaplan-Meier Estimate ----

# a survival obj
objsurv <- with(dat, Surv(accountlength, churn))

# basic survival curve 
km.basic <- survfit(objsurv ~ 1, data = dat)
ggsurvplot(km.basic, conf.int = FALSE)

# group by explainatory variable
km.intl <- survfit(objsurv ~ internationalplan, data = dat)
ggsurvplot(km.intl, conf.int = FALSE)

# More Customized Survival Curves ----

# add p-value and confidence interval 
ggsurvplot(km.intl, pval = TRUE, conf.int = TRUE, pval.method = TRUE)

# add risk-table
ggsurvplot(km.intl, 
           data = dat,
           pval = TRUE,
           conf.int = TRUE,
           legend.labs = c("Without Intl. Line", "With Intl. Line"), 
           surv.median.line = "hv", 
           risk.table = TRUE,
           risk.table.y.text.col = T,
           break.time.by = 30, 
           xlab  ="Time (Days)")

# multi-panel
ggsurvplot_facet(km.intl, 
                 data = dat, 
                 facet.by = "voicemailplan", # further split 
                 legend.labs = c("Without Intl. Line", "With Intl. Line"), # mod legend
                 ncol = 1, # make it easier to compare
                 surv.median.line = "hv",
                 ggtheme = theme_light()) # add median line
