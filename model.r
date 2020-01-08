#!/usr/bin/Rscript
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(ggeffects)
library(ggplot2)

args <- commandArgs(trailingOnly=T)
if (length(args) != 1) {
    writeLines('Usage: ./analysis.r <data-file-name>')
    quit()
}

judgments <- read.csv(args[1])

## normalize the ratings from 1-7 to 0-1
normalize <- function (x) (x-1) / 6
judgments <- judgments %>%
    mutate(rating=normalize(rating),
           confidence=normalize(confidence),
           vivid=normalize(vivid),
           self_credit_blame=normalize(self_credit_blame),
           other_credit_blame=normalize(other_credit_blame),
           self_resp=normalize(self_resp),
           other_resp=normalize(other_resp),
           imagination=factor(imagination, levels=c('Remember', 'WhatIf?', 'Cause')))


data.rem <- judgments %>% subset(imagination=='Remember') %>%
    mutate(model = ifelse(success == 'S',
                          1/2 + confidence/2,       # a number in [0.5, 1.0]
                          1/2 - confidence/2))      # a number in [0.0, 0.5]
data.cf <- judgments %>% subset(imagination=='WhatIf?') %>%
    mutate(model = ifelse(success == 'S',
                          1/2 - confidence/2,      # a number in [0.5, 1.0]
                          1/2 + confidence/2))     # a number in [0.0, 0.5]
data.cause <- judgments %>% subset(imagination=='Cause') %>%
    mutate(model = abs(data.rem$model - data.cf$model))


model.rem <- lm(rating ~ success * condition * confidence, data=data.rem)
data.rem$model <- predict(model.rem)
summary(model.rem)
png('plots/model/remember.png', width=750, height=750)
plot(ggpredict(model.rem, terms=c('confidence', 'condition', 'success')))
dev.off()


model.cf <- lm(rating ~ success * condition * confidence, data=data.cf)
data.cf$model <- predict(model.cf)
summary(model.cf)
png('plots/model/whatif.png', width=750, height=750)
plot(ggpredict(model.cf, terms=c('confidence', 'condition', 'success')))
dev.off()

data.cause$rem <- data.rem$model
data.cause$cf <- data.cf$model
model.cause <- lm(rating ~ rem * cf, data=data.cause)
data.cause$model <- predict(model.cause)
summary(model.cause)
png('plots/model/cause.png', width=750, height=750)
plot(ggpredict(model.cause, terms=c('rem', 'cf')))
dev.off()

judgments <- rbind(data.rem, data.cf, data.cause %>% select(-rem, -cf))

png('plots/model/estimates.png', width=750, height=750)
ggplot(judgments) + aes(x=condition, y=model, color=success) +
    stat_summary(fun.data='mean_cl_boot', geom='pointrange') +
    facet_grid(. ~ imagination) + theme_classic() + ylim(0, 1)
dev.off()
