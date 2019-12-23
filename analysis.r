#!/usr/bin/Rscript
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)

library(ggeffects)

args <- commandArgs(trailingOnly=T)
if (length(args) != 1) {
    writeLines('Usage: ./pre-process.r <data-file-name>')
    quit()
}

judgments <- read.csv(args[1])
judgments$imagination <- factor(judgments$imagination,
                                levels=c('Remember', 'WhatIf?', 'Cause'))

## Rating analysis
model.ratings <- lmer(rating ~ success * imagination * condition + (1|id), data=judgments)
summary(model.ratings)

png('plots/ratings.png')
plot(ggpredict(model.ratings, terms=c('condition', 'success', 'imagination')))
dev.off()


## Spread the self/other credit_blame/resp columns into two rows
judgments_long <- judgments %>%
    pivot_longer(self_credit_blame:other_resp,
                 names_pattern='^([^_]*)_(.*)',
                 names_to=c('object', 'measure'),
                 values_to='response') %>%
    pivot_wider(names_from=measure, values_from=response)


## Blame analysis
model.blame <- lmer(credit_blame ~ success * condition * object + (1|id),
          data=judgments_long)
summary(model.blame)

png('plots/blame.png')
plot(ggpredict(model.blame, terms=c('condition', 'object', 'success')))
dev.off()

## Responsibility analysis
model.resp <- lmer(resp ~ success * condition * object + (1|id),
          data=judgments_long)
summary(model.resp)

png('plots/responsibility.png')
plot(ggpredict(model.resp, terms=c('condition', 'object', 'success')))
dev.off()
