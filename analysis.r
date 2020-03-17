#!/usr/bin/Rscript
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)

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


judgments_long <- judgments %>%
    pivot_longer(self_credit_blame:other_resp,
                 names_pattern='^([^_]*)_(.*)',
                 names_to=c('object', 'measure'),
                 values_to='response') %>%
    pivot_wider(names_from=measure, values_from=response)
model.resp <- lmer(resp ~ success * condition * object + (1|id),
          data=judgments_long)
summary(model.resp)

png('plots/responsibility.png', width=750, height=750)
plot(ggpredict(model.resp, terms=c('condition', 'success', 'object'))) +
    theme_classic() + ylim(0, 1)
dev.off()


quit()


library(GGally)
for (c in levels(judgments$condition)) {
    for (s in levels(judgments$success)) {
        print(ggcorr(subset(judgments, condition==c & success==s & imagination=='Cause',
                            select=c('rating', 'self_credit_blame', 'self_resp',
                                     'other_credit_blame', 'other_resp')),
                     label=TRUE) + ggtitle(sprintf('Condition: %s, %s', c, s)))
    }
}
quit()

ggcorr(subset(judgments, condition=='the goalie' & imagination=='Cause',
              select=c('rating', 'self_credit_blame', 'self_resp',
                       'other_credit_blame', 'other_resp')), label=TRUE)
##quit()


## Show the sample size
writeLines(sprintf('# of participants: %d', length(unique(judgments$id))))
writeLines('Design: 2 (success: score/miss) x 3 (imagination: Remember, WhatIf?, Cause) x 2 (condition: ball/goalie)')
table(judgments[,c('success', 'imagination', 'condition')])

## Rating analysis
model.ratings <- lmer(rating ~ success * imagination * condition + (1|id), data=judgments)
#summary(model.ratings)
png('plots/ratings.png', width=750, height=750)
plot(ggpredict(model.ratings, terms=c('condition', 'success', 'imagination'))) +
    theme_classic() + ylim(0, 1)
dev.off()



## Spread the self/other credit_blame/resp columns into two rows
judgments_long <- judgments %>%
    pivot_longer(self_credit_blame:other_resp,
                 names_pattern='^([^_]*)_(.*)',
                 names_to=c('object', 'measure'),
                 values_to='response') %>%
    pivot_wider(names_from=measure, values_from=response)

model.ratings <- lm(cbind(rating, credit_blame, resp) ~
                        success * condition * object,
                    data=subset(judgments_long, imagination == 'Cause'))
summary(model.ratings)

lm(rating ~ success * condition,
   data=subset(judgments_long, imagination == 'Cause')) %>%
    ggpredict(terms=c('condition')) %>%
    plot() + theme_classic() + ylim(0, 1)
quit()

## Blame analysis
model.blame <- lmer(credit_blame ~ success * condition * object + (1|id),
          data=judgments_long)
summary(model.blame)

png('plots/blame.png', width=750, height=750)
plot(ggpredict(model.blame, terms=c('condition', 'success', 'object'))) +
    theme_classic() + ylim(0, 1)
dev.off()


## Responsibility analysis
model.resp <- lmer(resp ~ success * condition * object + (1|id),
          data=judgments_long)
summary(model.resp)

png('plots/responsibility.png', width=750, height=750)
plot(ggpredict(model.resp, terms=c('condition', 'success', 'object'))) +
    theme_classic() + ylim(0, 1)
dev.off()
