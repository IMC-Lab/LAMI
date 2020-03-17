
######Kristina's below######
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(car)
library(gridGraphics)
library(emmeans)
library(sjPlot)

#set the directory
setwd('/Users/kristinakrasich/Documents/ResearchProjects/Counterfactual/LAMI')

#read in the data
judgements<-read.table('LAMI_MTurk_processed.txt',header=TRUE,sep='\t')

#double check data
summary(judgements)

## normalize the ratings from 1-7 to 0-1
normalize <- function (x) (x-1) / 6
judgements <- judgements %>%
  mutate(rating=normalize(rating),
         confidence=normalize(confidence),
         vividness=normalize(vividness),
         self_credit_blame=normalize(self_credit_blame),
         other_credit_blame=normalize(other_credit_blame),
         self_resp=normalize(self_resp),
         other_resp=normalize(other_resp),
         imagination=factor(imagination, levels=c('Outcome assessment', 'Counterfactual thinking', 'Causal reasoning')))

## normalize the ratings from 1-5 to 0-1
normalize.vivid <- function (x) (x-1) / 4
judgements <- judgements %>%
  mutate(rating=normalize.vivid(vividness));

#Participants
## Show the sample size
writeLines(sprintf('# of participants: %d', length(unique(judgements$id))))
writeLines('Design: 2 (Outcome: score/miss) x 3 (imagination: Outcome assessment, Counterfactual thinking, Causal reasoning) x 2 (condition: ball/goalie)')
table(judgements[,c('Outcome', 'imagination', 'condition')])

judgements$imagination <- relevel(judgements$imagination, ref = 'Outcome assessment');
judgements.goalie <- subset(judgements, judgements$condition == "goalie");
judgements.ball <- subset(judgements, judgements$condition == "ball");


#Vividness
#run the models: vividness
vividness.ball <- lmer(vividness ~ Outcome * imagination + (1|id), judgements.ball)
summary(vividness.ball)
emmeans(vividness.ball, pairwise ~ imagination, reverse=FALSE)$contrasts

vividness.goalie <- lmer(vividness ~ Outcome * imagination + (1|id), judgements.goalie)
summary(vividness.goalie)
emmeans(vividness.goalie, pairwise ~ imagination, reverse=FALSE)$contrasts

#plot results
plot1 <- plot(ggpredict(vividness.ball, terms=c('imagination', 'Outcome'))) +
  xlab("Retrospective Thoughts") + ylab("Vividness Ratings") + 
  theme_classic(
    base_size = 10,
    base_family = "Arial") + ylim(0.2, 1)
plot1 

plot2 <- plot(ggpredict(vividness.goalie, terms=c('imagination', 'Outcome'))) +
  xlab("Retrospective Thoughts") + ylab("Vividness Ratings") + 
  theme_classic(
    base_size = 10,
    base_family = "Arial") + ylim(0.2, 1)
plot2

p1title<-textGrob("Retrospectively thinking about the ball", gp=gpar(fontface="bold"))
p2title<-textGrob("Retrospectively thinking about the goalie", gp=gpar(fontface="bold"))

p1<-grid.arrange(plot1,nrow=1, top = p1title)
p2<-grid.arrange(plot2,nrow=1, top = p2title)

g <- arrangeGrob(p1,p2,nrow=2)
ggsave(file="FinalplotVividness.png",g)

#Table of results
Table2 <- sjPlot::tab_model(vividness.ball, vividness.goalie, show.se = F, digits = 3)

#Event judgements
## Rating analysis ball
model.ratings.ball <- lmer(rating ~ Outcome * imagination + vividness + (1|id), data=judgements.ball)
summary(model.ratings.ball)
Anova(model.ratings.ball)

plot1 <- plot(ggpredict(model.ratings.ball, terms=c('imagination', 'Outcome'))) +
  xlab("Retrospective Thoughts") + ylab("Event Ratings") + 
  theme_classic(
    base_size = 10,
    base_family = "Arial")
plot1  

## Rating analysis goalie
model.ratings.goalie <- lmer(rating ~ Outcome * imagination + vividness + (1|id), data=judgements.goalie)
summary(model.ratings.goalie)
Anova(model.ratings.goalie)

plot2 <- plot(ggpredict(model.ratings.goalie, terms=c('imagination', 'Outcome'))) +
  xlab("Retrospective Thoughts") + ylab("Event Ratings") + 
  theme_classic(
    base_size = 10,
    base_family = "Arial")
plot2  

p1title<-textGrob("Retrospectively thinking about the ball", gp=gpar(fontface="bold"))
p2title<-textGrob("Retrospectively thinking about the goalie", gp=gpar(fontface="bold"))

p1<-grid.arrange(plot1,nrow=1, top = p1title)
p2<-grid.arrange(plot2,nrow=1, top = p2title)

g <- arrangeGrob(p1,p2,nrow=2)
ggsave(file="Finalplot.png",g)




######Kevin's original below######

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


## Show the sample size
writeLines(sprintf('# of participants: %d', length(unique(judgments$id))))
writeLines('Design: 2 (success: score/miss) x 3 (imagination: Remember, WhatIf?, Cause) x 2 (condition: ball/goalie)')
table(judgments[,c('success', 'imagination', 'condition')])

## Rating analysis
model.ratings <- lmer(rating ~ success * imagination * condition + (1|id), data=judgments)
summary(model.ratings)
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


## Blame analysis
model.blame <- lmer(credit_blame ~ success * condition * object + (1|id),
          data=judgments_long)
summary(model.blame)

png('plots/blame.png', width=750, height=750)
plot(ggpredict(model.blame, terms=c('condition', 'object', 'success'))) +
    theme_classic() + ylim(0, 1)
dev.off()

## Responsibility analysis
model.resp <- lmer(resp ~ success * condition * object + (1|id),
          data=judgments_long)
summary(model.resp)

png('plots/responsibility.png', width=750, height=750)
plot(ggpredict(model.resp, terms=c('condition', 'object', 'success'))) +
    theme_classic() + ylim(0, 1)
dev.off()
