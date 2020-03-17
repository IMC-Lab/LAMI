######Kristina's below######
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(grid)
library(gridExtra)
library(car)
library(gridGraphics)
library(emmeans)
library(sjPlot)

#set the directory
setwd('/Users/kristinakrasich/Documents/ResearchProjects/Counterfactual/LAMI')

#read in the data
judgments <- read.csv('data/LAMI_MTurk_processed.csv', header=TRUE)

#double check data
summary(judgments)

## normalize the ratings from 1-7 or 1-5 to 0-1
normalize <- function (x) (x-1) / 6
normalize.vivid <- function (x) (x-1) / 4
judgments <- judgments %>%
  mutate(rating=normalize(rating),
         confidence=normalize(confidence),
         vividness=normalize.vivid(vividness),
         self_credit_blame=normalize(self_credit_blame),
         other_credit_blame=normalize(other_credit_blame),
         self_resp=normalize(self_resp),
         other_resp=normalize(other_resp),
         imagination=factor(imagination,
                            levels=c('outcome', 'counterfactual', 'causal')),
         condition=factor(condition,
                            levels=c('ball', 'goalie')))

## Participants
## Show the sample size
writeLines(sprintf('# of participants: %d', length(unique(judgments$id))))
writeLines('Design: 2 (Outcome: score/miss) x 3 (imagination: Outcome assessment, Counterfactual thinking, Causal reasoning) x 2 (condition: ball/goalie)')
table(judgments[,c('outcome', 'imagination', 'condition')])


## Vividness
## run the models: vividness
model.vividness <- lmer(vividness ~ outcome * imagination * condition + (1 | id),
                        data=judgments)
summary(model.vividness)
anova(model.vividness)

## print out marginal means and contrasts over imagination
emmeans.vividness <- emmeans(model.vividness, ~ outcome * imagination * condition)
emmeans.vividness
emmeans(model.vividness, pairwise ~ imagination)

## plot results
plot1 <- emmeans.vividness %>% as.data.frame %>%
    subset(condition == 'ball') %>%
    ggplot(aes(x=imagination, y=emmean, group=outcome)) +
    ylab("Vividness Ratings") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=vividness, fill=outcome,
                    group=interaction(imagination, outcome)),
                width=0.75, adjust=1.75, position=position_dodge(1),
                data=subset(judgments, condition=='ball')) +
    geom_point(size=3, position=position_dodge(1)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(1)) +
    scale_x_discrete(name=NULL,
                     labels=c('Outcome Assessment', 'Counterfactual Thinking',
                              'Causal Reasoning')) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot1

plot2 <- emmeans.vividness %>% as.data.frame %>%
    subset(condition == 'goalie') %>%
    ggplot(aes(x=imagination, y=emmean, group=outcome)) +
    ylab("Vividness Ratings") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=vividness, fill=outcome,
                    group=interaction(imagination, outcome)),
                width=0.75, adjust=1.75, position=position_dodge(1),
                data=subset(judgments, condition=='goalie')) +
    geom_point(size=3, position=position_dodge(1)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(1)) +
    scale_x_discrete(name=NULL,
                     labels=c('Outcome Assessment', 'Counterfactual Thinking',
                              'Causal Reasoning')) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot2

g <- arrangeGrob(grid.arrange(plot1, nrow=1,
                              top=textGrob("Retrospectively thinking about the ball",
                                           gp=gpar(fontface="bold"))),
                 grid.arrange(plot2, nrow=1,
                              top=textGrob("Retrospectively thinking about the goalie",
                                           gp=gpar(fontface="bold"))),
                 nrow=2)
ggsave(file="plots/vividness.png",g)

#Table of results
Table2 <- sjPlot::tab_model(model.vividness, show.se = F, digits = 3)
Table2

#Event judgments
## Rating analysis ball
model.ratings <- lmer(rating ~ outcome * imagination * condition +
                           vividness + (1|id),
                       data=judgments)
summary(model.ratings)
anova(model.ratings)

emmeans.ratings <- emmeans(model.ratings, ~ outcome * imagination * condition)
emmeans.ratings
emmeans(model.ratings, pairwise ~ outcome, by='imagination')


plot1 <- emmeans.ratings %>% as.data.frame %>%
    subset(condition == 'ball') %>%
    ggplot(aes(x=imagination, y=emmean, group=outcome)) +
    ylab("Judgments") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=rating, fill=outcome,
                    group=interaction(imagination, outcome)),
                width=1, adjust=1.5, position=position_dodge(0.66),
                data=subset(judgments, condition=='ball')) +
    geom_point(size=3, position=position_dodge(0.66)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(0.66)) +
    scale_x_discrete(name=NULL,
                     labels=c('Outcome Assessment', 'Counterfactual Thinking',
                              'Causal Reasoning')) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot1

plot2 <- emmeans.ratings %>% as.data.frame %>%
    subset(condition == 'goalie') %>%
    ggplot(aes(x=imagination, y=emmean, group=outcome)) +
    ylab("Judgments") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=rating, fill=outcome,
                    group=interaction(imagination, outcome)),
                width=1, adjust=1.5, position=position_dodge(0.66),
                data=subset(judgments, condition=='goalie')) +
    geom_point(size=3, position=position_dodge(0.66)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(0.66)) +
    scale_x_discrete(name=NULL,
                     labels=c('Outcome Assessment', 'Counterfactual Thinking',
                              'Causal Reasoning')) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot2

g <- arrangeGrob(grid.arrange(plot1, nrow=1,
                              top=textGrob("Retrospectively thinking about the ball",
                                           gp=gpar(fontface="bold"))),
                 grid.arrange(plot2, nrow=1,
                              top=textGrob("Retrospectively thinking about the goalie",
                                           gp=gpar(fontface="bold"))),
                 nrow=2)
ggsave(file="plots/ratings.png", g)


########################################################################################
##
##   Exploratory analyses for credit/blame and responsibility
##
##   Note: imagination could be added as a factor, though we have no
##         reason to expect it to make a difference
##
judgments_long <- judgments %>%
    pivot_longer(self_credit_blame:other_resp,
                 names_pattern='^([^_]*)_(.*)',
                 names_to=c('object', 'measure'),
                 values_to='response') %>%
    pivot_wider(names_from=measure, values_from=response)

## Blame analysis
model.blame <- lmer(credit_blame ~ outcome * condition * object +
                           vividness + (1|id),
                       data=judgments_long)
summary(model.blame)
anova(model.blame)

emmeans.blame <- emmeans(model.blame, ~ outcome * condition * object)
emmeans.blame

plot1 <- emmeans.blame %>% as.data.frame %>%
    subset(condition == 'ball') %>%
    ggplot(aes(x=object, y=emmean, group=outcome)) +
    ylab("Credit/Blame") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=credit_blame, fill=outcome,
                    group=interaction(object, outcome)),
                width=0.75, adjust=1.5, position=position_dodge(0.85),
                data=subset(judgments_long, condition=='ball')) +
    geom_point(size=3, position=position_dodge(0.85)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(0.85)) +
    scale_x_discrete(name=NULL,
                     labels=c('The Ball', 'The Goalie')) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot1

plot2 <- emmeans.blame %>% as.data.frame %>%
    subset(condition == 'goalie') %>%
    ggplot(aes(x=object, y=emmean, group=outcome)) +
    ylab("Judgments") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=rating, fill=outcome,
                    group=interaction(object, outcome)),
                width=0.75, adjust=1.5, position=position_dodge(0.85),
                data=subset(judgments_long, condition=='goalie')) +
    geom_point(size=3, position=position_dodge(0.85)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(0.85)) +
    scale_x_discrete(name=NULL,
                     labels=c('Outcome Assessment', 'Counterfactual Thinking',
                              'Causal Reasoning')) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot2

g <- arrangeGrob(grid.arrange(plot1, nrow=1,
                              top=textGrob("Retrospectively thinking about the ball",
                                           gp=gpar(fontface="bold"))),
                 grid.arrange(plot2, nrow=1,
                              top=textGrob("Retrospectively thinking about the goalie",
                                           gp=gpar(fontface="bold"))),
                 nrow=2)
ggsave(file="plots/blame.png", g)





## Responsibility analysis
model.resp <- lmer(resp ~ outcome * condition * object +
                           vividness + (1|id),
                       data=judgments_long)
summary(model.resp)
anova(model.resp)

emmeans.resp <- emmeans(model.resp, ~ outcome * condition * object)
emmeans.resp

plot1 <- emmeans.resp %>% as.data.frame %>%
    subset(condition == 'ball') %>%
    ggplot(aes(x=object, y=emmean, group=outcome)) +
    ylab("Credit/Resp") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=resp, fill=outcome,
                    group=interaction(object, outcome)),
                width=0.75, adjust=1.5, position=position_dodge(0.85),
                data=subset(judgments_long, condition=='ball')) +
    geom_point(size=3, position=position_dodge(0.85)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(0.85)) +
    scale_x_discrete(name=NULL,
                     labels=c('The Ball', 'The Goalie')) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot1

plot2 <- emmeans.resp %>% as.data.frame %>%
    subset(condition == 'goalie') %>%
    ggplot(aes(x=object, y=emmean, group=outcome)) +
    ylab("Judgments") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=rating, fill=outcome,
                    group=interaction(object, outcome)),
                width=0.75, adjust=1.5, position=position_dodge(0.85),
                data=subset(judgments_long, condition=='goalie')) +
    geom_point(size=3, position=position_dodge(0.85)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(0.85)) +
    scale_x_discrete(name=NULL,
                     labels=c('Outcome Assessment', 'Counterfactual Thinking',
                              'Causal Reasoning')) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot2

g <- arrangeGrob(grid.arrange(plot1, nrow=1,
                              top=textGrob("Retrospectively thinking about the ball",
                                           gp=gpar(fontface="bold"))),
                 grid.arrange(plot2, nrow=1,
                              top=textGrob("Retrospectively thinking about the goalie",
                                           gp=gpar(fontface="bold"))),
                 nrow=2)
ggsave(file="plots/responsibility.png", g)
