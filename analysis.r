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
library(viridis)
library(shiny)
library(simr)

#read in the data
judgments <- read.csv('data/LAMI_Full2_MTurk_processed.csv', header=TRUE)

#double check data
summary(judgments)

## normalize the ratings from 0-100 to 0-1
normalize <- function (x) (x/100)
judgments <- judgments %>%
  mutate(rating=normalize(rating),
         confidence=normalize(confidence),
         vividness=normalize(vividness),
         imagination=factor(imagination,
                            levels=c('outcome', 'counterfactual', 'causal')),
         condition=factor(condition,
                          levels=c('ball', 'goalie')))

## Participants
## Show the sample size
writeLines(sprintf('# of participants: %d', length(unique(judgments$id))))
writeLines('Design: 2 (Outcome: score/miss) x 3 (imagination: Outcome assessment, Counterfactual thinking, Causal reasoning) x 2 (condition: ball/goalie)')
table(judgments[c('outcome', 'imagination', 'condition')])


## Vividness
hist(judgments$vividness)

## run the models: vividness
model.vividness <- lmer(vividness ~ condition * imagination * outcome + (1 | id),
                         data=judgments)
summary(model.vividness)
#anova(model.vividness)

## print out marginal means and contrasts over imagination
emmeans.vividness <- emmeans(model.vividness, ~ condition * outcome * imagination)
emmeans.vividness
#emmeans(model.vividness, pairwise ~ imagination) 
emmeans(model.vividness, pairwise ~ imagination | condition, adjust='none')$contrasts #to explore the interaction

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
Table2 <- sjPlot::tab_model(model.vividness, show.se = F, digits = 3, file = "LAMI_Table2_edit.html")
Table2

#####
#Predicting causal judgements from internal thoughts
data.outcome <- judgments %>% subset(imagination=='outcome')
data.cf <- judgments %>% subset(imagination=='counterfactual')
data.causal <- judgments %>% subset(imagination=='causal')

model.outcome <- lmer(rating ~ outcome * condition * vividness + (1|id),
                      data=data.outcome)
summary(model.outcome)
#anova(model.outcome)
emmeans.outcome <- emmeans(model.outcome, ~ outcome * condition * vividness, at=list(vividness=seq(0, 1, 0.01)))
emtrends(model.outcome, pairwise ~ outcome, var='vividness')

plot1 <- emmeans.outcome %>%
    as.data.frame %>% subset(condition == 'ball') %>%
    ggplot(aes(x=vividness, y=emmean, group=outcome, fill=outcome)) +
    ylab("Judgments") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_line(size=1.5) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot2 <- emmeans.outcome %>%
    as.data.frame %>% subset(condition == 'ball') %>%
    ggplot(aes(x=vividness, y=emmean, group=outcome, fill=outcome)) +
    ylab("Judgments") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_line(size=1.5) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))

g <- arrangeGrob(grid.arrange(plot1, nrow=1,
                              top=textGrob("Retrospectively thinking about the ball",
                                           gp=gpar(fontface="bold"))),
                 grid.arrange(plot2, nrow=1,
                              top=textGrob("Retrospectively thinking about the goalie",
                                           gp=gpar(fontface="bold"))),
                 nrow=2)
ggsave(file="plots/ratings-outcome.png", g)



model.cf <- lmer(rating ~ outcome * condition * vividness + (1|id),
                 data=data.cf)
summary(model.cf)
#anova(model.cf)
emmeans.cf <- emmeans(model.cf, ~ outcome * condition * vividness, at=list(vividness=seq(0, 1, 0.01)))
emtrends(model.cf, pairwise ~ outcome, var='vividness')

plot1 <- emmeans.cf %>%
    as.data.frame %>% subset(condition == 'ball') %>%
    ggplot(aes(x=vividness, y=emmean, group=outcome, fill=outcome)) +
    ylab("Judgments") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_line(size=1.5) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))
plot2 <- emmeans.cf %>%
    as.data.frame %>% subset(condition == 'ball') %>%
    ggplot(aes(x=vividness, y=emmean, group=outcome, fill=outcome)) +
    ylab("Judgments") + ylim(0, 1) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_line(size=1.5) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) +
    scale_color_discrete(name='Outcome', labels=c('Miss', 'Score'))

g <- arrangeGrob(grid.arrange(plot1, nrow=1,
                              top=textGrob("Retrospectively thinking about the ball",
                                           gp=gpar(fontface="bold"))),
                 grid.arrange(plot2, nrow=1,
                              top=textGrob("Retrospectively thinking about the goalie",
                                           gp=gpar(fontface="bold"))),
                 nrow=2)
ggsave(file="plots/ratings-cf.png", g)


#Table of models 1 and 2
Table3 <- sjPlot::tab_model(model.outcome, model.cf, show.se = F, digits = 3, file = "LAMI_Table3.html")
Table3

data.causal$outcome <- predict(model.outcome)
data.causal$cf <- predict(model.cf)
model.causal <- lmer(rating ~ outcome * cf + (1|id), data=data.causal)
summary(model.causal)
#anova(model.causal)

emmeans.causal <- model.causal %>%
    emmeans(~ outcome * cf,
            at=list(cf=(0:100)/100,
                    outcome=(0:100)/100)) %>%
    as.data.frame()

g <- emmeans.causal %>%
    ggplot() +
    aes(x=outcome, y=cf, fill=emmean) +
    geom_tile() +
    scale_fill_viridis(option="magma", name='Causal judgements') +
    scale_x_continuous(expand=c(0,0), limits=c(0, 1)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 1)) +
    xlab('Outcome model estimates') + ylab('Counterfactual model estimates') +
    theme_classic(base_size = 10, base_family = "Arial") +
    coord_fixed()
ggsave(file="plots/ratings-causal.png",g, width=6, height=3)

#judgments <- rbind(data.rem, data.cf, data.cause %>% select(-rem, -cf))

png('plots/model/estimates_edit.png', width=750, height=750)
ggplot(judgments) + aes(x=condition, y=model, color=outcome) +
    stat_summary(fun.data='mean_cl_boot', geom='pointrange') +
    facet_grid(. ~ imagination) + theme_classic() + ylim(0, 1)
dev.off()


#Modeling Causal Judgements to look at vividness as a predictor
model.cause <- lmer(rating ~ outcome * condition * vividness + (1|id), data=data.causal)
summary(model.cause)
Table4 <- sjPlot::tab_model(model.cause, show.se = F, digits = 2, file = "LAMI_Table4.html")



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
