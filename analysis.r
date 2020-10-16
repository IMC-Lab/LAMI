library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(car)
library(emmeans)
library(sjPlot)
library(viridis)
library(patchwork)

#read in the data
judgments <- read.csv('data/LAMI_Prolific_processed.csv', header=TRUE)
summary(judgments)

## remove participants that fail the comprehension checks
judgments <- judgments %>%
    filter(CheckQ1=='Think about and visualize how ${e://Field/Condition} just moved.' |
           CheckQ1Again=='Think about and visualize how ${e://Field/Condition} just moved.') %>%
    filter(CheckQ2=='Think about and visualize what would have happened if ${e://Field/Condition} had moved in the other direction.' |
           CheckQ2Again=='Think about and visualize what would have happened if ${e://Field/Condition} had moved in the other direction.') %>%
    filter(CheckQ3=="Think about and visualize whether ${e://Field/Condition}'s movement caused the ball to score or not score." |
           CheckQ3Again=="Think about and visualize whether ${e://Field/Condition}'s movement caused the ball to score or not score.") %>%
    filter(check == lr_other & Catch == 'IM_74Ef0wh1bD6qdZb' & AttnCheck == 'Yes') %>%
    select(-CheckQ1, -CheckQ1Again, -CheckQ2, -CheckQ2Again,
           -CheckQ3, -CheckQ3Again, -check, -lr_other, -Catch, -AttnCheck)

## normalize the ratings from 0-100 to 0-1
normalize <- function (x) (x/100)
judgments <- judgments %>%
  mutate(rating=normalize(rating),
         confidence=normalize(confidence),
         vividness=normalize(vividness),
         imagination=factor(imagination,
                            levels=c('Remember', 'What If?', 'Cause')),
         condition=factor(condition,
                          levels=c('ball', 'goalie')))

## Participants
## Show the sample size
writeLines(sprintf('# of participants: %d', length(unique(judgments$id))))
writeLines('Design: 2 (Outcome: score/miss) x 3 (imagination: Outcome assessment, Counterfactual thinking, Causal reasoning) x 2 (condition: ball/goalie)')
table(judgments[c('outcome', 'imagination', 'condition')])


## A summary plot of all ratings
judgments %>%
    ggplot(aes(x=vividness, y=rating, color=outcome)) +
    geom_point(alpha=0.4) +
    ##stat_density2d() +
    stat_smooth(method='lm') +
    coord_cartesian(c(0,1), c(0,1)) +
    facet_grid(imagination ~ condition * outcome) +
    theme_classic()
ggsave('plots/summary.png')



judgments %>%
    ggplot(aes(x=vividness, y=confidence, color=rating)) +
    stat_smooth(method='lm', color='red') +
    geom_point() +
    facet_grid(imagination ~ condition * outcome) +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
    theme_bw()
ggsave('plots/summary-vividness-confidence.png')


## Vividness
hist(judgments$vividness, breaks=101)

## run the models: vividness
model.vividness <- lmer(vividness ~ condition*imagination*outcome +
                            (outcome*imagination|id),
                        data=judgments)
summary(model.vividness)

## print out marginal means and contrasts over imagination
emmeans.vividness <- emmeans(model.vividness, ~ condition * outcome * imagination)
emmeans.vividness


## plot results
plot1 <- emmeans.vividness %>% as.data.frame %>%
    subset(condition == 'ball') %>%
    ggplot(aes(x=imagination, y=emmean, group=outcome)) +
    ylab("Vividness Ratings") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=vividness, fill=outcome,
                    group=interaction(imagination, outcome)),
                width=0.75, adjust=1.75, position=position_dodge(0.75),
                data=subset(judgments, condition=='ball')) +
    geom_point(size=3, position=position_dodge(0.75)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(0.75)) +
    scale_x_discrete(name=NULL,
                     labels=c('Outcome Assessment', 'Counterfactual Thinking',
                              'Causal Reasoning')) +
    scale_fill_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    ggtitle('Retrospectively thinking about the ball') +
    theme(plot.title = element_text(hjust=0.5, face="bold"))
plot2 <- emmeans.vividness %>% as.data.frame %>%
    subset(condition == 'goalie') %>%
    ggplot(aes(x=imagination, y=emmean, group=outcome)) +
    ylab("Vividness Ratings") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_violin(aes(y=vividness, fill=outcome,
                    group=interaction(imagination, outcome)),
                width=0.75, adjust=1.75, position=position_dodge(0.75),
                data=subset(judgments, condition=='goalie')) +
    geom_point(size=3, position=position_dodge(0.75)) +
    geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                  size=0.75, width=0.3, position=position_dodge(0.75)) +
    scale_x_discrete(name=NULL,
                     labels=c('Outcome Assessment', 'Counterfactual Thinking',
                              'Causal Reasoning')) +
    scale_fill_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    ggtitle('Retrospectively thinking about the goalie') +
    theme(plot.title = element_text(hjust=0.5, face="bold"))

plot1 / plot2 + plot_layout(guides='collect')
ggsave(file="plots/vividness.png")

#Table of results
Table2 <- sjPlot::tab_model(model.vividness, show.se=F, digits=3, file="LAMI_Table2_edit.html")
Table2

#####
#Predicting causal judgements from internal thoughts
data.remember <- judgments %>% filter(imagination=='Remember')
data.whatif <- judgments %>% filter(imagination=='What If?')
data.cause <- judgments %>% filter(imagination=='Cause')

model.remember <- lmer(rating ~ outcome * condition * vividness + (outcome|id),
                       data=data.remember)
summary(model.remember)

emmeans.remember <- emmeans(model.remember, ~ outcome * condition * vividness,
                            at=list(vividness=seq(0, 1, 0.01)))
emtrends(model.remember, pairwise ~ outcome | condition, var='vividness')

plot1 <- emmeans.remember %>%
    as.data.frame %>% subset(condition == 'ball') %>%
    ggplot(aes(x=vividness, y=emmean, group=outcome, fill=outcome)) +
    ylab("Judgments") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_line(size=1.5) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) +
    geom_point(aes(y=rating, color=outcome), show.legend=FALSE, alpha=0.5,
               data=filter(data.remember, condition=='ball')) +
    scale_fill_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    ggtitle('Retrospectively thinking about the ball') +
    theme(plot.title = element_text(hjust=0.5, face="bold"))
plot2 <- emmeans.remember %>%
    as.data.frame %>% subset(condition == 'goalie') %>%
    ggplot(aes(x=vividness, y=emmean, group=outcome, fill=outcome)) +
    ylab("Judgments") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_line(size=1.5) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) +
    geom_point(aes(y=rating, color=outcome), show.legend=FALSE, alpha=0.5,
               data=filter(data.remember, condition=='goalie')) +
    scale_fill_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    ggtitle('Retrospectively thinking about the goalie') +
    theme(plot.title = element_text(hjust=0.5, face="bold"))

plot1 / plot2 + plot_layout(guides='collect')
ggsave(file="plots/ratings-remember.png")


model.whatif <- lmer(rating ~ outcome * condition * vividness + (outcome|id),
                     data=data.whatif)
summary(model.whatif)

emmeans.whatif <- emmeans(model.whatif, ~ outcome * condition * vividness,
                          at=list(vividness=seq(0, 1, 0.01)))
emtrends(model.whatif, pairwise ~ outcome | condition, var='vividness')

plot1 <- emmeans.whatif %>%
    as.data.frame %>% subset(condition == 'ball') %>%
    ggplot(aes(x=vividness, y=emmean, group=outcome, fill=outcome)) +
    ylab("Judgments") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_line(size=1.5) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) +
    geom_point(aes(y=rating, color=outcome), show.legend=FALSE, alpha=0.5,
               data=filter(data.whatif, condition=='ball')) +
    scale_fill_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    ggtitle('Retrospectively thinking about the ball') +
    theme(plot.title = element_text(hjust=0.5, face="bold"))
plot2 <- emmeans.whatif %>%
    as.data.frame %>% subset(condition == 'goalie') %>%
    ggplot(aes(x=vividness, y=emmean, group=outcome, fill=outcome)) +
    ylab("Judgments") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_line(size=1.5) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.3) +
    geom_point(aes(y=rating, color=outcome), show.legend=FALSE, alpha=0.5,
               data=filter(data.whatif, condition=='goalie')) +
    scale_fill_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    ggtitle('Retrospectively thinking about the goalie') +
    theme(plot.title = element_text(hjust=0.5, face="bold"))

plot1 / plot2 + plot_layout(guides='collect')
ggsave(file="plots/ratings-whatif.png")


#Table of models 1 and 2
Table3 <- sjPlot::tab_model(model.remember, model.whatif,
                            show.se=F, digits=3, file = "LAMI_Table3.html")
Table3



data.cause$remember <- predict(model.remember, newdata=data.cause, re.form=NULL)
data.cause$whatif <- predict(model.whatif, newdata=data.cause, re.form=NULL)

model.cause <- lmer(rating ~ condition*remember*whatif + (1|id), data=data.cause)
summary(model.cause)

emmeans.cause <- model.cause %>%
    emmeans(~ condition*remember*whatif, at=list(whatif=(0:100)/100,
                                                 remember=(0:100)/100)) %>%
    as.data.frame()


ggplot(data.cause) +
    aes(x=remember, y=whatif, color=rating) +
    geom_point(alpha=0.5) +
    coord_cartesian(xlim=c(0, 1), ylim=c(0,1)) +
    theme_classic() +
    scale_color_viridis(option="magma", name='Causal judgment') +
    facet_grid(condition ~ .)
ggsave('plots/ratings-causal-raw.png')
    

plot1 <- emmeans.cause %>%
    filter(condition=='ball') %>%
    ggplot(aes(x=remember, y=whatif, fill=emmean)) +
    geom_tile() +
    geom_point(fill='black', color='black',
               size=0.1, data=filter(data.cause, condition=='ball')) +
    scale_fill_viridis(option="magma", name='Causal judgment', limits=0:1) +
    scale_x_continuous(expand=c(0,0), limits=c(0, 1.1)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 1.1)) +
    xlab('Outcome model estimates') + ylab('Counterfactual model estimates') +
    theme_classic(base_size = 10, base_family = "Arial") +
    coord_fixed() +
    ggtitle('Retrospectively thinking about the ball') +
    theme(plot.title = element_text(hjust=0.5, face="bold"))

plot2 <- emmeans.cause %>%
    filter(condition=='goalie') %>%
    ggplot(aes(x=remember, y=whatif, fill=emmean)) +
    geom_tile() +
    geom_point(fill='black', color='black',
               size=0.1, data=filter(data.cause, condition=='goalie')) +
    scale_fill_viridis(option="magma", name='Causal judgment', limits=0:1) +
    scale_x_continuous(expand=c(0,0), limits=c(0, 1.1)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 1.1)) +
    xlab('Outcome model estimates') + ylab('Counterfactual model estimates') +
    theme_classic(base_size = 10, base_family = "Arial") +
    coord_fixed() +
    ggtitle('Retrospectively thinking about the goalie') +
    theme(plot.title = element_text(hjust=0.5, face="bold"))

plot1 / plot2
ggsave(file="plots/ratings-causal.png", width=6, height=6)


ggplot(judgments) + aes(x=condition, y=rating, color=outcome) +
    stat_summary(fun.data='mean_cl_boot', geom='pointrange') +
    facet_grid(. ~ imagination) + theme_classic() + ylim(0, 1)
ggsave('plots/ratings.png')

#Modeling Causal Judgements to look at confidence as a predictor
model.cause <- lmer(rating ~ outcome * condition * vividness + (outcome|id),
                    data=data.cause)
summary(model.cause)
Table4 <- sjPlot::tab_model(model.cause, show.se = F, digits = 2, file = "LAMI_Table4.html")
