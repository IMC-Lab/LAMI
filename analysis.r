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

library(brms)
library(tidybayes)
library(bayestestR)

## set sum contrasts as default
options(contrasts=c('contr.sum', 'contr.poly'))

## read in the data
judgments <- read.csv('data/LAMI_Prolific_processed.csv',
                      header=TRUE, stringsAsFactors=FALSE)
summary(judgments)

## # of participants & trials pre-exclusion
judgments %>%
    group_by(condition) %>%
    summarize(N=length(unique(id)))
nrow(judgments)

## remove participants that fail the comprehension checks
judgments <- judgments %>%
    filter(CheckQ1=='Think about and visualize how ${e://Field/Condition} just moved.' |
           CheckQ1Again=='Think about and visualize how ${e://Field/Condition} just moved.') %>%
    filter(CheckQ2=='Think about and visualize what would have happened if ${e://Field/Condition} had moved in the other direction.' |
           CheckQ2Again=='Think about and visualize what would have happened if ${e://Field/Condition} had moved in the other direction.') %>%
    filter(CheckQ3=="Think about and visualize whether ${e://Field/Condition}'s movement caused the ball to score or not score." |
           CheckQ3Again=="Think about and visualize whether ${e://Field/Condition}'s movement caused the ball to score or not score.") %>%
    filter(Catch == 'IM_74Ef0wh1bD6qdZb' &
           AttnCheck == 'Yes') %>%
    select(-CheckQ1, -CheckQ1Again, -CheckQ2, -CheckQ2Again,
           -CheckQ3, -CheckQ3Again, -Catch, -AttnCheck)

## # of participants & trials pre-trial-exclusion
judgments %>%
    group_by(condition) %>%
    summarize(N=length(unique(id)))
nrow(judgments)

judgments <- judgments %>%
    filter(as.character(check) == as.character(lr_other)) %>%
    select(-check, -lr_other)

## # of participants & trials post-exclusion
judgments %>%
    group_by(condition) %>%
    summarize(N=length(unique(id)))
nrow(judgments)

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
judgments %>%
    group_by(condition) %>%
    summarize(N=length(unique(id)))
nrow(judgments)


## A summary plot of all ratings
judgments %>%
    ggplot(aes(x=vividness, y=rating, color=outcome)) +
    geom_point(alpha=0.4) +
    ##stat_density2d() +
    stat_smooth(method='lm', fullrange=TRUE) +
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


#####
#Predicting causal judgements from internal thoughts
data.remember <- judgments %>% filter(imagination=='Remember')
data.whatif <- judgments %>% filter(imagination=='What If?')
data.cause <- judgments %>% filter(imagination=='Cause')




model.remember <- brm(rating ~ condition*outcome + (outcome|id),
                      data=data.remember,
                      prior=c(set_prior(paste0('normal(0, ',
                                               sd(data.remember$rating), ')'),
                                        class='b')),
                      sample_prior='yes', save_pars=save_pars(all=TRUE),
                      cores=4, file='remember')

summary(model.remember, prior=TRUE)
describe_posterior(model.remember, ci=.95, rope_ci=.95)

## get BFs
hypothesis(model.remember, c('condition1 = 0', 'outcome1 = 0',
                             'condition1:outcome1 = 0'))

emmeans.remember <- emmeans(model.remember, ~ outcome * condition)

emmeans.remember %>%
    as.data.frame %>% 
    ggplot(aes(x=condition, y=emmean, ymin=lower.HPD, ymax=upper.HPD,
               fill=outcome)) +
    scale_x_discrete(name='Focused Object', labels=c('Ball', 'Goalie')) +
    ylab("Judgments") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    facet_wrap(~ outcome) +
    geom_col(position=position_dodge(1)) +
    geom_errorbar(position=position_dodge(1)) +
    scale_fill_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    ggtitle('Reflected on the ball') +
    theme(plot.title = element_text(hjust=0.5),
          strip.text=element_blank())
ggsave(file="plots/ratings-remember.png")



model.whatif <- brm(rating ~ condition*outcome + (outcome|id),
                    data=data.whatif,
                    prior=c(set_prior(paste0('normal(0, ',
                                             sd(data.whatif$rating), ')'),
                                      class='b')),
                      sample_prior='yes', save_pars=save_pars(all=TRUE),
                    cores=4, file='whatif')

summary(model.whatif, prior=TRUE)
describe_posterior(model.whatif, ci=.95, rope_ci=.95)

## get BFs
hypothesis(model.whatif, c('condition1 = 0', 'outcome1 = 0',
                           'condition1:outcome1 = 0'))


emmeans.whatif <- emmeans(model.whatif, ~ outcome * condition)

emmeans.whatif %>%
    as.data.frame %>% 
    ggplot(aes(x=condition, y=emmean, ymin=lower.HPD, ymax=upper.HPD,
               fill=outcome)) +
    scale_x_discrete(name='Focused Object', labels=c('Ball', 'Goalie')) +
    ylab("Judgments") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    facet_wrap(~ outcome) +
    geom_col(position=position_dodge(1)) +
    geom_errorbar(position=position_dodge(1)) +
    scale_fill_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    ggtitle('Reflected on the ball') +
    theme(plot.title = element_text(hjust=0.5),
          strip.text=element_blank())
ggsave(file="plots/ratings-whatif.png")

## Table of models 1 and 2
Table2 <- sjPlot::tab_model(model.remember, model.whatif,
                            show.se=F, digits=3, file = "LAMI_Table2.html")
Table2

## Plot results from models 1 and 2
rbind(emmeans.remember %>% as.data.frame %>% mutate(imagination='Remember'),
      emmeans.whatif %>% as.data.frame %>% mutate(imagination='What If?')) %>%
    ggplot(aes(x=outcome, y=emmean, ymin=lower.HPD, ymax=upper.HPD,
               fill=outcome)) +
    ylab("Judgments") + coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size = 10, base_family = "Arial") +
    geom_col(position=position_dodge(1), show.legend=FALSE) +
    geom_errorbar(position=position_dodge(1)) +
    facet_grid(imagination ~ condition,
               labeller=labeller(condition=c('ball'='Offensive',
                                             'goalie'='Defensive'))) +
    scale_x_discrete(name='Outcome', labels=c('Miss', 'Score')) +
    theme(plot.title=element_text(hjust=0.5),
          strip.text=element_text(size=12),
          strip.background=element_blank())
ggsave(file="plots/ratings-remember-whatif.png")




data.cause$remember <- fitted(model.remember, newdata=data.cause)[,1]
data.cause$whatif <- fitted(model.whatif, newdata=data.cause)[,1]

model.cause <- brm(rating ~ condition*scale(remember)*scale(whatif) + (1|id),
                   data=data.cause,
                   prior=c(set_prior(paste0('normal(0, ',
                                            sd(data.cause$rating), ')'), class='b')),
                   sample_prior='yes', save_pars=save_pars(all=TRUE),
                   cores=4, file='cause')

summary(model.cause, prior=TRUE)
describe_posterior(model.cause, ci=.95, rope_ci=.95)

## get BFs
hypothesis(model.cause,
           c('condition1 = 0', 'scaleremember = 0', 'scalewhatif = 0',
             'condition1:scaleremember = 0', 'condition1:scalewhatif = 0',
             'scaleremember:scalewhatif = 0',
             'condition1:scaleremember:scalewhatif = 0'))


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
    

plot1.cause <- emmeans.cause %>%
    filter(condition=='ball') %>%
    ggplot(aes(x=remember, y=whatif, fill=emmean)) +
    geom_tile() +
    geom_point(fill='black', color='black',
               size=0.3, data=filter(data.cause, condition=='ball')) +
    scale_fill_viridis(option="magma", name='Cause', limits=0:1) +
    scale_x_continuous(expand=c(0,0), limits=c(0, 1.1)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 1.1)) +
    xlab('Remember model estimates') + ylab('What If? model estimates') +
    theme_classic(base_size = 10, base_family = "Arial") +
    coord_fixed() +
    ggtitle('Offensive position') +
    theme(plot.title = element_text(hjust=0.5))

plot2.cause <- emmeans.cause %>%
    filter(condition=='goalie') %>%
    ggplot(aes(x=remember, y=whatif, fill=emmean)) +
    geom_tile() +
    geom_point(fill='black', color='black',
               size=0.3, data=filter(data.cause, condition=='goalie')) +
    scale_fill_viridis(option="magma", name='Cause', limits=0:1) +
    scale_x_continuous(expand=c(0,0), limits=c(0, 1.1)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 1.1)) +
    xlab('Remember model estimates') + ylab('What If? model estimates') +
    theme_classic(base_size = 10, base_family = "Arial") +
    coord_fixed() +
    ggtitle('Defensive position') +
    theme(plot.title = element_text(hjust=0.5))

plot1.cause + plot2.cause + plot_layout(guides='collect')
ggsave(file="plots/ratings-causal.png", width=6, height=3)

#Table of causal selection model
Table3 <- sjPlot::tab_model(model.cause,
                            show.se=F, digits=3, file = "LAMI_Table3.html")
Table3


ggplot(judgments) + aes(x=condition, y=rating, color=outcome) +
    stat_summary(fun.data='mean_cl_boot', geom='pointrange') +
    facet_grid(. ~ imagination) + theme_classic() + ylim(0, 1)
ggsave('plots/ratings.png')

Table4 <- sjPlot::tab_model(model.cause, show.se = F, digits = 2, file = "LAMI_Table4.html")
