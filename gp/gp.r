library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)
library(bayestestR)
library(viridis)
library(patchwork)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source('grid.r')

data.fix <- read.csv('../data/test/KKTest1_BlankFixations_report.csv',
                     header=TRUE) %>%
    mutate(CURRENT_FIX_X = CURRENT_FIX_X - 1920/2,
           CURRENT_FIX_Y = CURRENT_FIX_Y - 1080/2) %>%
    group_by(TRIAL_INDEX) %>%
    mutate(PREV_FIX_X = lag(CURRENT_FIX_X),
           PREV_FIX_Y = lag(CURRENT_FIX_Y)) %>%
    ungroup()
    
## generate fake subjects/conditions
data.groups <- data.frame(TRIAL_INDEX=unique(data.fix$TRIAL_INDEX),
                          PAR=as.factor(c(2, 1, 1, 2, 1, 2,
                                          2, 1, 1, 1, 2, 2,
                                          2, 1, 2, 1, 2, 1)),
                          COND=as.factor(c(1, 1, 1, 3, 2, 1,
                                           2, 1, 3, 2, 1, 2,
                                           3, 3, 3, 2, 2, 3)))

data.fix <- data.fix %>% mutate(PAR=data.groups$PAR[TRIAL_INDEX],
                                COND=data.groups$COND[TRIAL_INDEX])

data.fix %>%
    ggplot(aes(x=CURRENT_FIX_X, y=CURRENT_FIX_Y, color=as.factor(PAR))) +
    geom_point(size=.75, show.legend=FALSE) +
    coord_fixed(xlim=c(-450, 550), ylim=c(-200, 300)) +
    ggtitle('Raw Data (by participant)') + xlab('X') + ylab('Y') + 
    facet_grid(COND ~ PAR, labeller=label_both) +
    theme_classic()
ggsave('plots/raw_data.png', width=5, height=4)

data.fix %>%
    ggplot(aes(x=CURRENT_FIX_X, y=CURRENT_FIX_Y, color=as.factor(PAR))) +
    geom_point(size=.75, show.legend=FALSE) +
    coord_fixed(xlim=c(-450, 550), ylim=c(-200, 300)) +
    ggtitle('Raw Data (group)') + xlab('X') + ylab('Y') + 
    facet_grid(COND ~ ., labeller=label_both) +
    theme_classic()
ggsave('plots/raw_data_group.png', width=5, height=4)

## Bin fixations into hexagons
data.grid <- data.fix %>%
    grid.hex(xmin=-450, xmax=550, xsize=50,
             ymin=-200, ymax=300, ysize=50)

data.hex <- data.fix %>%
    group_by(COND, PAR, TRIAL_INDEX) %>%
    bin.hex(xmin=-450, xmax=550, xsize=50,
            ymin=-200, ymax=300, ysize=50)

plot.data.par <- data.hex %>%
    unnest(c(vi, vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=count, color=count)) +
    geom_polygon(size=0.3) + coord_fixed() +
    ggtitle('Raw Data (by participant)') + xlab('X') + ylab('Y') + 
    scale_color_viridis(option='magma', name='Count', limits=c(0, NA)) +
    scale_fill_viridis(option='magma', name='Count', limits=c(0, NA)) +
    facet_grid(COND ~ PAR, labeller=label_both) +
    theme_classic()
plot.data.par
ggsave('plots/raw_data_hex.png', width=5, height=4)

plot.data.group <- data.hex %>%
    group_by(COND, bin_x, bin_y, grid_idx, vi, vx, vy) %>%
    summarize(count=mean(count)) %>%
    unnest(c(vi, vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=count, color=count)) +
    geom_polygon(size=0.3) + coord_fixed() +
    ggtitle('Raw Data (group)') + xlab('X') + ylab('Y') + 
    scale_color_viridis(option='magma', name='Count', limits=c(0, NA)) +
    scale_fill_viridis(option='magma', name='Count', limits=c(0, NA)) +
    facet_grid(COND ~ ., labeller=label_both) +
    theme_classic()
plot.data.group
ggsave('plots/raw_data_group_hex.png', width=5, height=4)

data.stan <- list(N=nrow(data.hex),
                  G=max(data.hex$grid_idx),
                  P=length(unique(data.hex$PAR)),
                  D=2,  ## 2 dimensions (X, Y)
                  C=length(levels(data.hex$COND)),
                  g=data.hex$grid_idx,
                  p=as.integer(data.hex$PAR),
                  c=as.integer(data.hex$COND),
                  grid=select(data.grid, bin_x, bin_y),
                  y=data.hex$count)
str(data.stan)

gp.fit <- readRDS('gp-hex.rds')

gp.fit <- stan(file='gp.stan', data=data.stan)

saveRDS(gp.fit, 'gp-hex.rds')


print(gp.fit, prob=c(0.025, 0.5, 0.975), 
      pars=c('a', 'rho', 'rho_tilde', 'alpha', 'alpha_tilde',
             'prior_a', 'prior_rho', 'prior_rho_tilde',
             'prior_alpha', 'prior_alpha_tilde', 'lp__'))

plot(gp.fit, pars=c('prior_rho', 'rho', 'prior_rho_tilde', 'rho_tilde'))
plot(gp.fit, pars=c('prior_alpha', 'alpha', 'prior_alpha_tilde', 'alpha_tilde'))



## extract model fits for each trial
draws.trial <- spread_draws(gp.fit, lambda[.row], yhat[.row],
                            prior_lambda[.row]) %>%
    bind_cols(., data.hex[.$.row, c('PAR', 'COND', 'grid_idx', 'TRIAL_INDEX',
                                    'bin_x', 'bin_y', 'vi', 'vx', 'vy', 'count')])

## extract group-level model fits for each condition
draws.group <- spread_draws(gp.fit, a, f[COND, grid_idx], prior_a,
                            prior_f[COND, grid_idx]) %>%
    mutate(lambda=a+f,
           prior_lambda=prior_a+prior_f) %>%
    bind_cols(., data.grid[.$grid_idx, c('bin_x', 'bin_y', 'vi', 'vx', 'vy')])



plot.pred.par <- draws.trial %>%
    group_by(PAR, COND, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Fit (by participant)') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    facet_grid(COND ~ PAR, labeller=label_both) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() + coord_fixed()
plot.pred.par
ggsave('plots/gp_fit_hex.png', width=5, height=4)

plot.prior.par <- draws.trial %>%
    group_by(PAR, COND, grid_idx, vx, vy) %>%
    median_hdci(prior_lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Fit (by participant)') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    geom_polygon(aes(fill=exp(prior_lambda), color=exp(prior_lambda)), size=0.3) +
    facet_grid(COND ~ PAR, labeller=label_both) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() + coord_fixed()



plot.pred.group <- draws.group %>%
    group_by(COND, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Fit (group)') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(COND ~ ., labeller=label_both) +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() + coord_fixed()
plot.pred.group
ggsave('plots/gp_group_fit_hex.png', width=5, height=4)

plot.prior.group <- draws.group %>%
    group_by(COND, grid_idx, vx, vy) %>%
    median_hdci(prior_lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Fit (group)') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(COND ~ ., labeller=label_both) +
    geom_polygon(aes(fill=exp(prior_lambda), color=exp(prior_lambda)), size=0.3) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() + coord_fixed()





## Group-level contrasts
contr.group <- draws.group %>%
    group_by(COND, grid_idx, bin_x, bin_y, vx, vy) %>%
    compare_levels(lambda, by=COND, comparison='control') %>%
    left_join(draws.group %>%
              group_by(COND, grid_idx, bin_x, bin_y, vx, vy) %>%
              compare_levels(prior_lambda, by=COND, comparison='control')) %>%
    group_by(COND, grid_idx, bin_x, bin_y, vx, vy) %>%
    mutate(BF=1/bayesfactor_pointull(lambda, prior_lambda)$BF)


# get ranges of contrasts and BFs
contr.group %>% median_hdci %>% pull(lambda) %>% range
contr.group %>% median_hdci %>% pull(prior_lambda) %>% range
contr.group %>% median_hdci %>% pull(BF) %>% log10 %>% range

contr.group %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    facet_grid(COND ~ .) +
    ggtitle('Group-level Fixation Rate Contrasts') +
    geom_polygon(size=0.3) + xlab('X') + ylab('Y') +
    scale_fill_scico(palette='vik', limits=c(-6, 6), name='Rate') +
    scale_color_scico(palette='vik', limits=c(-6, 6), name='Rate') +
    theme_classic() + coord_fixed()
ggsave('plots/gp_contrast_hex.png', width=5, height=4)

contr.group %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=prior_lambda, color=prior_lambda)) +
    facet_grid(COND ~ .) +
    ggtitle('Group-level Prior Fixation Rate Contrasts') +
    geom_polygon(size=0.3) + xlab('X') + ylab('Y') +
    scale_fill_scico(palette='vik', limits=c(-.05, .05), name='Rate') +
    scale_color_scico(palette='vik', limits=c(-.05, .05), name='Rate') +
    theme_classic() + coord_fixed()
ggsave('plots/gp_contrast_prior_hex.png', width=5, height=4)

contr.group %>% median_hdci() %>%
    mutate(BF=ifelse(BF==0, 1/4000,
              ifelse(BF==1, 4000, BF))) %>%
    unnest(c(vx, vy)) %>%
    ##filter(BF < .1 & (lambda.lower > 0 | lambda.upper < 0)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    facet_grid(COND ~ .) + xlab('X') + ylab('Y') +
    ggtitle('Group-level Fixation Rate Contrast BFs') +
    geom_polygon(size=0.3) +
    scale_fill_scico(palette='vik', direction=-1,
                     limits=c(-3, 3), breaks=-3:3,
                     labels=10^seq(-3, 3, 1),
                     name='BF_null') +
    scale_color_scico(palette='vik', direction=-1,
                      limits=c(-3, 3), breaks=-3:3,
                      labels=10^seq(-3, 3, 1),
                      name='BF_null') +
    theme_classic() + coord_fixed()
ggsave('plots/gp_contrast_bf_hex.png', width=5, height=4)


contr.group %>%
    median_hdci() %>%
    filter((BF < .1 | BF > 10)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    facet_grid(COND ~ .) + xlab('X') + ylab('Y') +
    geom_polygon(size=0.3) +
    scale_fill_scico(palette='vik', limits=c(-6, 6)) +
    scale_color_scico(palette='vik', limits=c(-6, 6)) +
    theme_classic() +
    coord_fixed(xlim=range(contr.group$vx), ylim=range(contr.group$vy))

ggsave('plots/gp_group_contrast_sig_hex.png', width=5, height=4)
