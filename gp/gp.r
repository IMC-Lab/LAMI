library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)
library(bayestestR)
library(viridis)
library(patchwork)
library(scico)
options(mc.cores = parallel::detectCores())

source('grid.r')

data.fix <- read.csv('../data/test/KKTest1_BlankFixations_report.csv',
                     header=TRUE) %>%
    filter(CURRENT_FIX_INDEX > 1) %>%
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
             ymin=-150, ymax=300)

data.hex <- data.fix %>%
    group_by(COND, PAR, TRIAL_INDEX) %>%
    bin.hex(xmin=-450, xmax=550, xsize=50,
            ymin=-150, ymax=300)

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

## Sample the GP prior
gp.prior <- readRDS('gp-prior.rds')
gp.prior <- stan(file='gp-prior.stan', data=data.stan, algorithm='Fixed_param', iter=1000, warmup=0)
saveRDS(gp.prior, 'gp-prior.rds')

## Fit & sample the model posterior
gp.fit <- readRDS('gp.rds')
gp.fit <- stan(file='gp.stan', data=data.stan)
saveRDS(gp.fit, 'gp.rds')


print(gp.prior, prob=c(0.025, 0.5, 0.975), 
      pars=c('prior_a', 'prior_rho', 'prior_rho_tilde',
             'prior_alpha', 'prior_alpha_tilde'))
print(gp.fit, prob=c(0.025, 0.5, 0.975), 
      pars=c('a', 'rho', 'rho_tilde', 'alpha', 'alpha_tilde', 'lp__'))

plot(gp.prior, pars=c('prior_rho', 'prior_rho_tilde'))
plot(gp.prior, pars=c('prior_alpha', 'prior_alpha_tilde'))
plot(gp.fit, pars=c('rho', 'rho_tilde'))
plot(gp.fit, pars=c('alpha', 'alpha_tilde'))


## extract model fits for each trial
draws.trial <- full_join(spread_draws(gp.prior, prior_lambda[.row]),
                         spread_draws(gp.fit, lambda[.row])) %>%
    bind_cols(., data.hex[.$.row, c('PAR', 'COND', 'grid_idx', 'TRIAL_INDEX',
                                    'bin_x', 'bin_y', 'vi', 'vx', 'vy', 'count')])

## extract group-level model fits for each condition
draws.group <- full_join(spread_draws(gp.prior, prior_a, prior_f[COND, grid_idx]),
                         spread_draws(gp.fit, a, f[COND, grid_idx])) %>%
    mutate(prior_lambda=prior_a+prior_f,
           lambda=a+f,
           BF=1/bayesfactor_pointull(f, prior_f)$BF) %>%
    mutate(BF=max(1/max(.draw), min(1-1/max(.draw), BF))) %>%   ## round BFs to make logs finite
    bind_cols(., data.grid[.$grid_idx, c('bin_x', 'bin_y', 'vi', 'vx', 'vy')])



draws.trial %>%
    group_by(PAR, COND, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Fit (participant-level)') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    facet_grid(COND ~ PAR, labeller=label_both) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() + coord_fixed()
ggsave('plots/gp_par.png', width=5, height=4)

draws.trial %>%
    group_by(PAR, COND, grid_idx, vx, vy) %>%
    median_hdci(prior_lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Prior (participant-level)') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    geom_polygon(aes(fill=exp(prior_lambda), color=exp(prior_lambda)), size=0.3) +
    facet_grid(COND ~ PAR, labeller=label_both) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() + coord_fixed()
ggsave('plots/gp_par_prior.png', width=5, height=4)


draws.group %>%
    group_by(COND, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Fit') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(COND ~ ., labeller=label_both) +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() + coord_fixed()
ggsave('plots/gp_fit.png', width=5, height=4)

draws.group %>%
    group_by(COND, grid_idx, vx, vy) %>%
    median_hdci(prior_lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Prior') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(COND ~ ., labeller=label_both) +
    geom_polygon(aes(fill=exp(prior_lambda), color=exp(prior_lambda)), size=0.3) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() + coord_fixed()
ggsave('plots/gp_fit_prior.png', width=5, height=4)


draws.group$BF %>% log10 %>% summary
bf.breaks <- seq(-4, 4, 1)
draws.group %>%
    group_by(COND, grid_idx, vx, vy) %>%
    median_hdci(f, BF) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Bayes Factors') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(COND ~ ., labeller=label_both) +
    geom_polygon(aes(fill=log10(BF), color=log10(BF)), size=0.3) +
    scale_fill_scico(palette='vik', direction=-1,
                     limits=range(bf.breaks), breaks=bf.breaks,
                     labels=10^bf.breaks, name='BF_null') +
    scale_color_scico(palette='vik', direction=-1,
                      limits=range(bf.breaks), breaks=bf.breaks,
                      labels=10^bf.breaks, name='BF_null') +
    theme_classic() + coord_fixed()
ggsave('plots/gp_fit_bf.png', width=5, height=4)

draws.group %>%
    group_by(COND, grid_idx, vx, vy) %>%
    median_hdci(f, BF) %>%
    unnest(c(vx, vy)) %>%
    filter((BF < .1 | BF > 10) & (f.lower > 0 | f.upper < 0)) %>%
    ggplot() + ggtitle('GP Bayes Factors (significant pixels)') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(COND ~ ., labeller=label_both) +
    geom_polygon(aes(fill=log10(BF), color=log10(BF)), size=0.3) +
    scale_fill_scico(palette='vik', direction=-1,
                     limits=range(bf.breaks), breaks=bf.breaks,
                     labels=10^bf.breaks, name='BF_null') +
    scale_color_scico(palette='vik', direction=-1,
                      limits=range(bf.breaks), breaks=bf.breaks,
                      labels=10^bf.breaks, name='BF_null') +
    theme_classic() +
    coord_fixed(xlim=range(draws.group$vx), ylim=range(draws.group$vy))
ggsave('plots/gp_fit_bf_sig.png', width=5, height=4)

draws.group %>%
    group_by(COND, grid_idx, vx, vy) %>%
    median_hdci(f, lambda) %>%
    filter(f.lower > 0 | f.upper < 0) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Fit (significant pixels)') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(COND ~ ., labeller=label_both) +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    theme_classic() +
    coord_fixed(xlim=range(draws.group$vx), ylim=range(draws.group$vy))
ggsave('plots/gp_fit_sig.png', width=5, height=4)


## Group-level contrasts
contr.group <- draws.group %>%
    group_by(COND, grid_idx, bin_x, bin_y, vx, vy) %>%
    compare_levels(lambda, by=COND, comparison='control') %>%
    left_join(draws.group %>%
              group_by(COND, grid_idx, bin_x, bin_y, vx, vy) %>%
              compare_levels(prior_lambda, by=COND, comparison='control')) %>%
    group_by(COND, grid_idx, bin_x, bin_y, vx, vy) %>%
    mutate(BF=1/bayesfactor_pointull(lambda, prior_lambda)$BF) %>%
    mutate(BF=max(1/max(.draw), min(1-1/max(.draw), BF)))   ## round BFs to make logs finite


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
    scale_fill_scico(palette='vik', limits=c(-6.1, 6.1), name='Rate') +
    scale_color_scico(palette='vik', limits=c(-6.1, 6.1), name='Rate') +
    theme_classic() + coord_fixed()
ggsave('plots/gp_contrast.png', width=5, height=4)

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
ggsave('plots/gp_contrast_prior.png', width=5, height=4)

contr.group %>% median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    facet_grid(COND ~ .) + xlab('X') + ylab('Y') +
    ggtitle('Group-level Fixation Rate Contrast BFs') +
    geom_polygon(size=0.3) +
    scale_fill_scico(palette='vik', direction=-1,
                     limits=range(bf.breaks), breaks=bf.breaks,
                     labels=10^bf.breaks, name='BF_null') +
    scale_color_scico(palette='vik', direction=-1,
                     limits=range(bf.breaks), breaks=bf.breaks,
                     labels=10^bf.breaks, name='BF_null') +
    theme_classic() + coord_fixed()
ggsave('plots/gp_contrast_bf.png', width=5, height=4)

contr.group %>% median_hdci() %>%
    filter((BF < .1 | BF > 10) & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    facet_grid(COND ~ .) + xlab('X') + ylab('Y') +
    ggtitle('Group-level Fixation Rate Contrast BFs\n(significant pixels)') +
    geom_polygon(size=0.3) +
    scale_fill_scico(palette='vik', direction=-1,
                     limits=range(bf.breaks), breaks=bf.breaks,
                     labels=10^bf.breaks, name='BF_null') +
    scale_color_scico(palette='vik', direction=-1,
                     limits=range(bf.breaks), breaks=bf.breaks,
                     labels=10^bf.breaks, name='BF_null') +
    theme_classic() +
    coord_fixed(xlim=range(contr.group$vx), ylim=range(contr.group$vy))
ggsave('plots/gp_contrast_bf_sig.png', width=5, height=4)


contr.group %>%
    median_hdci() %>%
    filter((BF < .1 | BF > 10) & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    facet_grid(COND ~ .) + xlab('X') + ylab('Y') +
    ggtitle('Group-level Fixation Rate Contrasts\n(significant pixels)') +
    geom_polygon(size=0.3) +
    scale_fill_scico(palette='vik', limits=c(-6.1, 6.1), name='Rate') +
    scale_color_scico(palette='vik', limits=c(-6.1, 6.1), name='Rate') +
    theme_classic() +
    coord_fixed(xlim=range(contr.group$vx), ylim=range(contr.group$vy))
ggsave('plots/gp_contrast_sig.png', width=5, height=4)
