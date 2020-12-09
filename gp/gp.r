library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)
library(viridis)
library(patchwork)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## data_grid(df, xmin=-600, xmax=600, xsize=10,
##           ymin=-300, ymax=300, ysize=10)
##
## Create a grid of spatial locations determined by
## the minimum, maximum, and step size for the x and y axes.
## This grid will be replicated for all existing grouping variables
## in the data frame df.
data_grid <- function(df, xmin=-600, xmax=600, xsize=10,
                      ymin=-300, ymax=300, ysize=10) {
    df %>%
        expand(bin_x=seq(xmin, xmax, xsize),
               bin_y=seq(ymin, ymax, ysize)) %>%
        mutate(grid_idx=row_number()) %>%
        return()
}

bin_space <- function(df, xmin=-600, xmax=600, xsize=10,
                      ymin=-300, ymax=300, ysize=10,
                      grid=data_grid(df, xmin=xmin, xmax=xmax, xsize=xsize,
                                     ymin=ymin, ymax=ymax, ysize=ysize)) {
    ## find break points from bin centers
    x_bins <- grid %>% pull(bin_x) %>% unique
    y_bins <- grid %>% pull(bin_y) %>% unique
    x_breaks <- x_bins[-length(x_bins)] + xsize/2
    y_breaks <- y_bins[-length(y_bins)] + ysize/2
    
    ## bin df according to the grid
    df %>%
        mutate(bin_x=x_bins[findInterval(CURRENT_FIX_X, x_breaks) + 1],
               bin_y=y_bins[findInterval(CURRENT_FIX_Y, y_breaks) + 1]) %>%
        group_by(bin_x, bin_y, .add=TRUE) %>%
        summarize(count=n()) %>%
        right_join(grid) %>%
        mutate(count=replace_na(count, 0)) %>%
        return()
}


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
                          PAR=as.factor(c(1, 1, 1, 2, 2,
                                          1, 2, 1, 1, 2,
                                          1, 2, 2, 1, 2,
                                          2, 2, 1))) %>%
    group_by(PAR) %>%
    mutate(COND=as.factor(rep(1:3, 3)))

data.fix <- data.fix %>% mutate(PAR=data.groups$PAR[TRIAL_INDEX],
                                COND=data.groups$COND[TRIAL_INDEX])

ggplot(data.fix) +
    aes(x=CURRENT_FIX_X, y=CURRENT_FIX_Y, color=as.factor(PAR)) +
    geom_point() +
    facet_wrap(PAR ~ COND) +
    coord_cartesian(c(-450, 500), c(-150, 300)) +
    theme_bw() + theme(aspect.ratio=0.5)

ggplot(data.fix) +
    aes(x=CURRENT_FIX_X, y=CURRENT_FIX_Y, color=as.factor(PAR)) +
    geom_point() +
    facet_wrap(~ TRIAL_INDEX) +
    coord_cartesian(c(-600, 600), c(-300, 300)) +
    theme_bw() + theme(aspect.ratio=0.5)



data.grid <- data.fix %>%
    data_grid(xmin=-450, xmax=550, xsize=50,
              ymin=-200, ymax=300, ysize=50)

data.binned <- data.fix %>%
    group_by(PAR, COND, TRIAL_INDEX) %>%
    bin_space(xmin=-450, xmax=550, xsize=50,
              ymin=-200, ymax=300, ysize=50)

plot.data.par <- data.binned %>%
    group_by(COND, PAR, bin_x, bin_y) %>%
    summarize(count=mean(count)) %>%
    ggplot(aes(x=bin_x, y=bin_y, fill=count)) +
    geom_raster() +
    facet_grid(COND ~ PAR, labeller=label_both) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, 4)) +
    theme_classic() + theme(aspect.ratio=0.5)

plot.data.group <- data.binned %>%
    group_by(COND, bin_x, bin_y) %>%
    summarize(count=mean(count)) %>%
    ggplot(aes(x=bin_x, y=bin_y, fill=count)) +
    geom_raster() +
    facet_grid(COND ~ ., labeller=label_both) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, 2.5)) +
    theme_classic() + theme(aspect.ratio=0.5)


design.matrix <- model.matrix(~ COND, data.binned)
data.pred <- expand(data.grid, nesting(grid_idx, bin_x, bin_y),
                    COND=unique(data.fix$COND))
data.stan <- list(N=nrow(data.binned),
                  G=nrow(data.grid),
                  P=length(unique(data.binned$PAR)),
                  D=2,  ## 2 dimensions (X, Y)
                  K=ncol(design.matrix),
                  M=ncol(design.matrix),
                  C=3,
                  g=data.binned$grid_idx,
                  p=as.integer(data.binned$PAR),
                  c=as.integer(data.binned$COND),
                  grid=select(data.grid, bin_x, bin_y),
                  X=design.matrix,
                  Z=design.matrix,
                  y=data.binned$count,
                  Npred=nrow(data.pred),
                  gpred=data.pred$grid_idx,
                  cpred=as.integer(data.pred$COND),
                  Xpred=model.matrix(~ COND, data.pred))


gp.fit <- readRDS('gp.rds')

gp.fit <- stan(file='gp.stan', data=data.stan, chains=2)

saveRDS(gp.fit, 'gp.rds')


print(gp.fit, prob=c(0.025, 0.5, 0.975), 
      pars=c('a', 'beta', 'sigma','gamma',
             'rho', 'rho_tilde', 'alpha', 'alpha_tilde'))

plot(gp.fit, pars=c('a', 'beta', 'sigma', 'gamma'))
pairs(gp.fit, pars=c('sigma', 'gamma'))

## extract model fits for each trial
draws.trial <- spread_draws(gp.fit, lambda[.row], yhat[.row]) %>%
    bind_cols(., data.binned[.$.row, c('PAR', 'COND', 'bin_x', 'bin_y', 'grid_idx')])

## extract group-level model fits for each condition
draws.group <- spread_draws(gp.fit, lambda_group[.row], yhat_group[.row]) %>%
    rename(lambda=lambda_group,
           yhat=yhat_group) %>%
    bind_cols(., data.pred[.$.row, c('COND', 'bin_x', 'bin_y', 'grid_idx')])


plot.pred.par <- draws.trial %>%
    group_by(PAR, COND, bin_x, bin_y) %>%
    median_hdci(lambda) %>%
    ggplot() + ggtitle('GP Fit (by subject)') + 
    aes(x=bin_x, y=bin_y) + xlab('X') + ylab('Y') +
    geom_raster(aes(fill=exp(lambda))) +
    facet_grid(COND ~ PAR, labeller=label_both) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, 4)) +
    theme_classic() + theme(aspect.ratio=0.5)

(plot.data.par | plot.pred.par) + plot_layout(guides='collect')
ggsave('plots/gp_fit.png')


plot.pred.group <- draws.group %>%
    group_by(COND, bin_x, bin_y) %>%
    median_hdci(lambda) %>%
    ggplot() + ggtitle('GP Fit (group)') + 
    aes(x=bin_x, y=bin_y) + xlab('X') + ylab('Y') +
    facet_grid(COND ~ ., labeller=label_both) +
    geom_raster(aes(fill=exp(lambda))) +
    scale_fill_viridis(option='magma', name='Rate') + ##, limits=c(0, 2.5)) +
    theme_classic() + theme(aspect.ratio=0.5)

(plot.data.group | plot.pred.group) + plot_layout(guides='collect')
ggsave('plots/gp_group_fit.png')




## Group-level contrasts
contr.group <- draws.group %>%
    group_by(COND, bin_x, bin_y) %>%
    compare_levels(lambda, by=COND)

contr.group %>% median_hdci() %>%
    ggplot(aes(x=bin_x, y=bin_y, fill=lambda)) +
    facet_grid(COND ~ .) +
    geom_raster(aes(fill=lambda)) +
    scale_fill_viridis(option='magma', name='Rate') +
    theme_classic() + theme(aspect.ratio=0.5)
    
contr.group %>% median_hdci() %>%
    filter(.upper < 0 | .lower > 0)
