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
                      ymin=-300, ymax=300, ysize=10) {
    ## initialize an empty grid (including the existing grouping structure)
    df.grid <- data_grid(df, xmin=xmin, xmax=xmax, xsize=xsize,
                         ymin=ymin, ymax=ymax, ysize=ysize)
    
    ## bin df according to the grid
    df %>%
        mutate(bin_x=xsize*round(CURRENT_FIX_X/xsize),
               bin_y=ysize*round(CURRENT_FIX_Y/ysize)) %>%
        group_by(bin_x, bin_y, .add=TRUE) %>%
        summarize(count=n()) %>%
        right_join(df.grid) %>%
        mutate(count=replace_na(count, 0)) %>%
        return()
}


data.fix <- read.csv('data/KKTest1_BlankFixations_report.csv', header=TRUE) %>%
    mutate(CURRENT_FIX_X = CURRENT_FIX_X - 1920/2,
           CURRENT_FIX_Y = CURRENT_FIX_Y - 1080/2) %>%
    group_by(TRIAL_INDEX) %>%
    mutate(PREV_FIX_X = lag(CURRENT_FIX_X),
           PREV_FIX_Y = lag(CURRENT_FIX_Y)) %>%
    ungroup()

data.groups <- data.frame(TRIAL_INDEX=unique(data.fix$TRIAL_INDEX),
                          SUB=c(1, 1, 1, 2, 2,
                                1, 2, 1, 1, 2,
                                1, 2, 2, 1, 2,
                                2, 2, 1))

data.fix <- data.fix %>% mutate(SUB=data.groups$SUB[TRIAL_INDEX])

ggplot(data.fix) +
    aes(x=CURRENT_FIX_X, y=CURRENT_FIX_Y, color=as.factor(SUB)) +
    geom_point() +
    coord_cartesian(c(-600, 600), c(-300, 300)) +
    theme_bw() + theme(aspect.ratio=0.5)


ggplot(data.fix) +
    aes(x=CURRENT_FIX_X, y=CURRENT_FIX_Y, color=as.factor(SUB)) +
    geom_point() +
    facet_wrap(~ TRIAL_INDEX) +
    coord_cartesian(c(-600, 600), c(-300, 300)) +
    theme_bw() + theme(aspect.ratio=0.5)

plot.data <- data.fix %>%
    group_by(SUB, TRIAL_INDEX) %>%
    bin_space(xsize=100, ysize=100) %>%
    group_by(SUB, bin_x, bin_y) %>%
    summarize(count=mean(count)) %>%
    ggplot() + ggtitle('Raw Data') +
    aes(x=bin_x, y=bin_y, fill=count) + xlab('X') + ylab('Y') +
    geom_raster() +
    facet_wrap(~ SUB) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, 6)) +
    theme_classic() + theme(aspect.ratio=0.5)


data.grid <- data.fix %>%
    data_grid(xsize=100, ysize=100) %>%
    select(-grid_idx)
data.binned <- data.fix %>%
    group_by(SUB, TRIAL_INDEX) %>%
    bin_space(xsize=100, ysize=100)



data.stan <- list(D=2,  ## 2 dimensions (X, Y)
                  ## data for fitting
                  N=nrow(data.binned),
                  N_grid=nrow(data.grid),
                  N_group=length(unique(data.binned$SUB)),
                  idx_group=data.binned$SUB,
                  idx_grid=data.binned$grid_idx,
                  x=data.grid,
                  y=data.binned$count)


gp.fit <- readRDS('gp.rds')

gp.fit <- stan(file='gp.stan', data=data.stan, chains=2)

saveRDS(gp.fit, 'gp.rds')


print(gp.fit, prob=c(0.025, 0.5, 0.975),
      pars=c('m_intercept', 'sd_intercept', 'intercept',
             'md_rho', 'sd_rho', 'rho',
             'md_alpha', 'sd_alpha', 'alpha',
             'm_eta', 'sd_eta'))


p <- tidy_draws(gp.fit) %>% select(-starts_with('eta'))

pred <- p %>% select(.chain, .iteration, .draw,
                     starts_with('lambda'), starts_with('yhat')) %>%
    pivot_longer('lambda[1]':paste0('yhat[', data.stan$N,']'),
                 names_to=c('parameter', '.row'),
                 names_pattern='([[:alnum:]]+)\\[([[:digit:]]+)\\]') %>%
    mutate(.row=as.integer(.row)) %>%
    cbind(., data.binned[.$.row, c('SUB', 'TRIAL_INDEX',
                                  'bin_x', 'bin_y', 'grid_idx')])  %>%
    pivot_wider(names_from='parameter') %>%
    full_join(select(p, .chain, .iteration, .draw))


plot.pred <- pred %>%
    group_by(SUB, bin_x, bin_y) %>%
    mean_hdi(lambda) %>%
    ggplot() + ggtitle('GP Fit') + 
    aes(x=bin_x, y=bin_y) + xlab('X') + ylab('Y') +
    geom_raster(aes(fill=exp(lambda))) +
    facet_wrap(~ SUB) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, 6)) +
    theme_classic() + theme(aspect.ratio=0.5)

plot.data / plot.pred + plot_layout(guides='collect')
