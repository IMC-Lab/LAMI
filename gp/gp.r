library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(tidybayes)
library(bayestestR)
library(viridis)
library(patchwork)
library(scico)
library(ggimage)

options(mc.cores=parallel::detectCores())
source('grid.r')

data.fix <- read_csv('../data/LAMI_fixations.csv') %>%
    filter(fix_on_screen & fix_on_video & fix_duration > 75 & fix_index != 1) %>%
    mutate(fix_x=ifelse(ball_direction=='left', -fix_x, fix_x),
           fix_y=ifelse(display=='down', -fix_y, fix_y),
           simulation=factor(simulation, levels=c('Remember', 'What if?', 'Cause')),
           stage=factor(stage, levels=c('encoding', 'simulation'), labels=c('Encoding', 'Simulation')),
           outcome=factor(outcome, levels=c('miss', 'score'), labels=c('Miss', 'Score')),
           condition=interaction(stage, simulation, outcome),
           image=paste0('../stimuli/img/LAMI_', ball_direction, '_', outcome, '_', display, '.png'),
           image.reflected=paste0('../stimuli/img/LAMI_', outcome, '_', position, '.png'),
           ## exclude trials in which participants failed the attention check
           attn_check_lr=ifelse(attn_check <= 0.5, 'left', 'right'),
           check=ifelse(position=='ball', attn_check_lr == goalie_direction, attn_check_lr == ball_direction)) %>%
    filter(check)


data.img <- data.fix %>%
    group_by(direction, outcome, display, simulation, position) %>%
    summarize(image=image[1],
              image.reflected=image.reflected[1]) %>%
    filter(direction == 'right')


plot.encoding.ball <- data.fix %>%
    filter(position=='ball' & stage=='Encoding') %>%
    ggplot(aes(x=fix_x, y=fix_y)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img %>% filter(position=='ball')) +
    geom_point(size=0.33, alpha=0.25) +
    ##coord_fixed(xlim=c(-960, 960), ylim=c(-540, 540), expand=FALSE) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
plot.encoding.goalie <- data.fix %>%
    filter(position=='goalie' & stage=='Encoding') %>%
    ggplot(aes(x=fix_x, y=fix_y)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img %>% filter(position=='goalie')) +
    geom_point(size=0.33, alpha=0.25) +
    ##coord_fixed(xlim=c(-960, 960), ylim=c(-540, 540), expand=FALSE) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())

(plot.encoding.ball / plot.encoding.goalie) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/raw_data_encoding.png', width=7, height=7)

plot.simulation.ball <- data.fix %>%
    filter(position=='ball' & stage=='Simulation') %>%
    ggplot(aes(x=fix_x, y=fix_y)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img %>% filter(position=='ball')) +
    geom_point(size=0.33, alpha=0.25) +
    ##coord_fixed(xlim=c(-960, 960), ylim=c(-540, 540), expand=FALSE) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
plot.simulation.goalie <- data.fix %>%
    filter(position=='goalie' & stage=='Simulation') %>%
    ggplot(aes(x=fix_x, y=fix_y)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img %>% filter(position=='goalie')) +
    geom_point(size=0.33, alpha=0.25) +
    ##coord_fixed(xlim=c(-960, 960), ylim=c(-540, 540), expand=FALSE) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())

(plot.simulation.ball / plot.simulation.goalie) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/raw_data_simulation.png', width=7, height=7)



## Bin fixations into hexagons
data.grid <- data.fix %>%
    grid.hex(xmin=-400, xmax=400, xsize=35,
             ymin=-300, ymax=300)

data.hex <- data.fix %>%
    group_by(participant, trial, condition,
             stage, simulation, outcome, position) %>%
    bin.hex(xmin=-400, xmax=400, xsize=35,
            ymin=-300, ymax=300)

data.ball <- data.hex %>% filter(position == 'ball')
data.goalie <- data.hex %>% filter(position == 'goalie')


plot.data.group <- data.hex %>%
    group_by(stage, simulation, outcome, position,
             bin_x, bin_y, grid_idx, vi, vx, vy) %>%
    summarize(count=mean(count)) %>%
    unnest(c(vi, vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=count, color=count)) +
    geom_polygon(size=0.3) + 
    ggtitle('Raw Data (group)') + xlab('X') + ylab('Y') + 
    scale_color_viridis(option='magma', name='Count', limits=c(0, NA)) +
    scale_fill_viridis(option='magma', name='Count', limits=c(0, NA)) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(stage+outcome ~ position+simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
plot.data.group
ggsave('plots/raw_data_group_hex.png', width=12, height=8)

plot.encoding.ball <- data.ball %>%
    filter(stage=='Encoding') %>%
    group_by(stage, simulation, outcome, position,
             bin_x, bin_y, grid_idx, vi, vx, vy) %>%
    summarize(count=mean(count)) %>%
    unnest(c(vi, vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=count, color=count)) +
    geom_polygon(size=0.3) + 
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
plot.encoding.goalie <- data.goalie %>%
    filter(stage=='Encoding') %>%
    group_by(stage, simulation, outcome, position,
             bin_x, bin_y, grid_idx, vi, vx, vy) %>%
    summarize(count=mean(count)) %>%
    unnest(c(vi, vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=count, color=count)) +
    geom_polygon(size=0.3) + 
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
((plot.encoding.ball / plot.encoding.goalie) &
 scale_color_viridis(option='magma', name='Count', limits=c(0, 0.35)) &
 scale_fill_viridis(option='magma', name='Count', limits=c(0, 0.35))) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A') 
ggsave('plots/raw_data_encoding_hex.png', width=7, height=7)

plot.simulation.ball <- data.ball %>%
    filter(stage=='Simulation') %>%
    group_by(stage, simulation, outcome, position,
             bin_x, bin_y, grid_idx, vi, vx, vy) %>%
    summarize(count=mean(count)) %>%
    unnest(c(vi, vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=count, color=count)) +
    geom_polygon(size=0.3) + 
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
plot.simulation.goalie <- data.goalie %>%
    filter(stage=='Simulation') %>%
    group_by(stage, simulation, outcome, position,
             bin_x, bin_y, grid_idx, vi, vx, vy) %>%
    summarize(count=mean(count)) %>%
    unnest(c(vi, vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=count, color=count)) +
    geom_polygon(size=0.3) + 
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
((plot.simulation.ball / plot.simulation.goalie) &
 scale_color_viridis(option='magma', name='Count', limits=c(0, 0.35)) &
 scale_fill_viridis(option='magma', name='Count', limits=c(0, 0.35))) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/raw_data_simulation_hex.png', width=7, height=7)




data.ball.stan <- list(N=nrow(data.ball),
                       G=max(data.ball$grid_idx),
                       P=length(unique(data.ball$participant)),
                       D=2,  ## 2 dimensions (X, Y)
                       C=length(levels(data.ball$condition)),
                       g=data.ball$grid_idx,
                       p=as.integer(as.factor(data.ball$participant)),
                       c=as.integer(data.ball$condition),
                       grid=select(data.grid, bin_x, bin_y),
                       y=data.ball$count)
str(data.ball.stan)
data.goalie.stan <- list(N=nrow(data.goalie),
                       G=max(data.goalie$grid_idx),
                       P=length(unique(data.goalie$participant)),
                       D=2,  ## 2 dimensions (X, Y)
                       C=length(levels(data.goalie$condition)),
                       g=data.goalie$grid_idx,
                       p=as.integer(as.factor(data.goalie$participant)),
                       c=as.integer(data.goalie$condition),
                       grid=select(data.grid, bin_x, bin_y),
                       y=data.goalie$count)
str(data.goalie.stan)



## Sample the GP prior
mod.prior <- cmdstan_model('gp-prior.stan')

gp.ball.prior <- readRDS('gp-ball-prior-35.rds')
gp.ball.prior <- mod.prior$sample(data=data.ball.stan, iter_warmup=0, iter_sampling=4000, fixed_param=TRUE, output_dir='draws')
gp.ball.prior$save_object('gp-ball-prior-35.rds')

gp.goalie.prior <- readRDS('gp-goalie-prior-35.rds')
gp.goalie.prior <- mod.prior$sample(data=data.goalie.stan, iter_warmup=0, iter_sampling=4000, fixed_param=TRUE, output_dir='draws')
gp.goalie.prior$save_object('gp-goalie-prior-35.rds')

gp.ball.prior$draws(c('prior_a', 'prior_rho', 'prior_rho_tilde',
                      'prior_alpha', 'prior_alpha_tilde')) %>%
    summarize_draws()
gp.goalie.prior$draws(c('prior_a', 'prior_rho', 'prior_rho_tilde',
                      'prior_alpha', 'prior_alpha_tilde')) %>%
    summarize_draws()
gp.ball.prior$draws(c('prior_a', 'prior_alpha', 'prior_alpha_tilde')) %>% mcmc_areas(prob=.95)
gp.goalie.prior$draws(c('prior_a', 'prior_alpha', 'prior_alpha_tilde')) %>% mcmc_areas(prob=.95)
gp.ball.prior$draws(c('prior_rho', 'prior_rho_tilde')) %>% mcmc_areas(prob=.95)
gp.goalie.prior$draws(c('prior_rho', 'prior_rho_tilde')) %>% mcmc_areas(prob=.95)




## Fit & sample the model posterior
mod <- cmdstan_model('gp.stan')

gp.ball.fit <- readRDS('gp-ball-35.rds')
gp.ball.fit <- mod$sample(data=data.ball.stan, iter_warmup=1000, iter_sampling=1000, output_dir='draws')
gp.ball.fit$save_object('gp-ball-35.rds')

gp.goalie.fit <- readRDS('gp-goalie-35.rds')
gp.goalie.fit <- mod$sample(data=data.goalie.stan, iter_warmup=1000, iter_sampling=1000, output_dir='draws')
gp.goalie.fit$save_object('gp-goalie-35.rds')


gp.ball.fit$draws(c('a', 'rho', 'rho_tilde', 'alpha', 'alpha_tilde')) %>%
    summarize_draws()
gp.goalie.fit$draws(c('a', 'rho', 'rho_tilde', 'alpha', 'alpha_tilde')) %>%
    summarize_draws()
gp.ball.fit$draws('a') %>% mcmc_areas(prob=.95)
gp.goalie.fit$draws('a') %>% mcmc_areas(prob=.95)
gp.ball.fit$draws(c('alpha', 'alpha_tilde')) %>% mcmc_areas(prob=.95)
gp.goalie.fit$draws(c('alpha', 'alpha_tilde')) %>% mcmc_areas(prob=.95)
gp.ball.fit$draws(c('rho', 'rho_tilde')) %>% mcmc_areas(prob=.95)
gp.goalie.fit$draws(c('rho', 'rho_tilde')) %>% mcmc_areas(prob=.95)



## extract group-level model fits for each condition
draws.ball.group <- full_join(spread_draws(gp.ball.prior, prior_a, prior_f[COND, grid_idx]) %>%
                              mutate(.chain=(.iteration-1) %/% 1000 + 1,
                                     .iteration=(.iteration-1) %% 1000 + 1),
                              spread_draws(gp.ball.fit, a, f[COND, grid_idx])) %>%
    mutate(prior_lambda=prior_a+prior_f,
           lambda=a+f) %>%
    bind_cols(., data.grid[.$grid_idx, c('bin_x', 'bin_y', 'vi', 'vx', 'vy')]) %>%
    mutate(condition=levels(data.fix$condition)[COND]) %>%
    separate(condition, c('stage', 'simulation', 'outcome'), sep='\\.') %>%
    mutate(stage=as.factor(stage),
           position='ball',
           simulation=factor(simulation, levels=c('Remember', 'What if?', 'Cause')),
           outcome=factor(outcome))
draws.goalie.group <- full_join(spread_draws(gp.goalie.prior, prior_a, prior_f[COND, grid_idx]) %>%
                              mutate(.chain=(.iteration-1) %/% 1000 + 1,
                                     .iteration=(.iteration-1) %% 1000 + 1),
                                spread_draws(gp.goalie.fit, a, f[COND, grid_idx])) %>%
    mutate(prior_lambda=prior_a+prior_f,
           lambda=a+f) %>%
    bind_cols(., data.grid[.$grid_idx, c('bin_x', 'bin_y', 'vi', 'vx', 'vy')]) %>%
    mutate(condition=levels(data.fix$condition)[COND]) %>%
    separate(condition, c('stage', 'simulation', 'outcome'), sep='\\.') %>%
    mutate(stage=as.factor(stage),
           position='goalie',
           simulation=factor(simulation, levels=c('Remember', 'What if?', 'Cause')),
           outcome=factor(outcome))
draws.group <- bind_rows(draws.ball.group, draws.goalie.group)


plot.encoding.ball <- draws.ball.group %>%
    filter(stage=='Encoding') %>%
    group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() +
    aes(x=vx, y=vy, group=grid_idx) +
    facet_grid(stage+outcome ~ position+simulation) +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
plot.encoding.goalie <- draws.goalie.group %>%
    filter(stage=='Encoding') %>%
    group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() +
    aes(x=vx, y=vy, group=grid_idx) +
    facet_grid(stage+outcome ~ position+simulation) +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
((plot.encoding.ball / plot.encoding.goalie) &
 scale_color_viridis(option='magma', name='Count', limits=c(0, 0.2)) &
 scale_fill_viridis(option='magma', name='Count', limits=c(0, 0.2))) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A') 
ggsave('plots/gp_fit_encoding.png', width=7, height=7)

plot.simulation.ball <- draws.ball.group %>%
    filter(stage=='Simulation') %>%
    group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() +
    aes(x=vx, y=vy, group=grid_idx) +
    facet_grid(stage+outcome ~ position+simulation) +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
plot.simulation.goalie <- draws.goalie.group %>%
    filter(stage=='Simulation') %>%
    group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() +
    aes(x=vx, y=vy, group=grid_idx) +
    facet_grid(stage+outcome ~ position+simulation) +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    facet_grid(outcome ~ simulation) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
((plot.simulation.ball / plot.simulation.goalie) &
 scale_color_viridis(option='magma', name='Count', limits=c(0, 0.2)) &
 scale_fill_viridis(option='magma', name='Count', limits=c(0, 0.2))) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A') 
ggsave('plots/gp_fit_simulation.png', width=7, height=7)


draws.group %>%
    group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
    median_hdci(lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Fit') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(stage+outcome ~ position+simulation) +
    geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
ggsave('plots/gp_fit.png', width=12, height=8)

draws.group %>%
    group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
    median_hdci(prior_lambda) %>%
    unnest(c(vx, vy)) %>%
    ggplot() + ggtitle('GP Prior') + 
    aes(x=vx, y=vy, group=grid_idx) + xlab('X') + ylab('Y') +
    facet_grid(stage+outcome ~ position+simulation) +
    geom_polygon(aes(fill=exp(prior_lambda), color=exp(prior_lambda)), size=0.3) +
    scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
ggsave('plots/gp_fit_prior_35.png', width=12, height=8)


## Group-level contrasts
data.img.contr <- data.img %>%
    mutate(simulation=factor(case_when(simulation == 'Remember' ~ 'What if? - Remember',
                                       simulation == 'What if?' ~ 'Cause - Remember',
                                       simulation == 'Cause' ~ 'Cause - What if?'),
                             levels=c('What if? - Remember', 'Cause - Remember', 'Cause - What if?')))

contr.group <- draws.group %>%
    group_by(stage, simulation, outcome, position, grid_idx, bin_x, bin_y, vx, vy) %>%
    compare_levels(lambda, by=simulation) %>%
    left_join(draws.group %>%
              group_by(stage, simulation, outcome, position, grid_idx, bin_x, bin_y, vx, vy) %>%
              compare_levels(prior_lambda, by=simulation)) %>%
    group_by(stage, simulation, outcome, position, grid_idx, bin_x, bin_y, vx, vy) %>%
    mutate(BF=exp(bayesfactor_pointnull(lambda, prior_lambda)$log_BF)) %>%
    mutate(BF=max(1/max(.draw), min(max(.draw), BF)),     ## round BFs to make logs finite
           simulation=factor(simulation, levels=c('What if? - Remember', 'Cause - Remember', 'Cause - What if?')))


## get cluster maximums
clusters.max <- contr.group %>% median_hdci %>%
    filter(BF > 10 & lambda.lower > 0) %>%
    group_by(stage, simulation, outcome, position) %>%
    summarize(i=which.max(lambda),
              lambda=lambda[i], upper=lambda.upper[i], lower=lambda.lower[i],
              BF=BF[i], grid_idx=grid_idx[i], bin_x=bin_x[i], bin_y=bin_y[i], vx=vx[i], vy=vy[i])
clusters.min <- contr.group %>% median_hdci %>%
    filter(BF > 10 & lambda.upper < 0) %>%
    group_by(stage, simulation, outcome, position) %>%
    summarize(i=which.min(lambda),
              lambda=lambda[i], upper=lambda.upper[i], lower=lambda.lower[i],
              BF=BF[i], grid_idx=grid_idx[i], bin_x=bin_x[i], bin_y=bin_y[i], vx=vx[i], vy=vy[i])

clusters.max %>%
    bind_rows(clusters.min) %>%
    arrange(stage, simulation, outcome, position) %>%
    write_csv('clusters.csv')

clusters.max %>%
    bind_rows(clusters.min) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda)) +
    facet_grid(stage+outcome ~ position+simulation) + xlab('X') + ylab('Y') +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr) +
    ggtitle('Group-level Fixation Rate Contrasts') +
    geom_polygon(size=0.3, alpha=0.66) +
    scale_fill_scico(palette='vik', midpoint=0, name='Rate') +
    theme_classic() + coord_fixed(xlim=range(contr.group$vx), ylim=range(contr.group$vy))
ggsave('plots/clusters.png', width=8, height=6)


# get ranges of contrasts and BFs
contr.group %>% median_hdci %>% pull(lambda) %>% range
contr.group %>% median_hdci %>% pull(prior_lambda) %>% range
contr.group %>% median_hdci %>% pull(BF) %>% log10 %>% range
bf.breaks <- -4:4


plot.encoding.ball <- contr.group %>%
    filter(position=='ball' & stage=='Encoding') %>%
    median_hdci() %>%
    filter((BF < .1 | BF > 10) & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='ball')) +
 geom_polygon(size=0.3, alpha=0.85)
plot.encoding.goalie <- contr.group %>%
    filter(position=='goalie' & stage=='Encoding') %>%
    median_hdci() %>%
    filter((BF < .1 | BF > 10) & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='goalie')) +
 geom_polygon(size=0.3, alpha=0.85)

((plot.encoding.ball / plot.encoding.goalie) &
 facet_grid(outcome ~ simulation) &
 coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) &
 theme_classic() &
 theme(axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank(),
       axis.line=element_blank()) &
 scale_fill_scico(palette='vik', midpoint=0, limits=c(-4.75, 4.75), breaks=c(-4, -2, 0, 2, 4), name='Fixation Rate Contrast\n(log scale)',
                  labels=c('-4 (Condition 2 > Condition 1)', '-2', '0 (Condition 1 = Condition 2)', '2', '4 (Condition 1 > Condition 2)')) &
 scale_color_scico(palette='vik', midpoint=0, limits=c(-4.75, 4.75), breaks=c(-4, -2, 0, 2, 4), name='Fixation Rate Contrast\n(log scale)',
                   labels=c('-4 (Condition 2 > Condition 1)', '-2', '0 (Condition 1 = Condition 2)', '2', '4 (Condition 1 > Condition 2)'))) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/contrast_encoding_sig.png', width=8, height=6)


plot.simulation.ball <- contr.group %>%
    filter(position=='ball' & stage=='Simulation') %>%
    median_hdci() %>%
    filter((BF < .1 | BF > 10) & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='ball')) +
 geom_polygon(size=0.3, alpha=0.85)
plot.simulation.goalie <- contr.group %>%
    filter(position=='goalie' & stage=='Simulation') %>%
    median_hdci() %>%
    filter((BF < .1 | BF > 10) & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='goalie')) +
 geom_polygon(size=0.3, alpha=0.85)

((plot.simulation.ball / plot.simulation.goalie) &
 facet_grid(outcome ~ simulation) &
 coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) &
 theme_classic() &
 theme(axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank(),
       axis.line=element_blank()) &
 scale_fill_scico(palette='vik', midpoint=0, limits=c(-4.75, 4.75), breaks=c(-4, -2, 0, 2, 4), name='Fixation Rate Contrast\n(log scale)',
                  labels=c('-4 (Condition 2 > Condition 1)', '-2', '0 (Condition 1 = Condition 2)', '2', '4 (Condition 1 > Condition 2)')) &
 scale_color_scico(palette='vik', midpoint=0, limits=c(-4.75, 4.75), breaks=c(-4, -2, 0, 2, 4), name='Fixation Rate Contrast\n(log scale)',
                   labels=c('-4 (Condition 2 > Condition 1)', '-2', '0 (Condition 1 = Condition 2)', '2', '4 (Condition 1 > Condition 2)'))) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/contrast_simulation_sig.png', width=8, height=6)



plot.encoding.ball <- contr.group %>%
    filter(position=='ball' & stage=='Encoding') %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='ball')) +
    geom_polygon(size=0.3, alpha=0.66) +
    geom_polygon(fill=NA, color='black', size=0.3,
                 data=contr.group %>% filter(position=='ball' & stage=='Encoding') %>%
                     median_hdci() %>%
                     filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
                     unnest(c(vx, vy)))
plot.encoding.goalie <- contr.group %>%
    filter(position=='goalie' & stage=='Encoding') %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='goalie')) +
    geom_polygon(size=0.3, alpha=0.66) +
    geom_polygon(fill=NA, color='black', size=0.3,
                 data=contr.group %>% filter(position=='goalie' & stage=='Encoding') %>%
                     median_hdci() %>%
                     filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
                     unnest(c(vx, vy)))

((plot.encoding.ball / plot.encoding.goalie) &
 facet_grid(outcome ~ simulation) &
 coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) &
 theme_classic() &
 theme(axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank(),
       axis.line=element_blank()) &
 scale_fill_scico(palette='vik', midpoint=0, limits=c(-4.5, 4.5), breaks=c(-4, -2, 0, 2, 4), name='Fixation Rate Contrast\n(log scale)',
                  labels=c('-4 (Condition 2 > Condition 1)', '-2', '0 (Condition 1 = Condition 2)', '2', '4 (Condition 1 > Condition 2)')) &
 scale_color_scico(palette='vik', midpoint=0, limits=c(-4.5, 4.5), breaks=c(-4, -2, 0, 2, 4), name='Fixation Rate Contrast\n(log scale)',
                   labels=c('-4 (Condition 2 > Condition 1)', '-2', '0 (Condition 1 = Condition 2)', '2', '4 (Condition 1 > Condition 2)'))) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/contrast_encoding.png', width=8, height=6)


plot.simulation.ball <- contr.group %>%
    filter(position=='ball' & stage=='Simulation') %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='ball')) +
    geom_polygon(size=0.3, alpha=0.66) +
    geom_polygon(fill=NA, color='black', size=0.3,
                 data=contr.group %>% filter(position=='ball' & stage=='Simulation') %>%
                     median_hdci() %>%
                     filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
                     unnest(c(vx, vy)))
plot.simulation.goalie <- contr.group %>%
    filter(position=='goalie' & stage=='Simulation') %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=lambda, color=lambda)) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='goalie')) +
    geom_polygon(size=0.3, alpha=0.66) +
    geom_polygon(fill=NA, color='black', size=0.3,
                 data=contr.group %>% filter(position=='goalie' & stage=='Simulation') %>%
                     median_hdci() %>%
                     filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
                     unnest(c(vx, vy)))

((plot.simulation.ball / plot.simulation.goalie) &
 facet_grid(outcome ~ simulation) &
 coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) &
 theme_classic() &
 theme(axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank(),
       axis.line=element_blank()) &
 scale_fill_scico(palette='vik', midpoint=0, limits=c(-4.75, 4.75), breaks=c(-4, -2, 0, 2, 4), name='Fixation Rate Contrast\n(log scale)',
                  labels=c('-4 (Condition 2 > Condition 1)', '-2', '0 (Condition 1 = Condition 2)', '2', '4 (Condition 1 > Condition 2)')) &
 scale_color_scico(palette='vik', midpoint=0, limits=c(-4.75, 4.75), breaks=c(-4, -2, 0, 2, 4), name='Fixation Rate Contrast\n(log scale)',
                   labels=c('-4 (Condition 2 > Condition 1)', '-2', '0 (Condition 1 = Condition 2)', '2', '4 (Condition 1 > Condition 2)'))) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/contrast_simulation.png', width=8, height=6)




plot.encoding.ball <- contr.group %>%
    filter(position=='ball' & stage=='Encoding') %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='ball')) +
    geom_polygon(size=0.3, alpha=0.66)
plot.encoding.goalie <- contr.group %>%
    filter(position=='goalie' & stage=='Encoding') %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='goalie')) +
    geom_polygon(size=0.3, alpha=0.66)

((plot.encoding.ball / plot.encoding.goalie) &
 facet_grid(outcome ~ simulation) &
 coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) &
 theme_classic() &
 theme(axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank(),
       axis.line=element_blank()) &
 scale_fill_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks, labels=10^bf.breaks, name='BF') &
 scale_color_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks, labels=10^bf.breaks, name='BF')) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/contrast_encoding_bf.png', width=6, height=6)


plot.simulation.ball <- contr.group %>%
    filter(position=='ball' & stage=='Simulation') %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='ball')) +
    geom_polygon(size=0.3, alpha=0.66)
plot.simulation.goalie <- contr.group %>%
    filter(position=='goalie' & stage=='Simulation') %>%
    median_hdci() %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='goalie')) +
    geom_polygon(size=0.3, alpha=0.66)

((plot.simulation.ball / plot.simulation.goalie) &
 facet_grid(outcome ~ simulation) &
 coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) &
 theme_classic() &
 theme(axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank(),
       axis.line=element_blank()) &
 scale_fill_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks, labels=10^bf.breaks, name='BF') &
 scale_color_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks, labels=10^bf.breaks, name='BF')) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/contrast_simulation_bf.png', width=6, height=6)




plot.encoding.ball <- contr.group %>%
    filter(position=='ball' & stage=='Encoding') %>%
    median_hdci() %>%
    filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='ball')) +
    geom_polygon(size=0.3, alpha=0.66)
plot.encoding.goalie <- contr.group %>%
    filter(position=='goalie' & stage=='Encoding') %>%
    median_hdci() %>%
    filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='goalie')) +
    geom_polygon(size=0.3, alpha=0.66)

((plot.encoding.ball / plot.encoding.goalie) &
 facet_grid(outcome ~ simulation) &
 coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) &
 theme_classic() &
 theme(axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank(),
       axis.line=element_blank()) &
 scale_fill_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks, labels=10^bf.breaks, name='BF') &
 scale_color_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks, labels=10^bf.breaks, name='BF')) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/contrast_encoding_bf_sig.png', width=6, height=6)


plot.simulation.ball <- contr.group %>%
    filter(position=='ball' & stage=='Simulation') %>%
    median_hdci() %>%
    filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='ball')) +
    geom_polygon(size=0.3, alpha=0.66)
plot.simulation.goalie <- contr.group %>%
    filter(position=='goalie' & stage=='Simulation') %>%
    median_hdci() %>%
    filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr %>% filter(position=='goalie')) +
    geom_polygon(size=0.3, alpha=0.66)

((plot.simulation.ball / plot.simulation.goalie) &
 facet_grid(outcome ~ simulation) &
 coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) &
 theme_classic() &
 theme(axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank(),
       axis.line=element_blank()) &
 scale_fill_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks, labels=10^bf.breaks, name='BF') &
 scale_color_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks, labels=10^bf.breaks, name='BF')) +
    plot_layout(guides='collect') +
    plot_annotation(tag_levels='A')
ggsave('plots/contrast_simulation_bf_sig.png', width=6, height=6)



contr.group %>% median_hdci() %>%
    filter(BF > 10 & (lambda.upper < 0 | lambda.lower > 0)) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx, fill=log10(BF), color=log10(BF))) +
    facet_grid(stage+outcome ~ position+simulation) + xlab('X') + ylab('Y') +
    ggtitle('Group-level Fixation Rate Contrast BFs (significant pixels)') +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img.contr) +
    geom_polygon(size=0.3, alpha=0.75) +
    scale_fill_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks,
                     labels=10^bf.breaks, name='BF') +
    scale_color_scico(palette='vik', limits=range(bf.breaks), breaks=bf.breaks,
                     labels=10^bf.breaks, name='BF') +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
ggsave('plots/gp_contrast_bf_sig.png', width=12, height=8)


















## participant-level trends (warning: this uses a lot of memory)
draws.ball.par <- gp.ball.fit$draws(c('a', 'f_tilde'), format='df') %>%
    pivot_longer(starts_with('f'), names_pattern='(.*)\\[(.*),(.*),(.*)\\]',
                 names_to=c('variable', 'COND', 'PAR', 'grid_idx'), values_to='f_tilde') %>%
    select(-variable) %>%
    left_join(gp.ball.fit$draws('f', format='df') %>%    
              pivot_longer(starts_with('f'), names_pattern='(.*)\\[(.*),(.*)\\]',
                           names_to=c('variable', 'COND', 'grid_idx'), values_to='f') %>%
              select(-variable))

draws.goalie.par <- gp.goalie.fit$draws(c('a', 'f_tilde'), format='df') %>%
    pivot_longer(starts_with('f'), names_pattern='(.*)\\[(.*),(.*),(.*)\\]',
                 names_to=c('variable', 'COND', 'PAR', 'grid_idx'), values_to='f_tilde') %>%
    select(-variable) %>%
    left_join(gp.goalie.fit$draws('f', format='df') %>%    
              pivot_longer(starts_with('f'), names_pattern='(.*)\\[(.*),(.*)\\]',
                           names_to=c('variable', 'COND', 'grid_idx'), values_to='f') %>%
              select(-variable))

median.ball.par <- draws.ball.par %>%
    group_by(PAR, COND, grid_idx) %>%
    median_hdci(f_tilde) %>%
    mutate(COND=as.integer(COND),
           PAR=as.integer(PAR)) %>%
    bind_cols(., data.grid[.$grid_idx, c('bin_x', 'bin_y', 'vi', 'vx', 'vy')]) %>%
    mutate(condition=levels(data.fix$condition)[COND],
           participant=levels(as.factor(data.ball$participant))[PAR]) %>%
    separate(condition, c('stage', 'simulation', 'outcome'), sep='\\.') %>%
    mutate(stage=as.factor(stage),
           position='ball',
           simulation=factor(simulation, levels=c('Remember', 'What if?', 'Cause')),
           outcome=factor(outcome))

median.goalie.par <- draws.goalie.par %>%
    group_by(PAR, COND, grid_idx) %>%
    median_hdci(f_tilde) %>%
    mutate(COND=as.integer(COND),
           PAR=as.integer(PAR)) %>%
    bind_cols(., data.grid[.$grid_idx, c('bin_x', 'bin_y', 'vi', 'vx', 'vy')]) %>%
    mutate(condition=levels(data.fix$condition)[COND],
           participant=levels(as.factor(data.goalie$participant))[PAR]) %>%
    separate(condition, c('stage', 'simulation', 'outcome'), sep='\\.') %>%
    mutate(stage=as.factor(stage),
           position='goalie',
           simulation=factor(simulation, levels=c('Remember', 'What if?', 'Cause')),
           outcome=factor(outcome))

median.par <- bind_rows(median.ball.par, median.goalie.par)

d <- data.fix %>% group_by(participant, stage, simulation, outcome,trial) %>%
    summarize(rating=mean(rating)) %>%
    full_join(median.par) %>%
    group_by(position, stage, simulation, outcome, grid_idx, bin_x, bin_y, vi, vx, vy) %>%
    summarize(r=cor(rating, f_tilde),
              p=cor.test(rating, f_tilde)$p.value,
              rating=mean(rating))

d %>%
    filter(p<0.05) %>%
    unnest(c(vx, vy)) %>%
    ggplot(aes(x=vx, y=vy, group=grid_idx)) + 
    facet_grid(stage+outcome ~ position+simulation) +
    geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
               data=data.img) +
    geom_polygon(aes(fill=r, color=r), alpha=0.3, size=0.3) +
    scale_fill_scico(palette='vik', midpoint=0, name='Correlation') +
    scale_color_scico(palette='vik', midpoint=0, name='Correlation') +
    coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
    theme_classic() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank())
ggsave('plots/correlation-trial.png', width=12, height=8)

for (p in as.character(1:data.ball.stan$P)) {
    print(p)
    d <- draws.ball.par %>%
        filter(PAR==p) %>%
        mutate(COND=as.integer(COND),
               PAR=as.integer(PAR),
               lambda=a+f+f_tilde) %>%
        bind_cols(., data.grid[.$grid_idx, c('bin_x', 'bin_y', 'vi', 'vx', 'vy')]) %>%
        mutate(condition=levels(data.fix$condition)[COND],
               participant=levels(as.factor(data.ball$participant))[PAR]) %>%
        separate(condition, c('stage', 'simulation', 'outcome'), sep='\\.') %>%
        mutate(stage=as.factor(stage),
               position='ball',
               simulation=factor(simulation, levels=c('Remember', 'What if?', 'Cause')),
               outcome=factor(outcome))

    contr <- d %>%
        group_by(stage, simulation, outcome, position, grid_idx, bin_x, bin_y, vx, vy) %>%
        compare_levels(lambda, by=simulation, comparison='control') %>%
        mutate(simulation=factor(simulation, levels=c('What if? - Remember', 'Cause - Remember'))) %>%    
        group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
        median_hdci(lambda)
    
    plot.fit <- d %>%
        group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
        median_hdci(lambda) %>%
        unnest(c(vx, vy)) %>%
        ggplot() + ggtitle(paste0('GP Fit: ', d$participant[1], ' (', d$position[1], ')')) + 
        aes(x=vx, y=vy, group=grid_idx) +
        facet_grid(stage+outcome ~ simulation) +
        geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
        scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
        scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
        coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
        theme_classic() +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.line=element_blank())
    ggsave(paste0('plots/participant_level/fit_', d$position[1], '_', d$participant[1], '_.png'),
           plot=plot.fit, width=6, height=6)
    
    
    plot.contr <- contr %>%
        unnest(c(vx, vy)) %>%
        ggplot() + ggtitle(paste0('Contrast: ', d$participant[1], ' (', d$position[1], ')')) + 
        aes(x=vx, y=vy, group=grid_idx) +
        facet_grid(stage+outcome ~ simulation) +
        geom_polygon(aes(fill=lambda, color=lambda), size=0.3) +
        scale_fill_scico(palette='vik', midpoint=0, name='Rate') +
        scale_color_scico(palette='vik', midpoint=0, name='Rate') +
        coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
        theme_classic() +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.line=element_blank())
    ggsave(paste0('plots/participant_level/contrast_', d$position[1], '_', d$participant[1], '_.png'),
           plot=plot.contr, width=6, height=8)
    
    plot.contr.sig <- contr %>%
        filter(.upper < 0 | .lower > 0) %>%
        unnest(c(vx, vy)) %>%
        ggplot() + ggtitle(paste0('Contrast: ', d$participant[1], ' (', d$position[1], ')')) + 
        aes(x=vx, y=vy, group=grid_idx) +
        facet_grid(stage+outcome ~ simulation) +
        geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
                   data=data.img %>% filter(position==d$position[1], simulation!='Remember') %>%
                       mutate(simulation=factor(paste0(simulation, ' - Remember'),
                                                levels=c('What if? - Remember', 'Cause - Remember')))) +
        geom_polygon(aes(fill=lambda, color=lambda), alpha=0.66, size=0.3) +
        scale_fill_scico(palette='vik', midpoint=0, name='Rate') +
        scale_color_scico(palette='vik', midpoint=0, name='Rate') +
        coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
        theme_classic() +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.line=element_blank())
    ggsave(paste0('plots/participant_level/contrast_sig_', d$position[1], '_', d$participant[1], '_.png'),
           plot=plot.contr.sig, width=6, height=8)
}

for (p in as.character(1:data.goalie.stan$P)) {
    print(p)
    d <- draws.goalie.par %>%
        filter(PAR==p) %>%
        mutate(COND=as.integer(COND),
               PAR=as.integer(PAR),
               lambda=a+f+f_tilde) %>%
        bind_cols(., data.grid[.$grid_idx, c('bin_x', 'bin_y', 'vi', 'vx', 'vy')]) %>%
        mutate(condition=levels(data.fix$condition)[COND],
               participant=levels(as.factor(data.goalie$participant))[PAR]) %>%
        separate(condition, c('stage', 'simulation', 'outcome'), sep='\\.') %>%
        mutate(stage=as.factor(stage),
               position='goalie',
               simulation=factor(simulation, levels=c('Remember', 'What if?', 'Cause')),
               outcome=factor(outcome))

    contr <- d %>%
        group_by(stage, simulation, outcome, position, grid_idx, bin_x, bin_y, vx, vy) %>%
        compare_levels(lambda, by=simulation, comparison='control') %>%
        mutate(simulation=factor(simulation, levels=c('What if? - Remember', 'Cause - Remember'))) %>%    
        group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
        median_hdci(lambda)
    
    plot.fit <- d %>%
        group_by(stage, simulation, outcome, position, grid_idx, vx, vy) %>%
        median_hdci(lambda) %>%
        unnest(c(vx, vy)) %>%
        ggplot() + ggtitle(paste0('GP Fit: ', d$participant[1], ' (', d$position[1], ')')) + 
        aes(x=vx, y=vy, group=grid_idx) +
        facet_grid(stage+outcome ~ simulation) +
        geom_polygon(aes(fill=exp(lambda), color=exp(lambda)), size=0.3) +
        scale_fill_viridis(option='magma', name='Rate', limits=c(0, NA)) +
        scale_color_viridis(option='magma', name='Rate', limits=c(0, NA)) +
        coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
        theme_classic() +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.line=element_blank())
    ggsave(paste0('plots/participant_level/fit_', d$position[1], '_', d$participant[1], '_.png'),
           plot=plot.fit, width=6, height=6)
    
    
    plot.contr <- contr %>%
        unnest(c(vx, vy)) %>%
        ggplot() + ggtitle(paste0('Contrast: ', d$participant[1], ' (', d$position[1], ')')) + 
        aes(x=vx, y=vy, group=grid_idx) +
        facet_grid(stage+outcome ~ simulation) +
        geom_polygon(aes(fill=lambda, color=lambda), size=0.3) +
        scale_fill_scico(palette='vik', midpoint=0, name='Rate') +
        scale_color_scico(palette='vik', midpoint=0, name='Rate') +
        coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
        theme_classic() +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.line=element_blank())
    ggsave(paste0('plots/participant_level/contrast_', d$position[1], '_', d$participant[1], '_.png'),
           plot=plot.contr, width=6, height=8)
    
    plot.contr.sig <- contr %>%
        filter(.upper < 0 | .lower > 0) %>%
        unnest(c(vx, vy)) %>%
        ggplot() + ggtitle(paste0('Contrast: ', d$participant[1], ' (', d$position[1], ')')) + 
        aes(x=vx, y=vy, group=grid_idx) +
        facet_grid(stage+outcome ~ simulation) +
        geom_image(aes(x=0, y=0, image=image.reflected), size=1, by='height', asp=800/600, inherit.aes=FALSE,
                   data=data.img %>% filter(position==d$position[1], simulation!='Remember') %>%
                       mutate(simulation=factor(paste0(simulation, ' - Remember'),
                                                levels=c('What if? - Remember', 'Cause - Remember')))) +
        geom_polygon(aes(fill=lambda, color=lambda), alpha=0.66, size=0.3) +
        scale_fill_scico(palette='vik', midpoint=0, name='Rate') +
        scale_color_scico(palette='vik', midpoint=0, name='Rate') +
        coord_fixed(xlim=c(-400, 400), ylim=c(-300, 300), expand=FALSE) +
        theme_classic() +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.line=element_blank())
    ggsave(paste0('plots/participant_level/contrast_sig_', d$position[1], '_', d$participant[1], '_.png'),
           plot=plot.contr.sig, width=6, height=8)
}





data.ratings <- data.fix %>%
    group_by(participant, position, simulation, outcome, trial) %>%
    summarize(rating=mean(rating)) %>%
    ungroup

library(brms)
library(modelr)

m.remember <- brm(rating ~ position*outcome + (1|participant),
                  data.ratings %>% filter(simulation=='Remember'),
                  prior=prior(normal(0, 1), class=b),
                  sample_prior="yes", save_pars=save_pars(all=TRUE))
m.whatif <- brm(rating ~ position*outcome + (1|participant),
                data.ratings %>% filter(simulation=='What if?'),
                prior=prior(normal(0, 1), class=b),
                sample_prior="yes", save_pars=save_pars(all=TRUE))
m.cause <- brm(rating ~ position*outcome + (1|participant),
               data.ratings %>% filter(simulation=='Cause'),
               prior=prior(normal(0, 1), class=b),
               sample_prior="yes", save_pars=save_pars(all=TRUE))

describe_posterior(m.remember, test=c('p_direction', 'bf'), ci=.95, null=0) %>% as.data.frame
describe_posterior(m.whatif, test=c('p_direction', 'bf'), ci=.95, null=0) %>% as.data.frame
describe_posterior(m.cause, test=c('p_direction', 'bf'), ci=.95, null=0) %>% as.data.frame

draws.remember <- data.ratings %>% data_grid(position, outcome) %>%
    add_epred_draws(m.remember, re_formula=NA) %>%
    mutate(simulation='Remember')
draws.whatif <- data.ratings %>% data_grid(position, outcome) %>%
    add_epred_draws(m.whatif, re_formula=NA) %>%
    mutate(simulation='What if?')
draws.cause <- data.ratings %>% data_grid(position, outcome) %>%
    add_epred_draws(m.cause, re_formula=NA) %>%
    mutate(simulation='Cause')


draws.remember %>%
    bind_rows(draws.whatif, draws.cause) %>%
    mutate(simulation=factor(simulation, levels=c('Remember', 'What if?', 'Cause'))) %>%
    ggplot(aes(x=outcome, y=.epred, group=position, fill=position)) +
    stat_ccdfinterval(aes(slab_alpha = stat(f)), thickness = 1, position = "dodge",
                      n=4000, point_interval=median_hdi, .width=.95,
                      size=0.5, show.legend=c(slab_alpha=FALSE)) +
    facet_wrap(~simulation) + ylab('Mean Rating') +
    scale_fill_discrete(name='Position', labels=c('Offensive', 'Defensive')) +
    theme_classic() +
    theme(axis.title.x=element_blank())
ggsave('plots/ratings.png', width=6, height=3, type='cairo')
