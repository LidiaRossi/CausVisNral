library(tidyverse)
spikes_outcome <- readRDS("data/spikes_outcome.rds")
spikes_outcome
if(!dir.exists('output/')){dir.create('output/'); cat('Output folder created.') }

plots_path <- 'output/plots/'
data_path <- 'output/data/'
if(!dir.exists(plots_path)){dir.create(plots_path); cat('Output plots folder created.') }
if(!dir.exists(data_path)){dir.create(data_path); cat('Output data folder created.') }

#### stimuli colors ####
clrs <- tribble(
    ~stimulus, ~clr,
    'drifting_gratings', '#a97b50',
    'natural_movie_three', '#9f1f63',
    'natural_movie_one', '#7f3e98',
    'spontaneous', '#939598',
    'static_gratings', '#25aae1',
    'natural_scenes', '#f26522',
    'none', '#3D3D3D'
)
clrs_vec <- clrs$clr
names(clrs_vec) <- clrs$stimulus

#### rearrange data ####
# spikes count outcome
count <- spikes_outcome %>%
    select(imaging_depth, session_type, donor_name, spikes_average_count) %>%
    mutate(spikes_average_count = map(spikes_average_count, ~ .x %>%
                                          gather( key = 'cell_id', val = 'average_count', starts_with('cell')))) %>%
    unnest(spikes_average_count) %>%
    mutate(stimulus = map2_chr(session_type, stimulus, ~if_else(.x == 'three_session_A' & .y == 'spontaneous', 'spontaneous_A', .y)),
           stimulus = map2_chr(session_type, stimulus, ~if_else(.x == 'three_session_A' & .y == 'natural_movie_one', 'natural_movie_one_A', .y)))

# spikes intensity outcome
intensity <- spikes_outcome %>%
    select(imaging_depth, session_type, donor_name, spikes_average_intensity) %>%
    mutate(spikes_average_intensity = map(spikes_average_intensity, ~ .x %>%
                                              gather( key = 'cell_id', val = 'average_intensity', starts_with('cell')))) %>%
    unnest(spikes_average_intensity) %>%
    mutate(stimulus = map2_chr(session_type, stimulus, ~if_else(.x == 'three_session_A' & .y == 'spontaneous', 'spontaneous_A', .y)),
           stimulus = map2_chr(session_type, stimulus, ~if_else(.x == 'three_session_A' & .y == 'natural_movie_one', 'natural_movie_one_A', .y)))

# join table
complete_tab <- count %>%
    left_join(intensity, by = c('imaging_depth', 'session_type', 'donor_name', 'block', 'stimulus', 'cell_id'))


#### data for test (1): depth as treatment within sessions ####

# source tab
depth_tab <- complete_tab %>%
    select(cell_id, session_type, imaging_depth, average_count, average_intensity) %>%
    group_by(cell_id, session_type, imaging_depth) %>%
    mutate(session_type = factor(session_type, levels = c('three_session_A', 'three_session_B'), labels = c('Session A', 'Session B'))) %>%
    summarise_at(vars(starts_with('average')), mean)

# divide per session
depthA <- depth_tab %>% filter(session_type == 'Session A')
depthB <- depth_tab %>% filter(session_type == 'Session B')

# plots (note xlim because of long tails visualisation)
gg_int <- depth_tab %>%
    ggplot(aes(x = average_count, fill = imaging_depth)) +
    geom_density(alpha = .5) +
    xlim(0, 0.02)+
    labs(y = ' ', fill = 'Depth:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~session_type) +
    scale_fill_viridis_d()

gg_count <- depth_tab %>%
    ggplot(aes(x = average_intensity, fill = imaging_depth)) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Depth:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~session_type) +
    scale_fill_viridis_d()

gg_treat_depth <- ggpubr::ggarrange(gg_count, gg_int, common.legend = T, ncol = 1, legend = 'right')

# save results
ggsave(gg_treat_depth, height = 6, width = 10, filename = paste0(plots_path, 'treat_depth.pdf'))
save(depthA, depthB, gg_treat_depth, file = paste0(data_path,'treat_depth.rda'))

#### data for test (2): session as treatment within depth ####

# divide per depth
depth175 <- depth_tab %>% filter(imaging_depth == '175')
depth375 <- depth_tab %>% filter(imaging_depth == '375')

# plots (again xlim)
gg_count <- depth_tab %>%
    ggplot(aes(x = average_count, fill = session_type)) +
    geom_density(alpha = .5) +
    xlim(0, 0.02) +
    labs(y = ' ', fill = ' ', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free')+
    scale_fill_viridis_d()

gg_int <- depth_tab %>%
    ggplot(aes(x = average_intensity, fill = session_type)) +
    geom_density(alpha = .5) +
    xlim(0, 0.5)+
    labs(y = ' ', fill = ' ', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free')+
    scale_fill_viridis_d()

gg_treat_session <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')

# save results
ggsave(gg_treat_session, height = 6, width = 10, filename = paste0(plots_path, 'treat_session.pdf'))
save(depth175, depth375, gg_treat_session, file = paste0(data_path,'treat_session.rda'))

#### data for test (3): stimulus as treatments within session~depth ####

# pair 1
stims_pair <- c('drifting_gratings', 'static_gratings')
pair1 <- complete_tab %>%
    filter(stimulus %in% stims_pair)  %>%
    select(cell_id, imaging_depth, stimulus, average_count, average_intensity)

gg_int <- pair1 %>%
    ggplot(aes(x = average_count, fill = stimulus)) +
    geom_density(alpha = .5) +
    xlim(0, 0.025) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_count <- pair1 %>%
    ggplot(aes(x = average_intensity, fill = stimulus)) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_pair1 <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')
ggsave(gg_pair1, height = 6, width = 10, filename = paste0(plots_path, 'pair1.pdf'))

# pair 2
stims_pair <- c('drifting_gratings', 'natural_scenes')
pair2 <- complete_tab %>%
    filter(stimulus %in% stims_pair)  %>%
    select(cell_id, imaging_depth, stimulus, average_count, average_intensity)

gg_int <- pair2 %>%
    ggplot(aes(x = average_count, fill = stimulus)) +
    geom_density(alpha = .5) +
    xlim(0, 0.025) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_count <- pair2 %>%
    ggplot(aes(x = average_intensity, fill = stimulus)) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_pair2 <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')
ggsave(gg_pair2, height = 6, width = 10, filename = paste0(plots_path, 'pair2.pdf'))

# pair 3
stims_pair <- c('natural_movie_three', 'static_gratings')
pair3 <- complete_tab %>%
    filter(stimulus %in% stims_pair)  %>%
    select(cell_id, imaging_depth, stimulus, average_count, average_intensity)

gg_int <- pair3 %>%
    ggplot(aes(x = average_count, fill = stimulus)) +
    geom_density(alpha = .5) +
    xlim(0, 0.025) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_count <- pair3 %>%
    ggplot(aes(x = average_intensity, fill = stimulus)) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_pair3 <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')
ggsave(gg_pair3, height = 6, width = 10, filename = paste0(plots_path, 'pair3.pdf'))

# pair 4
stims_pair <- c('natural_movie_three', 'natural_scenes')
pair4 <- complete_tab %>%
    filter(stimulus %in% stims_pair)  %>%
    select(cell_id, imaging_depth, stimulus, average_count, average_intensity)

gg_int <- pair4 %>%
    ggplot(aes(x = average_count, fill = stimulus)) +
    geom_density(alpha = .5) +
    xlim(0, 0.025) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_count <- pair4 %>%
    ggplot(aes(x = average_intensity, fill = stimulus)) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_pair4 <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')
ggsave(gg_pair4, height = 6, width = 10, filename = paste0(plots_path, 'pair4.pdf'))


save(pair1, pair2, pair3, pair4, gg_pair1, gg_pair2, gg_pair3, gg_pair4,
     file = paste0(data_path, 'treat_stimulus.rda'))

#### data for test (4): stimulus as treatments within session~depth~position ####

# pair 1
stims_pair <- c('drifting_gratings', 'static_gratings')
pair1 <- complete_tab %>%
    filter(stimulus %in% stims_pair)  %>%
    select(cell_id, imaging_depth, stimulus, average_count, average_intensity, block)

gg_int <- pair1 %>%
    ggplot(aes(x = average_count, fill = stimulus, group = interaction(stimulus, block))) +
    geom_density(alpha = .5) +
    xlim(0, 0.025) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_count <- pair1 %>%
    ggplot(aes(x = average_intensity, fill = stimulus, group = interaction(stimulus, block))) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_pair1 <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')
ggsave(gg_pair1, height = 6, width = 10, filename = paste0(plots_path, 'pair1_position.pdf'))

# pair 2
stims_pair <- c('drifting_gratings', 'natural_scenes')
pair2 <- complete_tab %>%
    filter(stimulus %in% stims_pair)  %>%
    select(cell_id, imaging_depth, stimulus, average_count, average_intensity, block)

gg_int <- pair2 %>%
    ggplot(aes(x = average_count, fill = stimulus, group = interaction(stimulus, block))) +
    geom_density(alpha = .5) +
    xlim(0, 0.025) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_count <- pair2 %>%
    ggplot(aes(x = average_intensity, fill = stimulus, group = interaction(stimulus, block))) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_pair2 <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')
ggsave(gg_pair2, height = 6, width = 10, filename = paste0(plots_path, 'pair2_position.pdf'))

# pair 3
stims_pair <- c('natural_movie_three', 'static_gratings')
pair3 <- complete_tab %>%
    filter(stimulus %in% stims_pair)  %>%
    select(cell_id, imaging_depth, stimulus, average_count, average_intensity, block)

gg_int <- pair3 %>%
    ggplot(aes(x = average_count, fill = stimulus, group = interaction(stimulus, block))) +
    geom_density(alpha = .5) +
    xlim(0, 0.025) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_count <- pair3 %>%
    ggplot(aes(x = average_intensity, fill = stimulus, group = interaction(stimulus, block))) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_pair3 <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')
ggsave(gg_pair3, height = 6, width = 10, filename = paste0(plots_path, 'pair3_position.pdf'))

# pair 4
stims_pair <- c('natural_movie_three', 'natural_scenes')
pair4 <- complete_tab %>%
    filter(stimulus %in% stims_pair)  %>%
    select(cell_id, imaging_depth, stimulus, average_count, average_intensity, block)

gg_int <- pair4 %>%
    ggplot(aes(x = average_count, fill = stimulus, group = interaction(stimulus, block))) +
    geom_density(alpha = .5) +
    xlim(0, 0.025) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes intensity') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_count <- pair4 %>%
    ggplot(aes(x = average_intensity, fill = stimulus, group = interaction(stimulus, block))) +
    geom_density(alpha = .5) +
    xlim(0, 0.5) +
    labs(y = ' ', fill = 'Visual\nstimulus:', x = 'Average spikes count') +
    theme_bw() +
    facet_wrap(~imaging_depth, scales = 'free') +
    scale_fill_manual(values = clrs_vec)

gg_pair4 <- ggpubr::ggarrange(gg_int, gg_count, common.legend = T, ncol = 1, legend = 'right')
ggsave(gg_pair4, height = 6, width = 10, filename = paste0(plots_path, 'pair4_position.pdf'))


save(pair1, pair2, pair3, pair4, gg_pair1, gg_pair2, gg_pair3, gg_pair4,
     file = paste0(data_path, 'treat_position.rda'))
