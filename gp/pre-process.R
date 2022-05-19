#!/usr/bin/Rscript
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

video_file <- '../data/raw/LAMI_fixationreport_video.xlsx'
sim_file <- '../data/raw/LAMI_fixationreport_simulation.xlsx'
out_fix_file <- '../data/LAMI_fixations.csv'
out_ratings_file <- '../data/LAMI_ratings.csv'

fix_data <- bind_rows(read_excel(video_file) %>%
                      mutate(VIDEO_FRAME_INDEX_START=as.numeric(VIDEO_FRAME_INDEX_START),
                             stage='encoding'),
                      read_excel(sim_file) %>%
                      mutate(stage='simulation')) %>%
    rename(participant=RECORDING_SESSION_LABEL,
           trial=TRIAL_INDEX,
           fix_index=CURRENT_FIX_INDEX,
           fix_x=CURRENT_FIX_X,
           fix_y=CURRENT_FIX_Y,
           fix_start=CURRENT_FIX_START,
           fix_end=CURRENT_FIX_END,
           fix_start_frame=VIDEO_FRAME_INDEX_START,
           fix_end_frame=VIDEO_FRAME_INDEX_END,
           fix_duration=CURRENT_FIX_DURATION,
           blink=CURRENT_FIX_BLINK_AROUND,
           video=Video,
           direction=LorRdirection,
           question=Question,
           rating=RatingLXlocation,
           attn_check=TrialAttnCheckXLocation) %>%
    mutate(participant=str_to_upper(participant),
           fix_x=fix_x-1920/2,
           fix_y=-(fix_y-1080/2),
           direction=ifelse(direction==1, 'left', 'right'),
           rating=pmax(pmin(rating/1000 - .5, 1), 0),
           attn_check=pmax(pmin(attn_check/1000 - .5, 1), 0),
           fix_on_screen=(abs(fix_x) < 1920/2 & abs(fix_y) < 1080/2),
           fix_on_video=(abs(fix_x) < 400 & abs(fix_y) < 300)
           ##fix_on_screen=str_detect(CURRENT_FIX_INTEREST_AREAS, '1'),
           ##fix_on_video=str_detect(CURRENT_FIX_INTEREST_AREAS, '2')
           ) %>%
    select(-CURRENT_FIX_INTEREST_AREAS, -RatingLYlocation, -BlockCount) %>%
    filter(participant != "LAMI054", participant != "LAMI057")

ratings <- list.files(pattern='*.txt', path='../data/raw', full.names=TRUE) %>%
    lapply(function(f) read.table(f, header=TRUE)) %>%
    do.call(rbind, .) %>%
    tibble() %>%
    rename(participant=Session_Name_,
           trial=Trial_Index_,
           video=Video,
           direction=LorRdirection,
           rating=RatingLXlocation,
           attn_check=TrialAttnCheckXLocation) %>%
    separate(position, into=c('position', 'first_display'), sep='_') %>%
    mutate(participant=str_to_upper(participant),
           direction=ifelse(direction==1, 'left', 'right'),
           rating=pmax(pmin(rating/1000 - .5, 1), 0),
           attn_check=pmax(pmin(attn_check/1000 - .5, 1), 0),
           position=ifelse(position=='YB', 'ball', 'goalie'),
           ball_direction=ifelse(position=='ball', direction, ifelse(outcome=='miss', direction, ifelse(direction=='left', 'right', 'left'))),
           goalie_direction=ifelse(position=='goalie', direction, ifelse(outcome=='miss', direction, ifelse(direction=='left', 'right', 'left'))),) %>%
    select(-trialcount, -BlockCount, -feedbackpoint) %>%
    relocate(participant, trial, position)
    

write.csv(ratings, out_ratings_file, row.names=FALSE)
write.csv(left_join(fix_data, ratings), out_fix_file, row.names=FALSE)


