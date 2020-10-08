#!/usr/bin/Rscript
library(dplyr)
library(tidyr)
library(stringr)

in_file <- 'data/LAMI_Full_test.csv'
out_file <- 'data/LAMI_Full_test_processed.csv'

data_wide <- read.csv(in_file, header=TRUE, stringsAsFactors=FALSE)

## remove unneeded rows/cols
data_wide <- data_wide[-c(1:2), -c(1:5, 7:8, 10:18)] #, 219, 223)]

## rename columns and filter subjects by attention checks
data_wide <- data_wide %>%
    rename(duration=Duration..in.seconds.,
           id=ResponseId,
           condition=Condition,
           gender=Gender,
           race=Race,
           hispanic=Hispanic.,
           education=Education,
           age=Age,
           feedback=Feedback,
           display=Display,
           random_id=RandomID) %>%
    subset(Catch == 'IM_74Ef0wh1bD6qdZb' & AttnCheck == 'Yes') %>%
    select(-Catch, -AttnCheck)

# remove _1 from the end of slider question names
colnames(data_wide) <- gsub('_1$', '', colnames(data_wide))

## make 6 (2: success/failure x 3: Remember/What If?/Cause) rows per participant
conditions <- data.frame(loop=1:12,
                         outcome=rep(c('S', 'M'), 2, each=3),
                         imagination=rep(c('Remember', 'What If?', 'Cause'), 4))

df <- data_wide %>%
    pivot_longer(X1_LorRB1:X12_CheckB4,
                 names_pattern='X(\\d+)_(.+)B(\\d+)',
                 names_to=c('loop', 'measure', 'block'),
                 values_to='value') %>%
    mutate(loop=as.numeric(loop),
           block=as.numeric(block),
           display=ifelse(block < 3, 'up', 'down'),
           measure=tolower(str_replace_all(measure, 'LorR', 'lr'))) %>%
    full_join(conditions, by='loop') %>%
    pivot_wider(names_from=measure, values_from=value) %>%
    rename(rating=question,
           vividness=vivid,
           gender_text=Gender_4_TEXT,
           race_text=Race_7_TEXT,
           VorV=VorV_5) %>%
    mutate(condition=word(condition, 2),
           lr_other=ifelse(outcome=='M', lr, ifelse(lr=='right', 'left', 'right')))

## reorder column names
df <- df[, c('id', 'condition', 'block', 'display', 'loop', 'outcome', 'imagination',
             'lr', 'vividness', 'rating', 'confidence', 'check', 'lr_other',
             'duration', 'gender', 'gender_text', 'age', 'race', 'race_text',
             'hispanic', 'education', 'VorV', 'feedback', 'CheckQ1', 'CheckQ1Again',
             'CheckQ2', 'CheckQ2Again', 'CheckQ3', 'CheckQ3Again')]
    

write.csv(df, out_file, row.names=FALSE)


