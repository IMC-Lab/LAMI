#!/usr/bin/Rscript
library(dplyr)
library(tidyr)
library(stringr)

in_file <- 'data/LAMI_Full2_MTurk_raw.csv'
out_file <- 'data/LAMI_Full2_MTurk_processed.csv'

data_wide <- read.csv(in_file, header=TRUE, stringsAsFactors=FALSE)

## remove unneeded rows/cols
data_wide <- data_wide[-c(1:2), -c(1:5, 7:8, 10:18, 219, 223)]

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
    pivot_longer(X1_LorRB1:X12_ConfidentB4,
                 names_pattern='X(\\d+)_(.+)B(\\d+)',
                 names_to=c('loop', 'measure', 'block'),
                 values_to='value') %>%
    mutate(loop=as.numeric(loop),
           block=as.numeric(block),
           display=ifelse(block < 3, 'up', 'down'),
           measure=tolower(replace(replace(measure, measure=='Confident',
                                           'Confidence'),
                                   measure=='LorR', 'lr'))) %>%
    full_join(conditions, by='loop') %>%
    pivot_wider(names_from=measure, values_from=value) %>%
    rename(rating=question,
           vividness=vivid) %>%
    mutate(condition=word(condition, 2))

## reorder column names
df <- df[, c('id', 'condition', 'block', 'display', 'loop', 'outcome', 'imagination',
             'lr', 'vividness', 'rating', 'confidence',
             'duration', 'gender', 'age', 'race', 'hispanic', 'education', 
             'display', 'feedback', 'CheckQ1', 'CheckQ1Again',
             'CheckQ2', 'CheckQ2Again', 'CheckQ3', 'CheckQ3Again')]
    

write.csv(df, out_file, row.names=FALSE)
