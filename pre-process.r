#!/usr/bin/Rscript
library(dplyr)
library(tidyr)
library(stringr)

args <- commandArgs(trailingOnly=T)
if (length(args) != 2) {
    writeLines('Usage: ./pre-process.r <input-file-name> <output-file-name>')
    quit()
}
in_file <- args[1]
out_file <- args[2]

data_wide <- read.csv(in_file, header=TRUE, stringsAsFactors=FALSE)

## remove unneeded rows/cols
data_wide <- data_wide[-c(1:3),]
data_wide <- data_wide[,-c(1:5, 7, 8, 10:24, 75, 79, 82, 84:91)]

## rename columns and filter subjects by attention checks
data_wide <- data_wide %>%
    rename(duration=Duration..in.seconds.,
           id=ResponseId,
           condition=Condition,
           gender=Gender,
           age=Age,
           AttnCheck1=Q118,
           AttnCheck2=AttnCheck,
           race=Q149,
           hispanic=Q147,
           education=Q151) %>%
    subset(AttnCheck1=='Option 3' & AttnCheck2=='Yes') %>%
    subset(select=-c(AttnCheck1, AttnCheck2))

# remove _1 from the end of slider question names
colnames(data_wide) <- gsub('_1$', '', colnames(data_wide))

## make 6 (2: success/failure x 3: Remember/What If?/Cause) rows per participant
df <- data_wide %>%
    pivot_longer(SR_LR:MC_other_resp,
                 names_pattern='(.)(.)_(.*)',
                 names_to=c('success', 'imagination', 'measure'),
                 values_to='response') %>%

    # group ratings into a single column
    mutate(measure=replace(measure, measure=='outcome' |
                                    measure=='counterfactual' |
                                    measure=='cause', 'rating')) %>%

    ## group credit/blame into a single column
    mutate(measure=replace(measure, measure=='self_credit' |
                                    measure=='self_blame',
                           'self_credit_blame')) %>%
    mutate(measure=replace(measure, measure=='other_credit' |
                                    measure=='other_blame',
                           'other_credit_blame')) %>%
    ## fix a misspelling of self_resp
    mutate(measure=replace(measure, measure=='self_rep', 'self_resp')) %>%
    
    pivot_wider(names_from=measure, values_from=response)

## reorder column names
df <- df[, c('id', 'condition', 'success', 'imagination', 'LR', 'vivid',
             'rating', 'confidence', 'self_credit_blame', 'other_credit_blame',
             'self_resp', 'other_resp', 'duration', 'gender', 'age', 'race',
             'hispanic', 'education')]

## clean up the imagination column
df$imagination <- str_replace_all(df$imagination,
                                c("R"="Remember", "W"="WhatIf?", "C"="Cause"))

write.csv(df, out_file, row.names=FALSE)
