#!/usr/bin/env Rscript
library(cheddar)
args <- commandArgs(trailingOnly=TRUE)
internal <- data(package='cheddar')$results[,'Item']
for(arg in args) {
    if(arg %in% internal) {
        data(list=arg, package='cheddar')
        community <- get(arg)
    } else {
        community <- LoadCommunity(arg)
    }
    cheddar:::.WriteForRob(community, path='')
}
