#!/usr/bin/env Rscript
library(cheddar)
args <- commandArgs(trailingOnly=TRUE)
internal <- data(package='cheddar')$results[,'Item']
for(arg in args) {
    if(arg %in% internal) {
        #cat('Loading internal dataset', arg, '\n')
        data(list=arg, package='cheddar')
        community <- get(arg)
    } else {
        #cat('Loading community from', arg, '\n')
        community <- LoadCommunity(arg)
    }
    cheddar:::.PrintChains(community)
}
