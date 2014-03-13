#!/usr/bin/env Rscript
cat('Running examples\n')
options(warn=2)

output.dir <- 'output'

library(cheddar)

data(TL84, TL86, YthanEstuary, SkipwithPond, Benguela, ChesapeakeBay)

# Create figures
if(file.exists(output.dir))
{
    unlink(output.dir, TRUE, TRUE)
}

dir.create(output.dir)

DefaultGraphics <- function()
{
    par(mar=c(3.5, 4, 2.5, 1),    # bottom, left, top, right
        mgp=c(2, 0.6, 0),         # axis title, axis labels, axis line
        cex=1, 
        cex.axis=1.2, 
        cex.lab=1.2, 
        cex.main=1.5)
}

PlotExample <- function(safename, example.number, example)
{
    # Open a png
    fname <- paste0(safename, example.number, '.png')
    stopifnot(!file.exists(file.path(output.dir, fname)))
    png(file.path(output.dir, fname), height=400, width=400)
    DefaultGraphics()

    # Execute the example
    if(TRUE)
    {
        TextExample(safename, 'graphical', example.number, example)
    }
    else
    {
        eval(parse(text=example))
    }

    dev.off()
}

TextExample <- function(safename, type, example.number, example)
{
    fname <- paste0(safename, type, example.number, '.txt')
    stopifnot(!file.exists(file.path(output.dir, fname)))

    if(TRUE)
    {
        cat(example, file='~/x.R')
        sink(file.path(output.dir, fname))
        source('~/x.R', echo=TRUE, keep.source=TRUE, max.deparse.length=1e4)
        sink()
    }
    else
    {
        sink(file.path(output.dir, fname))
        eval(parse(text=example))
        sink()
    }
}

fname <- 'examples.csv'
methods <- read.csv(fname)
methods$safename <- gsub('[ -.]*', '', tolower(methods$description))
write.csv(methods, file=fname, row.names=FALSE, na='')

methods$graphical1 <- as.character(methods$graphical1)
methods$graphical2 <- as.character(methods$graphical2)
methods$textual1 <- as.character(methods$textual1)

for(row in 1:nrow(methods))
{
    method <- methods[row,]
    if(''!=method$graphical1)   PlotExample(method$safename, 1, method$graphical1)
    if(''!=method$graphical2)   PlotExample(method$safename, 2, method$graphical2)
    if(''!=method$textual1)     TextExample(method$safename, 'textual', 1, method$textual1)
}
