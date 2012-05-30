# Creates image for http://quicklizard99.github.com/cheddar/ homepage
# Four different views of the community of Tuesday Lake, 1984 are written to 
# images/cheddar.png and images/cheddar.svg.

options(warn=2)
library(cheddar)
data(TL84)

Plots <- function()
{
    par(mfrow=c(2,2),
        mar=c(4, 4, 1, 1),   # bottom, left, top, right
        mgp=c(2, 0.5, 0), 
        lwd=0.5)    # axis title, axis labels and axis line
    PlotWebByLevel(TL84, show.nodes.as='both', cex=2, main='', y.layout='stagger')
    mtext('a)', side=2, at=par('usr')[4], line=1, las=1)

    PlotNPS(TL84, 'Log10M', 'PreyAveragedTrophicLevel', show.nodes.as='both', 
            cex=2, main='', xlab=Log10MLabel(TL84), 
            ylab='Prey-averaged trophic level')
    mtext('b)', side=2, at=par('usr')[4], line=1, las=1)

    PlotNvM(TL84, show.nodes.as='both', cex=2, main='')
    mtext('c)', side=2, at=par('usr')[4], line=1, las=1)

    PlotMCvMR(TL84, main='')
    mtext('d)', side=2, at=par('usr')[4], line=1, las=1)
}

png('images/cheddar.png', width=550, height=500)
Plots()
dev.off()

svg('images/cheddar.svg', width=7.7, height=7)
Plots()
dev.off()

