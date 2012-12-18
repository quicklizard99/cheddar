# Creates image for http://quicklizard99.github.com/cheddar/ homepage
# Six different views of the community of Tuesday Lake, 1984 are written to 
# images/cheddar.png.
library(cheddar)
data(TL84)


NewPlot <- function(fname)
{
    png(file.path('images', paste(fname, '.png', sep='')), width=400,height=400)
    par(mar=c(3, 3, 2, 0.5),    # bottom, left, top, right
        mgp=c(1.6, 0.5, 0),     # title, labels, line
        cex=1.5)
}

NewPlot('PlotNvM')
PlotNvM(TL84)
dev.off()

NewPlot('PlotWebByLevel')
PlotWebByLevel(TL84)
dev.off()

NewPlot('PlotNPS')
PlotNPS(TL84, 'Log10M', 'PreyAveragedTrophicLevel')
dev.off()

NewPlot('PlotPredationMatrix')
PlotPredationMatrix(TL84)
dev.off()

NewPlot('PlotMCvMR')
PlotMCvMR(TL84)
dev.off()



# Order the community's nodes by body mass
TL84 <- OrderCommunity(TL84, 'M')

# Create six plots on one image
png('images/cheddar.png', width=720, height=980)
par(mfrow=c(3,2),
    mar=c(3, 3, 0.5, 0.5),    # bottom, left, top, right
    mgp=c(1.6, 0.5, 0),       # title, labels, line
    cex=1.5)

PlotWebByLevel(TL84, show.nodes.as='both', cex=2, main='', 
               y.layout='stagger')
mtext('a', side=2, at=par('usr')[4], line=1, las=1, cex=1.5)

PlotCircularWeb(TL84, show.nodes.as='both', cex=2, main='')
mtext('b)', side=2, at=par('usr')[4], line=1, las=1, cex=1.5)

PlotNvM(TL84, show.nodes.as='both', cex=2, main='')
mtext('c)', side=2, at=par('usr')[4], line=1, las=1, cex=1.5)

PlotNPS(TL84, 'Log10M', 'PreyAveragedTrophicLevel', show.nodes.as='both', 
        cex=2, main='', xlab=Log10MLabel(TL84), 
        ylab='Prey-averaged trophic level')
mtext('d)', side=2, at=par('usr')[4], line=1, las=1, cex=1.5)

PlotPredationMatrix(TL84, main='')
mtext('e)', side=2, at=par('usr')[4], line=1, las=1, cex=1.5)

PlotMCvMR(TL84, main='')
mtext('f)', side=2, at=par('usr')[4], line=1, las=1, cex=1.5)

dev.off()

