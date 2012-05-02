# Writes cheddar.png
options(warn=2)
library(cheddar)
data(TL84)
png('images/cheddar.png', width=550, height=500)
par(mfrow=c(2,2),
    mar=c(4, 4, 1, 1),   # bottom, left, top, right
    mgp=c(2, 0.5, 0))    # axis title, axis labels and axis line
PlotWebByLevel(TL84, show.nodes.as='both', cex=2, main='')
PlotCircularWeb(TL84, show.nodes.as='both', cex=2, main='')
PlotNvM(TL84, show.nodes.as='both', cex=2, main='')
PlotMCvMR(TL84, main='')
dev.off()

