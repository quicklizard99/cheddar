#!/usr/bin/env RScript
# Example Cheddar plots
library(cheddar)

data(TL84)

TL84 <- OrderCommunity(TL84, 'M', 'Degree')

png('pages/content/pages/cheddar-example.png', width=760, height=525)
cex <- 1
par(mfrow=c(2, 3),
    mar=c(4, 3.7, 2, 1),  # bottom, left, top, right
    mgp=c(2, 0.5, 0),     # axis title, axis labels and axis line
    cex=cex, 
    lwd=0.8,
    cex.axis=cex*1.4, 
    cex.lab=cex*1.4,
    cex.main=cex*1.4)

link.col <- col2rgb(DefaultLinkColour())
link.col <- rgb(link.col[1], link.col[2], link.col[3], 0x77, maxColorValue=0xff)
PlotPredationMatrix(TL84, main='PlotPredationMatrix')
PlotCircularWeb(TL84, main='PlotCircularWeb', link.col=link.col)
PlotWagonWheel(TL84, focus='Daphnia pulex', main='PlotWagonWheel', link.col=link.col)

PlotWebByLevel(TL84, main='PlotWebByLevel', link.col=link.col)
PlotNPyramid(TL84, main='PlotNPyramid')
PlotNvM(TL84, main='PlotNvM', link.col=link.col)
models <- NvMLinearRegressions(TL84)[1:3]
colours <- PlotLinearModels(models, lwd=2)
dev.off()
