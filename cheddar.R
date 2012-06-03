# Creates image for http://quicklizard99.github.com/cheddar/ homepage
# Six different views of the community of Tuesday Lake, 1984 are written to 
# images/cheddar.png and images/cheddar.svg.

options(warn=2)
library(cheddar)
data(TL84)

# Order the community's nodes by body mass
TL84 <- OrderCommunity(TL84, 'M')

Plots <- function()
{
    # Creates the six plots on one piece of graph paper
    par(mfrow=c(3,2),
        mar=c(4, 4, 1, 1),   # bottom, left, top, right
        mgp=c(2, 0.5, 0), 
        lwd=0.5)    # axis title, axis labels and axis line

    # A function for writing the figure's sub-heading
    next.sub <- 1
    Sub <- function()
    {
        mtext(paste(letters[next.sub], ')', sep=''), side=2, at=par('usr')[4], 
              line=1, las=1)
        next.sub <<- 1 + next.sub
    }

    PlotWebByLevel(TL84, show.nodes.as='both', cex=2, main='', 
                   y.layout='stagger')
    Sub()

    PlotCircularWeb(TL84, show.nodes.as='both', cex=2, main='')
    Sub()

    PlotNvM(TL84, show.nodes.as='both', cex=2, main='')
    Sub()

    PlotNPS(TL84, 'Log10M', 'PreyAveragedTrophicLevel', show.nodes.as='both', 
            cex=2, main='', xlab=Log10MLabel(TL84), 
            ylab='Prey-averaged trophic level')
    Sub()

    PlotPredationMatrix(TL84, main='')
    Sub()

    PlotMCvMR(TL84, main='')
    Sub()
}

# PNG
png('images/cheddar.png', width=720, height=980)
Plots()
dev.off()

# SVG
svg('images/cheddar.svg', width=7.7, height=10.5)
Plots()
dev.off()

