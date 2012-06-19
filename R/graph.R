# Miscelaneous plot functions
.PlotPyramid <- function(community, values, xlim, xlab, ylab, col, main, 
                         show.level.labels, ...)
{
    # Plots a pyramid of values
    if(is.null(xlim))
    {
        xlim <- range(values)
    }

    # Values can be -ve so offset them
    v <- values - xlim[1] + 1
    v[is.infinite(v)] <- 0
    stopifnot(min(v)>=0)

    # The largest value that we can show
    max.lxl <- xlim[2] - xlim[1] + 1

    if(!is.null(names(col)))
    {
        col <- col[names(v)]
    }

    plot(NA, NA, type='n', xlim=c(-max.lxl/2, max.lxl/2), xaxt='n', xlab=xlab, 
         ylim=c(1,1+length(v)), yaxt='n', ylab=ylab, main=main, 
         frame.plot=FALSE, ...)

    rect(-abs(v)/2, 1:length(v), abs(v)/2, 1+(1:length(v)), 
         col=col)

    to.print <- sprintf('%.2f', values)
    to.print[is.infinite(values)] <- ''
    text(0, y=1:length(v)+0.5, to.print, ...)
    if(show.level.labels)
    {
        axis(2, at=1:length(v)+0.5, labels=names(v), las=1, tick=FALSE, ...)
    }
}

PlotNPyramid <- function(community, 
                         level=floor(PreyAveragedTrophicLevel(community)),
                         show.level.labels=TRUE,
                         xlab=Log10NLabel(community), 
                         ylab='', 
                         xlim=NULL, 
                         col=NULL, 
                         main=CPS(community)$title, 
                         ...)
{
    if(!is.Community(community)) stop('Not a Community')

    .RequireN(community)
    values <- log10(SumNByClass(community, class=level))
    .PlotPyramid(community, values=values, xlab=xlab, ylab=ylab, xlim=xlim, 
                 col=col, main=main, show.level.labels=show.level.labels, ...)
}

PlotBPyramid <- function(community,
                         level=floor(PreyAveragedTrophicLevel(community)),
                         show.level.labels=TRUE,
                         xlab=Log10BLabel(community), 
                         ylab='', 
                         xlim=NULL, 
                         col=NULL, 
                         main=CPS(community)$title, 
                         ...)
{
    if(!is.Community(community)) stop('Not a Community')

    .RequireM(community)
    .RequireN(community)
    values <- log10(SumBiomassByClass(community, class=level))
    .PlotPyramid(community, values=values, xlab=xlab, ylab=ylab, xlim=xlim, 
                 col=col, main=main, show.level.labels=show.level.labels, ...)
}

.PlotAbundanceSpectrum <- function(bins, binned.data, main, 
                                   xlab, ylab, xlim, ylim, pch, 
                                   show.bin.limits, show.bin.centres, ...)
{
    if(is.null(ylim))
    {
        ylim <- range(binned.data)
    }

    bin.centres <- attr(bins, 'bin.centres')
    breaks <- attr(bins, 'breaks')

    if(is.null(xlim))
    {
        xlim <- c(breaks[1], tail(breaks, 1))
    }

    plot(bin.centres[as.integer(names(binned.data))], binned.data, 
         xlab=xlab, xlim=xlim, ylab=ylab, ylim=ylim, 
         pch=pch, main=main, ..., xaxt='n')

    # Tick marks at top and right of plot
    axis(1, at=bin.centres, labels=round(bin.centres, 2), ...)

    dots <- list(...)
    if('n'!=par('xaxt') && !'n' %in% dots[['xaxt']])
    {
        axis(3, at=bin.centres, labels=FALSE, ...)
    }

    if('n'!=par('yaxt') && !'n' %in% dots[['yaxt']])
    {
        axis(4, labels=FALSE, ...)
    }

    if(show.bin.limits)
    {
        # Vertical lines delimiting bins
        abline(v=breaks, lty=3)
    }

    if(show.bin.centres)
    {
        # Vertical lines showing bin centres
        abline(v=bin.centres, lty=2)
    }

    # Plot lm
    x <- bin.centres[as.integer(names(binned.data))]
    y <- binned.data
    m <- lm(y~x)
    abline(m)

    to.return <- list(bins=bins, lm=m)

    invisible(to.return)
}

PlotNSpectrum <- function(community, 
                          lower=min(NP(community, 'M'), na.rm=TRUE), 
                          upper=max(NP(community, 'M'), na.rm=TRUE), 
                          n.bins=10, 
                          main=CPS(community)$title,  
                          xlab=Log10MLabel(community), 
                          ylab=Log10NLabel(community), 
                          xlim=NULL,
                          ylim=NULL, 
                          pch=19, 
                          show.bin.limits=TRUE, 
                          show.bin.centres=FALSE, 
                          ...)
{
    # The log10(sum(numerical abundance)) in equally-spaced log10(M) bins, 
    # together with a linear regression. If not NULL N.final is also binned 
    # and plotted together with it's own linear regression. The function 
    # returns a list(bin.centres, lm).

    if(!is.Community(community)) stop('Not a Community')

    .RequireM(community)
    .RequireN(community)

    # Bin totals
    bins <- BodyMassBins(community, lower=lower, upper=upper, n.bins=n.bins)
    binned.data <- log10(SumNByClass(community, class=bins))

    return (.PlotAbundanceSpectrum(bins, binned.data, main, xlab, ylab, 
                                   xlim, ylim, pch, show.bin.limits, 
                                   show.bin.centres, ...))
}

PlotBSpectrum <- function(community, 
                          lower=min(NP(community, 'M'), na.rm=TRUE), 
                          upper=max(NP(community, 'M'), na.rm=TRUE), 
                          n.bins=10, 
                          main=CPS(community)$title,
                          xlab=Log10MLabel(community), 
                          ylab=Log10BLabel(community), 
                          xlim=NULL,
                          ylim=NULL, 
                          pch=19, 
                          show.bin.limits=TRUE, 
                          show.bin.centres=FALSE, 
                          ...)
{
    # The log10(sum(biomass abundance)) in equally-spaced log10(M) bins, 
    # together with a linear regression. Returns a list(bin.centres, lm).

    if(!is.Community(community)) stop('Not a Community')

    .RequireM(community)
    .RequireN(community)

    # Bin totals
    bins <- BodyMassBins(community, lower=lower, upper=upper, n.bins=n.bins)
    binned.data <- log10(SumBiomassByClass(community, class=bins))

    return (.PlotAbundanceSpectrum(bins, binned.data, main, xlab, ylab, 
                                   xlim, ylim, pch, show.bin.limits, 
                                   show.bin.centres, ...))
}

PlotAuppervAlower <- function(community, 
                              main=CPS(community)$title,
                              xlab=~A[lower], 
                              ylab=~A[upper], 
                              xlim=c(-180, 180),
                              ylim=c(-180, 180), 
                              pch=19, 
                              ...)
{
    # Upper vs lower link angles. Cohen et al 2009 PNAS.
    community <- RemoveNodes(community, remove=with(NPS(community), 
                                                    node[is.na(M) | is.na(N)]))
    community <- RemoveCannibalisticLinks(community)
    tncp <- ThreeNodeChains(community, 
                            chain.properties='.NvMThreeNodeChainProperties')

    plot(tncp[,'Alower'], tncp[,'Aupper'], pch=pch, xlim=xlim, 
         ylim=ylim, main=main, xlab=xlab, ylab=ylab, ...)

    axis(3, labels=FALSE)
    axis(4, labels=FALSE)

    abline(v=median(tncp[,'Alower']))
    abline(h=median(tncp[,'Aupper']))
}

