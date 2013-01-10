# High-level and helper functions for plotting trophic links
PlotTLPS <- function(community, 
                     X, 
                     Y, 
                     xlab, 
                     ylab, 
                     axes.limits.equal=FALSE,
                     xlim=NULL, 
                     ylim=NULL, 
                     main=CPS(community)$title, 
                     highlight.links=NULL,
                     lowlight.links=NULL,
                     colour.by, 
                     colour.spec, 
                     col=NULL,
                     symbol.by, 
                     symbol.spec, 
                     pch=NULL,
                     bg.by,
                     bg.spec,
                     bg=NULL, 
                     cex.by=NULL,
                     cex.spec=NULL,
                     cex=NULL, 
                     are.values=FALSE, 
                     ...)
{
    if(!are.values)
    {
        if(missing(xlab))   xlab <- .ResolveTrophicLinkAxisLabel(X)
        if(missing(ylab))   ylab <- .ResolveTrophicLinkAxisLabel(Y)

        X <- .ResolveTrophicLinkProperty(community, X)
        Y <- .ResolveTrophicLinkProperty(community, Y)
    }
    else
    {
        if(missing(xlab))   xlab <- ''
        if(missing(ylab))   ylab <- ''

        stopifnot(length(X)==NumberOfTrophicLinks(community))
        stopifnot(length(Y)==NumberOfTrophicLinks(community))
    }

    if(missing(colour.by))
    {
        colour.by <- NULL
        if('category' %in% NodePropertyNames(community))
        {
            colour.by <- 'resource.category'
        }
    }

    if(missing(colour.spec))
    {
        colour.spec <- NULL
        if(!is.null(colour.by) && 
           colour.by %in% c('resource.category','consumer.category'))
        {
            colour.spec <- DefaultCategoryColours()
        }
    }

    # bg
    if(missing(bg.by))
    {
        bg.by <- NULL
        if('category' %in% NodePropertyNames(community))
        {
            bg.by <- 'resource.category'
        }
    }

    if(missing(bg.spec))
    {
        bg.spec <- NULL
        if(!is.null(bg.by) && 
           bg.by %in% c('resource.category','consumer.category'))
        {
            bg.spec <- DefaultCategoryColours()
        }
    }

    # pch
    if(missing(symbol.by))
    {
        symbol.by <- NULL
        if('category' %in% NodePropertyNames(community))
        {
            symbol.by <- 'resource.category'
        }
    }

    if(missing(symbol.spec))
    {
        symbol.spec <- NULL
        if(!is.null(symbol.by) && 
           symbol.by %in% c('resource.category','consumer.category'))
        {
            symbol.spec <- DefaultCategorySymbols()
        }
    }

    if(is.function(highlight.links))
    {
        highlight.links <- highlight.links(community)
    }
    highlight.links <- .ResolveTrophicLinksToRowIndices(community, 
                                                        highlight.links)

    if(is.function(lowlight.links))
    {
        lowlight.links <- lowlight.links(community)
    }
    lowlight.links <- .ResolveTrophicLinksToRowIndices(community,lowlight.links)


    col <- .TrophicLinkGraphParamFromSpec(community, 'col', col, 
                                          colour.by, colour.spec, 
                                          default=par('col'))
    col <- .HighlightColours(NumberOfTrophicLinks(community), col, 
                             highlight.links, lowlight.links)

    pch <- .TrophicLinkGraphParamFromSpec(community, 'pch', pch, 
                                          symbol.by, symbol.spec, default=19)
    pch <- .HighlightSymbols(NumberOfTrophicLinks(community), pch, 
                             highlight.links, lowlight.links)

    bg <- .TrophicLinkGraphParamFromSpec(community, 'bg', bg, 
                                         bg.by, bg.spec)
    bg <- .HighlightBG(NumberOfTrophicLinks(community), bg, highlight.links, 
                       lowlight.links)

    cex <- .TrophicLinkGraphParamFromSpec(community, 'cex', cex, 
                                          cex.by, cex.spec)
    cex <- .HighlightCex(NumberOfTrophicLinks(community), cex, highlight.links, 
                         lowlight.links)

    if(axes.limits.equal && 
       (is.null(xlim) || missing(xlim)) && (is.null(ylim) || missing(ylim)))
    {
        xlim <- ylim <- range(c(X, Y))
    }

    plot(X, Y, xlim=xlim, ylim=ylim, main=main, xlab=xlab, ylab=ylab, type='n', 
         ...)
    .AddAxisTicks(...)
    .PlotHighlightedPoints(X, Y, col=col, pch=pch, bg=bg, cex=cex, 
                           labels=NULL, label.col=NULL, label.cex=NULL, 
                           show.points=TRUE, show.labels=FALSE, 
                           highlight=highlight.links, ...)
}

PlotPredationMatrix <- function(community, 
                                xlab='Consumer', 
                                ylab='Resource', 
                                row.order, 
                                col.order,
                                ...)
{
    # Plots the predation matrix
    if(!is.Community(community)) stop('Not a Community')

    .RequireTrophicLinks(community)

    tlp <- TLPS(community)
    nodes <- unname(NP(community, 'node'))

    if(missing(row.order))
    {
        row.order <- nodes
    }
    else if(1==length(row.order))
    {
        row.order <- nodes[order(NPS(TL84, row.order)[,row.order])]
    }
    else if(is.numeric(row.order))
    {
        stopifnot(all(0 < row.order & row.order <= NumberOfNodes(community)))
        stopifnot(length(unique(row.order)) == NumberOfNodes(community))
        stopifnot(range(row.order) == c(1, NumberOfNodes(community)))
        row.order <- nodes[row.order]
    }

    if(missing(col.order))
    {
        col.order <- nodes
    }
    else if(1==length(col.order))
    {
        col.order <- nodes[order(NPS(TL84, col.order)[,col.order])]
    }
    else if(is.numeric(col.order))
    {
        stopifnot(all(0 < col.order & col.order <= NumberOfNodes(community)))
        stopifnot(length(unique(col.order)) == NumberOfNodes(community))
        stopifnot(range(col.order) == c(1, NumberOfNodes(community)))
        col.order <- nodes[col.order]
    }

    stopifnot(sort(row.order)==sort(nodes))
    stopifnot(sort(col.order)==sort(nodes))
    X <- sapply(tlp[,'consumer'], function(s) return (which(s==col.order)))
    Y <- sapply(tlp[,'resource'], function(s) return (which(s==row.order)))

    # Plot y values inverted
    n <- length(nodes)
    PlotTLPS(community, 
             X=X, 
             Y=1+n-Y,
             xlab=xlab, ylab=ylab, 
             xlim=c(1, n), ylim=c(1, n), 
             are.values=TRUE, 
             xaxt='n', yaxt='n', ...)

    if(all(row.order==col.order))
    {
        # Points on this line are cannibals
        abline(a=n+1, b=-1, lty=2)
    }
}

PlotMRvMC <- function(community, 
                      xlab=Log10MLabel(community, name='italic(M)[consumer]'),
                      ylab=Log10MLabel(community, name='italic(M)[resource]'),
                      axes.limits.equal=TRUE,
                      ...)
{
    # Plots log10M of resources against log10M of consumers
    if(!is.Community(community)) stop('Not a Community')
    .RequireM(community)
    .RequireTrophicLinks(community)
    PlotTLPS(community, X='consumer.Log10M', 
             Y='resource.Log10M', xlab=xlab, ylab=ylab, 
             are.values=FALSE, axes.limits.equal=axes.limits.equal, ...)
    abline(a=0, b=1, lty=2)
}

PlotMCvMR <- function(community, 
                      xlab=Log10MLabel(community, name='italic(M)[resource]'),
                      ylab=Log10MLabel(community, name='italic(M)[consumer]'),
                      axes.limits.equal=TRUE,
                      ...)
{
    # Plots log10M of resources against log10M of consumers
    if(!is.Community(community)) stop('Not a Community')
    .RequireM(community)
    .RequireTrophicLinks(community)
    PlotTLPS(community, X='resource.Log10M', 
             Y='consumer.Log10M', xlab=xlab, ylab=ylab, 
             are.values=FALSE, axes.limits.equal=axes.limits.equal, ...)
    abline(a=0, b=1, lty=2)
}

PlotNRvNC <- function(community, 
                      xlab=Log10NLabel(community, name='italic(N)[consumer]'), 
                      ylab=Log10NLabel(community, name='italic(N)[resource]'), 
                      axes.limits.equal=TRUE,
                      ...)
{
    # Plots log10M of resources against log10M of consumers
    if(!is.Community(community)) stop('Not a Community')
    .RequireN(community)
    .RequireTrophicLinks(community)
    PlotTLPS(community, X='consumer.Log10N', 
             Y='resource.Log10N', xlab=xlab, ylab=ylab, 
             are.values=FALSE, axes.limits.equal=axes.limits.equal, ...)
    abline(a=0, b=1, lty=2)
}

PlotNCvNR <- function(community, 
                      xlab=Log10NLabel(community, name='italic(N)[resource]'), 
                      ylab=Log10NLabel(community, name='italic(N)[consumer]'), 
                      axes.limits.equal=TRUE,
                      ...)
{
    # Plots log10M of resources against log10M of consumers
    if(!is.Community(community)) stop('Not a Community')
    .RequireN(community)
    .RequireTrophicLinks(community)
    PlotTLPS(community, X='resource.Log10N', 
             Y='consumer.Log10N', xlab=xlab, ylab=ylab, 
             are.values=FALSE, axes.limits.equal=axes.limits.equal, ...)
    abline(a=0, b=1, lty=2)
}

PlotBRvBC <- function(community, 
                      xlab=Log10BLabel(community, name='italic(B)[consumer]'),
                      ylab=Log10BLabel(community, name='italic(B)[resource]'),
                      axes.limits.equal=TRUE,
                      ...)
{
    # Plots log10M of resources against log10M of consumers
    if(!is.Community(community)) stop('Not a Community')
    .RequireM(community)
    .RequireTrophicLinks(community)
    PlotTLPS(community, X='consumer.Log10Biomass', 
             Y='resource.Log10Biomass', xlab=xlab, ylab=ylab, 
             are.values=FALSE, axes.limits.equal=axes.limits.equal, ...)
    abline(a=0, b=1, lty=2)
}

PlotBCvBR <- function(community, 
                      xlab=Log10BLabel(community, name='italic(B)[resource]'),
                      ylab=Log10BLabel(community, name='italic(B)[consumer]'),
                      axes.limits.equal=TRUE,
                      ...)
{
    # Plots log10M of resources against log10M of consumers
    if(!is.Community(community)) stop('Not a Community')
    .RequireM(community)
    .RequireTrophicLinks(community)
    PlotTLPS(community, X='resource.Log10Biomass', 
             Y='consumer.Log10Biomass', xlab=xlab, ylab=ylab, 
             are.values=FALSE, axes.limits.equal=axes.limits.equal, ...)
}

PlotDegreeDistribution <- function(community, 
                                   xlab='Number of links', 
                                   ...)
{
    if(!is.Community(community)) stop('Not a Community')
    .RequireTrophicLinks(community)
    d <- DegreeDistribution(community)
    barplot(height=d, xlab=xlab, ...)
}

