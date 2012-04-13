\name{PlotNPS}
\alias{PlotNPS}
\alias{PlotBvM}
\alias{PlotMvB}
\alias{PlotMvN}
\alias{PlotNvM}
\title{Plot node properties}
\description{High-level functions for plotting node properties.}
\usage{
PlotNPS(community, 
        X,
        Y,
        main = CPS(community)$title, 
        xlab, 
        ylab, 
        xlim = NULL, 
        ylim = NULL, 
        colour.by, 
        colour.spec, 
        col = NULL, 
        symbol.by, 
        symbol.spec, 
        pch = NULL, 
        bg.by, 
        bg.spec, 
        bg = NULL, 
        cex.by = NULL, 
        cex.spec = NULL, 
        cex = NULL, 
        label.colour.by = NULL, 
        label.colour.spec = NULL, 
        label.colour = NULL, 
        link.colour.by = NULL, 
        link.colour.spec = NULL, 
        link.col = NULL, 
        link.line.type.by = NULL, 
        link.line.type.spec = NULL, 
        link.lty = NULL, 
        link.lwd = NULL, 
        highlight.links = NULL, 
        highlight.nodes = Cannibals, 
        lowlight.nodes, 
        show.web = TRUE, 
        show.nodes.as = "points", 
        node.labels = NULL, 
        label.cex = 0.6, 
        are.values = FALSE, 
        frame.plot = TRUE,
        \dots)

PlotMvN(community, 
        xlab = Log10NLabel(community), 
        ylab = Log10MLabel(community), 
        \dots)

PlotNvM(community, 
        xlab = Log10MLabel(community), 
        ylab = Log10NLabel(community), 
        \dots)

PlotBvM(community, 
        xlab = Log10MLabel(community), 
        ylab = Log10BLabel(community), 
        \dots)

PlotMvB(community, 
        xlab = Log10BLabel(community), 
        ylab = Log10MLabel(community), 
        \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}}
  \item{X}{the name of a property that is plotted on the x axis. Must meet the 
    criteria of the \code{properties} parameter of \code{NPS}. 
    If \code{are.values} is \code{TRUE} then \code{X} and \code{Y} should be 
    vectors of length \code{NumberOfNodes}.}
  \item{Y}{plotted on the y axis; see \code{X}.}
  \item{xlab}{title of the x axis.}
  \item{ylab}{title of the y axis.}
  \item{main}{title of the plot.}
  \item{xlim}{limits of the x axis.} 
  \item{ylim}{limits of the y axis.} 
  \item{colour.by}{node colours property. Either \code{NULL}, a vector of 
    length \code{NumberOfNodes} or the name of a property that meets the 
    criteria of the \code{properties} parameter of \code{NPS}.}
  \item{colour.spec}{node colours specification. Either \code{NULL} or a named 
    vector that maps values of \code{colour.by} to plotting values.}
  \item{col}{node colours.}
  \item{symbol.by}{node symbols property; must meet the criteria of 
    \code{colour.by}.}
  \item{symbol.spec}{node symbols specification.}
  \item{pch}{node symbols.}
  \item{bg.by}{node background colours property; must meet the criteria of 
    \code{colour.by}.}
  \item{bg.spec}{node background colours specification; must meet the 
    criteria of \code{colour.spec}.}
  \item{bg}{node background colours.}
  \item{cex.by}{node cex values property; must meet the criteria of 
    \code{colour.by}.}
  \item{cex.spec}{node cex values specification; must meet the 
    criteria of \code{colour.spec}.}
  \item{cex}{node cex values.}
  \item{label.colour.by}{node label colours property; must meet the criteria 
    of \code{colour.by}.}
  \item{label.colour.spec}{node label colours specification; must meet the 
    criteria of \code{colour.spec}.}
  \item{label.colour}{node label colours.}
  \item{link.colour.by}{link colours; either \code{NULL}, a vector of 
    length \code{NumberOfTrophicLinks} or the name of a property 
    that meets the criteria of the \code{link.properties} parameter of 
    \code{TLPS}.}
  \item{link.colour.spec}{link line colour specification; either 
    \code{NULL} or a named vector that maps values of \code{link.colour.by} to 
    plotting values.}
  \item{link.col}{link colours.}
  \item{link.line.type.by}{link link types; must meet the criteria of 
    \code{link.colour.by}.}
  \item{link.line.type.spec}{link line type specification; must meet the 
    criteria of {link.colour.spec}.}
  \item{link.lty}{link line types.}
  \item{link.lwd}{line line widths.}
  \item{highlight.links}{either \code{NULL}, a vector of length 
    \code{NumberOfNodes} or a name that meets the criteria of the 
    \code{properties} paremeter of \code{NPS}..}
  \item{highlight.nodes}{nodes to be highlighted; either \code{NULL}, a vector 
    of node names, a vector of node indices or a function that takes a 
    \code{Community} as its only parameter and returns a vector of either 
    node names or indices.}
  \item{lowlight.nodes}{nodes to be lowlighted; must meet the criteria of 
    \code{highlight.nodes}.}
  \item{show.web}{logical - if \code{TRUE} and \code{community} has trophic 
    links then the food web is plotted using the \code{link*} and 
    \code{highlight.links} parameters}
  \item{show.nodes.as}{how nodes should be plotted. One of 
    \enumerate{
    \item \code{"points"} for symbols, 
    \item \code{"labels"} for text (see \code{node.labels}, \code{label.cex} 
    and \code{label.colour}), 
    \item \code{"points"} for symbols and text.
    }}
  \item{node.labels}{Either \code{NULL}, a vector of length 
    \code{NumberOfNodes} or a name that meets the criteria of the 
    \code{properties} paremeter of \code{NPS}. If \code{NULL} node labels 
     are \code{1:NumberOfNodes}.}
  \item{label.cex}{a character expansion factor; used only if 
    \code{show.nodes.as} is equal to "points".}
  \item{are.values}{logical - if \code{TRUE} \code{X} and \code{Y} must be 
    vectors of values of length \code{NumberOfNodes}.}
  \item{frame.plot}{logical - default \code{TRUE}.}
  \item{\dots}{other values to plot functions.}
}
\details{The general-purpose function \code{PlotNPS} plots one 
node property against another.

For \code{colour.by}, \code{symbol.by}, \code{bg.by}, \code{cex.by} and 
\code{label.colour.by}, if \code{X.by} is not \code{NULL} and a relevant 
\code{X.spec} is not given, the \code{X.by} values are converted to a 
\code{factor}, the levels of which are used as the plot parameter. 
An error is raised if \code{X.by} contains any values not present in 
\code{X.spec}.

If \code{colour.by}/\code{bg.by}/\code{symbol.by} is \code{NULL} and 
\code{community} has a node property named `category' then node 
colours/background colours/symbols are given by `category' using the 
\code{colour.spec}/\code{bg.spec}/\code{symbol.spec} given by 
\code{DefaultCategoryColours}/\code{DefaultCategorySymbols}.

\code{label.colour.by}, \code{node.labels} and \code{label.cex} are used only 
if \code{show.nodes.as} is equal to "points".

The convenience functions \code{PlotMvN}, \code{PlotNvM}, \code{PlotBvM} and 
\code{PlotMvB} are `wrappers' around \code{PlotNPS} that plot 
log10-transformed body mass (M), numerical abundance (N) or biomass (B). 
All of the parameters of \code{PlotNPS}, with the exception of 
\code{X}, \code{Y} and \code{are.values}, can be used with these four 
functions.

If \code{show.nodes.as} is equal to "points" then labels are plotted using 
\code{label.cex} and \code{label.colour}.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{NPS}}, 
         \code{\link{DefaultCategoryColours}}, 
         \code{\link{DefaultCategorySymbols}}, 
         \code{\link{PlotBSpectrum}}, 
         \code{\link{PlotCircularWeb}}, 
         \code{\link{PlotNPSDistribution}}, 
         \code{\link{PlotNSpectrum}}, 
         \code{\link{PlotRankNPS}}, 
         \code{\link{PlotTLPS}}, 
         \code{\link{PlotWebByLevel}}}

\examples{
data(TL84)
PlotNvM(TL84)

# Set colours and plot symbols directly
PlotNvM(TL84, col=1, pch=19, highlight.nodes=NULL)

# Plot each level of taxonomic resolution in a different colour
PlotNvM(TL84, colour.by='resolved.to', pch=19, highlight.nodes=NULL)

# Plot each level of taxonomic resolution in a specific colour
colour.spec <- c(Species='purple3', Genus='green3', Group='red3')
PlotNvM(TL84, colour.by='resolved.to', colour.spec=colour.spec, pch=19, 
        highlight.nodes=NULL)
legend("topright", legend=names(colour.spec), pch=19, col=colour.spec)

# Use PlotNPS to plot trophic height against log10 body mass
PlotNPS(TL84, 'Log10M', 'TrophicHeight', xlab=Log10MLabel(TL84), 
        ylab='Trophic height')
}
\keyword{hplot}

