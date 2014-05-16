\name{PlotTLPS}
\alias{PlotTLPS}
\alias{PlotPredationMatrix}
\alias{PlotBCvBR}
\alias{PlotBRvBC}
\alias{PlotMCvMR}
\alias{PlotMRvMC}
\alias{PlotNCvNR}
\alias{PlotNRvNC}
\title{Plot trophic-link properties}
\description{High-level functions for plotting trophic link properties.}
\usage{
PlotTLPS(community, 
         X, 
         Y,
         xlab, 
         ylab, 
         axes.limits.equal = FALSE,
         xlim = NULL, 
         ylim = NULL, 
         main = CPS(community)$title, 
         highlight.links = NULL, 
         lowlight.links = NULL, 
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
         are.values = FALSE,
         \dots)

PlotPredationMatrix(community, 
                    xlab='Consumer', 
                    ylab='Resource', 
                    resource.order,
                    consumer.order,
                    \dots)

PlotMRvMC(community, 
          xlab=Log10MLabel(community, name='italic(M)[consumer]'),
          ylab=Log10MLabel(community, name='italic(M)[resource]'),
          axes.limits.equal = TRUE,
          \dots)

PlotMCvMR(community, 
          xlab=Log10MLabel(community, name='italic(M)[resource]'),
          ylab=Log10MLabel(community, name='italic(M)[consumer]'),
          axes.limits.equal = TRUE,
          \dots)

PlotNRvNC(community, 
          xlab=Log10NLabel(community, name='italic(N)[consumer]'),
          ylab=Log10NLabel(community, name='italic(N)[resource]'),
          axes.limits.equal = TRUE,
          \dots)

PlotNCvNR(community, 
          xlab=Log10NLabel(community, name='italic(N)[resource]'),
          ylab=Log10NLabel(community, name='italic(N)[consumer]'),
          axes.limits.equal = TRUE,
          \dots)

PlotBRvBC(community, 
          xlab=Log10BLabel(community, name='italic(B)[consumer]'),
          ylab=Log10BLabel(community, name='italic(B)[resource]'),
          axes.limits.equal = TRUE,
          \dots)

PlotBCvBR(community, 
          xlab=Log10BLabel(community, name='italic(B)[resource]'),
          ylab=Log10BLabel(community, name='italic(B)[consumer]'),
          axes.limits.equal = TRUE,
          \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{X}{the name of a node or link property to plot on the x axis. 
    If the name begins with 'resource.' or 'consumer.', the remainder of the 
    name is assumed to be a node property and should meet the criteria of the 
    \code{node.properties} parameter of \code{TLPS}, otherwise the name 
    is assumed to be a link property and should meet the criteria of the 
    \code{link.properties} parameter of \code{TLPS}. 

    If \code{are.values} is \code{TRUE} then \code{X} and \code{Y} should be 
    vectors of length \code{NumberOfTrophicLinks}.}
  \item{Y}{plotted on the y axis; see \code{X}.}
  \item{xlab}{title of the x axis.}
  \item{ylab}{title of the y axis.}
  \item{axes.limits.equal}{logical - if \code{TRUE} and \code{xlim} and 
    \code{ylim} are \code{NULL} then the limits of the x and y axes will be 
    the same.}
  \item{xlim}{limits of the x axis} 
  \item{ylim}{limits of the y axis} 
  \item{main}{title of the plot}
  \item{highlight.links}{trophic links to be highlighted; either \code{NULL}, 
    a vector of trophic link indices or a function that takes a 
    \code{Community} as its only parameter and returns a \code{data.frame} 
    containing the columns `resource' and `consumer', which should 
    contain node names.}
  \item{lowlight.links}{trophic links to be lowlighted; should meet criteria 
    of \code{lowlight.links}.}
  \item{colour.by}{trophic link colours property. Either \code{NULL}, a vector 
    of length \code{NumberOfTrophicLinks} or a name. If the name begins with 
    'resource.' or 'consumer.', the remainder of the name is assumed to be a 
    node property and should meet the criteria of the \code{node.properties} 
    parameter of \code{TLPS}, otherwise the name is assumed to be a link 
    property and should meet the criteria of the \code{link.properties} 
    parameter of \code{TLPS}.}
  \item{colour.spec}{trophic links colours specification. either \code{NULL} 
    or a named vector that maps values of \code{colour.by} to plotting values.}
  \item{col}{trophic links colours.}
  \item{symbol.by}{trophic links symbols property; must meet the criteria of 
    \code{colour.by}.}
  \item{symbol.spec}{trophic links symbols specification specification; must 
    meet the criteria of \code{colour.spec}.}
  \item{pch}{trophic links symbols.}
  \item{bg.by}{trophic links background colours property; must meet the 
    criteria of \code{colour.by}}
  \item{bg.spec}{trophic links background colours specification; must meet the 
    criteria of \code{colour.spec}.}
  \item{bg}{trophic links background colours.}
  \item{cex.by}{trophic links cex property; must meet the criteria of 
    \code{colour.by}}
  \item{cex.spec}{cex values specification; must meet the criteria of 
    \code{colour.spec}.}
  \item{cex}{cex values.}
  \item{are.values}{logical - if \code{TRUE} \code{X} and \code{Y} must be 
    vectors of values of length \code{NumberOfTrophicLinks}.}
  \item{resource.order}{the order in which to show resources. Either missing, 
  which implies the native node order, a vector of length 
  \code{NumberOfTrophicLinks} containing the integer order of resources, 
  or the name of a property that meets the criteria of the \code{properties} 
  parameter of \code{NPS}.}
  \item{consumer.order}{the order in which to show consumer; requirements 
  are the same as \code{resource.order}.}
  \item{\dots}{other values to plot functions.}
}

\details{The general-purpose function \code{PlotTLPS} plots one trophic-link 
property against another.

If \code{colour.by}/\code{bg.by}/\code{symbol.by} is \code{NULL} and 
\code{community} has a node property named `category' then trophic-link 
colours/background colours/symbols are given by `resource.category' using 
\code{colour.spec}/\code{bg.spec}/\code{symbol.spec} given by 
\code{DefaultCategoryColours}/\code{DefaultCategorySymbols}.

\code{PlotPredationMatrix} shows trophic links as a binary matrix with species 
shown in node order, starting at the top-left. If \code{row.node} and 
\code{col.order} are both missing (the default) or are the same, then a 
dashed diagonal line is drawn. Points on the dashed line indicate 
cannibalistic trophic links.

The convenience functions \code{PlotMRvMC}, \code{PlotMCvMR}, \code{PlotNRvNC}, 
\code{PlotNCvNR}, \code{PlotBRvBC}, \code{PlotBCvBR} are `wrappers' around 
\code{PlotRankNPS} that plot a log10-transformed body mass, M, numerical 
abundance, N, or biomass abundance, B.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{TLPS}}, 
         \code{\link{PlotBSpectrum}}, 
         \code{\link{PlotCircularWeb}}, 
         \code{\link{PlotNPS}}, 
         \code{\link{PlotNPSDistribution}}, 
         \code{\link{PlotNSpectrum}}, 
         \code{\link{PlotRankNPS}}, 
         \code{\link{PlotWebByLevel}}}

\examples{
data(TL84)

# The predation matrix
PlotPredationMatrix(TL84)

# The predation matrix with rows ordered by body mass
PlotPredationMatrix(TL84, resource.order='M')

# Colours and symbols by resource.category
PlotMCvMR(TL84)

# Colours and symbols by consumer.category
PlotMCvMR(TL84, bg.by='consumer.category', symbol.by='consumer.category', 
          colour.by='consumer.category')

# Consumer trophic height against resource log10(M)
PlotTLPS(TL84, 'resource.Log10M', 'consumer.TrophicHeight')

# Log10(M of resource / M of consumer) against consumer log10(M)
PlotTLPS(TL84, 'consumer.Log10M', 'Log10RCMRatio')
}
\keyword{hplot}

