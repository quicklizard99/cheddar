\name{Pyramid plots}
\alias{PlotBPyramid}
\alias{PlotNPyramid}
\title{Pyramid plots}
\description{High-level functions that create pyramid plots.}
\usage{
PlotBPyramid(community, 
             level = floor(PreyAveragedTrophicLevel(community)),
             show.level.labels = TRUE, 
             xlab = Log10BLabel(community), 
             ylab = "", 
             xlim = NULL, 
             col = NULL, 
             main = CPS(community)$title,
             \dots)

PlotNPyramid(community, 
             level = floor(PreyAveragedTrophicLevel(community)),
             show.level.labels = TRUE, 
             xlab = Log10NLabel(community), 
             ylab = "", 
             xlim = NULL, 
             col = NULL, 
             main = CPS(community)$title,
             \dots)

}

\arguments{
  \item{community}{an object of class \code{Community}}
  \item{level}{levels by which values are summed. Can be either the name of a 
    node property, in which case it must meet the criteria of the 
    \code{properties} parameter of \code{NPS}, or a vector of length 
    \code{NumberOfNodes} that contains the levels.}
  \item{show.level.labels}{logical - if \code{TRUE} then values of \code{level} 
    are shown to the left of the pyramid.}
  \item{xlab}{title of the x axis.}
  \item{ylab}{title of the y axis.}
  \item{xlim}{limits of the x axis.} 
  \item{col}{fill colour; either a single colour a vector containing a colour 
    per \code{level}.}
  \item{main}{title of the plot.}
  \item{\dots}{other values to plot functions.}
}

\details{\code{PlotBPyramid} plots log10-transformed sum biomass abundance in 
each \code{level} and \code{PlotNPyramid} plots log10-transformed sum 
numerical abundance in each \code{level}.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{SumBiomassByClass}}, 
         \code{\link{SumNByClass}}}

\examples{
data(TL84)

PlotNPyramid(TL84)

# Use a different measure of trophic level
PlotNPyramid(TL84, level=floor(ChainAveragedTrophicLevel(TL84)))
}
\keyword{hplot}

