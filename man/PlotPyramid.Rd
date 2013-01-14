\name{Pyramid plots}
\alias{PlotBPyramid}
\alias{PlotNPyramid}
\title{Pyramid plots}
\description{High-level functions that create pyramid plots.}
\usage{
PlotBPyramid(community, 
             level = floor(PreyAveragedTrophicLevel(community)),
             expected.levels,
             fill.missing.levels = TRUE,
             order.by.expected = TRUE,
             show.level.labels = TRUE, 
             xlab = Log10BLabel(community, name=expression(~sum(italic(B)))), 
             ylab = "", 
             xlim = NULL, 
             col = NULL, 
             text.col = 'black',
             main = CPS(community)$title,
             \dots)

PlotNPyramid(community, 
             level = floor(PreyAveragedTrophicLevel(community)),
             expected.levels,
             fill.missing.levels = TRUE,
             order.by.expected = TRUE,
             show.level.labels = TRUE, 
             xlab = Log10NLabel(community, name=expression(~sum(italic(N)))),
             ylab = "", 
             xlim = NULL, 
             col = NULL, 
             text.col = 'black',
             main = CPS(community)$title,
             \dots)

}

\arguments{
  \item{community}{an object of class \code{Community}}
  \item{level}{levels by which values are summed. Can be either the name of a 
    node property, in which case it must meet the criteria of the 
    \code{properties} parameter of \code{NPS}, or a vector of length 
    \code{NumberOfNodes} that contains the levels.}
  \item{expected.levels}{the values that are expected to be in \code{level}; 
  see Details for more information.}
  \item{fill.missing.levels}{if \code{TRUE}, values in \code{expected.levels} 
  that are not present in \code{level} are shown in the pyramid.}
  \item{order.by.expected}{if \code{TRUE} then the levels are plotted in 
  the order given in \code{expected.levels}.}
  \item{show.level.labels}{logical - if \code{TRUE} then values of \code{level} 
    are shown to the left of the pyramid.}
  \item{xlab}{title of the x axis.}
  \item{ylab}{title of the y axis.}
  \item{xlim}{limits of the x axis.} 
  \item{col}{fill colour; either a single colour a vector containing a colour 
    per \code{level}.}
  \item{text.col}{colour for the text showing the log10-transformed sums in the 
    blocks of the pyramid; ; either a single colour a vector containing a 
    colour per \code{level}.}
  \item{main}{title of the plot.}
  \item{\dots}{other values to plot functions.}
}

\details{\code{PlotBPyramid} plots log10-transformed sum biomass abundance in 
each \code{level} and \code{PlotNPyramid} plots log10-transformed sum 
numerical abundance in each \code{level}.

\code{expected.levels} provides two behaviours. First, it provides error 
checking: an error is raised if values are in \code{level} that 
are not in \code{expected.level}. Second, it interacts with 
\code{fill.missing.levels} and \code{order.by.expected} to control which levels 
are drawn and how. If \code{fill.missing.levels} is \code{TRUE} then values 
in \code{expected.levels} that are not present in \code{level} are shown on the 
pyramid plot. If \code{order.by.level} is \code{TRUE} then the levels are 
plotted in the order given in \code{expected.levels}.
If \code{level} contains numbers then \code{expected.levels} defaults to a 
sequence of integers \code{floor(min(level)):ceiling(max(level))}. 
If \code{level} is `category' then \code{expected.levels} defaults to the 
intersection of values of `category' that are present in \code{community} and 
the usual Cheddar default values: `<unnamed>', `producer', 
`invertebrate', `vert.ecto', `vert.endo'.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{SumBiomassByClass}}, 
         \code{\link{SumNByClass}}, \code{\link{Log10BLabel}}, 
         \code{\link{Log10NLabel}}, \code{\link{floor}}, \code{\link{ceiling}}}

\examples{
data(TL84)

# Use a large left-hand margin to show level text
reset.par <- par(mar=c(5,8,1,1))

# Using prey-averaged trophic level
PlotNPyramid(TL84)

# Using chain-averaged of trophic level
PlotNPyramid(TL84, level=floor(ChainAveragedTrophicLevel(TL84)))

# Show by category
PlotNPyramid(TL84, level='category')

# Taxonomic kingdoms as levels
PlotNPyramid(TL84, level='kingdom')

# Taxonomic kingdoms as levels, with a defined order
PlotNPyramid(TL84, level='kingdom', expected.levels=c("<unnamed>", "Plantae", 
             "Chromista","Bacteria","Protozoa","Animalia"))


# Compare the YthanEstuary and the TL84 datasets. YthanEstuary has nodes in 
# each of the categories whereas TL84 only has producer, invertebrate and 
# vert.ecto nodes. Show categories that are not present in TL84
par(mfrow=c(1,2))
data(YthanEstuary)
xlim <- range(c(Log10N(TL84), Log10N(YthanEstuary)), na.rm=TRUE)
PlotNPyramid(TL84, level='category', xlim=xlim, 
             expected.levels=c('<unnamed>', 'producer', 'invertebrate', 
                               'vert.ecto', 'vert.endo'))
PlotNPyramid(YthanEstuary, level='category',  xlim=xlim)
par(mfrow=c(1,1))


# For the BroadstoneStream dataset, the LongestTrophicLevel function returns 
# nodes in levels 1 and 7 to 10 but no nodes in levels 2 to 6. 
# By default all levels between the minimum and maximum are shown, so levels 
# 2 to 6 appear with no boxes.
data(BroadstoneStream)
PlotNPyramid(BroadstoneStream, 
             level=floor(LongestTrophicLevel(BroadstoneStream)))

# Set fill.missing.levels to FALSE to prevent levels 2 to 6 from being drawn.
PlotNPyramid(BroadstoneStream, 
             level=floor(LongestTrophicLevel(BroadstoneStream)), 
             fill.missing.levels=FALSE)

par(reset.par)
}
\keyword{hplot}

