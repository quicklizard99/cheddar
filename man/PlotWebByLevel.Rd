\name{PlotWebByLevel}
\alias{PlotWebByLevel}
\title{Plot web by level}
\description{A high-level function for plotting a food-web by vertically with 
the lowest trophic-level nodes at the bottom.}
\usage{
PlotWebByLevel(community, 
               level='PreyAveragedTrophicLevel', 
               max.nodes.per.row=20,
               round.levels.to.nearest=0.2,
               stagger=0.1, 
               x.layout='wide', 
               y.layout='compress', 
               show.level.labels=TRUE,
               show.level.lines=FALSE, 
               xaxt='n', 
               yaxt='n', 
               xlab='', 
               ylab='', 
               frame.plot=FALSE,
               ylim=NULL,
               \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}}
  \item{level}{either a function, a name that meets the criteria of the 
    \code{properties} parameter of \code{NPS} or a vector of length 
    \code{NumberOfNodes}, which must contain numbers greater than 0.}
  \item{max.nodes.per.row}{a number greater than 2.}
  \item{round.levels.to.nearest}{a number greater or equal to 0 and less than 
    1.}
  \item{stagger}{a number greater or equal to 0 and less than 1. Only used if 
    \code{y.layout} is 'stagger'.}
  \item{x.layout}{'skinny', 'narrow' or 'wide'.}
  \item{y.layout}{'stagger' or 'compress'. Only has an effect if 
    \code{round.levels.to.nearest} is greater than 0.}
  \item{show.level.labels}{logical - if \code{TRUE} then integer values of 
    \code{level} are shown to the left of the plot.}
  \item{show.level.lines}{logical - if \code{TRUE} then a horizontal line 
    is drawn for each unique value of \code{level}.}
  \item{xaxt}{a character that specifies the type of the x axis.}
  \item{yaxt}{a character that specifies the type of the y axis.}
  \item{xlab}{title of the x axis.}
  \item{ylab}{title of the y axis.}
  \item{frame.plot}{logical - if \code{TRUE} then a border is drawn around 
    the plot.}
  \item{ylim}{limits of the y axis} 
  \item{\dots}{other values to \code{PlotNPS}.}
}

\details{If \code{round.levels.to.nearest} is greater than 0, values in 
\code{level} are rounded to the nearest \code{round.levels.to.nearest}. 
Rounded values are used by the \code{x.layout} and \code{y.layout} engines.

If \code{x.layout} is 'skinny' then nodes are spaced one x unit apart and 
\code{max.nodes.per.row} is ignored. If \code{x.layout} is 'narrow', 
nodes are spaced one x unit apart if fewer than \code{max.nodes.per.row} on 
that row, otherwise nodes are squashed in to the available x space. If 
\code{x.layout} is 'wide', nodes are spaced widely.

If \code{y.layout} is 'compress', then nodes are always shown at the values in 
\code{level}. If \code{y.layout} is 'stagger' and there are more than 
\code{max.nodes.per.row} on a \code{level} then the plotted levels are 
staggered by the values in \code{stagger}. 
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{PlotBSpectrum}}, 
         \code{\link{PlotCircularWeb}}, 
         \code{\link{PlotNPS}}, 
         \code{\link{PlotNPSDistribution}}, 
         \code{\link{PlotNSpectrum}}, 
         \code{\link{PlotRankNPS}}, 
         \code{\link{PlotTLPS}}
}

\examples{
# Compare prey-averaged and chain-averaged trophic level
data(TL84)
par(mfrow=c(1,2))
PlotWebByLevel(TL84, ylim=c(1,5.8), main='Prey-averaged')
PlotWebByLevel(TL84, ylim=c(1,5.8), level='ChainAveragedTrophicLevel', 
               main='Chain-averaged')

# Compare the three different x layouts
par(mfrow=c(1,3))
for(x.layout in c('skinny', 'narrow', 'wide'))
{
    PlotWebByLevel(TL84, x.layout=x.layout, main=x.layout)
}

# Compare the effect of round levels before plotting
# Different x-spacing of the four nodes around level 3
par(mfrow=c(1,2))
PlotWebByLevel(TL84, round.levels.to.nearest=0.2)
PlotWebByLevel(TL84, round.levels.to.nearest=0)

# Compare the effect of staggering levels
# Primary producers are staggered in the second plot
par(mfrow=c(1,2))
# No staggering - stagger and max.nodes.per.row are ignored
PlotWebByLevel(TL84, y.layout='compress')
# Stagger
PlotWebByLevel(TL84, y.layout='stagger', stagger=0.1, 
               max.nodes.per.row=20)
}
\keyword{hplot}

