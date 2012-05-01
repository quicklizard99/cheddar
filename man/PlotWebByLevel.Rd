\name{PlotWebByLevel}
\alias{PlotWebByLevel}
\title{Plot web by level}
\description{A high-level function for plotting a food-web by vertically with 
the lowest trophic-level nodes at the bottom.}
\usage{
PlotWebByLevel(community, 
               level='PreyAveragedTrophicLevel', 
               max.nodes.per.row=20,
               jitter.overlap=0.2,
               jitter.amount=0, 
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
  \item{max.nodes.per.row}{a number greater than 0.}
  \item{jitter.overlap}{a number greater than 0.}
  \item{jitter.amount}{a number greater or equal to 0 and less than 1.}
  \item{x.layout}{'skinny', 'narrow' or 'wide'.}
  \item{y.layout}{'stagger' or 'compress'. Only has an effect if 
    \code{jitter.overlap} is greater than 0.}
  \item{show.level.labels}{logical - if \code{TRUE} then values of \code{level} 
    are shown to the left of the plot.}
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
}
\keyword{hplot}

