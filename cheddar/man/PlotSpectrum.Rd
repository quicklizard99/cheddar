\name{Spectrum plots}
\alias{PlotBSpectrum}
\alias{PlotNSpectrum}
\title{Spectrum plots}
\description{High-level functions that plot the sum numerical abundance (N) or 
biomass abundance (B) in equally-spaced log10 body-mass bins.}

\usage{
PlotBSpectrum(community, 
              lower = min(NP(community, "M"), na.rm = TRUE), 
              upper = max(NP(community, "M"), na.rm = TRUE), 
              n.bins = 10, 
              main = CPS(community)$title, 
              xlab = Log10MLabel(community), 
              ylab = Log10BLabel(community), 
              xlim = NULL, 
              ylim = NULL, 
              pch = 19, 
              show.bin.limits = TRUE, 
              show.bin.centres = FALSE, 
              \dots)

PlotNSpectrum(community, 
              lower = min(NP(community, "M"), na.rm = TRUE), 
              upper = max(NP(community, "M"), na.rm = TRUE), 
              n.bins = 10, 
              main = CPS(community)$title, 
              xlab = Log10MLabel(community), 
              ylab = Log10NLabel(community), 
              xlim = NULL, 
              ylim = NULL, 
              pch = 19, 
              show.bin.limits = TRUE, 
              show.bin.centres = FALSE, 
              \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{lower}{lower bound of the bins.}
  \item{upper}{upper bound of the bins.}
  \item{n.bins}{the number of bins.}
  \item{main}{title of the plot}
  \item{xlab}{title of the x axis.}
  \item{ylab}{title of the y axis.}
  \item{xlim}{limits of the x axis.} 
  \item{ylim}{limits of the y axis.} 
  \item{pch}{plotting symbol.}
  \item{show.bin.limits}{logical - if \code{TRUE} the centres of the bins are 
    marked with a line.}
  \item{show.bin.centres}{logical - if \code{TRUE} the centres of the bins are 
    marked with a line.}
  \item{\dots}{other values to plot functions.}
}

\value{A \code{list}:
  \item{bins}{value returned by the \code{BodyMassBins} function.}
  \item{lm}{a linear regression fitted through the data.}
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{BodyMassBins}}, 
         \code{\link{PlotCircularWeb}}, 
         \code{\link{PlotNPS}}, 
         \code{\link{PlotNPSDistribution}}, 
         \code{\link{PlotRankNPS}}, 
         \code{\link{PlotTLPS}}, 
         \code{\link{PlotWebByLevel}}}

\examples{
data(TL84)
PlotNSpectrum(TL84)
PlotBSpectrum(TL84)
}
\keyword{hplot}

