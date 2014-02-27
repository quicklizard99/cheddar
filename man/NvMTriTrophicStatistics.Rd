\name{NvMTriTrophicStatistics}
\alias{NvMTriTrophicStatistics}
\title{N-versus-M tri-trophic statistics}
\description{Tri-trophic statistics.}
\usage{NvMTriTrophicStatistics(community)}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\details{Tri-trophic statistics as described by Cohen et al 2009 PNAS.
}

\value{A \code{list} containing
\item{links}{a \code{data.frame}.}
\item{three.node.chains}{a \code{data.frame}.}
\item{trophic.chains}{a \code{data.frame}.}
}

\references{Cohen, J.E. and Schittler, D.N. and Raffaelli, D.G. and Reuman, 
D.C. (2009) Food webs are more than the sum of their tritrophic parts. 
Proceedings of the National Academy of Sciences of the United States of America 
\bold{106}, 52, 22335--22340.}

\author{Lawrence Hudson}

\seealso{\code{\link{TLPS}}, 
         \code{\link{ThreeNodeChains}}, 
         \code{\link{TrophicChains}}, 
         \code{\link{PlotAuppervAlower}},
         \code{\link{NvMTriTrophicTable}}}

\examples{
data(TL84)
tts <- NvMTriTrophicStatistics(TL84)
nrow(tts$links)
head(tts$links)

nrow(tts$three.node.chains)
head(tts$three.node.chains)

nrow(tts$trophic.chains)
head(tts$trophic.chains)
}
\keyword{utilities}

