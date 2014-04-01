\name{NvMTriTrophicTable}
\alias{NvMTriTrophicTable}
\title{N-versus-M tri-trophic statistics}
\description{Tri-trophic statistics.}
\usage{NvMTriTrophicTable(collection)}

\arguments{
  \item{collection}{an object of class \code{CommunityCollection}.}
}

\details{Returns a \code{data.frame} that contains the same statistics 
  presented in Table 1 on Cohen et al 2009 PNAS. The function removes 
  nodes lacking body mass (M) and/or numerical abundance (N), cannibalistic 
  links and isolated nodes from each community. The last eight rows of the 
  table contain four network statistics both with and without these removals.
}

\references{Cohen, J.E. and Schittler, D.N. and Raffaelli, D.G. and Reuman, 
D.C. (2009) Food webs are more than the sum of their tritrophic parts. 
Proceedings of the National Academy of Sciences of the United States of America 
\bold{106}, 52, 22335--22340.}

\value{A \code{data.frame} with a column per community and the rows
  \item{Mean link length}{}
  \item{Mean L upper}{}
  \item{Mean L lower}{}
  \item{2 x mean link length}{}
  \item{Mean 2-span}{}
  \item{Mean L upper + L lower}{}
  \item{2 x mean link length / mean 2-span}{}
  \item{Mean L upper + L lower/ mean 2-span}{}
  \item{Mean count chain length}{}
  \item{Mean count chain length x mean link length}{}
  \item{Community span}{}
  \item{Mean count chain length x mean link length / community span}{}
  \item{Mean sum chain lengths}{}
  \item{Mean chain span}{}
  \item{Mean chain span / community span}{}
  \item{Mean sum chain lengths / mean chain span}{}
  \item{Mean sum chain lengths / community span}{}
  \item{L}{number of trophic links after removals.}
  \item{S^2}{number of nodes squared after removals.}
  \item{L/S^2}{directed connectance links after removals.}
  \item{L/S}{linkage density after removals.}
  \item{L}{number of trophic links before removals.}
  \item{S^2}{number of nodes squared before removals.}
  \item{L/S^2}{directed connectance links before removals.}
  \item{L/S}{linkage density before removals.}
}

\author{Lawrence Hudson}

\seealso{\code{\link{NvMTriTrophicStatistics}}, \code{\link{CommunityCollection}}}

\examples{
data(TL84, TL86, YthanEstuary)
collection <- CommunityCollection(list(TL84, TL86, YthanEstuary))
table <- NvMTriTrophicTable(collection)
print(round(table, 2))
}
\keyword{utilities}
