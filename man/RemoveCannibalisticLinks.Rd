\name{RemoveCannibalisticLinks}
\alias{RemoveCannibalisticLinks}
\title{Remove cannibalistic trophic links}
\description{Remove cannibalistic trophic links.}
\usage{RemoveCannibalisticLinks(community, title)}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{title}{a title for the new community.}
}

\details{Returns a new \code{Community} with any cannibalistic trophic links 
removed.}

\value{A new object of class \code{Community}.}
\author{Lawrence Hudson}

\seealso{\code{\link{Community}}}

\examples{
data(TL84)
NumberOfTrophicLinks(TL84)

TL84.no.cannibal <- RemoveCannibalisticLinks(TL84)
NumberOfTrophicLinks(TL84.no.cannibal)
}

\keyword{utilities}
