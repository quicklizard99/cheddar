\name{Community has property?}
\alias{HasM}
\alias{HasN}
\alias{HasTrophicLinks}
\title{Community has property?}
\description{Functions that return whether or not a community has a 
particular property.}

\usage{
HasM(community)
HasN(community)
HasTrophicLinks(community)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
}

\value{A \code{logical}.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{CPS}}, \code{\link{NPS}}, 
         \code{\link{TLPS}}}

\examples{
# Tuesday Lake 1984 has all three
data(TL84)
HasM(TL84)
HasN(TL84)
HasTrophicLinks(TL84)

# Skipwith Pond has trophic links but not M or N
data(SkipwithPond)
HasM(SkipwithPond)
HasN(SkipwithPond)
HasTrophicLinks(SkipwithPond)
}
\keyword{utilities} 
