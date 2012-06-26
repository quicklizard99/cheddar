% This file is called BodyMassB.Rd in order for the windows build to work.
% Both R 2.14.2 + Rtools 2.14 and R 2.15.1 + Rtools 2.15 on 32-bit XP and 
% 64-bit Windows 7 failed to detect a file called BodyMassBins.Rd. 
% Renaming the file to BodyMassB.Rd fixes the problem.
\name{BodyMassBins}
\alias{BodyMassBins}
\title{Body-mass bins}
\description{Function that assigns each node in a \code{Community} to a 
body-mass bin.}

\usage{
BodyMassBins(community, lower=min(NP(community,'M'), na.rm=TRUE), 
             upper=max(NP(community,'M'), na.rm=TRUE), n.bins=10) 
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{lower}{lower bound of the bins.}
  \item{upper}{upper bound of the bins.}
  \item{n.bins}{the number of bins.}
}

\details{Divides the range \code{lower} to \code{upper} in to \code{n.bins} 
equally-spaced log10(M) bins. Assigns each node in the 
\code{community} to one of these bins and returns the bins numbers. 
The returned vector has attributes \code{bin.centres} and \code{breaks}.
}

\value{A vector of length \code{NumberOfNodes}.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{NumberOfNodes}}}

\examples{
data(TL84)
BodyMassBins(TL84)
}

\keyword{utilities}

