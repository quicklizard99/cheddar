\name{AggregateHelpers}
\alias{AggregateHelpers}
\alias{MaxNaRm}
\alias{MeanNaRm}
\alias{MedianNaRm}
\alias{MinNaRm}
\alias{SumNaRm}
\alias{JoinUnique}
\alias{ExpectSingleUnique}
\alias{ExpectSingleUniqueEmptyRm}
\title{Aggregate helpers}
\description{Helper functions that govern how nodes and communities are 
aggregated.}

\usage{
MeanNaRm(x)
MedianNaRm(x)
SumNaRm(x)
MinNaRm(x)
MaxNaRm(x)

JoinUnique(x)
ExpectSingleUnique(x)
ExpectSingleUniqueEmptyRm(x)
}

\arguments{
  \item{x}{Values.}
}

\details{\code{MeanNaRm}, \code{MedianNaRm}, \code{SumNaRm}, \code{MinNaRm} 
and \code{MaxNaRm} are wrappers around the relevant R function with \code{NA} 
values removed.

\code{JoinUnique} pastes together unique values in \code{x}, separated by a 
comma. \code{ExpectSingleUnique} throws an errror if there is more than one 
unique value in \code{x}. \code{ExpectSingleUniqueEmptyRm} is the same as 
code{ExpectSingleUnique} but empty values are ignored.
}

\value{Either a single number or character string.}

\author{Lawrence Hudson}

\seealso{\code{\link{LumpNodes}},
         \code{\link{AggregateCommunities}},
         \code{\link{AggregateCommunitiesBy}}}

\keyword{utilities}

