\name{Intervality}
\alias{Intervality}
\alias{MinimiseSumDietGaps}
\alias{SumDietGaps}
\alias{MinimiseSumConsumerGaps}
\alias{SumConsumerGaps}
\title{Food web Intervality}
\description{Functions for computing the sum diet/consumer gaps of each 
species in a \code{Community} and for minimising the sum diet/consumer gaps 
using a simulated annealing learning method.}

\usage{
SumDietGaps(community)
SumConsumerGaps(community)

MinimiseSumDietGaps(community, T.start = 10, T.stop = 0.1, c = 0.9, 
                    swaps.per.T = 1000, trace.anneal = FALSE, n = 1, 
                    best = TRUE)
MinimiseSumConsumerGaps(community, T.start = 10, T.stop = 0.1, c = 0.9, 
                        swaps.per.T = 1000, trace.anneal = FALSE, n = 1, 
                    best = TRUE)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{T.start}{the temperature at which annealing starts; must be >0}
  \item{T.stop}{annealing will stop when the system temperature drops below 
    \code{T.stop}; must be >0 and <\code{T>start}}
  \item{c}{cooling coefficient; must be >0 and <1.}
  \item{swaps.per.T}{the number of predation matrix row swaps per temperature.}
  \item{trace.anneal}{logical - if \code{TRUE} the annealing process prints 
    feedback.}
  \item{n}{numeric - the number of repetitions of the minimisation procedure.}
  \item{best}{logical - if \code{TRUE} then only the result of the best of the 
  \code{n} minimsations is returned.}
}

\details{\code{SumDietGaps} and \code{SumConsumerGaps} return the total number 
of gaps in each species' diet (Stouffer et al 2006) and each species' consumers 
(Zook et al 2011) respectively.

\code{MinimiseSumDietGaps} and \code{MinimiseSumConsumerGaps} use the 
simulated annealing learning method described by Stouffer et al (2006) to 
minimise either \code{SumDietGaps} or \code{SumConsumerGaps}. Simulated 
annealing learning is a stochastic method so several optimisations might be 
required to find the global minimum. Use a value of \code{n} greater than 1 to 
perform several optimisations.}

\value{For \code{SumDietGaps} and \code{SumConsumerGaps}, a single number. 
For the two minimisation functions, if \code{n} is 1 or \code{best} is 
\code{TRUE}, a \code{list} containing the values
  \item{sum.gaps}{the lowest \code{SumDietGaps} or \code{SumConsumerGaps} 
    resulting from the best ordering that was found.}
  \item{order}{a vector of node names giving the best ordering.}
  \item{reordered}{\code{community} reordered by the best ordering.}
If \code{n} is greater than 1 and \code{best} is \code{FALSE} then a list of 
\code{n} lists, each list containing the above three values, sorted by 
increasing \code{sum.gaps}.
}

\references{Stouffer, D.B. and Camacho, J. and Amaral, L.A.N. (2006) 
Proceedings of the National Academy of Sciences of the United States of 
America \bold{103}, 50, 19015--19020.

Zook, A.E. and Eklof, A. and Jacob, U. and Allesina, S. (2011) 
Journal of Theoretical Biology \bold{271}, 1 106--113.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{OrderCommunity}}, 
         \code{\link{PredationMatrix}}, \code{\link{PlotPredationMatrix}}}

\examples{
data(TL84)
# Perform 5 independent optimisations
res <- MinimiseSumDietGaps(TL84, n=5)

# Compare the original, ordered by body mass and minimised predation matrices
par(mfrow=c(1,3))
PlotPredationMatrix(TL84, main=paste('Sum diet gap', SumDietGaps(TL84)))
TL84.by.M <- OrderCommunity(TL84, 'M')
PlotPredationMatrix(TL84.by.M, 
                    main=paste('Sum diet gap', SumDietGaps(TL84.by.M)))
PlotPredationMatrix(res$reordered, main=paste('Sum diet gap', res$sum.gaps))

# The same comparison but retaining the original column ordering
par(mfrow=c(1,3))
PlotPredationMatrix(TL84)
PlotPredationMatrix(TL84, resource.order=NP(TL84.by.M, 'node'))
PlotPredationMatrix(TL84, resource.order=res$order)
}

\keyword{utilities}
