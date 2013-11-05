\name{QuantitativeDescriptors}
\alias{QuantitativeDescriptors}
\alias{NodeQuantitativeDescriptors}
\title{Quantitative descriptors}
\description{Quantitative descriptors after Bersier at al Ecology 2002.}

\usage{
NodeQuantitativeDescriptors(community, weight)
QuantitativeDescriptors(community, weight, top.level.threshold=0.99)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{weight}{the name of a tropic-link property with which quantitative 
  descriptors should be computed. It can be the name of a first-class property 
  (returned by \code{TrophicLinkPropertyNames}) or the name of a function that 
  takes a \code{Community} object as the only parameter and a vector of length 
  \code{NumberOfTrophicLinks}.}
  \item{top.level.threshold}{TODO}
}

\details{TODO Overview of the descriptors and why they are useful.
Warning about performance. 

NodeQuantitativeDescriptors computes a table of node-level quantitative 
descripttors, as presented in Bersier et al 2002, Table 1. It returns a 
\code{matrix} with columns NResources, NConsumers, bIn, bOut, nN, nP, d.prime, 
d, o.prime, o, g.prime, g, v.prime, v.

QuantitativeDescriptors computes values presented in Bersier et al 2002 Table 
2. It returns a \code{matrix} with columns Qualitative, Unweighted and 
Weighted and rows Fraction top level, Fraction intermediate, Fraction basal, 
Ratio resources:consumers, Link density, Connectance, 
Fraction links top:intermediate, Fraction links top:basal, 
Fraction links intermediate:intermediate, Fraction links intermediate:basal, 
Mean chain length, Median chain length, SD chain length, Max chain length, 
Degree of omnivory, Generality, Vulnerability, SD standardised generality, 
SD standardised vulnerability.

TODO Warn that QuantitativeDescriptors not suitable for use with CollectionNPS. 
Have an example that computes QuantitativeDescriptors for several communities
}

\value{A \code{matrix}.
}

\references{Bersier, L. and Banasek-Richter, C. and Cattin, M. (2002) Ecology 
\bold{80} 2394--2407.
}

\author{Lawrence Hudson}

\seealso{\code{\link{TrophicLinkPropertyNames}}, 
         \code{\link{NumberOfTrophicLinks}}, \code{\link{NumberOfNodes}}}

\examples{
data(ChesapeakeBay)
QuantitativeDescriptors(ChesapeakeBay, 'biomass.flow')
NodeQuantitativeDescriptors(ChesapeakeBay, 'biomass.flow')
}

\keyword{utilities}
