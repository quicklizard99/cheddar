\name{cheddar-package}
\alias{cheddar-package}
\alias{cheddar}
\docType{package}
\title{Analysis and visualisation of ecological communities}
\description{
Cheddar provides a flexible, extendable representation of an ecological 
community and a range of functions for analysis and visualisation, focusing 
on food web, body mass and numerical abundance data. It also allows inter-web 
comparisons such as examining changes in community structure over 
environmental, temporal or spatial gradients.
}

\details{
    \tabular{ll}{
    Package: \tab cheddar\cr
    Type: \tab Package\cr
    Version: \tab 0.1-609\cr
    Date: \tab 2012-06-06\cr
    License: \tab BSD\cr
    }
}

\author{Maintainer: Lawrence Hudson <lawrence.hudson08@imperial.ac.uk>}

%\references{}

\keyword{ package }

\examples{
# The dataset of Tuesday Lake sampled in 1984
data(TL84)

# Properties of the community
CPS(TL84)

# Properties of each node
head(NPS(TL84))

# Some computed node properties
head(NPS(TL84, c('Log10M', 'Log10N', 'Log10Biomass', 
                 TL='PreyAveragedTrophicLevel', TS='TrophicSpecies')))

# Properties of each trophic link
head(TLPS(TL84))

# Computed properties of each node in each link
head(TLPS(TL84, node.properties=c('Log10M', 'Log10N', 'Log10Biomass', 
                                  'PreyAveragedTrophicLevel')))


# A collection of 10 webs sampled across a wide pH gradient
data(pHWebs)

# A data.frame of predictors and responses
CollectionCPS(pHWebs, c('pH',
                        S='NumberOfNodes',
                        L='NumberOfTrophicLinks', 
                        C='DirectedConnectance',
                        Slope='NvMSlope'))
}
