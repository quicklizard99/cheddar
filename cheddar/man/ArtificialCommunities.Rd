\name{ArtificialCommunities}
\alias{ArtificialCommunities}
\alias{CommunityFactory}
\alias{NicheModelLinks}
\alias{RandomLinks}
\alias{CascadeModelLinks}
\title{Artificial communities}
\description{Functions for creating artificial communities.}

\usage{
CommunityFactory(S, nodes, generator = NicheModelLinks, 
                 n = 1, accept = NULL, energetically.feasible = TRUE,
                 trace.progress = FALSE, validate=TRUE, properties=NULL, ...)

RandomLinks(pool, n, C = 0.15, nodes)
CascadeModelLinks(pool, n, C = 0.15, nodes)
NicheModelLinks(pool, n, C = 0.15, niche.positions = NULL, 
                probabilistic = FALSE, nodes)
}

\arguments{
  \item{S}{numeric - the number of nodes in the generated communities; nodes
    will be named Node 1...Node S}
  \item{nodes}{character -  either node names or a data.frame of node
    properties; either \code{S} or \code{nodes} should be provided.}
  \item{generator}{the function used to generate sets of trophic links.}
  \item{n}{numeric - the number of communities to generate.}
  \item{accept}{either \code{NULL} or a function that takes a \code{Community}
    object as its only argument and returns a \code{logical}; if not \code{NULL}
    then only communities for which the function returns \code{TRUE} will be
    returned.}
  \item{energetically.feasible}{logical - if \code{TRUE}, then then only
    communities that are energetically feasible according to the
    \code{PreyAveragedTrophicLevel} function will be returned.}
  \item{trace.progress}{logical - if \code{TRUE} the function prints feedback.}
  \item{validate}{if \code{TRUE}, created communities are validated using the
    checks provided by the \code{Community} function.}
  \item{properties}{list - the \code{properties} argument of the
    \code{Community} function.}
  \item{...}{other values to functions that create trophic links.}
  \item{pool}{character - the pool of node names.}
  \item{C}{numeric - the target directed connectance of the generated 
    communities.}
  \item{niche.positions}{numeric - each species' position on the niche axis; 
    must be of length \code{S}; $0<=niche.positions<=1$.}
  \item{probabilistic}{logical - if \code{TRUE} then links are generated using 
    the probabilistic niche model.}
}

\details{\code{CommunityFactory} is used to create collections of artificially 
generated communities using a food-web structural model, which is given by 
\code{generator}. Two such functions are \code{RandomLinks} and 
\code{NicheModelLinks}. If \code{energetically.feasible} is \code{TRUE} 
then those communities that are not energetically feasible according to 
\code{PreyAveragedTrophicLevel} are discarded.

\code{RandomLinks} returns a \code{data.frame} containing exactly $CSS$ trophic 
links. Each possible trophic link has an equal chance of being drawn. The 
target directed connectance, \code{C}, must satisfy the condition $0<C<1$. 

\code{CascadeModelLinks} implements the cascade model of Cohen and Newman. The 
target directed connectance, \code{C}, must satisfy the condition $0<C<0.5$. 
The cascade model is stochastic so the returned communities are not guaranteed 
to contain exactly $CSS$ trophic links.

\code{NicheModelLinks} implements the niche model of Williams and Martinez. 
The target directed connectance, \code{C}, must satisfy the condition $0<C<0.5$. 
The niche model is stochastic so the returned communities are not guaranteed 
to contain exactly $CSS$ trophic links. If provided, \code{niche.positions} 
gives the location of each node on the niche axis, defined by Williams and 
Martinez as the vector $n$. If \code{niche.positions} is \code{NULL} then a set 
of uniformly distributed niche positions is created for each of the \code{n} 
communities that are created. If \code{probabilistic} is \code{TRUE} then the 
links are generated using the probabilistic niche model described by Williams 
et al 2010.

You will probably never has a reason to call \code{RandomLinks},
\code{CascadeModelLinks} or \code{NicheModelLinks} directly - just pass them as
arguments to \code{CommunityFactory}.

TODO LH - writing your own trophic links functions.
}

\value{
\code{CommunityFactory} returns a \code{CommunityCollection}. 

\code{RandomLinks}, \code{CascadeModelLinks}, \code{NicheModelLinks} return a
\code{data.frame} containing columns `resource' and `consumer'.
}

\references{
Cohen, J.E. and Newman, C.M. (1985) A stochastic theory of community food 
webs. I. Models and aggregated data. Proceedings of the Royal Society of London 
Series B \bold{224}, 421--448.

Williams, R.J. and Martinez, N.D. (2000) Nature \bold{404}, 6447, 
180--183.

Williams, R.J. and Anandanadesan, A. and Purves, D.W. (2010) PLoS One 
\bold{5}, 8, e12092.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{CommunityCollection}}, 
         \code{\link{PreyAveragedTrophicLevel}}, 
         \code{\link{DirectedConnectance}}, \code{\link{NumberOfNodes}}, 
         \code{\link{NumberOfTrophicLinks}}, \code{\link{data.frame}}}

\examples{
# 50 communities with 10 nodes and 15 randomly assigned trophic links
res <- CommunityFactory(10, generator=RandomLinks, n=50, C=0.15)
length(res)    # 50
summary(sapply(res, NumberOfNodes))    # All 10
summary(sapply(res, NumberOfTrophicLinks))    # All 15
summary(sapply(res, DirectedConnectance))    # All 0.15

# 100 communities generated using the niche model, with properties similar to 
# TL84
data(TL84)
res <- CommunityFactory(nodes=NPS(TL84),
                        C=DirectedConnectance(TL84),
                        properties=CPS(TL84),
                        trace.progress=TRUE,
                        n=100)
length(res)    # 100
summary(sapply(res, NumberOfNodes))    # All 56

# The niche model is stochastic so the number of trophic links is variable
summary(sapply(res, NumberOfTrophicLinks))
hist(sapply(res, NumberOfTrophicLinks))
abline(v=mean(sapply(res, NumberOfTrophicLinks)))
abline(v=NumberOfTrophicLinks(TL84), col=2)

summary(sapply(res, DirectedConnectance))
hist(sapply(res, DirectedConnectance))
abline(v=mean(sapply(res, DirectedConnectance)))
abline(v=DirectedConnectance(TL84), col=2)

# 100 communities similar to TL84 using the order of body masses as the niche
# positions
niche.positions <- order(NP(TL84, 'M'))/NumberOfNodes(TL84)
res <- CommunityFactory(nodes=NPS(TL84),
                        C=DirectedConnectance(TL84),
                        properties=CPS(TL84),
                        trace.progress=TRUE,
                        niche.positions=niche.positions,
                        n=100)

# Using the probabilistic niche model
res <- CommunityFactory(nodes=NPS(TL84),
                        C=DirectedConnectance(TL84),
                        properties=CPS(TL84),
                        trace.progress=TRUE,
                        probabilistic=TRUE,
                        n=100)

# Williams et al (2010) described the allometric niche model as being the 
# probabilistic niche model with niche positions computed from log-transformed 
# body masses scaled to be between 0 and 1
logM <- Log10M(TL84)
niche.positions <- (logM-min(logM)) / diff(range(logM))
res <- CommunityFactory(nodes=NPS(TL84),
                        C=DirectedConnectance(TL84),
                        properties=CPS(TL84),
                        trace.progress=TRUE,
                        probabilistic=TRUE,
                        niche.positions=niche.positions,
                        n=100)
}

\keyword{utilities}
