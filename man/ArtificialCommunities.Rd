\name{ArtificialCommunities}
\alias{ArtificialCommunities}
\alias{CommunityFactory}
\alias{CommunitiesLike}
\alias{NicheModelLinks}
\alias{RandomLinks}
\title{Artificial communities}
\description{Functions for creating artificial communities.}

\usage{
CommunityFactory(S, node=paste('Node', 1:S), generator = NicheModelLinks, 
                 n = 1, accept = NULL, energetically.feasible = TRUE, 
                 trace.progress = FALSE, ...)

CommunitiesLike(community, ...)

RandomLinks(pool, n, C = 0.15)
NicheModelLinks(pool, n, C = 0.15, niche.positions = NULL, 
                probabilistic = FALSE)
}

\arguments{
  \item{S}{numeric - the number of nodes that the generated communities will 
    contain.}
  \item{node}{character - the names of the nodes in the generated communities; 
    must be of length \code{S}.}
  \item{generator}{the function used to generate sets of trophic links.}
  \item{n}{numeric - the number of communities to generate.}
  \item{accept}{if not \code{NULL}, a that takes a community as its only 
    argument and returns a bool.}
  \item{energetically.feasible}{logical - if \code{TRUE} only communities 
    that are energetically feasible are returned.}
  \item{trace.progress}{logical - if \code{TRUE} the function prints feedback.}
  \item{community}{an object of class \code{Community}.}
  \item{...}{other values to functions that create trophic links.}
  \item{pool}{character - the names of the nodes in the generated communities.}
  \item{C}{numeric - the directed connectance of the generated communities.}
  \item{niche.positions}{numeric - each species' position on the niche axis; 
    must be of length \code{S} and $0<=niche.positions<=1$.}
  \item{probabilistic}{logical - if \code{TRUE} then links are generated using 
    the probabilistic niche model.}
}

\details{\code{CommunityFactory} is used to create collections of artificially 
generated communities using a food-web structural model, which is given by 
\code{generator}. Two such functions are \code{RandomLinks} and 
\code{NicheModelLinks}. If \code{energetically.feasible} is \code{TRUE} 
then those communities that are not energetically feasible according to 
\code{PreyAveragedTrophicLevel} are discarded.

\code{CommunitiesLike} uses \code{CommunityFactory} to generate communities 
that are similar to an existing \code{Community}. The new communities will have 
the same number of nodes, the same node names and the same or a similar number 
of trophic links, depending on how the trophic links are generated.

\code{RandomLinks} returns a \code{data.frame} containing exaclty $CSS$ trophic 
links. Each possible trophic link has an equal chance of being drawn.

\code{NicheModelLinks} implements the niche model of Williams and Martinez. 
The niche model is stochastic so the returned communities are not guaranteed 
to contain exactly $CSS$ trophic links. If provided, \code{niche.positions} 
gives the location of each node on the niche axis, defined by Williams and 
Martinez as the vector $n$. If \code{niche.positions} is \code{NULL} then a set 
of uniformly distributed niche positions is created for each of the \code{n} 
communities that are created. If \code{probabilistic} is \code{TRUE} then the 
links are generated using the probabilistic niche model described by Williams 
et al 2010.

You should never need to call \code{RandomLinks} or \code{NicheModelLinks} 
directly - just pass them as arguments to \code{CommunityFactory} or 
\code{CommunitiesLike}.

TODO - writing your own trophic links functions
}

\value{
\code{CommunityFactory} and \code{CommunitiesLike} return a 
\code{CommunityCollection}. 

\code{RandomLinks} and \code{NicheModelLinks} return a \code{data.frame} 
containing columns `resource' and `consumer'.
}

\references{Williams, R.J. and Martinez, N.D. (2000) Nature \bold{404}, 6447, 
180--183.

Williams, R.J. and Anandanadesan, A. and  Purves, D.W. (2010) PLoS One 
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
res <- CommunitiesLike(TL84, n=100, trace.progress=TRUE)
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
res <- CommunitiesLike(TL84, n=100, trace.progress=TRUE, 
                       niche.positions=niche.positions)

# The probabilistic niche model
res <- CommunitiesLike(TL84, n=100, trace.progress=TRUE, probabilistic=TRUE)

# Williams et al (2010) described the allometric niche model as being the 
# probabilistic niche model with niche positions computed from log-transformed 
# body masses scaled to be between 0 and 1
logM <- Log10M(TL84)
niche.positions <- (logM-min(logM)) / diff(range(logM))
#niche.positions <- sort(niche.positions)
res <- CommunitiesLike(TL84, n=100, trace.progress=TRUE, probabilistic=TRUE, 
                       niche.positions=niche.positions)
}

\keyword{utilities}
