# Trophic chains.
is.Chains <- function(x)
{
    return ('Chains' %in% class(x))
}

.Chains <- function(chains, community, node.properties=NULL, 
                    chain.properties=NULL)
{
    # Derives from data.frame. 
    # Could be a full-blown class with rbind, '[' etc but I've not 
    # had time to think about this. 

    # An attribute 'link.cols' holds column names that are links in chains.

    # Other columns are properties either of nodes in the chain or of the 
    # entire chain.
    stopifnot(is.data.frame(chains))
    self <- chains

    # Functions in chain.properties should be passed a Chains object
    class(self) <- c('Chains', class(self))
    attr(self, 'link.cols') <- colnames(chains)

    cp <- NULL
    if(!is.null(chain.properties))
    {
        cp <- lapply(chain.properties, function(p) 
              {
                  do.call(p, args=list(community=community, chains=self))
              })

        names(cp) <- chain.properties
        cp <- do.call('cbind', cp)
        self <- cbind(self, cp)
    }

    # Add node properties
    np <- .NodePropertiesOfChains(community, chains=chains, 
                                  node.properties=node.properties)
    if(!is.null(np))
    {
        self <- cbind(self, np)
    }

    class(self) <- c('Chains', class(self))
    attr(self, 'link.cols') <- colnames(chains)
    return (self)
}

.ChainsLinkColumns <- function(chains)
{
    stopifnot(is.Chains(chains))
    return (chains[,attr(chains, 'link.cols')])
}


.TrophicChainsSize <- function(community)
{
    # Returns two integers: n.chains, longest

    if(!is.Community(community)) stop('Not a Community')

    # Get the food web as an adjancency list
    alist <- .CAdjacencyList(community, ConsumersByNode(community))

    # 1 if basal, 0 otherwise
    is.basal <- as.integer(IsBasalNode(community))

    # Outputs to be filled by the C function
    n.chains <- as.integer(0)
    longest <- as.integer(0)
    status <- as.integer(-1)

    res <- .C('trophic_chains_size', 
              as.integer(alist), 
              as.integer(length(alist)), 
              as.integer(is.basal), 
              as.integer(nrow(alist)), 
              as.integer(0),
              n.chains=n.chains,
              longest=longest,
              status=status, 
              PACKAGE='cheddar', NAOK=TRUE, DUP=FALSE)

    if(-1==res$status)
    {
        stop('Unexpected error')
    }
    else if(0==res$status)
    {
        stopifnot((0==n.chains && 0==longest) || (0<n.chains && 0<longest))
        stopifnot(longest <= NumberOfNodes(community))
        return (c(n.chains=n.chains, longest=longest))
    }
    else if(1==res$status)
    {
        stop('Problem with an input parameter')
    }
    else
    {
        stop(paste('Unknown status [', res$status, ']', sep=''))
    }
}

TrophicChainsStats <- function(community)
{
    if(!is.Community(community)) stop('Not a Community')

    # Get the food web as an adjancency list
    alist <- .CAdjacencyList(community, ConsumersByNode(community))

    # 1 if basal, 0 otherwise
    is.basal <- as.integer(IsBasalNode(community))

    # Outputs to be filled by the C function
    # Compute the number of chains and the longest chain
    chains_size <- .TrophicChainsSize(community)
    n.chains <- chains_size['n.chains']
    longest <- chains_size['longest']
    if(n.chains>0)
    {
        n <- NumberOfNodes(community)
        node.pos.counts <- as.integer(rep(NA, n*longest))
        chain.lengths <- as.integer(rep(NA, n.chains))
        status <- as.integer(-1)

        res <- .C('trophic_chains_stats', 
                  as.integer(alist), 
                  as.integer(length(alist)), 
                  as.integer(is.basal), 
                  as.integer(n),
                  as.integer(n.chains),
                  as.integer(longest), 
                  node.pos.counts=node.pos.counts,
                  chain.lengths=chain.lengths,
                  status=status, 
                  PACKAGE='cheddar', NAOK=TRUE, DUP=FALSE)

        if(-1==res$status)
        {
            stop('Unexpected error')
        }
        else if(0==res$status)
        {
            dim(node.pos.counts) <- c(longest, n)
            node.pos.counts <- t(node.pos.counts)
            rownames(node.pos.counts) <- unname(NP(community, 'node'))
            return (list(chain.lengths=chain.lengths, 
                         node.pos.counts=node.pos.counts))
                         
        }
        else if(1==res$status)
        {
            stop('Problem with an input parameter')
        }
        else
        {
            stop(paste('Unknown status [', res$status, ']', sep=''))
        }
    }
    else
    {
        return (NULL)
    }
}

TrophicChains <- function(community, node.properties=NULL, 
                          chain.properties=NULL)
{
    # Returns an object of class Chains. 
    # One row per unique trophic chain in the food web. 
    # Shorter chains are suffixed with ''. 
    # Cannibalism and loops are ignored, i.e. each node appears no more 
    # than once in a chain.
    # Isolated nodes are excluded so all chains are of length 2 or greater.

    if(!is.Community(community)) stop('Not a Community')

    community <- RemoveCannibalisticLinks(community)
    if(0==NumberOfTrophicLinks(community))
    {
        return (NULL)
    }

    # Delegate to the C++ implementation.
    #    On iguana, ubuntu 10.04, R 2.14.1 (2011-12-22)
    #    library(cheddar)
    #    data(Benguela, BroadstoneStream, TL84, TL86, SkipwithPond,YthanEstuary)
    #    for(community in list(Benguela, BroadstoneStream, TL84, TL86, 
    #                          YthanEstuary))
    #    {
    #        print(community)
    #        print(system.time(chains <- TrophicChains(community)))
    #    }
    #   print(SkipwithPond)
    #   print(system.time(chains <- TrophicChains(SkipwithPond,max.chains=3e6)))

    # C++ implementation with dynamic queue
    #    Benguela containing 29 nodes.
    #       user  system elapsed 
    #      0.108   0.036   0.147 
    #    Broadstone Stream containing 37 nodes.
    #       user  system elapsed 
    #      0.088   0.048   0.136 
    #    Tuesday Lake sampled in 1984 containing 56 nodes.
    #       user  system elapsed 
    #      0.132   0.068   0.199 
    #    Tuesday Lake sampled in 1986 containing 57 nodes.
    #       user  system elapsed 
    #      0.124   0.076   0.197 
    #    Ythan Estuary containing 92 nodes.
    #       user  system elapsed 
    #      0.221   0.100   0.320 
    #    Skipwith Pond containing 37 nodes.
    #       user  system elapsed 
    #     12.608   3.436  16.345 

    # Old C implementation with fixed-size queue
    #    Tuesday Lake sampled in 1984 containing 56 nodes.
    #       user  system elapsed 
    #      0.092   0.024   0.115 
    #    Tuesday Lake sampled in 1986 containing 57 nodes.
    #       user  system elapsed 
    #      0.084   0.024   0.109 
    #    Ythan Estuary containing 92 nodes.
    #       user  system elapsed 
    #      0.112   0.052   0.168 
    #    Benguela containing 29 nodes.
    #       user  system elapsed 
    #      0.064   0.024   0.086 

    # Get the food web as an adjancency list
    alist <- .CAdjacencyList(community, ConsumersByNode(community))

    # 1 if basal, 0 otherwise
    is.basal <- as.integer(IsBasalNode(community))

    # Outputs
    # Compute the number of chains and the longest chain
    chains_size <- .TrophicChainsSize(community)
    ncols <- chains_size['n.chains']
    nrows <- chains_size['longest']

    # chains will be filled by the C function
    chains <- as.integer(rep(NA, ncols*nrows))
    status <- as.integer(-1)

    res <- .C('trophic_chains', 
              as.integer(alist), 
              as.integer(length(alist)), 
              as.integer(is.basal), 
              as.integer(nrow(alist)), 
              as.integer(ncols), 
              as.integer(nrows), 
              chains=chains, 
              status=status, 
              PACKAGE='cheddar', NAOK=TRUE, DUP=FALSE)

    if(-1==res$status)
    {
        stop('Unexpected error')
    }
    else if(0==res$status)
    {
        # Take the subset of the matrix that we are interested in
        stopifnot(res$max.chain.length!=chains_size['longest'])
        stopifnot(res$n.chains.found!=chains_size['n.chains'])

        chains <- res$chains
        dim(chains) <- c(nrows, ncols)

        # 1-indexed
        chains <- 1+chains

        # Prefer tall and narrow
        chains <- t(chains)
    }
    else if(1==res$status)
    {
        stop('Problem with an input parameter')
    }
    else if(2==res$status)
    {
        stop(paste('Limit of', ncols, 'chains exceeded'))
    }
    else
    {
        stop(paste('Unknown status [', res$status, ']', sep=''))
    }

    # Get chains as node names
    chains.dim <- dim(chains)
    chains <- unname(NP(community, 'node'))[chains]
    chains[is.na(chains)] <- ''
    dim(chains) <- chains.dim

    # Add chain properties
    colnames(chains) <- paste('Node', 1:ncol(chains), sep='.')
    chains <- as.data.frame(chains, stringsAsFactors=FALSE)
    return (.Chains(chains, community, node.properties, chain.properties))
}

ChainLength <- function(chains)
{
    # Vector of length nrow(chains).
    if(is.null(chains))
    {
        return (NULL)
    }
    else
    {
        stopifnot(is.Chains(chains))
        return (apply(.ChainsLinkColumns(chains), 1, 
                       function(r) length(which(''!=r))-1))
    }
}

.ReverseChains <- function(chains)
{
    # Reverses chains. Chain properties discarded.
    stopifnot(is.Chains(chains))

    chains <- .ChainsLinkColumns(chains)
    rchains <- matrix('', nrow=nrow(chains), ncol=ncol(chains))
    colnames(rchains) <- rev(colnames(chains))
    chains <- as.matrix(chains)
    for(row in 1:nrow(chains))
    {
        cols <- max(which(''!=chains[row,]))
        rchains[row, 1:cols] <- chains[row,cols:1]
    }
    return (.Chains(as.data.frame(rchains, stringsAsFactors=FALSE), 
                    attr(chains, 'community')))
}

ThreeNodeChains <- function(community, exclude.loops=FALSE, 
                            node.properties=NULL, chain.properties=NULL)
{
    # Returns a matrix of 3 cols and a row per tri-trophic chain
    # Each node appears no more than once in a chain except for the 
    # case where a chain goes R > C > R, i.e. bottom==top. 
    # Setting exclude.loops=TRUE excludes these links
    # Cannibalism is ignored
    if(!is.Community(community)) stop('Not a Community')
    if(0==NumberOfTrophicLinks(community))
    {
        return (NULL)
    }

    # Get consumers of each node as indices
    consumers <- ConsumersByNode(community)
    chains <- NULL
    for(bottom in NP(community, 'node'))
    {
        for(intermediate in consumers[[bottom]])
        {
            top <- consumers[[intermediate]]
            if(length(top)>0)
            {
                new.chains <- cbind(bottom, intermediate, top)

                # Remove rows with duplicated node
                dup <- new.chains[,1] == new.chains[,2] | 
                       new.chains[,2] == new.chains[,3]
                if(any(dup))
                {
                    new.chains <- new.chains[!dup,,drop=FALSE]
                }

                if(nrow(new.chains)>0)
                {
                    chains <- rbind(chains, new.chains)
                }
            }
        }
    }

    if(is.null(chains))
    {
        return (NULL)
    }
    else
    {
        if(exclude.loops)
        {
            # Remove chains for which bottom==top
            # Chains with bottom==intermediate or intermediate==top were never 
            # added.
            chains <- chains[!apply(chains, 1, function(r) r[1]==r[3]),,
                             drop=FALSE]
        }

        chains <- as.data.frame(chains, stringsAsFactors=FALSE)
        return (.Chains(chains, community, node.properties, chain.properties))
    }
}

.NodePropertiesOfChains <- function(community, chains, node.properties=NULL, 
                                    chain.columns=colnames(chains))
{
    # Adds properties
    # chains - a data.frame
    # node.properties
    # chain.columns - 

    res <- NULL

    if(!is.null(node.properties))
    {
        bad <- !chain.columns %in% colnames(chains)
        if(any(bad))
        {
            stop(paste('The names [', paste(chain.columns[bad], collapse=','), 
                       '] are not columns of chains', sep=''))
        }

        np <- NPS(community, node.properties)

        if(is.null(colnames(chains)))
        {
            chain.columns <- 1:ncol(chains)
        }
    
        for(column in chain.columns)
        {
            v <- lapply(colnames(np), 
                        function(p) np[chains[,column], p, drop=FALSE])
            v <- data.frame(v, stringsAsFactors=FALSE)
            colnames(v) <- paste(column, colnames(v), sep='.')
            if(is.null(res))    res <- v
            else                res <- cbind(res, v)
        }

        rownames(res) <- NULL
    }

    return (res)
}

