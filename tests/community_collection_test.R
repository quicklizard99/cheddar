# Community collections
TestCommunityCollectionSingle <- function()
{
    # Test collections containing a single community
    for(community in list(c1,c2,c3,c4,c5,c6,TL84,TL86,SkipwithPond))
    {
        collection <- CommunityCollection(list(community))
        stopifnot(1==length(collection))
        stopifnot(CP(community,'title')==names(collection))
        stopifnot(all(summary(community)==summary(collection)))

        stopifnot(identical(collection, CommunityCollection(collection)))

        cpa <- CollectionCPS(collection)
        cpb <- data.frame(CPS(community), stringsAsFactors=FALSE)
        rownames(cpb) <- cpb$title
        stopifnot(identical(cpa, cpb))

        npa <- CollectionNPS(collection)
        npb <- cbind(community=CP(community,'title'), NPS(community), 
                     stringsAsFactors=FALSE)
        rownames(npb) <- NULL
        stopifnot(identical(npa, npb))
    
        if('c1'==CP(community, 'title'))
        {
            stopifnot(is.null(CollectionTLPS(collection)))
        }
        else
        {
            cpa <- CollectionTLPS(collection)
            cpb <- cbind(community=CP(community,'title'), TLPS(community), 
                         stringsAsFactors=FALSE)
            stopifnot(identical(cpa, cpb))
        }
    }
}

TestCommunityCollectionProperties <- function()
{
    # TODO Test these cases
    col <- CommunityCollection(list(TL84, TL86, YthanEstuary))

    # Functions that return a single value
    CollectionCPS(col)
    CollectionCPS(col, 'title')
    CollectionCPS(col, c(S='NumberOfNodes'))
    CollectionCPS(col, c(S='NumberOfNodes', L='NumberOfTrophicLinks'))

    # A function that return more than one value
    CollectionCPS(col, 'SumBiomassByClass')
    CollectionCPS(col, c(B='SumBiomassByClass'))
    CollectionCPS(col, list(list('SumBiomassByClass', class='kingdom')))
    CollectionCPS(col, list(B=list('SumBiomassByClass', class='kingdom')))
    CollectionCPS(col, list(S='NumberOfNodes', B=list('SumBiomassByClass', class='kingdom'), L='NumberOfTrophicLinks'))

    # Functions that return more than one value
    CollectionCPS(col, c('SumBiomassByClass', 'SumNByClass'))
    CollectionCPS(col, c(B='SumBiomassByClass', 'SumNByClass'))
    CollectionCPS(col, list(list('SumBiomassByClass', class='kingdom'), 'SumNByClass'))
    CollectionCPS(col, list(B=list('SumBiomassByClass', class='kingdom'), N='SumNByClass'))
    CollectionCPS(col, list(B=list('SumBiomassByClass', class='kingdom'), S='NumberOfNodes', N='SumNByClass'))
    CollectionCPS(col, list(B=list('SumBiomassByClass', class='kingdom'), N=list('SumNByClass', class='kingdom')))
    CollectionCPS(col, list(B=list('SumBiomassByClass', class='kingdom'), S='NumberOfNodes', N=list('SumNByClass', class='kingdom')))

    head(CollectionNPS(col))
    head(CollectionNPS(col, 'Biomass'))
    head(CollectionNPS(col, c(B='Biomass')))
    head(CollectionNPS(col, c(B='Biomass', 'M')))
    head(CollectionNPS(col, 'Log10MNBiomass'))

    head(CollectionTLPS(col))
    head(CollectionTLPS(col, node.properties='Biomass'))
    head(CollectionTLPS(col, node.properties=c(B='Biomass')))
    head(CollectionTLPS(col, node.properties=c(B='Biomass', 'M')))

    S2 <- Community(properties=list(title='Skipwith (no links)'), 
                                    nodes=NPS(SkipwithPond))
    col <- CommunityCollection(list(SkipwithPond, S2))
    head(CollectionTLPS(col))
    head(CollectionTLPS(col, link.properties='link.evidence'))
    NULL
}

TestCommunityCollectionFailures <- function()
{
    # Not communities
    F(CommunityCollection(list()))
    F(CommunityCollection(1))
    F(CommunityCollection(''))
    F(CommunityCollection(list(c1,'')))
    F(CommunityCollection(list(c1,1)))

    # Duplications
    F(CommunityCollection(list(c1,c1)))
    F(CommunityCollection(list(c1,c2,c1)))

    # Inconsistent units
    F(CommunityCollection(list(c5, c6)))

    # Modifications are illegal
    collection <- CommunityCollection(list(TL84))
    F(collection[1] <- 1)
    F(collection[[1]] <- 1)
    F(collection$silly <- 1)
    F(names(collection) <- letters[1:3])
    F(length(collection) <- 1)
}

TestAggregateCommunitiesSingle <- function()
{
    # Aggregating a collection containing one community should not change 
    # anything
    for(community in list(c1,c2,c3,c4,c5,c6,TL84,TL86,SkipwithPond))
    {
        a <- community
        b <- AggregateCommunities(CommunityCollection(list(community)), 
                                  title=CP(community,'title'))

        # Node, trophic link and whole-community properties should not be 
        # different.
        npa <- NPS(a)
        npa <- npa[order(npa$node),]
        npb <- NPS(b)
        npb <- npb[order(npb$node),]
        stopifnot(all.equal(npa, npb))

        cpsa <- CPS(a)
        cpsa <- cpsa[order(names(cpsa))]
        cpsb <- CPS(b)
        cpsb <- cpsb[order(names(cpsb))]
        stopifnot(identical(cpsa, cpsb))

        if('c1'==CP(a, 'title'))
        {
            stopifnot(0==NumberOfTrophicLinks(a))
            stopifnot(0==NumberOfTrophicLinks(b))
            stopifnot(is.null(TLPS(a)))
            stopifnot(is.null(TLPS(b)))
        }
        else
        {
            tlpa <- TLPS(a)
            tlpa <- tlpa[order(tlpa$resource, tlpa$consumer),]
            rownames(tlpa) <- NULL
            tlpb <- TLPS(b)
            tlpb <- tlpb[order(tlpb$resource, tlpb$consumer),]
            rownames(tlpb) <- NULL
            stopifnot(all.equal(tlpa, tlpb))
        }
    }
}

TestAggregateCommunitiesFailures <- function()
{
    # Some failure cases
    collection <- CommunityCollection(list(c1,c2,c3,c4,c5))
    F(AggregateCommunities(collection, 0))
    F(AggregateCommunities(collection, 6))
    F(AggregateCommunities(collection, 0:1))
    F(AggregateCommunities(collection, 1:6))
    F(AggregateCommunities(collection, ''))
    F(AggregateCommunities(collection, c('c1','x')))
}

TestAggregateCommunitiesProperties <- function()
{
    a <- CommunityCollection(list(TL84, TL86))

    # 1. mean
    b <- AggregateCommunities(a)

    # Sanity check
    all.spp <- sort(unique(c(NP(TL84,'node'), NP(TL86,'node'))))
    stopifnot(all(all.spp==sort(unique(CollectionNPS(a)[,'node']))))
    stopifnot(all(sort(NP(b, 'node')) == all.spp))

    # Check M and N averaged correctly
    stopifnot(all(NP(b,'M') == 
                  tapply(CollectionNPS(a)[,'M'], 
                         CollectionNPS(a)[,'node'], 
                         mean)))
    stopifnot(all(NP(b,'N') == 
                  tapply(CollectionNPS(a)[,'N'], 
                         CollectionNPS(a)[,'node'], 
                         mean)))

    # Check category
    stopifnot(all(NP(b,'category') == 
                  tapply(CollectionNPS(a)[,'category'], 
                         CollectionNPS(a)[,'node'], 
                         unique)))

    # 2. median M
    b <- AggregateCommunities(a, column.behaviour=list(M=median))

    # Sanity check
    stopifnot(all(sort(NP(b, 'node')) == all.spp))

    # Check M is median
    stopifnot(all(NP(b,'M') == 
                  tapply(CollectionNPS(a)[,'M'], 
                         CollectionNPS(a)[,'node'], 
                         median)))

    # Check N is mean
    stopifnot(all(NP(b,'N') == 
                  tapply(CollectionNPS(a)[,'N'], 
                         CollectionNPS(a)[,'node'], 
                         mean)))

    # 3. min M and max N
    b <- AggregateCommunities(a, column.behaviour=list(M=min, N=max))

    # Sanity check
    stopifnot(all(sort(NP(b, 'node')) == all.spp))

    # Check M is min
    stopifnot(all(NP(b,'M') == 
                  tapply(CollectionNPS(a)[,'M'], 
                         CollectionNPS(a)[,'node'], 
                         min)))

    # Check N is max
    stopifnot(all(NP(b,'N') == 
                  tapply(CollectionNPS(a)[,'N'], 
                         CollectionNPS(a)[,'node'], 
                         max)))
}

TestAggregateCommunitiesBy <- function()
{
    AggregateCommunitiesBy(pHWebs, 'pH')
    AggregateCommunitiesBy(pHWebs, 'lat')
    AggregateCommunitiesBy(pHWebs, 'NumberOfTrophicLinks')
    NULL
}

