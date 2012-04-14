TestCommunityBasic <- function()
{
    # Minimum community representation
    community <- Community(nodes=data.frame(node='S'), 
                           properties=list(title='test'))
    stopifnot(CPS(community)$title=='test')
    stopifnot(NPS(community)$node=='S')
    stopifnot(NodePropertyNames(community)=='node')
    stopifnot(is.Community(community))

    # Modifications are illegal
    F(community[1] <- 1)
    F(community[[1]] <- 1)
    F(community$silly <- 1)
    F(names(community) <- letters[1:3])
    F(length(community) <- 1)

    # Non-existent node properties
    stopifnot(all(is.na(NP(community, 'z'))))
    stopifnot(all(names(NP(community, 'z'))=='S'))

    # No nodes
    F(Community(nodes=data.frame(), properties=list(title='test')))

    # Nodes not a data.frame    
    F(Community(node='S', properties=list(title='test')))

    # No node columns
    F(Community(nodes=data.frame(a=LETTERS), properties=list(title='test')))

    # The node column appears twice
    F(Community(nodes=data.frame(node=LETTERS, node=LETTERS, 
                                 check.names=FALSE), 
                 properties=list(title='test')))

    # Column names must not start with 'resource.' or 'consumer.'
    F(Community(nodes=data.frame(node='S', resource.a=TRUE), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node='S', consumer.a=TRUE), 
                properties=list(title='test')))
 
    # Node names that are numbers are illegal
    F(Community(nodes=data.frame(node=1:10), properties=list(title='test')))
    F(Community(nodes=data.frame(node=c(1, LETTERS)), 
                properties=list(title='test')))

    # Ensure duplicate nodes are deteced
    F(Community(nodes=data.frame(node=c('A', 'A')), 
                                 properties=list(title='test')))
    F(Community(nodes=data.frame(node=c('A', 'A ')), 
                                 properties=list(title='test')))
    F(Community(nodes=data.frame(node=c('A', 'a')), 
                                 properties=list(title='test')))

    # Empty node name
    F(Community(nodes=data.frame(node=''), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=c('', LETTERS)), 
                properties=list(title='test')))

    # Some illegal node properties
    F(Community(nodes=data.frame(node=LETTERS, title=LETTERS), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS, M.units=LETTERS), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS, N.units=LETTERS), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS, resource=LETTERS), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS, consumer=LETTERS), 
                properties=list(title='test')))
}

TestCommunityNPS <- function()
{
    # Ensure node properties are picked up
    community <- Community(nodes=data.frame(node=LETTERS, x=letters, M=1:26, 
                                            N=26:1), 
                           properties=list(title='test', M.units='kg', 
                                           N.units='m^-2'))
    stopifnot(all(NodePropertyNames(community)==c('node', 'x', 'M', 'N')))
    stopifnot(all(NP(community, 'node')==LETTERS))
    stopifnot(all(NP(community, 'x')==letters))
    stopifnot(all(NP(community, 'M')==1:26))
    stopifnot(all(NP(community, 'N')==26:1))
    stopifnot(all(names(NP(community, 'x'))==LETTERS))
    stopifnot(all(names(NP(community, 'M'))==LETTERS))
    stopifnot(all(names(NP(community, 'N'))==LETTERS))

    # Non-existent node properties
    stopifnot(all(is.na(NP(community, 'z'))))
    stopifnot(all(names(NP(community, 'z'))==LETTERS))

    F(NP(community, ''))
}

TestCommunityProperties <- function()
{
    # No properties at all
    F(Community(nodes=data.frame(node='S')))

    # Properties does not have names
    F(Community(nodes=data.frame(node='S'), properties=list('test')))

    # Properties does not include title
    F(Community(nodes=data.frame(node='S'), properties=list(xyz='test')))

    # title duplicated
    F(Community(nodes=data.frame(node='S'), 
                 properties=list(title='test', title='test')))

    # Properties is not a vector
    F(Community(nodes=data.frame(node='S'), 
                properties=cbind(title='test')))

    # One or more properties of length!=1
    F(Community(nodes=data.frame(node='S'), 
                properties=list(title=paste('title', 1:10))))
    F(Community(nodes=data.frame(node='S'), 
                properties=list(title='test', weasel=1:10)))
    F(Community(nodes=data.frame(node='S'), 
                properties=list(title=paste('title', 1:10), weasel=1:10)))
    F(Community(nodes=data.frame(node='S'), 
                properties=list(title='test', x=NULL)))

    # Some illegal names
    F(Community(nodes=data.frame(node='S'), 
                properties=list(title='test', node='a')))
    F(Community(nodes=data.frame(node='S'), 
                properties=list(title='test', resource='a')))
    F(Community(nodes=data.frame(node='S'), 
                properties=list(title='test', consumer='a')))

    # Ensure properties are picked up
    community <- Community(nodes=data.frame(node='S'), 
                           properties=list(title='test', M.units='kg', pH=5.6, 
                                           lat=12, x=NA))
    stopifnot(CP(community, 'title')=='test')
    stopifnot(CP(community, 'M.units')=='kg')
    stopifnot(CP(community, 'pH')==5.6)
    stopifnot(CP(community, 'lat')==12)
    stopifnot(is.na(CP(community, 'x')))
    stopifnot(all.equal(list(title='test', pH=5.6, lat=12), 
                        CPS(community, c('title', 'pH', 'lat'))))
    stopifnot(all.equal(list(title='test', M.units='kg', pH=5.6, lat=12, x=NA), 
                        CPS(community)))
    stopifnot(all.equal(list(title='test', M.units='kg', pH=5.6, lat=12, x=NA), 
                        CPS(community)))

    # Computed properties
    stopifnot(all.equal(list(NumberOfTrophicLinks=0, pH=5.6), 
                        CPS(community, c('NumberOfTrophicLinks', 'pH'))))
    stopifnot(all.equal(list(NumberOfTrophicLinks=0), 
                        CPS(c1, 'NumberOfTrophicLinks')))
    stopifnot(all.equal(list(NumberOfTrophicLinks=269), 
                        CPS(TL84, 'NumberOfTrophicLinks')))

    # Computed property names
    stopifnot(all.equal(list(L=269), 
                        CPS(TL84, c(L='NumberOfTrophicLinks'))))

    # Computed property names length > 1
    stopifnot(all.equal(list(invertebrate=0.00537614600000000005, 
                             producer=0.00743826670000000047, 
                             vert.ecto=0.00231559000000000018), 
                        CPS(TL84, 'SumBiomassByClass')))

    # Properties as using c() should work just as well as a list()
    community <- Community(nodes=data.frame(node='S'), 
                           properties=c(title='test', a=1, b=TRUE))
    stopifnot(CP(community, 'title')=='test')
    stopifnot(CP(community, 'a')==1)
    stopifnot(CP(community, 'b')==TRUE)
}

TestCommunityTrophicLinks <- function()
{
    # No trophic Links
    community <- Community(nodes=data.frame(node=LETTERS), 
                           properties=list(title='test'))
    stopifnot(all(c(26,26)==dim(PredationMatrix(community))))
    stopifnot(all(colnames(PredationMatrix(community))==LETTERS))
    stopifnot(all(rownames(PredationMatrix(community))==LETTERS))
    stopifnot(0==sum(PredationMatrix(community)))
    stopifnot(0==NumberOfTrophicLinks(community))

    # Empty trophic links
    community <- Community(nodes=data.frame(node=LETTERS), 
                 trophic.links=matrix(0, nrow=0, ncol=2, 
                                      dimnames=list(NULL, c('resource', 
                                                            'consumer'))), 
                 properties=list(title='test'))
    stopifnot(all(c(26,26)==dim(PredationMatrix(community))))
    stopifnot(all(colnames(PredationMatrix(community))==LETTERS))
    stopifnot(all(rownames(PredationMatrix(community))==LETTERS))
    stopifnot(0==sum(PredationMatrix(community)))
    stopifnot(0==NumberOfTrophicLinks(community))

    # Make sure a minimum set of trophic.links is OK
    community <- Community(nodes=data.frame(node=LETTERS), 
                           trophic.links=data.frame(resource='A',consumer='A'),
                           properties=list(title='test'))
    stopifnot(all(colnames(PredationMatrix(community))==LETTERS))
    stopifnot(all(rownames(PredationMatrix(community))==LETTERS))
    stopifnot(1==sum(PredationMatrix(community)))
    stopifnot(1==NumberOfTrophicLinks(community))
    stopifnot('integer'==class(PredationMatrix(community)[1]))
    stopifnot(all(0:1==sort(unique(as.vector(PredationMatrix(community))))))
    stopifnot(all(1==PredationMatrix(community)[1,1]))
    stopifnot(all(0==PredationMatrix(community)[-1]))
    stopifnot(1==sum(PredationMatrix(community)))

    # resource and/or consumer not in nodes
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='a', consumer='A'), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='A', consumer='a'), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='a', consumer='a'), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='', consumer='A'), 
                properties=list(title='test')))

    # Check white space is removed
    community <- Community(nodes=data.frame(node=LETTERS), 
                           trophic.links=data.frame(resource='A ',consumer='A'),
                           properties=list(title='test'))
    stopifnot(all(matrix(c('A','A') == TLPS(community))))

    community <- Community(nodes=data.frame(node='A '), 
                           trophic.links=data.frame(resource='A',consumer='A'),
                           properties=list(title='test'))
    stopifnot(all(matrix(c('A','A') == TLPS(community))))
    community <- Community(nodes=data.frame(node=' A '), 
                       trophic.links=data.frame(resource=' A',consumer='A '),
                       properties=list(title='test'))
    stopifnot(all(matrix(c('A','A') == TLPS(community))))

    # Incorrect column names
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=cbind(x='A', t='A'), 
                properties=list(title='test')))

    # Some illegal trophic.link properties
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='A', consumer='A', node='A'), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='A', consumer='A', title='A'),
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='A', consumer='A', 
                                         M.units='A'), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='A', consumer='A', 
                                         N.units='A'), 
                properties=list(title='test')))

    # trophic.link properties start with either 'resource.' or 'consumer.'
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='A', consumer='A', 
                                         resource.a='A'), 
                properties=list(title='test')))
    F(Community(nodes=data.frame(node=LETTERS), 
                trophic.links=data.frame(resource='A', consumer='A', 
                                         consumer.='A'), 
                properties=list(title='test')))

    # All cannibals - no other trophic links
    community <- Community(nodes=data.frame(node=LETTERS), 
                           trophic.links=data.frame(resource=LETTERS,
                                                    consumer=LETTERS),
                           properties=list(title='test'))
    stopifnot(colnames(PredationMatrix(community))==LETTERS)
    stopifnot(rownames(PredationMatrix(community))==LETTERS)
    stopifnot(all(1==diag(PredationMatrix(community))))
    pm <- PredationMatrix(community)
    stopifnot(all(0==pm[upper.tri(pm, diag=FALSE)]))
    stopifnot(all(0==pm[lower.tri(pm, diag=FALSE)]))

    # No cannibals, Z consumes everything
    community <- Community(nodes=data.frame(node=LETTERS), 
                           trophic.links=data.frame(resource=LETTERS,
                                                    consumer='Z'),
                           properties=list(title='test'))
    stopifnot(colnames(PredationMatrix(community))==LETTERS)
    stopifnot(rownames(PredationMatrix(community))==LETTERS)
    stopifnot(all(1==PredationMatrix(community)[,'Z']))
    stopifnot(all(0==PredationMatrix(community)[,LETTERS[1:25]]))

    # A few specific links
    tl <- data.frame(resource=c('A', 'B', 'Z'), 
                     consumer=c('B', 'C', 'Z'), stringsAsFactors=FALSE)
    community <- Community(nodes=data.frame(node=LETTERS), 
                           trophic.links=tl,
                           properties=list(title='test'))
    stopifnot(colnames(PredationMatrix(community))==LETTERS)
    stopifnot(rownames(PredationMatrix(community))==LETTERS)
    invisible(apply(tl, 1, function(row)
    {
        res <- row['resource']
        con <- row['consumer']
        stopifnot(1==PredationMatrix(community)[res, con])
    }))
    stopifnot(all(0:1==sort(unique(as.vector(PredationMatrix(community))))))
}

TestNodePropertyNames <- function()
{
    for(community in list(c1,c2,c3,c4,c5))
    {
        stopifnot('node'==NodePropertyNames(community))
    }

    stopifnot(all(c('node','M','N','order','family')==NodePropertyNames(c6)))
}

TestNPS <- function()
{
    stopifnot('S'==NP(c1, 'node'))
    stopifnot('S'==NP(c2, 'node'))
    stopifnot(all(c('R','C')==NP(c3, 'node')))
    stopifnot(all(c('R','C','P')==NP(c4, 'node')))
    stopifnot(all(c('R','C','O')==NP(c5, 'node')))
    stopifnot(all(c('R','C','P')==NP(c6, 'node')))

    stopifnot(data.frame(node='S')==NPS(c1))
    stopifnot(data.frame(node='S')==NPS(c2))
    stopifnot(data.frame(node=c('R','C'))==NPS(c3))
    stopifnot(data.frame(node=c('R','C','P'))==NPS(c4))
    stopifnot(data.frame(node=c('R','C','O'))==NPS(c5))
    stopifnot(data.frame(node=c('R','C','P'), 
                         M=c(1.5,5,100), 
                         N=c(1000,10,5.5), 
                         order=c('Order 1','Order 2','Order 2'), 
                         family=paste('Family',1:3), 
                         row.names=c('R','C','P'))==NPS(c6))

    # Subsets of properties
    stopifnot(data.frame(M=c(1.5,5,100), 
                         row.names=c('R','C','P'))==NPS(c6, 'M'))
    stopifnot(data.frame(M=c(1.5,5,100), 
                         family=paste('Family',1:3), 
                         row.names=c('R','C','P'))==NPS(c6, c('M','family')))

    # Computed properties
    stopifnot(data.frame(Biomass=c(1500,50,550), 
                         row.names=c('R','C','P')) == NPS(c6, 'Biomass'))
    stopifnot(data.frame(M=c(1.5,5,100), 
                         PreyAveragedTrophicLevel=c(1,2,3), 
                         IsBasalNode=c(TRUE,FALSE,FALSE),
                         IsTopLevelNode=c(FALSE,FALSE,TRUE), 
                         row.names=c('R','C','P')) == 
              NPS(c6, c('M', 'PreyAveragedTrophicLevel', 'IsBasalNode', 
                        'IsTopLevelNode')))

    # Computed property names
    stopifnot(data.frame(x=c(1500,50,550), 
                         row.names=c('R','C','P')) == NPS(c6, c(x='Biomass')))

    # Computed property names with extra params
    a <- data.frame(TS=TrophicSpecies(TL84, include.isolated=FALSE))
    b <- NPS(TL84, list(TS=list('TrophicSpecies', include.isolated=FALSE)))
    stopifnot(identical(a, b))

    a <- data.frame(TS=TrophicSpecies(TL84, include.isolated=TRUE))
    b <- NPS(TL84, list(TS=list('TrophicSpecies', include.isolated=TRUE)))
    stopifnot(identical(a, b))

    # Function returns incorrect length
    F(NPS(TL84, 'NumberOfTrophicLinks'))

    # Missing node properties
    F(NP(TL84, ''))
    stopifnot(all(is.na(NP(TL84, 'x'))))
}

TestTrophicLinkPropertyNames <- function()
{
    stopifnot(is.null(TrophicLinkPropertyNames(c1)))
    for(community in list(c2,c3,c4,c5))
    {
        stopifnot(all(c('resource', 'consumer') == 
                      TrophicLinkPropertyNames(community)))
    }

    stopifnot(all(c('resource','consumer','link.evidence','link.strength') == 
                  TrophicLinkPropertyNames(c6)))
}

TestTLPS <- function()
{
    # First-class link properties
    stopifnot(is.null(TLPS(c1)))
    stopifnot(identical(data.frame(resource='S', consumer='S', 
                                   stringsAsFactors=FALSE), 
                       TLPS(c2)))
    stopifnot(identical(data.frame(resource='R', consumer='C', 
                                   stringsAsFactors=FALSE), 
                       TLPS(c3)))
    stopifnot(identical(data.frame(resource=c('R','C'), consumer=c('C','P'), 
                                   stringsAsFactors=FALSE), 
                       TLPS(c4)))
    stopifnot(identical(data.frame(resource=c('R','R','C'), 
                                   consumer=c('C','O','O'), 
                                   stringsAsFactors=FALSE), 
                       TLPS(c5)))
    stopifnot(identical(data.frame(resource=c('R','C'), consumer=c('C','P'), 
                                   link.evidence=c('Inferred','Known'), 
                                   link.strength=c(0.5, 0.2), 
                                   stringsAsFactors=FALSE), 
                       TLPS(c6)))

    stopifnot(269==nrow(TLPS(TL84)))
    stopifnot(264==nrow(TLPS(RemoveCannibalisticLinks(TL84))))

    stopifnot(all(c('Inferred','Known') == TLP(c6, 'link.evidence')))
    stopifnot(all(c(0.5, 0.2) == TLP(c6, 'link.strength')))
    stopifnot(all(is.na(TLP(c6, 'x'))))

    # Node properties
    tlps <- TLPS(TL84, c('M','N','Biomass'))
    stopifnot(all(colnames(tlps)==c('resource', 'consumer', 
                                    'resource.M', 'resource.N', 
                                    'resource.Biomass', 
                                    'consumer.M', 'consumer.N', 
                                    'consumer.Biomass')))
    stopifnot(all(tlps[,'resource.M'] == NP(TL84,'M')[tlps[,'resource']]))
    stopifnot(all(tlps[,'consumer.M'] == NP(TL84,'M')[tlps[,'consumer']]))
    stopifnot(all(tlps[,'resource.N'] == NP(TL84,'N')[tlps[,'resource']]))
    stopifnot(all(tlps[,'consumer.N'] == NP(TL84,'N')[tlps[,'consumer']]))
    stopifnot(all(tlps[,'resource.Biomass'] ==Biomass(TL84)[tlps[,'resource']]))
    stopifnot(all(tlps[,'consumer.Biomass'] ==Biomass(TL84)[tlps[,'consumer']]))

    # Missing node properties should be NA
    tlps <- TLPS(TL84, 'x')
    stopifnot(all(is.na(tlps[,'resource.x'])))
    stopifnot(all(is.na(tlps[,'consumer.x'])))
}

TestCommunityPropertyNames <- function()
{
    stopifnot('title'==CommunityPropertyNames(c1))
    stopifnot('title'==CommunityPropertyNames(c2))
    stopifnot('title'==CommunityPropertyNames(c3))
    stopifnot('title'==CommunityPropertyNames(c4))
    stopifnot('title'==CommunityPropertyNames(c5))
    stopifnot(all(c('title','M.units','N.units')==CommunityPropertyNames(c6)))
}

TestPersistCommunity <- function()
{
    # Tests LoadCommunity() and SaveCommunity()
    path <- tempfile()
    on.exit(unlink(path, recursive=TRUE))
    for(community in list(c1,c2,c3,c4,c5,c6,TL84,TL86,YthanEstuary,
                          SkipwithPond,BroadstoneStream,Benguela))
    {
        SaveCommunity(community, path)
        stopifnot(identical(community, LoadCommunity(path)))
    }
}

TestResolveToNodeIndices <- function()
{
    ResolveToNodeIndices <- cheddar:::.ResolveToNodeIndices
    stopifnot(1==ResolveToNodeIndices(c1, 'S'))
    stopifnot(1==ResolveToNodeIndices(c1, 1))
    stopifnot(1==ResolveToNodeIndices(c1, TRUE))
    stopifnot(0==length(ResolveToNodeIndices(c1, FALSE)))

    stopifnot(1==ResolveToNodeIndices(c4, 'R'))
    stopifnot(all(c(1,3)==ResolveToNodeIndices(c4, c('R','P'))))
    stopifnot(1==ResolveToNodeIndices(c4, 1))
    stopifnot(all(c(1,2)==ResolveToNodeIndices(c4, c(1,2))))
    stopifnot(all(c(2,3)==ResolveToNodeIndices(c4, c(FALSE,TRUE,TRUE))))
    stopifnot(0==length(ResolveToNodeIndices(c4, c(FALSE,FALSE,FALSE))))

    stopifnot(is.null(ResolveToNodeIndices(c4, NULL)))

    F(ResolveToNodeIndices(c4, ''))
    F(ResolveToNodeIndices(c4, 'x'))
    F(ResolveToNodeIndices(c4, 0))
    F(ResolveToNodeIndices(c4, 4))
    F(ResolveToNodeIndices(c4, TRUE))
    F(ResolveToNodeIndices(c4, rep(TRUE,4)))
}

TestNodeNameIndices <- function()
{
    stopifnot(all(1==NodeNameIndices(c1, 'S')))
    stopifnot(all(1==NodeNameIndices(c2, 'S')))
    stopifnot(all(2:1==NodeNameIndices(c3, c('C','R'))))
    stopifnot(all(1:2==NodeNameIndices(c3, c('R','C'))))
    stopifnot(all(c(2,2)==NodeNameIndices(c3, c('C','C'))))
    stopifnot(all(1:3==NodeNameIndices(c4, c('R','C','P'))))
    stopifnot(all(3:1==NodeNameIndices(c4, c('P','C','R'))))
    stopifnot(all(2==NodeNameIndices(c4, 'C')))

    F(NodeNameIndices(c1, ''))
    F(NodeNameIndices(c1, 'x'))
}

TestNumberOfNodes <- function()
{
    stopifnot(1==NumberOfNodes(c1))
    stopifnot(1==NumberOfNodes(c2))
    stopifnot(2==NumberOfNodes(c3))
    stopifnot(3==NumberOfNodes(c4))
    stopifnot(3==NumberOfNodes(c5))
    stopifnot(3==NumberOfNodes(c6))
    stopifnot(56==NumberOfNodes(TL84))
    stopifnot(57==NumberOfNodes(TL86))
    stopifnot(37==NumberOfNodes(SkipwithPond))
}

TestRemoveNodes <- function()
{
    F(RemoveNodes(c1, 0))  # Illegal node index
    F(RemoveNodes(c1, 1))  # Would result in empty community
    F(RemoveNodes(c1, 2))  # Illegal node index
    F(RemoveNodes(c2, 1))  # Would result in empty community

    stopifnot(identical(c1, RemoveNodes(c1, NULL)))
    stopifnot(identical(c1, RemoveNodes(c1, vector(mode='character'))))

    stopifnot(all.equal(RemoveNodes(c3, 'R', title='c3'), 
                        RemoveNodes(c3, 'R', title='c3')))
    stopifnot('C'==NP(RemoveNodes(c3, 1),'node'))
    stopifnot(is.null(TLPS(RemoveNodes(c3, 1))))
    stopifnot('R'==NP(RemoveNodes(c3, 2),'node'))
    stopifnot(is.null(TLPS(RemoveNodes(c3, 2))))

    stopifnot(all(c('C','P')==NP(RemoveNodes(c4, 1),'node')))
    stopifnot(all(c('R','P')==NP(RemoveNodes(c4, 2),'node')))
    stopifnot(all(c('R','C')==NP(RemoveNodes(c4, 3),'node')))
    stopifnot(all(c('R')==NP(RemoveNodes(c4, 2:3),'node')))
    F(RemoveNodes(c4, 1:3))

    stopifnot(identical(TL84, RemoveNodes(TL84, NULL)))
    stopifnot(identical(TL84, RemoveNodes(TL84, vector(mode='character'))))

    TL84r <- RemoveNodes(TL84, 56)
    stopifnot(identical(NPS(TL84)[-56,], NPS(TL84r)))
    TL84r <- RemoveNodes(TL84, 'Umbra limi')
    stopifnot(identical(NPS(TL84)[-56,], NPS(TL84r)))

    TL84r <- RemoveNodes(TL84, c(1,56))
    stopifnot(identical(NPS(TL84)[-c(1,56),],NPS(TL84r)))

    TL84r <- RemoveNodes(TL84, c('Nostoc sp.', 'Umbra limi'))
    stopifnot(identical(NPS(TL84)[-c(1,56),],NPS(TL84r)))
}

TestRemoveIsolatedNodes <- function()
{
    F(RemoveIsolatedNodes(c1))
    F(RemoveIsolatedNodes(c2))
    stopifnot(identical(c3, RemoveIsolatedNodes(c3, title=c3$title)))
    stopifnot(identical(c4, RemoveIsolatedNodes(c4, title=c4$title)))
    stopifnot(identical(c5, RemoveIsolatedNodes(c5, title=c5$title)))
    stopifnot(identical(c6, RemoveIsolatedNodes(c6, title=c6$title)))

    TL84.no.iso <- RemoveIsolatedNodes(TL84, title=TL84$title)
    stopifnot(50==NumberOfNodes(TL84.no.iso))

    isolated <- c('Asterionella formosa','Chrysosphaerella longispina',
                  'Diceras sp.', 'Rhizosolenia sp.', 'Spinocosmarium sp.',
                  'Staurastrum sp.')
    stopifnot(all(isolated == IsolatedNodes(TL84)))

    stopifnot(identical(NPS(TL84)[!NP(TL84,'node') %in% isolated,], 
                        NPS(TL84.no.iso)))
    stopifnot(identical(TLPS(TL84), TLPS(TL84.no.iso)))
}

TestLumpTrophicSpecies <- function()
{
    stopifnot(1==NumberOfNodes(c1))
    stopifnot(1==NumberOfNodes(LumpTrophicSpecies(c1)))
    stopifnot(1==NumberOfNodes(c2))
    stopifnot(1==NumberOfNodes(LumpTrophicSpecies(c2)))
    stopifnot(2==NumberOfNodes(c3))
    stopifnot(2==NumberOfNodes(LumpTrophicSpecies(c3)))
    stopifnot(3==NumberOfNodes(c4))
    stopifnot(3==NumberOfNodes(LumpTrophicSpecies(c4)))
    stopifnot(3==NumberOfNodes(c5))
    stopifnot(3==NumberOfNodes(LumpTrophicSpecies(c5)))
    stopifnot(3==NumberOfNodes(c6))
    stopifnot(3==NumberOfNodes(LumpTrophicSpecies(c6)))
    stopifnot(all(c('Inferred','Known') == 
                  TLPS(LumpTrophicSpecies(c6))[,'link.evidence']))
    stopifnot(all(c(0.5,0.2) == 
                  TLPS(LumpTrophicSpecies(c6))[,'link.strength']))

    stopifnot(56==NumberOfNodes(TL84))

    # Exclude isolated species.
    lumped <- LumpTrophicSpecies(TL84, include.isolated=FALSE)
    stopifnot(21==NumberOfNodes(lumped))

    # From Jonsson et al 2005 AER. Isolated species assigned NA.
    trophic.species <- c(1,2,NA,3,4,3,5,NA,6,1,1,NA,4,7,4,8,6,7,2,3,6,7,
                         4,6,3,NA,3,NA,NA,6,3,9,9,10,11,12,13,14,15,11,
                         15,16,15,9,15,17,18,15,15,15,15,12,19,20,20,21)

    # Are my trophic species the same as those calculated by cheddar?
    stopifnot(identical(trophic.species, 
                        unname(TrophicSpecies(TL84, include.isolated=FALSE))))

    # Check M and N have been averaged correctly
    for(ts in unique(trophic.species[!is.na(trophic.species)]))
    {
        stopifnot(identical(unname(NP(lumped,'M')[ts]), 
                            mean(NP(TL84,'M')[which(ts==trophic.species)])))
        stopifnot(identical(unname(NP(lumped,'N')[ts]), 
                            mean(NP(TL84,'N')[which(ts==trophic.species)])))
    }

    # Include isolated species.
    lumped <- LumpTrophicSpecies(TL84, include.isolated=TRUE)
    stopifnot(22==NumberOfNodes(lumped))

    trophic.species <- c(1,2,3,4,5,4,6,3,7,1,1,3,5,8,5,9,7,8,2,4,7,8,5,7,
                         4,3,4,3,3,7,4,10,10,11,12,13,14,15,16,12,16,17,16,
                         10,16,18,19,16,16,16,16,13,20,21,21,22)

    # Are my trophic species the same as those calculated by cheddar?
    stopifnot(identical(trophic.species, 
                        unname(TrophicSpecies(TL84, include.isolated=TRUE))))

    # Check M and N have been averaged correctly
    for(ts in unique(trophic.species[!is.na(trophic.species)]))
    {
        stopifnot(identical(unname(NP(lumped,'M')[ts]), 
                            mean(NP(TL84,'M')[which(ts==trophic.species)])))
        stopifnot(identical(unname(NP(lumped,'N')[ts]), 
                            mean(NP(TL84,'N')[which(ts==trophic.species)])))
    }
}

TestOrderCommunity <- function()
{
    # Order unchanged
    stopifnot(identical(c1, OrderCommunity(c1, 'node', title=CP(c1,'title'))))
    stopifnot(identical(c2, OrderCommunity(c2, 'node', title=CP(c2,'title'))))
    stopifnot(identical(c3, OrderCommunity(c3, new.order=1:2, 
                                           title=CP(c3,'title'))))
    stopifnot(identical(c4, OrderCommunity(c4, new.order=1:3, 
                                           title=CP(c4,'title'))))
    stopifnot(identical(c5, OrderCommunity(c5, new.order=1:3, 
                                           title=CP(c5,'title'))))
    stopifnot(identical(c6, OrderCommunity(c6, new.order=1:3, 
                                           title=CP(c6,'title'))))

    # Reverse order
    c6r <- OrderCommunity(c6, new.order=3:1)
    target1 <- NPS(c6)
    target2 <- NPS(c6r)[3:1,]
    stopifnot(identical(target1, target2))

    target1 <- TLPS(c6)
    target1 <- target1[order(target1$resource, target1$consumer),]
    target2 <- TLPS(c6r)
    target2 <- target2[order(target2$resource, target2$consumer),]
    stopifnot(identical(target1, target2))

    # Order nodes alphabetically
    SkipwithPondr <- OrderCommunity(SkipwithPond, 'node')
    target1 <- NPS(SkipwithPond)[order(NP(SkipwithPond,'node')),]
    target2 <- NPS(SkipwithPondr)
    stopifnot(identical(target1, target2))
    target1 <- TLPS(SkipwithPond)
    target1 <- target1[order(target1$resource, target1$consumer),]
    target2 <- TLPS(SkipwithPondr)
    target2 <- target2[order(target2$resource, target2$consumer),]
    stopifnot(identical(target1, target2))

    # Order by property
    stopifnot(all(c("Chromulina sp.","Dactylococcopsis fascicularis", 
                    "Diceras sp.", "Trachelomonas sp.", "Cryptomonas sp. 1", 
                    "Closteriopsis longissimus", "Chroococcus dispersus",
                    "Selenastrum minutum", "Unclassified flagellates",
                    "Dictyosphaerium pulchellum", "Dinobryon sociale",
                    "Rhizosolenia sp.","Nostoc sp.","Mallomonas sp. 1", 
                    "Asterionella formosa","Mallomonas sp. 2",
                    "Cryptomonas sp. 2","Arthrodesmus sp.", 
                    "Dinobryon cylindricum","Peridinium pulsillum", 
                    "Dinobryon bavaricum","Spinocosmarium sp.", 
                    "Staurastrum sp.",  "Glenodinium quadridens", 
                    "Keratella cochlearis","Keratella testudo",
                    "Dinobryon sertularia","Microcystis aeruginosa", 
                    "Kellicottia sp.",  "Conochilus (solitary)", 
                    "Peridinium wisconsinense","Peridinium cinctum", 
                    "Peridinium limbatum","Synedra sp.","Ploesoma sp.", 
                    "Gastropus stylifer","Ascomorpha eucadis", 
                    "Conochiloides dossuarius","Filinia longispina", 
                    "Trichocerca multicrinis", "Trichocerca cylindrica",
                    "Polyarthra vulgaris","Chrysosphaerella longispina", 
                    "Synchaeta sp.","Bosmina longirostris",
                    "Diaphanosoma leuchtenbergianum", "Tropocyclops prasinus",
                    "Leptodiaptomus siciloides","Cyclops varians rubellus",
                    "Orthocyclops modestus","Daphnia pulex",
                    "Holopedium gibberum", "Chaoborus punctipennis",
                    "Phoxinus eos","Phoxinus neogaeus","Umbra limi") == 
                  NP(OrderCommunity(TL84, 'M'), 'node')))
    stopifnot(all(c('Umbra limi','Phoxinus neogaeus','Phoxinus eos',
                    'Holopedium gibberum','Daphnia pulex','Filinia longispina',
                    'Leptodiaptomus siciloides','Keratella testudo',
                    'Cyclops varians rubellus','Chaoborus punctipennis',
                    'Ascomorpha eucadis','Diaphanosoma leuchtenbergianum',
                    'Orthocyclops modestus','Synchaeta sp.',
                    'Gastropus stylifer','Conochilus (solitary)',
                    'Trichocerca multicrinis','Tropocyclops prasinus',
                    'Ploesoma sp.','Trichocerca cylindrica',
                    'Bosmina longirostris','Conochiloides dossuarius',
                    'Kellicottia sp.','Polyarthra vulgaris',
                    'Keratella cochlearis','Synedra sp.','Nostoc sp.',
                    'Dinobryon sertularia','Spinocosmarium sp.',
                    'Dinobryon cylindricum','Chrysosphaerella longispina',
                    'Asterionella formosa','Peridinium cinctum',
                    'Staurastrum sp.','Dictyosphaerium pulchellum',
                    'Microcystis aeruginosa','Diceras sp.',
                    'Peridinium wisconsinense','Peridinium limbatum',
                    'Mallomonas sp. 1','Chroococcus dispersus',
                    'Mallomonas sp. 2','Cryptomonas sp. 2','Dinobryon sociale',
                    'Dinobryon bavaricum','Dactylococcopsis fascicularis',
                    'Arthrodesmus sp.','Rhizosolenia sp.','Cryptomonas sp. 1',
                    'Glenodinium quadridens','Closteriopsis longissimus',
                    'Peridinium pulsillum','Chromulina sp.',
                    'Selenastrum minutum','Trachelomonas sp.',
                    'Unclassified flagellates') == 
                  NP(OrderCommunity(TL84, 'N'), 'node')))

    # Reorder by function
    stopifnot(all(c('Asterionella formosa','Chrysosphaerella longispina',
                    'Diceras sp.','Rhizosolenia sp.','Spinocosmarium sp.',
                    'Staurastrum sp.','Dinobryon bavaricum',
                    'Microcystis aeruginosa','Peridinium limbatum',
                    'Peridinium wisconsinense','Synedra sp.',
                    'Dinobryon sertularia','Mallomonas sp. 1',
                    'Peridinium cinctum','Arthrodesmus sp.',
                    'Closteriopsis longissimus','Mallomonas sp. 2','Nostoc sp.',
                    'Dinobryon cylindricum','Dactylococcopsis fascicularis',
                    'Cryptomonas sp. 2','Dictyosphaerium pulchellum',
                    'Dinobryon sociale','Peridinium pulsillum',
                    'Glenodinium quadridens','Filinia longispina',
                    'Gastropus stylifer','Kellicottia sp.','Keratella testudo',
                    'Ploesoma sp.','Polyarthra vulgaris',
                    'Trichocerca multicrinis','Trichocerca cylindrica',
                    'Phoxinus eos','Phoxinus neogaeus','Ascomorpha eucadis',
                    'Synchaeta sp.','Conochilus (solitary)',
                    'Conochiloides dossuarius','Keratella cochlearis',
                    'Umbra limi','Bosmina longirostris','Cryptomonas sp. 1',
                    'Chroococcus dispersus','Unclassified flagellates',
                    'Chromulina sp.','Selenastrum minutum','Trachelomonas sp.',
                    'Diaphanosoma leuchtenbergianum','Holopedium gibberum',
                    'Orthocyclops modestus','Cyclops varians rubellus',
                    'Leptodiaptomus siciloides','Tropocyclops prasinus',
                    'Chaoborus punctipennis','Daphnia pulex') == 
                  NP(OrderCommunity(TL84, 'Degree'), 'node')))
    stopifnot(all(c("Daphnia pulex", "Chaoborus punctipennis", 
                    "Cyclops varians rubellus", "Leptodiaptomus siciloides", 
                    "Tropocyclops prasinus", "Holopedium gibberum", 
                    "Orthocyclops modestus", "Cryptomonas sp. 1",           
                    "Chroococcus dispersus", "Unclassified flagellates", 
                    "Chromulina sp.", "Selenastrum minutum", 
                    "Trachelomonas sp.", "Diaphanosoma leuchtenbergianum", 
                    "Bosmina longirostris", "Umbra limi",       
                    "Ascomorpha eucadis", "Synchaeta sp.", 
                    "Conochilus (solitary)", "Conochiloides dossuarius", 
                    "Keratella cochlearis", "Filinia longispina", 
                    "Gastropus stylifer", "Kellicottia sp.", 
                    "Keratella testudo", "Ploesoma sp.", 
                    "Polyarthra vulgaris", "Trichocerca multicrinis", 
                    "Trichocerca cylindrica", "Phoxinus eos", 
                    "Phoxinus neogaeus", "Glenodinium quadridens", 
                    "Cryptomonas sp. 2", "Dictyosphaerium pulchellum", 
                    "Dinobryon sociale", "Peridinium pulsillum", 
                    "Nostoc sp.", "Dinobryon cylindricum", 
                    "Dactylococcopsis fascicularis", "Arthrodesmus sp.", 
                    "Closteriopsis longissimus", "Mallomonas sp. 2", 
                    "Dinobryon sertularia", "Mallomonas sp. 1", 
                    "Peridinium cinctum", "Dinobryon bavaricum", 
                    "Microcystis aeruginosa", "Peridinium limbatum", 
                    "Peridinium wisconsinense", "Synedra sp.", 
                    "Asterionella formosa", "Chrysosphaerella longispina", 
                    "Diceras sp.", "Rhizosolenia sp.", "Spinocosmarium sp.", 
                    "Staurastrum sp.") == 
                  NP(OrderCommunity(TL84, 'Degree', decreasing=TRUE), 'node')))
}

