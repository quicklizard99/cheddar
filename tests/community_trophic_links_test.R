# Functions requiring trophic links
TestPredationMatrix <- function()
{
    stopifnot(all.equal(matrix(0, ncol=1, nrow=1, dimnames=list('S', 'S')), 
                        PredationMatrix(c1)))
    stopifnot(all.equal(matrix(1, ncol=1, nrow=1, dimnames=list('S', 'S')), 
                        PredationMatrix(c2)))
    stopifnot(all.equal(matrix(c(0,0,1,0), ncol=2, nrow=2, 
                               dimnames=list(c('R','C'), c('R','C'))), 
                        PredationMatrix(c3)))
    stopifnot(all.equal(matrix(c(0,0,0, 1,0,0, 0,1,0), ncol=3, nrow=3, 
                               dimnames=list(c('R','C','P'), c('R','C','P'))), 
                        PredationMatrix(c4)))
    stopifnot(all.equal(matrix(c(0,0,0, 1,0,0, 1,1,0), ncol=3, nrow=3, 
                               dimnames=list(c('R','C','O'), c('R','C','O'))), 
                        PredationMatrix(c5)))
    stopifnot(all.equal(matrix(c(0,0,0, 1,0,0, 0,1,0), ncol=3, nrow=3, 
                               dimnames=list(c('R','C','P'), c('R','C','P'))), 
                        PredationMatrix(c6)))
    stopifnot(all(NP(TL84, 'node')==rownames(PredationMatrix(TL84))))
    stopifnot(all(NP(TL84, 'node')==colnames(PredationMatrix(TL84))))
    stopifnot(269==sum(PredationMatrix(TL84)))
}

TestInDegree <- function()
{
    stopifnot(0==InDegree(c1))
    stopifnot(all(1==InDegree(c2)))
    stopifnot(all(c(0,1)==InDegree(c3)))
    stopifnot(all(c(0,1,1)==InDegree(c4)))
    stopifnot(all(c(0,1,2)==InDegree(c5)))
    stopifnot(all(c(0,1,1)==InDegree(c6)))
    stopifnot(all(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,6,6,11,7,16,14,28,6,7,6,19,6,6,6,16,17,6,6,6,6,16,
                    22,9,9,12) == InDegree(TL84)))
}

TestNormalisedTrophicGenerality <- function()
{
    stopifnot(is.nan(NormalisedTrophicGenerality(c1)))
    stopifnot(1==NormalisedTrophicGenerality(c2))
    stopifnot(c(0,2)==NormalisedTrophicGenerality(c3))
    stopifnot(c(0,1.5,1.5)==NormalisedTrophicGenerality(c4))
    stopifnot(c(0,1,2)==NormalisedTrophicGenerality(c5))
    stopifnot(c(0,1.5,1.5)==NormalisedTrophicGenerality(c6))
    stopifnot(all.equal(56, sum(NormalisedTrophicGenerality(TL84))))
    stopifnot(all.equal( 1, mean(NormalisedTrophicGenerality(TL84))))
}

TestOutDegree <- function()
{
    stopifnot(0==OutDegree(c1))
    stopifnot(all(1==OutDegree(c2)))
    stopifnot(all(c(1,0)==OutDegree(c3)))
    stopifnot(all(c(1,1,0)==OutDegree(c4)))
    stopifnot(all(c(2,1,0)==OutDegree(c5)))
    stopifnot(all(c(1,1,0)==OutDegree(c6)))
    stopifnot(all(c(4,3,0,18,5,18,3,0,1,4,4,0,5,2,5,6,1,2,3,18,1,2,5,1,18,
                    0,18,0,0,1,18,5,5,4,4,7,4,4,4,4,4,3,4,5,4,7,5,4,4,4,4,
                    7,4,1,1,1) == OutDegree(TL84)))
}

TestNormalisedTrophicVulnerability <- function()
{
    stopifnot(is.nan(NormalisedTrophicVulnerability(c1)))
    stopifnot(1==NormalisedTrophicVulnerability(c2))
    stopifnot(c(2,0)==NormalisedTrophicVulnerability(c3))
    stopifnot(c(1.5,1.5,0)==NormalisedTrophicVulnerability(c4))
    stopifnot(c(2,1,0)==NormalisedTrophicVulnerability(c5))
    stopifnot(c(1.5,1.5,0)==NormalisedTrophicVulnerability(c6))
    stopifnot(all.equal(56, sum(NormalisedTrophicVulnerability(TL84))))
    stopifnot(all.equal( 1, mean(NormalisedTrophicVulnerability(TL84))))
}

TestBasalNodes <- function()
{
    stopifnot(FALSE==IsBasalNode(c1))
    stopifnot(0==length(BasalNodes(c1)))
    stopifnot(all(FALSE==IsBasalNode(c2)))
    stopifnot(all(0==length(BasalNodes(c2))))
    stopifnot(all(c(TRUE, FALSE)==IsBasalNode(c3)))
    stopifnot(all('R'==BasalNodes(c3)))
    stopifnot(all(c(TRUE, FALSE,  FALSE)==IsBasalNode(c4)))
    stopifnot(all('R'==BasalNodes(c4)))
    stopifnot(all(c(TRUE, FALSE, FALSE)==IsBasalNode(c5)))
    stopifnot(all('R'==BasalNodes(c5)))
    stopifnot(all(c(TRUE, FALSE, FALSE)==IsBasalNode(c6)))
    stopifnot(all('R'==BasalNodes(c6)))
    stopifnot(all(c(TRUE, FALSE, FALSE, FALSE, FALSE)==IsBasalNode(c7)))
    stopifnot(all('A'==BasalNodes(c7)))
    stopifnot(all(c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,
                    TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                    TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,
                    TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE) == 
                    IsBasalNode(TL84)))
    stopifnot(all(c('Nostoc sp.','Arthrodesmus sp.','Cryptomonas sp. 1',
                    'Cryptomonas sp. 2','Chroococcus dispersus',
                    'Closteriopsis longissimus','Dinobryon bavaricum',
                    'Dinobryon cylindricum','Dactylococcopsis fascicularis',
                    'Dictyosphaerium pulchellum','Dinobryon sertularia',
                    'Dinobryon sociale','Glenodinium quadridens',
                    'Microcystis aeruginosa','Mallomonas sp. 1',
                    'Mallomonas sp. 2','Unclassified flagellates',
                    'Peridinium limbatum','Peridinium cinctum',
                    'Peridinium pulsillum','Peridinium wisconsinense',
                    'Chromulina sp.','Selenastrum minutum','Synedra sp.',
                    'Trachelomonas sp.') == BasalNodes(TL84)))
}

TestNonBasalNodes <- function()
{
    stopifnot(all(TRUE==IsNonBasalNode(c1)))
    stopifnot(all(1==length(NonBasalNodes(c1))))
    stopifnot(all(TRUE==IsNonBasalNode(c2)))
    stopifnot(all('S'==NonBasalNodes(c2)))
    stopifnot(all(c(FALSE,TRUE)==IsNonBasalNode(c3)))
    stopifnot(all('C'==NonBasalNodes(c3)))
    stopifnot(all(c(FALSE,TRUE,TRUE)==IsNonBasalNode(c4)))
    stopifnot(all(c('C','P')==NonBasalNodes(c4)))
    stopifnot(all(c(FALSE,TRUE,TRUE)==IsNonBasalNode(c5)))
    stopifnot(all(c('C','O')==NonBasalNodes(c5)))
    stopifnot(all(c(FALSE,TRUE,TRUE)==IsNonBasalNode(c6)))
    stopifnot(all(c('C','P')==NonBasalNodes(c6)))
    stopifnot(all(c(FALSE,TRUE,TRUE,TRUE,TRUE)==IsNonBasalNode(c7)))
    stopifnot(all(c('B','C','D','E')==NonBasalNodes(c7)))
}

TestTopLevelNodes <- function()
{
    stopifnot(all(FALSE==IsTopLevelNode(c1)))
    stopifnot(all(0==length(TopLevelNodes(c1))))
    stopifnot(all(FALSE==IsTopLevelNode(c2)))
    stopifnot(all(0==length(TopLevelNodes(c2))))
    stopifnot(all(c(FALSE,TRUE)==IsTopLevelNode(c3)))
    stopifnot(all('C'==TopLevelNodes(c3)))
    stopifnot(all(c(FALSE,FALSE,TRUE)==IsTopLevelNode(c4)))
    stopifnot(all('P'==TopLevelNodes(c4)))
    stopifnot(all(c(FALSE,FALSE,TRUE)==IsTopLevelNode(c5)))
    stopifnot(all('O'==TopLevelNodes(c5)))
    stopifnot(all(c(FALSE,FALSE,TRUE)==IsTopLevelNode(c6)))
    stopifnot(all('P'==TopLevelNodes(c6)))
    stopifnot(all(c(FALSE,FALSE,TRUE,FALSE,FALSE)==IsTopLevelNode(c7)))
    stopifnot(all('C'==TopLevelNodes(c7)))
    stopifnot(all(c(rep(FALSE,55), TRUE)==IsTopLevelNode(TL84)))
    stopifnot(all('Umbra limi'==TopLevelNodes(TL84)))
}

TestNonTopLevelNodes <- function()
{
    stopifnot(all(TRUE==IsNonTopLevelNode(c1)))
    stopifnot(all(1==length(NonTopLevelNodes(c1))))
    stopifnot(all(TRUE==IsNonTopLevelNode(c2)))
    stopifnot(all('S'==NonTopLevelNodes(c2)))
    stopifnot(all(c(TRUE,FALSE)==IsNonTopLevelNode(c3)))
    stopifnot(all('R'==NonTopLevelNodes(c3)))
    stopifnot(all(c(TRUE,TRUE,FALSE)==IsNonTopLevelNode(c4)))
    stopifnot(all(c('R','C')==NonTopLevelNodes(c4)))
    stopifnot(all(c(TRUE,TRUE,FALSE)==IsNonTopLevelNode(c5)))
    stopifnot(all(c('R','C')==NonTopLevelNodes(c5)))
    stopifnot(all(c(TRUE,TRUE,FALSE)==IsNonTopLevelNode(c6)))
    stopifnot(all(c('R','C')==NonTopLevelNodes(c6)))
    stopifnot(all(c(TRUE,TRUE,FALSE,TRUE,TRUE)==IsNonTopLevelNode(c7)))
    stopifnot(all(c('A','B','D','E')==NonTopLevelNodes(c7)))
    stopifnot(all(c(rep(TRUE,55), FALSE)==IsNonTopLevelNode(TL84)))
    stopifnot(all(c('Nostoc sp.','Arthrodesmus sp.','Asterionella formosa',
                    'Cryptomonas sp. 1','Cryptomonas sp. 2',
                    'Chroococcus dispersus','Closteriopsis longissimus',
                    'Chrysosphaerella longispina','Dinobryon bavaricum',
                    'Dinobryon cylindricum','Dactylococcopsis fascicularis',
                    'Diceras sp.','Dictyosphaerium pulchellum',
                    'Dinobryon sertularia','Dinobryon sociale',
                    'Glenodinium quadridens','Microcystis aeruginosa',
                    'Mallomonas sp. 1','Mallomonas sp. 2',
                    'Unclassified flagellates','Peridinium limbatum',
                    'Peridinium cinctum','Peridinium pulsillum',
                    'Peridinium wisconsinense','Chromulina sp.',
                    'Rhizosolenia sp.','Selenastrum minutum',
                    'Spinocosmarium sp.','Staurastrum sp.','Synedra sp.',
                    'Trachelomonas sp.','Ascomorpha eucadis','Synchaeta sp.',
                    'Bosmina longirostris','Conochilus (solitary)',
                    'Cyclops varians rubellus',
                    'Diaphanosoma leuchtenbergianum','Daphnia pulex',
                    'Filinia longispina','Conochiloides dossuarius',
                    'Gastropus stylifer','Holopedium gibberum',
                    'Kellicottia sp.','Keratella cochlearis',
                    'Keratella testudo','Leptodiaptomus siciloides',
                    'Orthocyclops modestus','Ploesoma sp.',
                    'Polyarthra vulgaris','Trichocerca multicrinis',
                    'Trichocerca cylindrica','Tropocyclops prasinus',
                    'Chaoborus punctipennis','Phoxinus eos',
                    'Phoxinus neogaeus') == NonTopLevelNodes(TL84)))
}

TestIntermediateNodes <- function()
{
    stopifnot(all(FALSE==IsIntermediateNode(c1)))
    stopifnot(all(0==length(IntermediateNodes(c1))))
    stopifnot(all(FALSE==IsIntermediateNode(c2)))
    stopifnot(all(0==length(IntermediateNodes(c2))))
    stopifnot(all(c(FALSE,FALSE)==IsIntermediateNode(c3)))
    stopifnot(all(0==length(IntermediateNodes(c3))))
    stopifnot(all(c(FALSE,TRUE,FALSE)==IsIntermediateNode(c4)))
    stopifnot(all('C'==IntermediateNodes(c4)))
    stopifnot(all(c(FALSE,TRUE,FALSE)==IsIntermediateNode(c5)))
    stopifnot(all('C'==IntermediateNodes(c5)))
    stopifnot(all(c(FALSE,TRUE,FALSE)==IsIntermediateNode(c6)))
    stopifnot(all('C'==IntermediateNodes(c6)))
    stopifnot(all(c(FALSE,TRUE,FALSE,FALSE,FALSE)==IsIntermediateNode(c7)))
    stopifnot(all('B'==IntermediateNodes(c7)))
    stopifnot(all(c(rep(FALSE,31), rep(TRUE,24), FALSE) == 
                  IsIntermediateNode(TL84)))
    stopifnot(all(c('Ascomorpha eucadis','Synchaeta sp.',
                    'Bosmina longirostris','Conochilus (solitary)',
                    'Cyclops varians rubellus',
                    'Diaphanosoma leuchtenbergianum','Daphnia pulex',
                    'Filinia longispina','Conochiloides dossuarius',
                    'Gastropus stylifer','Holopedium gibberum',
                    'Kellicottia sp.','Keratella cochlearis',
                    'Keratella testudo','Leptodiaptomus siciloides',
                    'Orthocyclops modestus','Ploesoma sp.',
                    'Polyarthra vulgaris','Trichocerca multicrinis',
                    'Trichocerca cylindrica','Tropocyclops prasinus',
                    'Chaoborus punctipennis','Phoxinus eos',
                    'Phoxinus neogaeus') == 
                  IntermediateNodes(TL84)))
}

TestIsolatedNodes <- function()
{
    stopifnot(all(TRUE==IsIsolatedNode(c1)))
    stopifnot(all('S'==IsolatedNodes(c1)))
    stopifnot(all(TRUE==IsIsolatedNode(c2)))
    stopifnot(all('S'==IsolatedNodes(c2))) # Cannibalism ignored
    stopifnot(all(c(FALSE,FALSE)==IsIsolatedNode(c3)))
    stopifnot(all(0==length(IsolatedNodes(c3))))
    stopifnot(all(c(FALSE,FALSE,FALSE)==IsIsolatedNode(c4)))
    stopifnot(all(0==length(IsolatedNodes(c4))))
    stopifnot(all(c(FALSE,FALSE,FALSE)==IsIsolatedNode(c5)))
    stopifnot(all(0==length(IsolatedNodes(c5))))
    stopifnot(all(c(FALSE,FALSE,FALSE)==IsIsolatedNode(c6)))
    stopifnot(all(0==length(IsolatedNodes(c6))))
    stopifnot(all(c(FALSE,FALSE,FALSE,TRUE,TRUE)==IsIsolatedNode(c7)))
    stopifnot(all(c('D','E')==IsolatedNodes(c7)))
    target <- rep(FALSE, 56)
    target[c(3,8,12,26,28,29)] <- TRUE
    stopifnot(all(target == IsIsolatedNode(TL84)))
    stopifnot(all(c('Asterionella formosa','Chrysosphaerella longispina',
                    'Diceras sp.', 'Rhizosolenia sp.', 'Spinocosmarium sp.',
                    'Staurastrum sp.') == IsolatedNodes(TL84)))
}

TestConnectedNodes <- function()
{
    stopifnot(all(FALSE==IsConnectedNode(c1)))
    stopifnot(all(0==length(ConnectedNodes(c1))))
    stopifnot(all(FALSE==IsConnectedNode(c2)))
    stopifnot(all(0==length(ConnectedNodes(c2)))) # Cannibalism ignored
    stopifnot(all(c(TRUE,TRUE)==IsConnectedNode(c3)))
    stopifnot(all(c('R','C')==ConnectedNodes(c3)))
    stopifnot(all(c(TRUE,TRUE,TRUE)==IsConnectedNode(c4)))
    stopifnot(all(c('R','C','P')==ConnectedNodes(c4)))
    stopifnot(all(c(TRUE,TRUE,TRUE)==IsConnectedNode(c5)))
    stopifnot(all(c('R','C','O')==ConnectedNodes(c5)))
    stopifnot(all(c(TRUE,TRUE,TRUE)==IsConnectedNode(c6)))
    stopifnot(all(c('R','C','P')==ConnectedNodes(c6)))
    stopifnot(all(c(TRUE,TRUE,TRUE,FALSE,FALSE)==IsConnectedNode(c7)))
    stopifnot(all(c('A','B','C')==ConnectedNodes(c7)))
    target <- rep(TRUE, 56)
    target[c(3,8,12,26,28,29)] <- FALSE
    stopifnot(all(target == IsConnectedNode(TL84)))
    stopifnot(all(c('Nostoc sp.','Arthrodesmus sp.','Cryptomonas sp. 1',
                    'Cryptomonas sp. 2','Chroococcus dispersus',
                    'Closteriopsis longissimus','Dinobryon bavaricum',
                    'Dinobryon cylindricum','Dactylococcopsis fascicularis',
                    'Dictyosphaerium pulchellum','Dinobryon sertularia',
                    'Dinobryon sociale','Glenodinium quadridens',
                    'Microcystis aeruginosa','Mallomonas sp. 1',
                    'Mallomonas sp. 2','Unclassified flagellates',
                    'Peridinium limbatum','Peridinium cinctum',
                    'Peridinium pulsillum','Peridinium wisconsinense',
                    'Chromulina sp.','Selenastrum minutum','Synedra sp.',
                    'Trachelomonas sp.','Ascomorpha eucadis','Synchaeta sp.',
                    'Bosmina longirostris','Conochilus (solitary)',
                    'Cyclops varians rubellus',
                    'Diaphanosoma leuchtenbergianum','Daphnia pulex',
                    'Filinia longispina','Conochiloides dossuarius',
                    'Gastropus stylifer','Holopedium gibberum',
                    'Kellicottia sp.','Keratella cochlearis',
                    'Keratella testudo','Leptodiaptomus siciloides',
                    'Orthocyclops modestus','Ploesoma sp.',
                    'Polyarthra vulgaris','Trichocerca multicrinis',
                    'Trichocerca cylindrica','Tropocyclops prasinus',
                    'Chaoborus punctipennis','Phoxinus eos',
                    'Phoxinus neogaeus','Umbra limi') == ConnectedNodes(TL84)))
}

TestFractionBasalNodes <- function()
{
    stopifnot(0==FractionBasalNodes(c1))
    stopifnot(0==FractionBasalNodes(c2))
    stopifnot(0.5==FractionBasalNodes(c3))
    stopifnot(1/3==FractionBasalNodes(c4))
    stopifnot(1/3==FractionBasalNodes(c5))
    stopifnot(1/3==FractionBasalNodes(c6))
    stopifnot(1/5==FractionBasalNodes(c7))
    stopifnot(25/56==FractionBasalNodes(TL84))
}

TestFractionNonBasalNodes <- function()
{
    stopifnot(1==FractionNonBasalNodes(c1))
    stopifnot(1==FractionNonBasalNodes(c2))
    stopifnot(0.5==FractionNonBasalNodes(c3))
    stopifnot(2/3==FractionNonBasalNodes(c4))
    stopifnot(2/3==FractionNonBasalNodes(c5))
    stopifnot(2/3==FractionNonBasalNodes(c6))
    stopifnot(4/5==FractionNonBasalNodes(c7))
    stopifnot(31/56==FractionNonBasalNodes(TL84))
}

TestFractionIntermediateNodes <- function()
{
    stopifnot(0==FractionIntermediateNodes(c1))
    stopifnot(0==FractionIntermediateNodes(c2))
    stopifnot(0==FractionIntermediateNodes(c3))
    stopifnot(1/3==FractionIntermediateNodes(c4))
    stopifnot(1/3==FractionIntermediateNodes(c5))
    stopifnot(1/3==FractionIntermediateNodes(c6))
    stopifnot(1/5==FractionIntermediateNodes(c7))
    stopifnot(24/56==FractionIntermediateNodes(TL84))
}

TestFractionTopLevelNodes <- function()
{
    stopifnot(0==FractionTopLevelNodes(c1))
    stopifnot(0==FractionTopLevelNodes(c2))
    stopifnot(0.5==FractionTopLevelNodes(c3))
    stopifnot(1/3==FractionTopLevelNodes(c4))
    stopifnot(1/3==FractionTopLevelNodes(c5))
    stopifnot(1/3==FractionTopLevelNodes(c6))
    stopifnot(1/5==FractionTopLevelNodes(c7))
    stopifnot(1/56==FractionTopLevelNodes(TL84))
}

TestFractionNonTopLevelNodes <- function()
{
    stopifnot(1==FractionNonTopLevelNodes(c1))
    stopifnot(1==FractionNonTopLevelNodes(c2))
    stopifnot(0.5==FractionNonTopLevelNodes(c3))
    stopifnot(2/3==FractionNonTopLevelNodes(c4))
    stopifnot(2/3==FractionNonTopLevelNodes(c5))
    stopifnot(2/3==FractionNonTopLevelNodes(c6))
    stopifnot(4/5==FractionNonTopLevelNodes(c7))
    stopifnot(55/56==FractionNonTopLevelNodes(TL84))
}

TestFractionIsolatedNodes <- function()
{
    stopifnot(1==FractionIsolatedNodes(c1))
    stopifnot(1==FractionIsolatedNodes(c2))
    stopifnot(0==FractionIsolatedNodes(c3))
    stopifnot(0==FractionIsolatedNodes(c4))
    stopifnot(0==FractionIsolatedNodes(c5))
    stopifnot(0==FractionIsolatedNodes(c6))
    stopifnot(2/5==FractionIsolatedNodes(c7))
    stopifnot(6/56==FractionIsolatedNodes(TL84))
}

TestFractionConnectedNodes <- function()
{
    stopifnot(0==FractionConnectedNodes(c1))
    stopifnot(0==FractionConnectedNodes(c2))
    stopifnot(1==FractionConnectedNodes(c3))
    stopifnot(1==FractionConnectedNodes(c4))
    stopifnot(1==FractionConnectedNodes(c5))
    stopifnot(1==FractionConnectedNodes(c6))
    stopifnot(3/5==FractionConnectedNodes(c7))
    stopifnot(50/56==FractionConnectedNodes(TL84))
}

TestFractions <- function()
{
    # These should sum to 1
    for(community in list(c1,c2,c3,c4,c5,c6,c7,TL84))
    {
        stopifnot(1==sum(FractionBasalNodes(community), 
                         FractionIntermediateNodes(community), 
                         FractionTopLevelNodes(community), 
                         FractionIsolatedNodes(community)))

        stopifnot(1==sum(FractionConnectedNodes(community), 
                         FractionIsolatedNodes(community)))

        stopifnot(1==sum(FractionBasalNodes(community), 
                         FractionNonBasalNodes(community)))

        stopifnot(1==sum(FractionTopLevelNodes(community), 
                         FractionNonTopLevelNodes(community)))
    }
}

TestResourcesByNode <- function()
{
    stopifnot(identical(list(S=vector('character')), ResourcesByNode(c1)))
    stopifnot(identical(list(S='S'), ResourcesByNode(c2)))
    stopifnot(identical(list(R=vector('character'), C='R'), 
                        ResourcesByNode(c3)))
    stopifnot(identical(list(R=vector('character'), C='R', P='C'), 
                        ResourcesByNode(c4)))
    stopifnot(identical(list(R=vector('character'), C='R', O=c('R','C')), 
                        ResourcesByNode(c5)))
    stopifnot(identical(list(R=vector('character'), C='R', P='C'), 
                        ResourcesByNode(c6)))
}

TestResourcesOfNodes <- function()
{
    stopifnot(identical(vector('character'), ResourcesOfNodes(c1, 'S')))
    stopifnot(identical('S', ResourcesOfNodes(c2, 'S')))
    stopifnot(identical(vector('character'), ResourcesOfNodes(c3, 'R')))
    stopifnot(identical('R', ResourcesOfNodes(c3, 'C')))
    stopifnot(identical(vector('character'), ResourcesOfNodes(c4, 'R')))
    stopifnot(identical('R', ResourcesOfNodes(c4, 'C')))
    stopifnot(identical('C', ResourcesOfNodes(c4, 'P')))

    # More than one node
    stopifnot(identical(list(R=vector('character'), C='R'), 
                        ResourcesOfNodes(c4, c('R','C'))))
    stopifnot(identical(list(R=vector('character'), C='R'), 
                        ResourcesOfNodes(c4, 1:2)))

    # Bad nodes
    F(ResourcesOfNodes(c1, 0))
    F(ResourcesOfNodes(c1, 2))
    F(ResourcesOfNodes(c1, ''))
    F(ResourcesOfNodes(c1, 'x'))
}

TestConsumersByNode <- function()
{
    stopifnot(identical(list(S=vector('character')), ConsumersByNode(c1)))
    stopifnot(identical(list(S='S'), ConsumersByNode(c2)))
    stopifnot(identical(list(R='C', C=vector('character')), 
                        ConsumersByNode(c3)))
    stopifnot(identical(list(R='C', C='P', P=vector('character')), 
                        ConsumersByNode(c4)))
    stopifnot(identical(list(R=c('C','O'), C='O', O=vector('character')), 
                        ConsumersByNode(c5)))
    stopifnot(identical(list(R='C', C='P', P=vector('character')), 
                        ConsumersByNode(c6)))
}

TestConsumersOfNodes <- function()
{
    stopifnot(identical(vector('character'), ConsumersOfNodes(c1, 'S')))
    stopifnot(identical('S', ConsumersOfNodes(c2, 'S')))
    stopifnot(identical('C', ConsumersOfNodes(c3, 'R')))
    stopifnot(identical(vector('character'), ConsumersOfNodes(c3, 'C')))
    stopifnot(identical('C', ConsumersOfNodes(c4, 'R')))
    stopifnot(identical('P', ConsumersOfNodes(c4, 'C')))
    stopifnot(identical(vector('character'), ConsumersOfNodes(c4, 'P')))

    # More than one node
    stopifnot(identical(list(R='C', C='P'), 
                        ConsumersOfNodes(c4, c('R','C'))))
    stopifnot(identical(list(R='C', C='P'), 
                        ConsumersOfNodes(c4, 1:2)))

    # Bad nodes
    F(ConsumersOfNodes(c1, 0))
    F(ConsumersOfNodes(c1, 2))
    F(ConsumersOfNodes(c1, ''))
    F(ConsumersOfNodes(c1, 'x'))
}

TestNumberOfTrophicLinks <- function()
{
    stopifnot(0==NumberOfTrophicLinks(c1))
    stopifnot(1==NumberOfTrophicLinks(c2))
    stopifnot(1==NumberOfTrophicLinks(c3))
    stopifnot(2==NumberOfTrophicLinks(c4))
    stopifnot(3==NumberOfTrophicLinks(c5))
    stopifnot(2==NumberOfTrophicLinks(c6))
    stopifnot(5==NumberOfTrophicLinks(c7))
    stopifnot(5==NumberOfTrophicLinks(c7))
    stopifnot(269==NumberOfTrophicLinks(TL84))
    stopifnot(264==NumberOfTrophicLinks(RemoveCannibalisticLinks(TL84)))
}

TestLinkageDensity <- function()
{
    stopifnot(0==LinkageDensity(c1))
    stopifnot(1==LinkageDensity(c2))
    stopifnot(0.5==LinkageDensity(c3))
    stopifnot(2/3==LinkageDensity(c4))
    stopifnot(1==LinkageDensity(c5))
    stopifnot(all.equal(4.80357142857142882519, LinkageDensity(TL84)))
}

TestDegree <- function()
{
    stopifnot(0==Degree(c1))
    stopifnot(2==Degree(c2))
    stopifnot(all(InDegree(c2)+OutDegree(c2) == Degree(c2)))
    stopifnot(all(c(1,1)==Degree(c3)))
    stopifnot(all(InDegree(c3)+OutDegree(c3) == Degree(c3)))
    stopifnot(all(c(1,2,1)==Degree(c4)))
    stopifnot(all(InDegree(c4)+OutDegree(c4) == Degree(c4)))
    stopifnot(all(c(2,2,2)==Degree(c5)))
    stopifnot(all(InDegree(c5)+OutDegree(c5) == Degree(c5)))
    stopifnot(all(c(4,3,0,18,5,18,3,0,1,4,4,0,5,2,5,6,1,2,3,18,1,2,5,1,
                    18,0,18,0,0,1,18,11,11,15,11,23,18,32,10,11,10,22,10,
                    11,10,23,22,10,10,10,10,23,26,10,10,13) == Degree(TL84)))
    stopifnot(all(InDegree(TL84)+OutDegree(TL84) == Degree(TL84)))
}

TestDegreeDistribution <- function()
{
    stopifnot(identical(c('0'=1), DegreeDistribution(c1)))
    stopifnot(identical(c('0'=0, '1'=0, '2'=1), DegreeDistribution(c2)))
    stopifnot(identical(c('0'=0, '1'=1), DegreeDistribution(c3)))
    stopifnot(identical(c('0'=0, '1'=2/3, '2'=1/3), DegreeDistribution(c4)))
    stopifnot(identical(c('0'=0, '1'=0, '2'=1), DegreeDistribution(c5)))
    stopifnot(identical(c('0'=0, '1'=2/3, '2'=1/3), DegreeDistribution(c6)))
}

TestDirectedConnectance <- function()
{
    stopifnot(0==DirectedConnectance(c1))
    stopifnot(1==DirectedConnectance(c2))
    stopifnot(0.25==DirectedConnectance(c3))
    stopifnot(all.equal(2/9, DirectedConnectance(c4)))
    stopifnot(all.equal(1/3, DirectedConnectance(c5)))
    stopifnot(all.equal(0.08577806122448979054, DirectedConnectance(TL84)))
}

TestCannibals <- function()
{
    stopifnot(FALSE==IsCannibal(c1))
    stopifnot(0==length(Cannibals(c1)))
    stopifnot(TRUE==IsCannibal(c2))
    stopifnot('S'==Cannibals(c2))
    stopifnot(all(rep(FALSE, 2)==IsCannibal(c3)))
    stopifnot(0==length(Cannibals(c3)))
    stopifnot(all(rep(FALSE, 3)==IsCannibal(c4)))
    stopifnot(0==length(Cannibals(c4)))
    stopifnot(all(rep(FALSE, 3)==IsCannibal(c5)))
    stopifnot(0==length(Cannibals(c5)))
    stopifnot(all(c(TRUE,FALSE,TRUE,TRUE,FALSE)==IsCannibal(c7)))
    stopifnot(all(c('A','C','D')==Cannibals(c7)))
    stopifnot(all(c("Cyclops varians rubellus", 
                    "Orthocyclops modestus",
                    "Tropocyclops prasinus",
                    "Chaoborus punctipennis", 
                    "Umbra limi") == Cannibals(TL84)))
}

TestTrophicChainsStats <- function()
{
    stopifnot(is.null(TrophicChainsStats(c1)))
    stopifnot(is.null(TrophicChainsStats(c2)))
    stopifnot(1==TrophicChainsStats(c3)$chain.lengths)
    stopifnot(c(1,0,0,1) == TrophicChainsStats(c3)$node.pos.counts)
    stopifnot(2==TrophicChainsStats(c4)$chain.lengths)
    stopifnot(c(1,0,0,0,1,0,0,0,1) == 
              TrophicChainsStats(c4)$node.pos.counts)
    stopifnot(c(1,2)==TrophicChainsStats(c5)$chain.lengths)
    stopifnot(c(2,0,0,0,1,1,0,0,1) == 
              TrophicChainsStats(c5)$node.pos.counts)
    stopifnot(2==TrophicChainsStats(c6)$chain.lengths)
    stopifnot(c(1,0,0,0,1,0,0,0,1) == 
              TrophicChainsStats(c6)$node.pos.counts)
    stopifnot(2==TrophicChainsStats(c7)$chain.lengths)
    stopifnot(c(1,0,0,0,0,0,1,0,0,0,0,0,1,0,0) == 
              TrophicChainsStats(c7)$node.pos.counts)

    # Simpler tests for larger communities
    #                    Mean chain length       n.chains, longest sum pos count
    expected <- matrix(c(4.83500334001336007361,    5988,   8,         34940, 
                         4.84470882905447730593,    1597,   7,          9334, 
                         5.08319118225954635903,    7621,  10,         46360, 
                         10.4850243487305565537, 2538120,  17,      29150370, 
                         5.63949610967024828057,    5398,  10,         35840), 
                       ncol=4, byrow=TRUE)
    communities <- list(TL84, TL86, YthanEstuary, SkipwithPond, 
                          BroadstoneStream)
    for(i in 1:length(communities))
    {
        s <- TrophicChainsStats(communities[[i]])
        actual <- c(mean(s$chain.lengths), length(s$chain.length), 
                    ncol(s$node.pos.counts),
                    sum(s$node.pos.counts))
        stopifnot(all.equal(actual, expected[i,]))
    }
}

TestTrophicChainsStatsOverflow <- function()
{
    community <- TL84
    alist <- cheddar:::.CAdjacencyList(community, ConsumersByNode(community))

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
              as.integer(1),
              n.chains=n.chains,
              longest=longest,
              status=status, 
              PACKAGE='cheddar', NAOK=TRUE, DUP=FALSE)

    stopifnot(-1==res$status)
}

TestTrophicChains <- function()
{
    CheckChains <- function(community, chains)
    {
        # Helper function
        # Chains of length 1 would indicate isolated node
        stopifnot(0==which(1==apply(chains, 1, 
                                    function(r) length(which(''!=r)))))

        # Should be no duplicated rows
        stopifnot(!any(duplicated(chains, MARGIN=1)))

        # No node should appear more than once in a chain
        stopifnot(!any(apply(chains, 1, function(r) any(duplicated(r[''!=r])))))

        # Each chain should start with a basal node
        stopifnot(all(chains[,1] %in% BasalNodes(community)))

        # Each chain should stop with a node with no consumers
        consumers <- ConsumersByNode(community)
        apply(chains, 1, function(chain)
        {
            last.in.chain <- tail(which(''!=chain), 1)
            last.in.chain <- chain[last.in.chain]
            stopifnot(0!=length(consumers[last.in.chain]))
        })

        # Make sure that every link in every chain appears in the predation 
        # matrix
        pm <- PredationMatrix(community)
        apply(chains, 1, function(chain)
        {
            last.in.chain <- tail(which(''!=chain), 1)
            from <- chain[1:(last.in.chain-1)]
            to <- chain[2:last.in.chain]
            stopifnot(all(1==diag(pm[from,to])))
        })

        # How could this not be the case?
        stopifnot(ncol(chains)<=NumberOfNodes(community))
    }

    # Some extra test cases
    test1 <- Community(nodes=data.frame(node=c('A', 'B', 'C')), 
                       trophic.links=cbind(resource=c('A', 'B', 'C'), 
                                           consumer=c('B', 'C', 'B')), 
                      properties=list(title='test1'))

    test2 <- Community(nodes=data.frame(node=c('A', 'B', 'C')), 
                       trophic.links=cbind(resource=c('A', 'B', 'A'), 
                                           consumer=c('B', 'C', 'C')), 
                       properties=list(title='test2'))


    communities <- list(c1, c2, c3, c4, c5, c6, c7, test1, test2)
    expected <- list(NULL, 
                     NULL, 
                     c('R','C'), 
                     c('R','C','P'), 
                     matrix(c('R','O','', 'R','C','O'), nrow=2, byrow=TRUE), 
                     c('R','C','P'), 
                     c('A','B','C'),
                     c('A', 'B', 'C'), 
                     matrix(c('A','C','', 'A','B','C'), nrow=2, byrow=TRUE))
    for(index in 1:length(communities))
    {
        chains <- TrophicChains(communities[[index]])
        stopifnot(all(expected[[index]]==chains))
        if(!is.null(chains))
        {
            CheckChains(communities[[index]], chains)
        }
    }

    # Webs with many nodes - don't check each link, just dims and one 
    # expected chains
    communities <- list(TL84, TL86, YthanEstuary, BroadstoneStream)
    expected.dims <- list(c(5988,8), c(1597,7), c(7621, 10), c(5398,  10))

    # These expected chains are the first found by TrophicChains() at the 
    # time of writing.
    expected.chain <- list(
                c('Nostoc sp.','Diaphanosoma leuchtenbergianum','Umbra limi'), 
                c('Ankyra judayi','Daphnia rosea','Chaoborus punctipennis',
                  'Micropterus salmoides'),
                c('Diatoms','Crangon crangon','Somateria mollissima'),
                c('Potamophylax cingulatus', 'Cordulegaster boltonii'))
    for(index in 1:length(communities))
    {
        chains <- TrophicChains(communities[[index]])
        stopifnot(all(expected.dims[[index]]==dim(chains)))
        expected <- expected.chain[[index]]
        stopifnot(all(chains[1,1:length(expected)] == expected))
        CheckChains(communities[[index]], chains)
    }

    # Skipwith Pond has over 2.5 million chains. Computing chains my 32-bit 
    # Windows machine.
    if(FALSE)
    {
        # Don't run all checks
        chains <- TrophicChains(SkipwithPond)
        stopifnot(all(dim(chains) == c(2538120,17)))
        stopifnot(all(chains[1,1:3] == 
                  c('Detritus','Small oligochaetes (principally Enchytraeidae)',
                    'Polycelis tenuis')))
    }
}

TestThreeNodeChains <- function()
{
    CheckChains <- function(community, chains)
    {
        # Helper function
        # Sanity checks
        # Should be no duplicated rows
        stopifnot(!any(duplicated(chains, MARGIN=1)))

        # No node should appear more than once in a chain except as bottom and 
        # top.
        stopifnot(!any(apply(chains, 1, function(r) r[1]==r[2] | r[2]==r[3])))

        # Make sure that every link in every chain appears in the predation 
        # matrix
        pm <- PredationMatrix(community)
        apply(chains, 1, function(chain)
        {
            from <- head(chain, -1)
            to <- tail(chain, -1)
            stopifnot(all(1==diag(pm[from,to])))
        })
    }

    # Some simple cases
    for(community in list(c1,c2,c3))
    {    
        stopifnot(is.null(ThreeNodeChains(community)))
        stopifnot(is.null(ThreeNodeChains(community, exclude.loops=TRUE)))
    }

    communities <- list(c4, c5, c6, c7)
    expected <- list(c('R','C','P'), 
                     c('R','C','O'), 
                     c('R','C','P'),
                     c('A','B','C'))
    for(index in 1:length(communities))
    {
        chains <- ThreeNodeChains(communities[[index]])
        stopifnot(all(expected[[index]]==chains))
        CheckChains(communities[[index]], chains)

        chains <- ThreeNodeChains(communities[[index]], exclude.loops=TRUE)
        stopifnot(all(expected[[index]]==chains))
        CheckChains(communities[[index]], chains)
    }

    # Webs with many nodes - don't check each link, just dims
    communities <- list(TL84, TL86, YthanEstuary, SkipwithPond)
    expected <- list(list(c(1044,3), c(1042,3)), 
                     list(c( 651,3), c( 645,3)),
                     list(c(1616,3), c(1616,3)),
                     list(c(2433,3), c(2401,3)))
    for(index in 1:length(communities))
    {
        chains <- ThreeNodeChains(communities[[index]])
        stopifnot(all(expected[[index]][[1]]==dim(chains)))
        CheckChains(communities[[index]], chains)

        chains <- ThreeNodeChains(communities[[index]], exclude.loops=TRUE)
        stopifnot(all(expected[[index]][[2]]==dim(chains)))
        CheckChains(communities[[index]], chains)
    }
}

TestChainLength <- function(chains)
{
    stopifnot(is.null(ChainLength(TrophicChains(c1))))
    stopifnot(is.null(ChainLength(TrophicChains(c2))))
    stopifnot(1==ChainLength(TrophicChains(c3)))
    stopifnot(2==ChainLength(TrophicChains(c4)))
    stopifnot(all(c(1,2)==ChainLength(TrophicChains(c5))))
    stopifnot(2==ChainLength(TrophicChains(c6)))
    stopifnot(all.equal(4.83500334001336007361, 
                        mean(ChainLength(TrophicChains(TL84)))))

    stopifnot(all(2 == ChainLength(ThreeNodeChains(c4))))
    stopifnot(all(2 == ChainLength(ThreeNodeChains(c5))))
    stopifnot(all(2 == ChainLength(ThreeNodeChains(c6))))
    stopifnot(all(2 == ChainLength(ThreeNodeChains(TL84))))
}

TestTrophicHeight <- function()
{
    stopifnot(is.na(TrophicHeight(c1, include.isolated=TRUE)))
    stopifnot(is.na(TrophicHeight(c1, include.isolated=FALSE)))
    stopifnot(is.na(TrophicHeight(c2, include.isolated=TRUE)))
    stopifnot(is.na(TrophicHeight(c2, include.isolated=FALSE)))
    stopifnot(all(c(1,2)==TrophicHeight(c3, include.isolated=TRUE)))
    stopifnot(all(c(1,2)==TrophicHeight(c3, include.isolated=FALSE)))
    stopifnot(all(c(1,2,3)==TrophicHeight(c4, include.isolated=TRUE)))
    stopifnot(all(c(1,2,3)==TrophicHeight(c4, include.isolated=FALSE)))
    stopifnot(all(c(1,2,2.5)==TrophicHeight(c5, include.isolated=TRUE)))
    stopifnot(all(c(1,2,2.5)==TrophicHeight(c5, include.isolated=FALSE)))
    stopifnot(all(c(1,2,3)==TrophicHeight(c6, include.isolated=TRUE)))
    stopifnot(all(c(1,2,3)==TrophicHeight(c6, include.isolated=FALSE)))
    stopifnot(all(c(1,2,3,1,1)==TrophicHeight(c7, include.isolated=TRUE)))
    stopifnot(all.equal(c(1,2,3,NA,NA), 
                        unname(TrophicHeight(c7, include.isolated=FALSE))))

    # Lower tolerance required for Mac Pro in GIS lab.
    stopifnot(all.equal(c(rep(1,31),2,2,2,2,10/3,2,2.41860465116279055309,2,2,
                          2,2,2,2,2,2,4.2,2,2,2,2,10/3,
                          4.60252672497570447518, 5.16833667334669311089, 
                          5.16833667334669311089, 5.83500334001336007361), 
                        unname(TrophicHeight(TL84)), 
                        tolerance=1e-6))
    stopifnot(all.equal(c(1,1,NA,1,1,1,1,NA,1,1,1,NA,rep(1,13),NA,1,NA,NA,1,1,
                          2,2,2,2,10/3,2,2.41860465116279055309,2,2,
                          2,2,2,2,2,2,4.2,2,2,2,2,10/3,
                          4.60252672497570447518, 5.16833667334669311089, 
                          5.16833667334669311089, 5.83500334001336007361), 
                        unname(TrophicHeight(TL84, include.isolated=FALSE)), 
                        tolerance=1e-6))
}

TestTrophicLevel <- function()
{
    stopifnot(is.na(PreyAveragedTrophicLevel(c1)))
    stopifnot(is.na(PreyAveragedTrophicLevel(c2)))
    stopifnot(all(c(1,2)==PreyAveragedTrophicLevel(c3)))
    stopifnot(all(c(1,2,3)==PreyAveragedTrophicLevel(c4)))
    stopifnot(all(c(1,2,2.5)==PreyAveragedTrophicLevel(c5)))
    stopifnot(all.equal(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                          1,1,1,1,1,1,1,2,2,2,2,3.14285714285714,2,
                          2.10714285714286,2,2,2,2,2,2,2,2,3.20535714285714,
                          2,2,2,2,3.14285714285714,3.17134353741497,
                          3.52995086923658,3.52995086923658,
                          3.80267814196386), 
              unname(PreyAveragedTrophicLevel(TL84))))


    stopifnot(is.na(PreyAveragedTrophicLevel(c1, include.isolated=FALSE)))
    stopifnot(is.na(PreyAveragedTrophicLevel(c2, include.isolated=FALSE)))
    stopifnot(all(c(1,2)==PreyAveragedTrophicLevel(c3, include.isolated=FALSE)))
    stopifnot(all(c(1,2,3) == 
                  PreyAveragedTrophicLevel(c4, include.isolated=FALSE)))
    stopifnot(all(c(1,2,2.5) == 
                  PreyAveragedTrophicLevel(c5, include.isolated=FALSE)))

    # c7 is not energetically feasible so all trophic levels are NA
    stopifnot(all(is.na(PreyAveragedTrophicLevel(c7, include.isolated=FALSE))))
    stopifnot(all(is.na(PreyAveragedTrophicLevel(c7, include.isolated=TRUE))))

    stopifnot(all.equal(c(1,1,NA,1,1,1,1,NA,1,1,1,NA,1,1,1,1,1,1,1,1,1,1,1,1,
                          1,NA,1,NA,NA,1,1,2,2,2,2,3.14285714285714,2,
                          2.10714285714286,2,2,2,2,2,2,2,2,3.20535714285714,
                          2,2,2,2,3.14285714285714,3.17134353741497,
                          3.52995086923658,3.52995086923658,
                          3.80267814196386), 
                unname(PreyAveragedTrophicLevel(TL84, include.isolated=FALSE))))
}

TestTrophicSpecies <- function()
{
    stopifnot(is.na(TrophicSpecies(c1, include.isolated=FALSE)))
    stopifnot(1==TrophicSpecies(c1, include.isolated=TRUE))
    stopifnot(is.na(TrophicSpecies(c2, include.isolated=FALSE)))
    stopifnot(1==TrophicSpecies(c2, include.isolated=TRUE))
    stopifnot(all(c(1,2)==TrophicSpecies(c3, include.isolated=FALSE)))
    stopifnot(all(c(1,2)==TrophicSpecies(c3, include.isolated=TRUE)))
    stopifnot(all(c(1,2,3)==TrophicSpecies(c4, include.isolated=FALSE)))
    stopifnot(all(c(1,2,3)==TrophicSpecies(c4, include.isolated=TRUE)))
    stopifnot(all(c(1,2,3)==TrophicSpecies(c5, include.isolated=FALSE)))
    stopifnot(all(c(1,2,3)==TrophicSpecies(c5, include.isolated=TRUE)))

    # From Jonsson et al 2005 AER. Isolated species assigned NA.
    target <- c(1,2,NA,3,4,3,5,NA,6,1,1,NA,4,7,4,8,6,7,2,3,6,7,
                4,6,3,NA,3,NA,NA,6,3,9,9,10,11,12,13,14,15,11,
                15,16,15,9,15,17,18,15,15,15,15,12,19,20,20,21)
    test <- unname(TrophicSpecies(TL84, include.isolated=FALSE))
    stopifnot(all(target == test | (is.na(target) & is.na(test))))

    # Isolated species included - these are given a trophic level of 1.
    target <- c(1,2,3,4,5,4,6,3,7,1,1,3,5,8,5,9,7,8,2,4,7,8,5,7,
                4,3,4,3,3,7,4,10,10,11,12,13,14,15,16,12,16,17,16,
                10,16,18,19,16,16,16,16,13,20,21,21,22)
    test <- unname(TrophicSpecies(TL84, include.isolated=TRUE))
    stopifnot(all(target == test))
}

TestTrophicLinksForNodes <- function()
{
    stopifnot(is.null(TrophicLinksForNodes(c1, 'S')))
    stopifnot(data.frame(resource='S', consumer='S') == 
              TrophicLinksForNodes(c2, 'S'))

    stopifnot(data.frame(resource='R', consumer='C') == 
              TrophicLinksForNodes(c3, 'R'))
    stopifnot(data.frame(resource='R', consumer='C') == 
              TrophicLinksForNodes(c3, 'C'))
    stopifnot(data.frame(resource='R', consumer='C') == 
              TrophicLinksForNodes(c3, c('R','C')))

    stopifnot(data.frame(resource='R', consumer='C') == 
              TrophicLinksForNodes(c4, 'R'))
    stopifnot(data.frame(resource=c('R','C'), consumer=c('C','P')) == 
              TrophicLinksForNodes(c4, 'C'))
    stopifnot(data.frame(resource='C', consumer='P') == 
              TrophicLinksForNodes(c4, 'P'))

    stopifnot(data.frame(resource=c('R','C'), consumer=c('C','P')) == 
              TrophicLinksForNodes(c4, c('R','C','P')))
}

TestResolveTrophicLinksToNodeNames <- function()
{
    f <- cheddar:::.ResolveTrophicLinksToNodeNames
    stopifnot(all(c('S', 'S') == f(c1, data.frame(resource='S', consumer='S'))))
    stopifnot(all(c('S', 'S') == f(c1, data.frame(resource=1, consumer='S'))))
    stopifnot(all(c('S', 'S') == f(c1, data.frame(resource=1, consumer=1))))
    stopifnot(all(c('S', 'S') == f(c1, data.frame(resource='S', consumer=1))))
    F(f(c3, data.frame(resource='x', consumer='R')))
    F(f(c3, data.frame(resource='R', consumer='')))
    F(f(c3, matrix(c(0, 1), ncol=2)))
    F(f(c3, matrix(c(0, 1), ncol=2)))
}

TestResolveTrophicLinksToRowIndices <- function()
{
    f <- cheddar:::.ResolveTrophicLinksToRowIndices
    stopifnot(is.na(f(c1, data.frame(resource='S', consumer='S'))))
    stopifnot(is.na(f(c1, data.frame(resource=1, consumer=1))))
    stopifnot(1==f(c2, data.frame(resource='S', consumer='S')))
    stopifnot(1==f(c2, data.frame(resource=1, consumer=1)))
    stopifnot(1==f(c3, data.frame(resource='R', consumer='C')))
    stopifnot(is.na(f(c3, data.frame(resource='R', consumer='R'))))
    stopifnot(is.na(f(c3, data.frame(resource='C', consumer='C'))))
    stopifnot(is.na(f(c3, data.frame(resource='C', consumer='R'))))
    stopifnot(all.equal(c(1,NA,NA,NA), 
                        f(c3, data.frame(resource=c('R','R','C','C'), 
                                         consumer=c('C','R','C','R')))))
    F(f(c3, data.frame(resource='x', consumer='R')))
    F(f(c3, data.frame(resource='R', consumer='')))
    F(f(c3, matrix(resource=0, consumer=1)))
    F(f(c3, matrix(resource=1, consumer=0)))
}

TestDoLinksExist <- function()
{
    DoLinksExist <- cheddar:::.DoLinksExist
    stopifnot(!DoLinksExist(c1, data.frame(resource='S', consumer='S')))
    stopifnot(!DoLinksExist(c1, data.frame(resource=1, consumer=1)))
    stopifnot(DoLinksExist(c2, data.frame(resource='S', consumer='S')))
    stopifnot(DoLinksExist(c2, data.frame(resource=1, consumer=1)))
    stopifnot(DoLinksExist(c3, data.frame(resource='R', consumer='C')))
    stopifnot(!DoLinksExist(c3, data.frame(resource='R', consumer='R')))
    stopifnot(!DoLinksExist(c3, data.frame(resource='C', consumer='C')))
    stopifnot(!DoLinksExist(c3, data.frame(resource='C', consumer='R')))
    stopifnot(all(c(TRUE,FALSE,FALSE,FALSE) == 
                  DoLinksExist(c3, data.frame(resource=c('R','R','C','C'), 
                                              consumer=c('C','R','C','R'))))) 
}

TestRemoveCannibalisticLinks <- function()
{
    stopifnot(0==NumberOfTrophicLinks(c1))
    stopifnot(0==NumberOfTrophicLinks(RemoveCannibalisticLinks(c1)))
    stopifnot(1==NumberOfTrophicLinks(c2))
    stopifnot(0==NumberOfTrophicLinks(RemoveCannibalisticLinks(c2)))
    stopifnot(1==NumberOfTrophicLinks(RemoveCannibalisticLinks(c3)))
    stopifnot(2==NumberOfTrophicLinks(RemoveCannibalisticLinks(c4)))
    stopifnot(3==NumberOfTrophicLinks(RemoveCannibalisticLinks(c5)))
    stopifnot(2==NumberOfTrophicLinks(RemoveCannibalisticLinks(c7)))
    stopifnot(269==NumberOfTrophicLinks(TL84))
    stopifnot(5==length(Cannibals(TL84)))
    stopifnot(264==NumberOfTrophicLinks(RemoveCannibalisticLinks(TL84)))
    stopifnot(0==length(Cannibals(RemoveCannibalisticLinks(TL84))))
}

TestShortestPathLengths <- function()
{
    stopifnot(0==ShortestPaths(c1))
    stopifnot(0==ShortestPaths(c2))
    stopifnot(matrix(c(0,1, 1,0), ncol=2)==ShortestPaths(c3))
    stopifnot(matrix(c(0,1,2, 1,0,1, 2,1,0), ncol=3)==ShortestPaths(c4))
    stopifnot(matrix(c(0,1,1, 1,0,1, 1,1,0), ncol=3)==ShortestPaths(c5))
    stopifnot(matrix(c(0,1,2, 1,0,1, 2,1,0), ncol=3)==ShortestPaths(c6))
}

TestSumDietGap <- function()
{
    # Test the C function directly
    F <- function(pm, expected)
    {
        n <- ncol(pm)
        sum_diet_gaps <- as.integer(0)
        status <- as.integer(-1)

        res <- .C('sum_diet_gaps', 
                  as.integer(pm), 
                  as.integer(n), 
                  as.integer((1:n) - 1),   # 0-indexed
                  sum_diet_gaps=sum_diet_gaps, 
                  status=status)
        if(res$sum_diet_gaps!=expected)
        {
            stop(paste('Incorrect cost. Expected [', expected, '] received ', 
                       '[', res$sum_diet_gaps, ']', sep=''))
        }

        return (res)
    }

    F(matrix(0, ncol=1), 0)
    F(matrix(1, ncol=1), 0)
    F(matrix(c(0,0,0,0), ncol=2), 0)
    F(matrix(c(1,1,1,1), ncol=2), 0)
    F(matrix(c(1,0,1,0), ncol=2), 0)
    F(matrix(c(0,1,0,1), ncol=2), 0)

    F(matrix(c(1, 1, 0, 
               0, 1, 1,
               1, 0, 1), ncol=3, byrow=TRUE), 1)
    F(matrix(c(1, 1, 0, 
               0, 0, 1,
               1, 1, 1), ncol=3, byrow=TRUE), 2)
    F(matrix(c(1, 1, 1, 
               0, 0, 0,
               1, 1, 1), ncol=3, byrow=TRUE), 3)
    F(matrix(c(1, 1, 1, 
               0, 0, 0,
               0, 0, 0), ncol=3, byrow=TRUE), 0)
    F(matrix(c(1, 1, 1, 
               0, 1, 1,
               0, 0, 1), ncol=3, byrow=TRUE), 0)
    F(matrix(c(0, 0, 1, 
               0, 1, 1,
               1, 1, 1), ncol=3, byrow=TRUE), 0)

    F(matrix(c(1, 0, 0, 1, 
               1, 0, 0, 0,
               0, 0, 0, 1,
               1, 0, 0, 1), ncol=4, byrow=TRUE), 2)

    F(matrix(c(1, 1, 0, 0, 0, 0, 
               1, 1, 1, 1, 0, 0, 
               1, 1, 1, 1, 0, 0, 
               0, 0, 0, 0, 0, 0, 
               1, 1, 1, 1, 0, 0, 
               0, 1, 0, 1, 0, 0), ncol=6, byrow=TRUE), 4)

    F(matrix(c(1, 1, 0, 1, 1, 1, 
               0, 1, 1, 1, 0, 0, 
               0, 1, 1, 1, 1, 1, 
               0, 1, 1, 0, 1, 1, 
               0, 0, 0, 1, 1, 1, 
               1, 1, 1, 1, 0, 1), ncol=6, byrow=TRUE), 9)

    F(matrix(c(0, 0, 0, 0, 0, 0, 
               0, 0, 0, 1, 0, 1, 
               0, 0, 1, 1, 1, 1, 
               0, 1, 1, 1, 1, 1, 
               1, 1, 1, 1, 1, 1, 
               0, 1, 0, 0, 0, 1), ncol=6, byrow=TRUE), 0)

    F(matrix(c(0, 0, 0, 0, 0, 0, 
               0, 0, 0, 1, 0, 1, 
               0, 0, 1, 0, 1, 0, 
               0, 1, 1, 0, 0, 0, 
               1, 1, 1, 1, 1, 0, 
               0, 1, 0, 0, 0, 1), ncol=6, byrow=TRUE), 6)

    F(matrix(c(1, 0, 1, 0, 1, 0, 
               0, 1, 0, 1, 0, 1, 
               1, 0, 1, 0, 1, 0, 
               0, 1, 0, 1, 0, 1, 
               1, 0, 1, 0, 1, 0, 
               0, 1, 0, 1, 0, 1), ncol=6, byrow=TRUE), 12)

    F(matrix(c(0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 1), ncol=6, byrow=TRUE), 0)

    F(PredationMatrix(TL84), 424)

    # Test the R function
    stopifnot(SumDietGaps(TL84)==424)
}

TestMinimiseSumDietGaps <- function()
{
    # Can't test for exact values because simulated annealing learning is 
    # stochastic and not guaranteed to reach a global minimum
    communities <- list(c2,c3,c4,c5,c6,TL84,TL86,YthanEstuary,SkipwithPond,
                        BroadstoneStream,Benguela)
    expected <- c(0, 0, 0, 0, 0, 10, 10, 277, 31, 7, 27)
    stopifnot(length(communities)==length(expected))
    names(expected) <- sapply(communities, CP, property='title')
    for(community in communities)
    {
        title <- CP(community, 'title')
        res <- MinimiseSumDietGaps(community, T.start=1, T.stop=0.1, c=0.99, 
                                   swaps.per.T=1000, n=3)

        if(0==expected[title])
        {
            stopifnot(0==res$sum.gaps)
        }
        else
        {
            if( (res$sum.gaps / expected[title]) > 3)
            {
                stop(paste('Best diet gap for', title, 'of', res$sum.gaps,
                           'is much higher than the expected value of', 
                            expected[title]))
            }
        }
    }
}

