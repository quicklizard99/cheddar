# TODO Seed RNG?
TestRandomLinks <- function()
{
    AssertRaises(RandomLinks(pool=NULL))
    AssertRaises(RandomLinks(pool='Node 1', n=0))
    AssertRaises(RandomLinks(pool='Node 1', n=1, C=0))
    AssertRaises(RandomLinks(pool='Node 1', n=1, C=1))

    # 10 sets of links with directed connectance of 0.15 taken from from a pool 
    # of 10 species.
    res <- RandomLinks(pool=paste('Node', 1:10), n=10, C=0.15)
    AssertEqual(10, length(res))
    AssertEqual(15, mean(sapply(res, nrow)))
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))

    # 100 sets of links with directed connectance of 0.5 taken from from a pool 
    # of 10 species.
    res <- RandomLinks(pool=paste('Node', 1:10), n=100, C=0.5)
    AssertEqual(100, length(res))
    AssertEqual(50, mean(sapply(res, nrow)))
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))
}

TestCascadeModelLinks <- function()
{
    AssertRaises(CascadeModelLinks(pool=NULL))
    AssertRaises(CascadeModelLinks(pool='Node 1', n=0))
    AssertRaises(CascadeModelLinks(pool='Node 1', n=1, C=0))
    AssertRaises(CascadeModelLinks(pool='Node 1', n=1, C=1))

    # 100 sets of links with directed connectance of 0.15 taken from from a 
    # pool of 10 species.
    res <- CascadeModelLinks(pool=paste('Node', 1:10), n=500, C=0.15)
    AssertEqual(500, length(res))
    AssertEqual(15, mean(sapply(res, nrow)), tolerance=1, scale=1)
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))

    # 100 sets of links with directed connectance of 0.5 taken from from a 
    # pool of 10 species.
    res <- CascadeModelLinks(pool=paste('Node', 1:10), n=500, C=0.5)
    AssertEqual(500, length(res))
    AssertEqual(50, mean(sapply(res, nrow)), tolerance=6, scale=1)
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))
}

TestNicheModelLinks <- function()
{
    AssertRaises(NicheModelLinks(pool=NULL))
    AssertRaises(NicheModelLinks(pool='Node 1', n=0))
    AssertRaises(NicheModelLinks(pool='Node 1', n=1, C=0))
    AssertRaises(NicheModelLinks(pool='Node 1', n=1, C=0.5))
    AssertRaises(NicheModelLinks(pool='Node 1', n=1, C=1))

    # 100 sets of links with directed connectance of 0.15 taken from from a 
    # pool of 10 species.
    res <- NicheModelLinks(pool=paste('Node', 1:10), n=500, C=0.15)
    AssertEqual(500, length(res))
    AssertEqual(15, mean(sapply(res, nrow)), tolerance=1, scale=1)
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))

    # 100 sets of links with directed connectance of 0.5 taken from from a 
    # pool of 10 species.
    res <- NicheModelLinks(pool=paste('Node', 1:10), n=500, C=0.45)
    AssertEqual(500, length(res))
    AssertEqual(45, mean(sapply(res, nrow)), tolerance=6, scale=1)
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))

    # Test probabilistic.
    # TODO probabilistic niche model generates lower than expected L
    # 100 sets of links with directed connectance of 0.15 taken from from a 
    # pool of 10 species.
    res <- NicheModelLinks(pool=paste('Node', 1:10), n=500, C=0.15, probabilistic=TRUE)
    AssertEqual(500, length(res))
    AssertEqual(15, mean(sapply(res, nrow)), tolerance=2.5, scale=1)
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))

    # 100 sets of links with directed connectance of 0.5 taken from from a 
    # pool of 10 species.
    res <- NicheModelLinks(pool=paste('Node', 1:10), n=500, C=0.45, probabilistic=TRUE)
    AssertEqual(500, length(res))
    AssertEqual(45, mean(sapply(res, nrow)), tolerance=7, scale=1)
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))
}
