VERSION=`cat DESCRIPTION  | grep Version: | sed s/.*:\ //g`
echo Building cheddar ${VERSION} using `R --version | head -1`
rm -rf build  && \
    mkdir build && \
    cd build && \
    R CMD build --compact-vignettes=gs .. && \
    ~/local/R-devel/R.framework/Resources/bin/R CMD check --as-cran cheddar_${VERSION}.tar.gz && \
    R CMD INSTALL --build --html cheddar_${VERSION}.tar.gz
