# Builds cheddar windows
# This script is meant for Lawrence's 64-bit Windows 7 VirtualBox machine
# The resulting zip file contains builds for 32- and 64-bit R
c:
cd \

# TODO git pull a specific version
set PACKAGE=cheddar
set VERSION=0.1-602

# Create the .tar.gz file
R CMD build cheddar
mv c:\Users\Win7-64x\AppData\Local\VirtualStore\%PACKAGE%_%VERSION%.tar.gz .

R CMD check %PACKAGE%_%VERSION%.tar.gz

# Create the .zip file that contains compiled code and vignette PDFs
R CMD INSTALL --build %PACKAGE%_%VERSION%.tar.gz
mv c:\Users\Win7-64x\AppData\Local\VirtualStore\%PACKAGE%.zip .

