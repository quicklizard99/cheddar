/**
 * R Brush for SyntaxHighlighter
 * http://www.demitri.com/code/
 *
 * @version
 * 1.0.0 (3 November 2009)
 * 
 * @copyright
 * Copyright (c) 2009 Demitri Muna.
 * Comments/questions welcome. (xxdemitrixx@xmex.com - remove the 'x's)
 *
 *
 * @license
 * Licensed under a GNU Lesser General Public License.
 * http://creativecommons.org/licenses/LGPL/2.1/
 * 
 * NOTES
 * -----
 * Much of this has been taken from R's documentation. For complete function lists
 * (which these should be!) of each type, I've included the location in the
 * documentation where they come from. These "links" are preceded by question marks.
 * To open the page in the documentation, enter the link in the R console,
 * e.g. "?Constants".
 *
 */

/*
	The function below replaces the built in "getKeywords" function. R has a very
	long list of built in functions which take parameters in parentheses. Because
	of this, there is a much more likely chance that these keywords will collide
	with variable names. Take the following trivial and entirely reasonable example:
	
	c = 10^8						[1]
	vector = c(10,20,30)			[2]
	velocity = vector(length=3)		[3]
	dimensions = length(vector)		[4]
	
	Should be highlighted:
	"c" in [2]
	"vector" in [3]
	"length" in [4]
	
	Should *not* be highlighted:
	"c" in [1]
	"vector" in [2]
	"length" in [3]
	
	The custom regular expressions below require that a "(" follow the keyword.
	Optionally, any amount of whitespace can lie between the keyword and the "(",
	as this is a valid R command:
	
	a = c     (12,23,34)
	
*/
var getRFunctionKeywords = function(str)
{
	str = str
		.replace(/^\s+|\s+$/g, '')
		.replace(/\s+/g, '|')
		;

	// for debugging
//	var regexp = '\\b(?:' + str + ')\\b(?=\\s*\\()';
//	console.log(regexp);

	return '\\b(?:' + str + ')\\b(?=\\s*\\()';
};

/*
	There are R functions that contain a "." in them. These are listed in a separate
	list as they require a slightly different regular expression and substitution.
*/
var getRFunctionKeywordsWithDot = function(str)
{
	str = str
		.replace(/^\s+|\s+$/g, '')
		.replace(/\./g, '\\.')
		.replace(/\s+/g, '|')
		;

//	var regexp = '(?:' + str + ')\\b(?=\\s*\\()';
//	console.log(regexp);

	return '(?:' + str + ')\\b(?=\\s*\\()';
};

SyntaxHighlighter.brushes.R = function()
{
	var keywords =	'if else repeat while function for in next break NULL Inf NaN '; // ?Reserved
					/* '... ..1 ..2 ..3 ..4 ..5 ..6 ..7 ..8 ..9' are implemented with a regexp - see arithmatic operators below */

	var constants = 'TRUE FALSE NA NA_integer_ NA_real_ NA_complex_ NA_character_ ' + // ?logical
					'LETTERS letters month.abb month.name pi'; // ?Constants
	
	// This list excludes function names that contain a "." .
	
	var functions = 'abbreviate abline abs acf acf2AR ' +
				'acos acosh add1 addGrob addTaskCallback ' +
				'addTclPath addterm aggregate agnes agrep ' +
				'AIC alias alist all allCoef ' +
				'allGenerics anova any aov aperm ' +
				'append apply applyEdit applyEdits approxfun ' +
				'apropos ar area Arg args ' +
				'arima arima0 Arith ARMAacf ARMAtoMA ' +
				'array arrows arrowsGrob as asin ' +
				'asinh asNamespace asOneFormula asOneSidedFormula assign ' +
				'assignInNamespace assocplot asTable asVector atan ' +
				'atan2 atanh attach attachNamespace attr ' +
				'attrassign attributes augPred autoload autoloader ' +
				'ave axis axTicks backSpline balancedGrouped ' +
				'banking bannerplot barchart barplot basehaz ' +
				'basename batchSOM bcv besselI besselJ ' +
				'besselK besselY beta BIC bindingIsActive ' +
				'bindingIsLocked binomial biplot bitmap bkde ' +
				'bkde2D bkfe body boot box ' +
				'boxcox boxplot bquote browseEnv browser ' +
				'browseURL bs buildVignettes builtins bwplot ' +
				'bxp by bzfile c call ' +
				'callGeneric callNextMethod cancor capabilities casefold ' +
				'cat cbind ccf ceiling censboot ' +
				'character charmatch charToRaw chartr checkDocFiles ' +
				'checkDocStyle checkFF checkMD5sums checkReplaceFuns checkS3methods ' +
				'checkTnF checkVignettes childNames chol chol2inv ' +
				'choose chull circleGrob citation citEntry ' +
				'citFooter citHeader clara class clearNames ' +
				'clogit close closeAllConnections cloud clusplot ' +
				'cluster cm cmdscale codoc codocClasses ' +
				'codocData coef coefficients col col2rgb ' +
				'collapse colMeans colnames comment Compare ' +
				'compareFits comparePred compareVersion complex computeRestarts ' +
				'con2tr condense conditionCall conditionMessage confint ' +
				'conflicts Conj constrOptim contour contourLines ' +
				'contourplot contrasts control convertHeight convertNative ' +
				'convertUnit convertWidth convertX convertY convolve ' +
				'cophenetic coplot cor corAR1 corARMA ' +
				'corCAR1 corCompSymm corExp corFactor corGaus ' +
				'corLin corMatrix corNatural corr corRatio ' +
				'correlogram corresp corSpatial corSpher corSymm ' +
				'cos cosh cov cov2cor covariate ' +
				'covratio coxph cpgram crossprod cum3 ' +
				'cummax cummin cumprod cumsum curve ' +
				'cut cutree cycle daisy data ' +
				'dataentry dataViewport dbeta dbinom dcauchy ' +
				'dchisq de debug debugger decompose ' +
				'delay delimMatch deltat demo dendrapply ' +
				'density densityplot denumerate deparse deriv3 ' +
				'det detach determinant dev2bitmap deviance ' +
				'dexp df dfbeta dfbetas dffits ' +
				'dgamma dgeom dget dhyper diag ' +
				'diana diff diffinv difftime digamma ' +
				'dim dimnames dirname dist dlnorm ' +
				'dlogis dmultinom dnbinom dnorm dotchart ' +
				'dotplot double downViewport dpih dpik ' +
				'dpill dpois dput dQuote drawDetails ' +
				'drop drop1 dropterm dsignrank dt ' +
				'dump dumpMethod dumpMethods dunif duplicated ' +
				'dweibull dwilcox eapply ecdf edit ' +
				'editDetails editGrob effects eigen ellipsoidhull ' +
				'ellipsoidPoints emacs empinf end envelope ' +
				'environment environmentIsLocked eqscplot eval evalq ' +
				'example exists existsMethod exp expcov ' +
				'expm1 expression extends extractAIC factanal ' +
				'factor factorial family fanny fdHess ' +
				'fft fifo file_path_as_absolute file_path_sans_ext file_test ' +
				'file filePathAsAbsolute filePathSansExt fileTest filter ' +
				'find findClass findFunction findInterval findMethod ' +
				'findRestart fitdistr fitted fivenum fix ' +
				'fixef fixInNamespace floor flush force ' +
				'formals format formatC formatDL formula ' +
				'formXtViX forwardsolve fourfoldplot fractions frailty ' +
				'frameGrob frequency ftable gam gamm ' +
				'gamma gammaCody gapply gaucov gaussian ' +
				'gc gcinfo gctorture gEdit gEditList ' +
				'get getAccess getAnywhere getCallingDLL getCConverterDescriptions ' +
				'getCConverterStatus getClass getClassDef getClasses getClassName ' +
				'getClassPackage getConnection getCovariate getCovariateFormula getData ' +
				'getDepList getDLLRegisteredRoutines geterrmessage getExportedValue getExtends ' +
				'getFromNamespace getGenerics getGrob getGroups getGroupsFormula ' +
				'getHook getInitial getMethod getMethods getNamespace ' +
				'getNamespaceExports getNamespaceImports getNamespaceInfo getNamespaceName getNamespaceUsers ' +
				'getNamespaceVersion getNativeSymbolInfo getNumCConverters getOption getPackageName ' +
				'getProperties getPrototype getResponse getResponseFormula getRversion ' +
				'getS3method getSubclasses getValidity getVarCov getVirtual ' +
				'getwd ginv gl gList glm ' +
				'glmmPQL globalenv gls glsControl glsStruct ' +
				'gnls gnlsControl gnlsStruct gpar gPath ' +
				'gray grep grey grid grob ' +
				'grobHeight grobWidth groupedData gsub gsummary ' +
				'gTree gzcon gzfile hasArg hasMethod ' +
				'hasTsp hat hatvalues hclust head ' +
				'heatmap heightDetails help hist histogram ' +
				'history HoltWinters hsv huber hubers ' +
				'I identical identify ifelse Im ' +
				'image importIntoEnv influence inherits initialize ' +
				'installFoundDepends integer integrate interaction interpSpline ' +
				'intersect intervals intToBits invisible invokeRestart ' +
				'invokeRestartInteractively IQR is isBalanced isBaseNamespace ' +
				'isClass isClassUnion isGeneric isGroup isIncomplete ' +
				'isInitialized ISOdate ISOdatetime isoMDS isOpen ' +
				'isoreg isRestart isSealedClass isSealedMethod isSeekable ' +
				'jitter jpeg julian KalmanForecast KalmanLike ' +
				'KalmanRun KalmanSmooth kappa Kaver kde2d ' +
				'Kenvl kernapply kernel Kfn kmeans ' +
				'knn knn1 knots kronecker ksmooth ' +
				'labels lag lapply larrows latticeParseFormula ' +
				'layout lazyLoad lbeta lchoose lcm ' +
				'lda ldahist LDEsysMat legend length ' +
				'levelplot levels lfactorial lgamma library ' +
				'line lines linesGrob lineToGrob list_files_with_exts ' +
				'list_files_with_type list listFilesWithExts listFilesWithType llines ' +
				'lm lme lmeControl lmeScale lmeStruct ' +
				'lmList lmsreg load loadedNamespaces loadhistory ' +
				'loadingNamespaceInfo loadings loadNamespace loadURL local ' +
				'locator lockBinding lockEnvironment locpoly loess ' +
				'log log10 log1p log2 logb ' +
				'logDet logical logit logLik loglin ' +
				'loglm loglm1 logtrans lowess lpoints ' +
				'lqs ls lsegments lset lsfit ' +
				'ltext ltransform3dMatrix ltransform3dto3d ltsreg lvq1 ' +
				'lvq2 lvq3 lvqinit lvqtest mad ' +
				'magic mahalanobis makeActiveBinding makeARIMA makeClassRepresentation ' +
				'makeLazyLoading makepredictcall manglePackageName manova mapply ' +
				'match Math Math2 matplot matpoints ' +
				'matrix max mca md5sum mean ' +
				'meanvar median medpolish menu merge ' +
				'methods MethodsListSelect mgcv mget min ' +
				'missing mle Mod mode mona ' +
				'monthplot months mosaicplot mostattributes moveToGrob ' +
				'mroot mtext multiedit multinom mvfft ' +
				'mvrnorm n2mfrow names namespaceExport namespaceImport ' +
				'namespaceImportClasses namespaceImportFrom namespaceImportMethods napredict naprint ' +
				'naresid nchar ncol needUpdate new ' +
				'NextMethod nextn nlevels nlm nlme ' +
				'nlmeControl nlmeStruct nls nlsList nlsModel ' +
				'NLSstAsymptotic NLSstClosestX NLSstLfAsymptote NLSstRtAsymptote nnet ' +
				'nnetHess noquote notExp notLog nrow ' +
				'ns nsl Null numeric numericDeriv ' +
				'objects offset oldClass olvq1 oneway ' +
				'open Ops optim optimise optimize ' +
				'options order ordered outer pacf ' +
				'package_version packageDescription packageEvent packageHasNamespace packageSlot ' +
				'packageStatus packBits packGrob page pairlist ' +
				'pairs palette pam par parallel ' +
				'parcoord parse parseNamespaceFile paste pbeta ' +
				'pbinom pbirthday pcauchy pchisq pcls ' +
				'pdBlocked pdCompSymm pdConstruct pdDiag pdf ' +
				'pdFactor pdIdent pdIdnot pdLogChol pdMat ' +
				'pdMatrix pdNatural pdSymm pdTens periodicSpline ' +
				'person personList persp pexp pf ' +
				'pgamma pgeom phenoModel phyper pico ' +
				'pictex pie pipe pkgDepends pkgVignettes ' +
				'placeGrob plclust plnorm plogis plot ' +
				'plotcp plotViewport pltree pmatch pmax ' +
				'pmin pnbinom png pnorm points ' +
				'pointsGrob poisson polr poly polygon ' +
				'polygonGrob polym polyroot polySpline pooledSD ' +
				'popViewport post postDrawDetails postscript postscriptFont ' +
				'postscriptFonts power ppinit pplik ppoints ' +
				'ppois ppr ppregion prcomp predict ' +
				'preDrawDetails preplot pretty prettyNum princomp ' +
				'print printCoefmat printcp prmat prmatrix ' +
				'prod profile profiler proj promax ' +
				'prompt promptClass promptData promptMethods prototype ' +
				'prune psigamma psignrank Psim pspline ' +
				'pt ptukey punif pushBack pushBackLength ' +
				'pushViewport pweibull pwilcox pyears qbeta ' +
				'qbinom qbirthday qcauchy qchisq qda ' +
				'qexp qf qgamma qgeom qhyper ' +
				'qlnorm qlogis qnbinom qnorm qpois ' +
				'qq qqline qqmath qqnorm qqplot ' +
				'qr qsignrank qt qtukey quantile ' +
				'quarters quartz quartzFont quartzFonts quasi ' +
				'quasibinomial quasipoisson quinModel quit qunif ' +
				'quote qweibull qwilcox r2dtable rainbow ' +
				'ranef range rank ratetable rational ' +
				'raw rawShift rawToBits rawToChar rbeta ' +
				'rbind rbinom rcauchy rchisq Rd_db ' +
				'Rd_parse Rdindex Re readBin readChar ' +
				'readCitationFile readline readLines real recalc ' +
				'Recall recordPlot rect rectGrob reformulate ' +
				'regexpr registerS3method registerS3methods relevel remove ' +
				'removeCConverter removeClass removeGeneric removeGrob removeMethod ' +
				'removeMethods removeTaskCallback renumerate reorder rep ' +
				'replace replayPlot replicate replications representation ' +
				'require resetClass reshape resid residuals ' +
				'restartDescription restartFormals reStruct return rev ' +
				'rexp rf rfs rgamma rgb ' +
				'rgb2hsv rgeom rhyper ridge rle ' +
				'rlm rlnorm rlogis rm rmultinom ' +
				'rnbinom rnegbin RNGkind RNGversion rnorm ' +
				'round row rowMeans rownames Rows ' +
				'rowsum rpart rpconvert rpois Rprof ' +
				'rsignrank rstandard rstudent rt Rtangle ' +
				'RtangleSetup rug runif runmed RweaveLatex ' +
				'RweaveLatexSetup rweibull rwilcox s saddle ' +
				'sammon sample sapply save savehistory ' +
				'saveNamespaceImage scale scan screen screeplot ' +
				'sd sealClass seek seekViewport segments ' +
				'segmentsGrob selectMethod selfStart semat seq ' +
				'sequence serialize sessionInfo setAs setCConverterStatus ' +
				'setChildren setClass setClassUnion setdiff setequal ' +
				'setGeneric setGrob setGroupGeneric setHook setIs ' +
				'setMethod setNames setNamespaceInfo setOldClass setReplaceMethod ' +
				'setwd Shepard shingle show showConnections ' +
				'showMethods shQuote sign signalCondition signature ' +
				'signif silhouette simpleCondition simpleKey simplex ' +
				'sin single sinh sink sizeDiss ' +
				'slot slotNames smooth smoothEnds socketConnection ' +
				'socketSelect solve SOM somgrid sort ' +
				'sortedXyData sortSilhouette source spectrum sphercov ' +
				'spline splineDesign splinefun splineKnots splineOrder ' +
				'split splitFormula splom sprintf sqrt ' +
				'sQuote SSasymp SSasympOff SSasympOrig SSbiexp ' +
				'SSfol SSfpl SSgompertz SSI SSlogis ' +
				'SSmicmen SSweibull stack standardGeneric Stangle ' +
				'stars start stderr stdin stdout ' +
				'stdres stem step stepAIC stepfun ' +
				'stl stop stopifnot str strata ' +
				'Strauss strftime strheight stringHeight stringWidth ' +
				'stripchart stripplot strptime strsplit StructTS ' +
				'structure strwidth strwrap studres sub ' +
				'subset substitute substr substring sum ' +
				'summary summaryRprof sunflowerplot suppressWarnings supsmu ' +
				'Surv survdiff survexp survfit survobrien ' +
				'survreg survSplit svd Sweave SweaveSyntConv ' +
				'sweep switch symbols symnum system ' +
				't table tabulate tail tan ' +
				'tanh tapply taskCallbackManager tcl tclArray ' +
				'tclclose tclObj tclopen tclputs tclread ' +
				'tclRequire tclvalue tclVar tcut te ' +
				'tempdir tempfile termplot terms testPlatformEquivalence ' +
				'texi2dvi text textConnection textGrob time ' +
				'title tkactivate tkadd tkaddtag tkbbox ' +
				'tkbell tkbind tkbindtags tkbutton tkcanvas ' +
				'tkcanvasx tkcanvasy tkcget tkcheckbutton tkchooseDirectory ' +
				'tkclose tkcmd tkcompare tkconfigure tkcoords ' +
				'tkcreate tkcurselection tkdchars tkdebug tkdelete ' +
				'tkdelta tkdeselect tkdestroy tkdialog tkdlineinfo ' +
				'tkdtag tkdump tkentry tkentrycget tkentryconfigure ' +
				'tkfind tkflash tkfocus tkfraction tkframe ' +
				'tkget tkgetOpenFile tkgetSaveFile tkgettags tkgrab ' +
				'tkgrid tkicursor tkidentify tkindex tkinsert ' +
				'tkinvoke tkitembind tkitemcget tkitemconfigure tkitemfocus ' +
				'tkitemlower tkitemraise tkitemscale tklabel tklistbox ' +
				'tklower tkmenu tkmenubutton tkmessage tkmessageBox ' +
				'tkmove tknearest tkopen tkpack tkpager ' +
				'tkplace tkpopup tkpost tkpostcascade tkpostscript ' +
				'tkputs tkradiobutton tkraise tkread tkscale ' +
				'tkscrollbar tksearch tksee tkselect tkset ' +
				'tksize tktext tktitle tktoggle tktoplevel ' +
				'tktype tkunpost tkwidget tkwinfo tkxview ' +
				'tkyposition tkyview tmd toBibtex toeplitz ' +
				'toLatex tolower topenv toString toupper ' +
				'trace traceback tracingState transform trigamma ' +
				'trmat truehist trunc truncate try ' +
				'tryCatch ts tsboot tsdiag tsp ' +
				'tsSmooth TukeyHSD typeof ucv unclass ' +
				'undebug undoc union unique uniquecombs ' +
				'uniroot unit unlink unlist unloadNamespace ' +
				'unlockBinding unname unserialize unsplit unstack ' +
				'untrace unz update upgrade upViewport ' +
				'url UseMethod validDetails validObject var ' +
				'varComb varConstPower VarCorr varExp varFixed ' +
				'varFunc varIdent varimax variogram varPower ' +
				'varWeights vcov vector vi viewport ' +
				'vignette vignetteDepends volume vpList vpPath ' +
				'vpStack vpTree warning warnings weekdays ' +
				'weights which widthDetails window wireframe ' +
				'with withCallingHandlers withRestarts write writeBin ' +
				'writeChar writeLines X11 X11Font X11Fonts ' +
				'xaxisGrob xedit xemacs xfig xinch ' +
				'xor xtabs xyinch xyplot xyVector ';
	
	// This list only includes function names that contain a "." .
	
	var dot_functions = '.checkMFClasses .Defunct .deparseOpts .Deprecated .dynLibs ' +
				'.Export .External.graphics .find.package .First.lib .getRequiredPackages ' +
				'.getRequiredPackages2 .getXlevels .handleSimpleError .Import .ImportFrom ' +
				'.Last.lib .libPaths .mergeExportMethods .MFclass .NotYetImplemented ' +
				'.NotYetUsed .onAttach .onLoad .onUnload .packages ' +
				'.path.package .Primitive .readRDS .S3method .saveRDS ' +
				'.Script .signalSimpleWarning .subset .subset2 .Tcl.args.objv ' +
				'.Tcl.args .Tcl.callback .Tcl.objv .Tcl .Tk.ID ' +
				'.Tk.newwin .Tk.subwin abc.ci absolute.size add.scope ' +
				'all.equal all.names all.vars anova.gam anova.lmlist ' +
				'anovalist.trls ansari.test ar.burg ar.mle ar.ols ' +
				'ar.yw arima.sim as.array as.call as.character ' +
				'as.complex as.data.frame as.date as.dendrogram as.difftime ' +
				'as.dist as.double as.environment as.expression as.factor ' +
				'as.factorOrShingle as.formula as.function as.hclust as.integer ' +
				'as.list.environment as.list as.logical as.matrix as.name ' +
				'as.null as.numeric as.ordered as.pairlist as.person ' +
				'as.personList as.polySpline as.POSIXct as.POSIXlt as.qr ' +
				'as.raw as.real as.shingle as.single as.stepfun ' +
				'as.symbol as.table as.tclObj as.ts as.vector ' +
				'attr.all.equal axis.Date axis.POSIXct bandwidth.kernel bandwidth.nrd ' +
				'bartlett.test binom.test boot.array boot.ci Box.test ' +
				'boxplot.stats browse.pkgs browse.update.pkgs bug.report bw.bcv ' +
				'bw.nrd bw.nrd0 bw.SJ bw.ucv canonical.theme ' +
				'capture.output case.names char.expand check.options chisq.test ' +
				'class.ind close.screen close.socket cm.colors co.intervals ' +
				'col.whitebg complete.cases contr.helmert contr.poly contr.SAS ' +
				'contr.sdif contr.sum contr.treatment cooks.distance cor.test ' +
				'count.fields cov.mcd cov.mve cov.rob cov.trob ' +
				'cov.wt cox.zph coxph.detail CRAN.packages current.transform ' +
				'current.viewport current.vpTree cv.glm data.class data.entry ' +
				'data.frame data.matrix date.ddmmmyy date.mdy date.mmddyy ' +
				'date.mmddyyyy delete.response dev.control dev.copy dev.copy2eps ' +
				'dev.cur dev.list dev.next dev.off dev.prev ' +
				'dev.print dev.set df.kernel df.residual diag.panel.splom ' +
				'dir.create do.breaks do.call dose.p download.file ' +
				'download.packages draw.colorkey draw.details draw.key drop.scope ' +
				'drop.terms dummy.coef dump.frames dyn.load dyn.unload ' +
				'EEF.profile eff.aovlist EL.profile engine.display.list equal.count ' +
				'erase.screen eval.parent exclude.too.far exp.tilt expand.grid ' +
				'expand.model.frame extract.lme.cov extract.lme.cov2 factor.scope file.access ' +
				'file.append file.choose file.copy file.create file.edit ' +
				'file.exists file.info file.path file.remove file.rename ' +
				'file.show file.symlink filled.contour fisher.test fitted.values ' +
				'fixed.effects fixPre1.8 fligner.test format.char format.info ' +
				'format.pval formula.gam frailty.gamma frailty.gaussian frailty.t ' +
				'freq.array friedman.test full.score gam.check gam.control ' +
				'gam.side.conditions gamma.dispersion gamma.shape gc.time get.gpar ' +
				'glm.control glm.convert glm.diag.plots glm.diag glm.fit ' +
				'glm.nb graphics.off grid.add grid.arrows grid.circle ' +
				'grid.collection grid.convert grid.convertHeight grid.convertWidth grid.convertX ' +
				'grid.convertY grid.copy grid.display.list grid.draw grid.edit ' +
				'grid.frame grid.get grid.grab grid.grill grid.grob ' +
				'grid.layout grid.line.to grid.lines grid.locator grid.move.to ' +
				'grid.newpage grid.pack grid.place grid.points grid.polygon ' +
				'grid.pretty grid.prompt grid.rect grid.remove grid.segments ' +
				'grid.set grid.show.layout grid.show.viewport grid.text grid.xaxis ' +
				'grid.yaxis heat.colors help.search help.start hist.FD ' +
				'hist.scott imp.moments imp.prob imp.quantile imp.weights ' +
				'index.search influence.gam influence.measures install.binaries install.from.file ' +
				'install.packages installed.packages interaction.plot inv.logit inverse.gaussian ' +
				'inverse.rle is.array is.atomic is.call is.character ' +
				'is.complex is.data.frame is.date is.double is.element ' +
				'is.empty.model is.environment is.expression is.factor is.finite ' +
				'is.function is.infinite is.integer is.language is.leaf ' +
				'is.list is.loaded is.logical is.matrix is.na ' +
				'is.name is.nan is.null is.numeric is.object ' +
				'is.ordered is.pairlist is.qr is.ratetable is.real ' +
				'is.recursive is.shingle is.single is.stepfun is.Surv ' +
				'is.symbol is.table is.tclObj is.tkwin is.ts ' +
				'is.tskernel is.unsorted is.vector jack.after.boot k3.linear ' +
				'kappa.tri knn.cv kruskal.test ks.test La.chol ' +
				'La.chol2inv La.svd lag.plot lattice.getOption lattice.options ' +
				'layout.show library.dynam.unload library.dynam linear.approx list.files ' +
				'lm.gls lm.influence lm.ridge lm.wfit loess.control ' +
				'loess.smooth logLik.gam lookup.xport lower.to.upper.tri.inds lower.tri ' +
				'lplot.xy ls.diag ls.print lsf.str magic.post.proc ' +
				'make.link make.names make.packages.html make.socket make.unique ' +
				'mantelhaen.test margin.table mat.or.vec match.arg match.call ' +
				'match.fun max.col mcnemar.test mdy.date mem.limits ' +
				'mgcv.control model.extract model.frame model.matrix model.offset ' +
				'model.response model.tables model.weights mono.con mood.test ' +
				'na.action na.contiguous na.exclude na.fail na.omit ' +
				'na.pass na.rpart nclass.FD nclass.scott nclass.Sturges ' +
				'negative.binomial new.env nls.control norm.ci null.space.basis.labels ' +
				'null.space.basis.powers null.space.dimension object.size old.packages on.exit ' +
				'oneway.test order.dendrogram p.adjust package.dependencies package.skeleton ' +
				'pairwise.prop.test pairwise.t.test pairwise.table pairwise.wilcox.test panel.3dscatter ' +
				'panel.3dwire panel.abline panel.arrows panel.axis panel.barchart ' +
				'panel.bwplot panel.cloud panel.contourplot panel.curve panel.densityplot ' +
				'panel.dotplot panel.fill panel.grid panel.histogram panel.identify ' +
				'panel.levelplot panel.linejoin panel.lines panel.lmline panel.loess ' +
				'panel.mathdensity panel.pairs panel.parallel panel.points panel.qq ' +
				'panel.qqmath panel.qqmathline panel.rug panel.segments panel.smooth ' +
				'panel.splom panel.stripplot panel.superpose.2 panel.superpose panel.text ' +
				'panel.tmd panel.wireframe panel.xyplot parent.env parent.frame ' +
				'path.expand path.rpart plot.design plot.gam plot.window ' +
				'plot.xy pop.viewport pos.to.env power.anova.test power.prop.test ' +
				'power.t.test PP.test pred.rpart predict.gam Predict.matrix ' +
				'prepanel.lmline prepanel.loess prepanel.qqmathline print.anova.gam print.summary.gam ' +
				'proc.time prop.table prop.test prop.trend.test ps.options ' +
				'push.viewport qr.coef qr.fitted qr.Q qr.qty ' +
				'qr.qy qr.R qr.resid qr.solve qr.X ' +
				'quade.test random.effects read.00Index read.csv read.csv2 ' +
				'read.dbf read.dcf read.delim read.delim2 read.dta ' +
				'read.epiinfo read.fortran read.ftable read.fwf read.mtp ' +
				'read.socket read.spss read.ssd read.systat read.table ' +
				'read.xport rect.hclust reduce.nn remove.packages rep.int ' +
				'residuals.gam residuals.survreg rms.curv row.names rpart.anova ' +
				'rpart.class rpart.control rpart.exp rpart.matrix rpart.poisson ' +
				'rsq.rpart saddle.distn save.image scatter.smooth se.contrast ' +
				'set.seed shapiro.test show.settings simulate.lme sink.number ' +
				'slice.index smooth.construct smooth.f smooth.spline snip.rpart ' +
				'sort.list spec.ar spec.pgram spec.taper spline.des ' +
				'split.screen stat.anova storage.mode strip.custom strip.default ' +
				'summary.gam surf.gls surf.ls survexp.fit survreg.control ' +
				'survreg.old symbol.C symbol.For sys.call sys.calls ' +
				'sys.frame sys.frames sys.function Sys.getenv Sys.getlocale ' +
				'sys.load.image Sys.localeconv sys.nframe sys.on.exit sys.parent ' +
				'sys.parents Sys.putenv Sys.putlocale sys.save.image Sys.setlocale ' +
				'Sys.sleep sys.source sys.status Sys.time Sys.timezone ' +
				'system.file system.time t.test tclfile.dir tclfile.tail ' +
				'tensor.prod.model.matrix tensor.prod.penalties terrain.colors theta.md theta.ml ' +
				'theta.mm tilt.boot tkclipboard.append tkclipboard.clear tkevent.add ' +
				'tkevent.delete tkevent.generate tkevent.info tkfile.dir tkfile.tail ' +
				'tkfont.actual tkfont.configure tkfont.create tkfont.delete tkfont.families ' +
				'tkfont.measure tkfont.metrics tkfont.names tkgrab.current tkgrab.release ' +
				'tkgrab.set tkgrab.status tkgrid.bbox tkgrid.columnconfigure tkgrid.configure ' +
				'tkgrid.forget tkgrid.info tkgrid.location tkgrid.propagate tkgrid.remove ' +
				'tkgrid.rowconfigure tkgrid.size tkgrid.slaves tkimage.cget tkimage.configure ' +
				'tkimage.create tkimage.names tkmark.gravity tkmark.names tkmark.next ' +
				'tkmark.previous tkmark.set tkmark.unset tkpack.configure tkpack.forget ' +
				'tkpack.info tkpack.propagate tkpack.slaves tkplace.configure tkplace.forget ' +
				'tkplace.info tkplace.slaves tkscan.dragto tkscan.mark tkselection.adjust ' +
				'tkselection.anchor tkselection.clear tkselection.from tkselection.includes tkselection.present ' +
				'tkselection.range tkselection.set tkselection.to tktag.add tktag.bind ' +
				'tktag.cget tktag.configure tktag.delete tktag.lower tktag.names ' +
				'tktag.nextrange tktag.prevrange tktag.raise tktag.ranges tktag.remove ' +
				'tkwait.variable tkwait.visibility tkwait.window tkwindow.cget tkwindow.configure ' +
				'tkwindow.create tkwindow.names tkwm.aspect tkwm.client tkwm.colormapwindows ' +
				'tkwm.command tkwm.deiconify tkwm.focusmodel tkwm.frame tkwm.geometry ' +
				'tkwm.grid tkwm.group tkwm.iconbitmap tkwm.iconify tkwm.iconmask ' +
				'tkwm.iconname tkwm.iconposition tkwm.iconwindow tkwm.maxsize tkwm.minsize ' +
				'tkwm.overrideredirect tkwm.positionfrom tkwm.protocol tkwm.resizable tkwm.sizefrom ' +
				'tkwm.state tkwm.title tkwm.transient tkwm.withdraw tkXselection.clear ' +
				'tkXselection.get tkXselection.handle tkXselection.own tkxview.moveto tkxview.scroll ' +
				'tkyview.moveto tkyview.scroll topo.colors trellis.device trellis.focus ' +
				'trellis.grobname trellis.last.object trellis.panelArgs trellis.par.get trellis.par.set ' +
				'trellis.switchFocus trellis.unfocus trellis.vpname trls.influence ts.intersect ' +
				'ts.plot ts.union type.convert unit.c unit.length ' +
				'unit.pmax unit.pmin unit.rep unix.time untangle.specials ' +
				'update.packages upper.to.lower.tri.inds upper.tri url.show var.linear ' +
				'var.test variable.names vis.gam weighted.mean weighted.residuals ' +
				'which.is.max which.max which.min width.SJ wilcox.test ' +
				'write.dbf write.dcf write.dta write.foreign write.ftable ' +
				'write.matrix write.socket write.table xpdrows.data.frame xpred.rpart ' +
				
				/* Below are other functions I found not included above. */
				'isTrue '
				;

	
	this.findMatches = function(regexList, code)
	{
		code = code.replace(/&gt;/g, '>').replace(/&lt;/g, '<');
		this.code = code;
		return SyntaxHighlighter.Highlighter.prototype.findMatches.apply(this, [regexList, code]);
	};

	this.regexList = [
		{ regex: /\+|\-|\*|\/|\^|\%\%|\%\/\%|\.\.\.|\.\.\d+/gm, 		css: 'keyword' },	// arithmatic operators ?Arithmetic
		{ regex: /=|<\-|<<\-|\->|\->>/gm,								css: 'keyword' }, 	// assignment operators ?assignOps
		{ regex: /<|>|<=|>=|==|\!=/gm,									css: 'keyword' },	// relational operators ?Comparison
		{ regex: /\!|&|&&|\||\|\|/gm, 									css: 'keyword' },	// logic operators ?Logic
		{ regex: /\$|:|@|::|:::|~/gm, 									css: 'keyword' },	// misc ?Colon, ?Extract, ?slotOp, ?ns-dblcolon, ?tilde

		{ regex: new RegExp(getRFunctionKeywords(functions), 'gm'),				css: 'functions'},
		{ regex: new RegExp(getRFunctionKeywordsWithDot(dot_functions), 'gm'),	css: 'functions'},

		{ regex: SyntaxHighlighter.regexLib.singleLinePerlComments,		css: 'comments' },		// comments
		{ regex: SyntaxHighlighter.regexLib.doubleQuotedString,			css: 'string' },		// double quoted strings
		{ regex: SyntaxHighlighter.regexLib.singleQuotedString,			css: 'string' },		// single quoted strings

		{ regex: /\b\d+\.?\w*/g, 										css: 'value' },			// numeric value

		{ regex: new RegExp(this.getKeywords(keywords), 'gm'),			css: 'keyword' },		// keywords
		{ regex: new RegExp(this.getKeywords(constants), 'gm'),			css: 'constants' },		// constants
		];
};

SyntaxHighlighter.brushes.R.prototype	= new SyntaxHighlighter.Highlighter();
SyntaxHighlighter.brushes.R.aliases		= ['R'];
