movie <- read.csv("~/Documents/SN/movies.csv")
attach(movie)
plot(TopMovie,Gross)
plot(TotalMovies,Gross)
plot(Theaters,Gross)
plot(c(1:length(TopMovie(regobj))),TopMovie(regobj),type="b",xlab="Time",ylab="TopMovie")
plot(c(1:length(Theaters(regobj))),Theaters(regobj),type="b",xlab="Time",ylab="Theaters")
plot(c(1:length(TotalMovies(regobj))),TotalMovies(regobj),type="b",xlab="Time",ylab="TotalMovies")
plot(c(1:length(Gross(regobj))),Gross(regobj),type="b",xlab="Time",ylab="Gross")

boxoffice<-lm(Gross ~ TopMovie + TotalMovies + Theaters)
summary(boxoffice)
library(car)
vif(boxoffice)

qqPlot(boxoffice,id.method="identify",simulate=TRUE, main="Q-Q Plot")
std.res1 <- rstandard(boxoffice)
plot(fitted(boxoffice),std.res1,xlab="Fitted values",ylab="Standardized residuals")
plot(time,std.res1,ylab="Standardized residuals")

durbinWatsonTest(boxoffice)
acf(std.res1, xlim=c(1,20), ylim=c(-.25, .8))

runs.test <- function (x, cont.corr = F)
{
    if(any(is.na(x)))
        stop("NAs in x")
    DNAME <- deparse(substitute(x))
    if(any(x == 0.0)) {
        cat("Removed", length(x[x==0.0]), "zero(es)\n")
        x <- x[x != 0.0]
    }
    d <- diff(sign(x))
    f <- factor(d)
    sp <- split(d, f)
    resL <- lapply(sp, length)
    n <- length(x)
    nplus <- sum(sign(x)==1)
    nminus <- sum(sign(x)==-1)
    m <- 2.*nplus*nminus/n+1
    s <- sqrt(2.*nplus*nminus*(2.*nplus*nminus-n)/(n*n*(n-1)))
    R <- 1
    if(!is.null(resL$"-2"))
        R <- R+resL$"-2"
    if(!is.null(resL$"2"))
        R <- R+resL$"2"
# Continuity correction is not used by default
    if (cont.corr) STATISTIC <- sign(R-m)*(abs(R-m)-.5)/s else STATISTIC <- (R-m)/s
    METHOD <- "Runs Test"
    PVAL <- 2 * pnorm(-abs(STATISTIC))
    names(STATISTIC) <- "z-statistic for runs test"
    runstuff <- c(R,m)
    names(runstuff) <- c("Observed number of runs", "Expected number of runs")
    structure(list(estimate = runstuff,
    		   statistic = STATISTIC,
                   p.value = PVAL,
                   method = METHOD,
                   data.name = DNAME),
              class = "htest")
}
runs.test(std.res1, cont.corr=T)

box<-lm(Gross ~ TopMovie + TotalMovies + Theaters + Q1 +Q2 + Q3)
summary(box)
qqPlot(box,id.method="identify",simulate=TRUE, main="Q-Q Plot")
std.res2 <- rstandard(box)
plot(fitted(boxoffice),std.res2,xlab="Fitted values",ylab="Standardized residuals")
plot(time,std.res2,ylab="Standardized residuals")
durbinWatsonTest(box)
acf(std.res1, xlim=c(1,20), ylim=c(-.25, .8))
runs.test(std.res2, cont.corr=T)






