install.packages("funtimes")
install.packages("TTR")
install.packages("AICcmodavg")
library(funtimes)
library(TTR)
library(AICcmodavg)
library(geoscale)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/figures_head")

dispersal_relative <- as.data.frame(relative2$relative)
dispersal_relative

#create time series object

ts_dis <- ts(dispersal_relative)
plot(ts_dis, xlim=c(50,0))

#https://cran.r-project.org/web/packages/funtimes/vignettes/trendtests.html

#2.1 Linear trend
#Consider the following pair of hypotheses
#H0: no trend
#H1: linear trend
#that can be tested specifically using t-test.
#Assuming the time series may be autocorrelated (which is the usual case with observational data), 
#we apply sieve-bootstrap version of the t-test, by adapting the approach of Noguchi, Gel, and Duguay (2011):

notrend_test(ts_dis) #H0: no trend

#Sieve-bootstrap Student's t-test for a linear trend

#data:  ts_dis
#Student's t value = -6.5608, p-value = 0.008
#alternative hypothesis: linear trend.
#sample estimates:
#  $AR_order
#[1] 1

#$AR_coefficients
#phi_1 
#0.6592018 

#2.2 Monotonic trend
notrend_test(ts_dis, test = "MK")$p.value
#[1] 0.003 (no trend)
#the null hypothesis could not be rejected, because ts_dis does not have a trend (see above)

#2.3 Any trend
notrend_test(ts_dis, test = "WAVK", factor.length = "adaptive.selection", j = c(1:50))$p.value
#[1] 0.008 (no trend)
#[1] 0.005

#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

#You can then use the “SMA()” function to smooth time series data.
disSMA <- SMA(ts_dis, n=7)
plot.ts(disSMA, xlim=c(50,0))
disSMA

#Figure
png("Figure_relative_dispersal_Smooth_trend.png", width = 480, height = 480)
plot.ts(disSMA, lwd=2, xlim = c(50,0), ylim = c(0,12),
     main = "(Dispersal events / LTT) smoothed", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Holt’s Exponential Smoothing (NOt used)

disHW <- HoltWinters(ts_dis,gamma = FALSE)
disHW
disHW$SSE
plot(disHW)

#http://r-statistics.co/Time-Series-Analysis-With-R.html

#Autocorrelation

auto_ts_dis <- acf(ts_dis)

#Dornelas2012paperfigures.R

# Auto-Correlation Function (ACF) calculates correlations of a series with itself
# at different lags, which allows estimating how long the historical effect is
# felt, as well as identifying cycles in the time series. More information with ?acf().
# Significant cross-correlations cross the blue dashed line.
acf(ts_dis)

# The same analysis can be done on detrended data by differencing the time series 
# (with the function diff()), which allows identifying auto-correlation patterns in 
# terms of the rates of change.
acf(diff(ts_dis))

# A complementary approach to study autocorrelation is using periodograms, which show
# the density of the time series at different frequencies (i.e. for cycles with different 
# periods). This allows identifying the "colour" of noise in the time series, which tells us 
# the type of stochasticity the system experiences, for example. 
# The null hypothesis for uncorrelated data is a horizontal line (all frequencies equally important).
# Important cycles in the time series appear as peaks in density.
# Ecological systems tend to decrease density with increasing frequency, which corresponds to
# noise that falls between red and pink. More information with ?spec.pgram()
spec.pgram(ts_dis,plot=T,detrend=T)

# GLOBAL TREND

# The global trend can be obtained by performing a linear regression 
# with the biodiversity metric as a function of time.
# Multiple predictors can be used in a similar framework.

dis_time_rel <- as.data.frame(cbind(relative2$time, relative2$relative))

# Trend estimated using Ordinary Least Squares
ols.trend<-lm(dis_time_rel$V2~dis_time_rel$V1)
ols.trend
#Coefficients:
#  (Intercept)  dis_time_rel$V1  
#8.6550          -0.1786

#LOESS Smoothing

png("Figure_relative_dispersal_LOESS_Smoothing2.png", width = 480, height = 480)
plot(relative2$time, relative2$relative, type = "l", lwd=2, xlim = c(50,0),
     main = "Dispersal events / LTT and smoothing", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
lines(loess.smooth(relative2$time, relative2$relative, span = 1/6),lty=1, lwd=4, col = "gray70")
dev.off()

# BREAKPOINT DETECTION
# Assessing deviations from stability in a regression model allows
# identifying time periods with different linear trends. This is particularly
# useful in the context of biodiversity time series to detect phase-shifts

# estimating the location of optimal breakpoints
library(strucchange)
bps=breakpoints(ts_dis~1)
summary(bps)

# comparing models with 0 to 2 breakpoints shows that
# 1 breakpoint at time step 17 is the model with most support
mbp0=lm(dis_time_rel$V2~1)
mbp1=lm(dis_time_rel$V2~breakfactor(bps,breaks=1))
summary(mbp1)
mbp2=lm(dis_time_rel$V2~breakfactor(bps,breaks=2))
anova(mbp0,mbp1)
anova(mbp1,mbp2)
anova(mbp0,mbp2)
mbp3=lm(dis_time_rel$V2~breakfactor(bps,breaks=3))
mbp4=lm(dis_time_rel$V2~breakfactor(bps,breaks=4))
mbp5=lm(dis_time_rel$V2~breakfactor(bps,breaks=5))
mbp6=lm(dis_time_rel$V2~breakfactor(bps,breaks=6))
anova(mbp1,mbp3)
anova(mbp2,mbp3)
anova(mbp3,mbp4)
anova(mbp4,mbp5)
anova(mbp3,mbp4)
anova(mbp3,mbp6)
summary(mbp3)

#15.3 Comparing regression models with anova()
#https://bookdown.org/ndphillips/YaRrr/comparing-regression-models-with-anova.html

# Plotting the different lines shows a marked difference between
# 0 and 1 breakpoint, but a much smaller difference for the second
# breakpoint
png("Figure_relative_dispersal_Breaking points.png", width = 480, height = 480)
plot(dis_time_rel$V1, dis_time_rel$V2, type = "l", lwd=2, xlim = c(50,0),
     main = "Dispersal / LTT and breaking points", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
#abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)#Epochs
#lines(dis_time_rel$V1, ts(fitted(mbp0),start=1),lwd=2,lty=1)
#lines(dis_time_rel$V1, ts(fitted(mbp1),start=1),lwd=2,lty=2)
#lines(dis_time_rel$V1, ts(fitted(mbp2),start=1),lwd=2,lty=3)
abline(v = c(28,20,12), lwd=0.5,lty=1)#breaking points
lines(dis_time_rel$V1, ts(fitted(mbp3),start=1),lwd=2,lty=4, col = "gray50")
#lines(dis_time_rel$V1, ts(fitted(mbp4),start=1),lwd=2,lty=5)
#lines(dis_time_rel$V1, ts(fitted(mbp5),start=1),lwd=2,lty=6)
#lines(dis_time_rel$V1, ts(fitted(mbp6),start=1),lwd=2,lty=7)
#lines(loess.smooth(relative2$time, relative2$relative, span = 1/6),lty=1, lwd=3, col = "gray70")
dev.off()

#geoscalePlot(dis_time_rel$V1, dis_time_rel$V2, type = "l", units = c("Epoch"), abbrev = TRUE,
#             cex.age = 1.5, cex.ts = 0.8, direction = "horizontal", boxes="Epoch")
########################################################################################

#https://www.statology.org/aic-in-r/

#Choosing best breaking point model by AIC
#list of models
models <- list(mbp0, mbp1, mbp2, mbp3, mbp4, mbp5, mbp6)
#specify model names
mod.names <- c("no breaking point", "1 Breaking point","2 Breaking point","3 Breaking point",
               "4 Breaking point","5 Breaking point","6 Breaking point")
#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)
