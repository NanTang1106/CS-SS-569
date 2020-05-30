library(MASS) ## robust regression
library(Epi) ## draw shaded area

## import data 
dt1 <- read.delim("iverRevised.txt", header = TRUE, sep = ",")
dt2 <- dt1
dt2$povertyReduction <- as.numeric(dt1$povertyReduction)
dt2$effectiveParties <- as.numeric(dt1$effectiveParties)
x_dt <- dt2$effectiveParties
y_dt <- dt2$povertyReduction

## text of points
x_txt1 <- x_dt[-c(3, 4, 9, 10)]
y_txt1 <- y_dt[-c(3, 4, 9, 10)]
label_txt1 <- dt1$country[-c(3, 4, 9, 10)]

## type and color of points
pch_dt <- numeric(14)
pch_dt[dt1$partySystem=='Unanimity'] <- 19
pch_dt[dt1$partySystem=='Majoritarian'] <- 17
pch_dt[dt1$partySystem=='Proportional'] <- 15

col_dt <- numeric(14)
col_dt[dt1$partySystem=='Unanimity'] <- 'firebrick3'
col_dt[dt1$partySystem=='Majoritarian'] <- 'dodgerblue2'
col_dt[dt1$partySystem=='Proportional'] <- 'chartreuse3'

## robust regression line and confidence interval
x=x_dt[-12]
y=y_dt[-12]
lm.out <- rlm(y ~ log10(x))
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)

## original scatter plot
plot(x_dt, y_dt, log='x', ylim=c(0, 80), xlim=c(1.6, max(x_dt)), 
     axes=F, pch=pch_dt, col=col_dt,
     xlab='Effective number of parties',
     ylab='% lifted from poverty by taxes & fransfers')

## rescale and rename axis
axis(side=1, c(2, 3, 4, 5, 6, 7), pos=0, lty=0)
axis(side=2, c(0, 20, 40, 60, 80), las=1, lty=0)

## add annotations
text(x_txt1, y_txt1, pos=1, labels=label_txt1, cex=0.5)
text(x_dt[3], y_dt[3], pos=4, labels=dt1$country[3], cex=0.5)
text(x_dt[c(4, 9)], y_dt[c(4, 9)], pos=3, labels=dt1$country[c(4, 9)], cex=0.5)
text(x_dt[10], y_dt[10], pos=2, labels=dt1$country[10], cex=0.5)

## add fitted line and confidence interval
matshade(newx, conf_interval, col = "gray5", log='x')

## add marginal distribution
segments(x0=1, y0=y, x1=1.6, y1=y)
segments(x0=x_dt, y0=-5, x1=x_dt, y1=0)

## add legend
legend(1.65, 83, legend=c("Majoritarian", "Proportional", "Unanimity"), 
       col=c("dodgerblue2","chartreuse3","firebrick3"), 
       text.col = c("dodgerblue2","chartreuse3","firebrick3"),
       pch=c(17, 15, 19), cex=0.8, pt.cex = 1, box.lty=0)

