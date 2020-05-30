library(ggplot2)
library(dplyr)
library(tile)
library(simcf)
library(RColorBrewer)
library(plyr)   
library(MASS)

setwd('/Users/nantang/Google Drive/CS&SS 569/lab')
iver <- read.delim2('iverRevised.txt', sep=',')
iver <- data.frame(iver)
iver$povertyReduction <- as.numeric(levels(iver$povertyReduction)[iver$povertyReduction])
iver$effectiveParties <- as.numeric(levels(iver$effectiveParties)[iver$effectiveParties])

attach(iver)

col <- brewer.pal(3, "Set1")[c(2,3,1)]

# Next, recode the partySystem variable into both a set of symbols..
symbSystem <- mapvalues(partySystem,
                        from=c("Majoritarian", "Proportional", "Unanimity"),
                        to=c(17, 15, 16))
symbSystem <- as.numeric(as.character(symbSystem))

# ... and a set of colors, taking care to avoid problems with converting factors
colSystem <- mapvalues(partySystem,
                       from=c("Majoritarian", "Proportional", "Unanimity"),
                       to=col)
colSystem <- as.character(colSystem)

# First, collect all the data inputs into a series of "traces"

# The actual scattered points
trace1 <- scatter(x = effectiveParties, # X coordinate of the data
                  
                  y = povertyReduction, # Y coordinate of the data
                  
                  labels = country, # Labels for each point
                  
                  # Plot symbol for each point
                  pch = symbSystem,
                  
                  # Color for each point
                  col = colSystem,
                  
                  # Offset text labels
                  labelsyoffset = -0.035,  # on npc scale
                  
                  # Fontsize
                  fontsize = 9,
                  
                  # Marker size
                  size = 1,  # could be vector for bubble plot
                  
                  # Add a robust fit line and CI
                  fit = list(method="mmest", ci = 0.95),
                  
                  # Which plot(s) to plot to
                  plot = 1
)

# The rugs with marginal distributions
rugX1 <- rugTile(x=effectiveParties, type="lines", plot = 1)

rugY1 <- rugTile(y=povertyReduction, type="lines", plot = 1)

# A legend
legendSymbols1 <- pointsTile(x=  c(1.8,   1.8,       1.8),
                             y=  c(78,     74,      70),
                             pch=c(17,     15,      16),
                             col=col,
                             fontsize=9,
                             size=1,
                             plot=1
)

legendLabels1 <- textTile(labels=c("Majoritarian",
                                   "Proportional",
                                   "Unanimity"),
                          x=  c(2.05,      2.05,       2.05),
                          y=  c(78,     74,      70),
                          col=col,
                          fontsize=9,
                          plot=1
)

# Now, send that trace to be plotted with tile
tile(trace1,                # Could list as many traces here as we want
     rugX1,
     rugY1,
     legendSymbols1,
     legendLabels1,
     
     # Some generic options for tile
     RxC = c(1,1),
     height = list(plot="golden", plottitle=4),
     width = list(yaxistitle=3),
     
     limits=c(1.6, 7.5, 0, 82), # Limits of plotting region
     
     # x-axis controls
     xaxis=list(log = TRUE,
                at = c(2,3,4,5,6,7)
     ),
     
     xaxistitle=list(labels=c("Effective number of parties")),
     
     # y-axis controls
     yaxistitle=list(labels=c("% lifted from poverty by taxes & transfers")),
     
     # Plot titles
     plottitle=list(labels=("Party Systems and Redistribution")),
     
     # Output control
     output=list(width=8)
)

#
#
#
#
#
library('WhatIf')
data2 <- read.csv('https://faculty.washington.edu/cadolph/vis/nes00a.csv')
attach(data2)
# Estimate logit model using glm()
# Set up model formula and model specific data frame
model <- vote00 ~ age + I(age^2) + hsdeg + coldeg
mdata <- extractdata(model, data2, na.rm=TRUE)

# Run logit & extract results
logit.result <- glm(model, family=binomial, data=mdata)
pe.glm <- logit.result$coefficients  # point estimates
vc.glm <- vcov(logit.result)         # var-cov matrix


## Simulate quantities of interest using simcf
##
## We could do this from the optim or glm results;
## here, we do from glm

# Simulate parameter distributions
sims <- 10000
simbetas <- mvrnorm(sims, pe.glm, vc.glm)

# Set up counterfactuals:  all ages, each of three educations
xhyp <- seq(18,97,1)
nscen <- length(xhyp)
nohsScen <- hsScen <- collScen <- cfMake(model, mdata, nscen)
for (i in 1:nscen) {
  # No High school scenarios (loop over each age)
  nohsScen <- cfChange(nohsScen, "age", x = xhyp[i], scen = i)
  nohsScen <- cfChange(nohsScen, "hsdeg", x = 0, scen = i)
  nohsScen <- cfChange(nohsScen, "coldeg", x = 0, scen = i)
  
  # HS grad scenarios (loop over each age)
  hsScen <- cfChange(hsScen, "age", x = xhyp[i], scen = i)
  hsScen <- cfChange(hsScen, "hsdeg", x = 1, scen = i)
  hsScen <- cfChange(hsScen, "coldeg", x = 0, scen = i)
  
  # College grad scenarios (loop over each age)
  collScen <- cfChange(collScen, "age", x = xhyp[i], scen = i)
  collScen <- cfChange(collScen, "hsdeg", x = 1, scen = i)
  collScen <- cfChange(collScen, "coldeg", x = 1, scen = i)
}

# Simulate expected probabilities for all scenarios
nohsSims <- logitsimev(nohsScen, simbetas, ci=0.95)
hsSims <- logitsimev(hsScen, simbetas, ci=0.95)
collSims <- logitsimev(collScen, simbetas, ci=0.95)

# Get 3 nice colors for traces
col <- brewer.pal(3,"Dark2")

# Set up lineplot traces of expected probabilities
#
# When recycling this code, omit the extrapolate input
# if you are unsure how to use it correctly
nohsTrace <- lineplot(x=xhyp,
                      y=nohsSims$pe,
                      lower=nohsSims$lower,
                      upper=nohsSims$upper,
                      col=col[1],
                      extrapolate=list(data=mdata[,2:ncol(mdata)],
                                       cfact=nohsScen$x[,2:ncol(nohsScen$x)],
                                       omit.extrapolated=TRUE),
                      plot=1)

hsTrace <- lineplot(x=xhyp,
                    y=hsSims$pe,
                    lower=hsSims$lower,
                    upper=hsSims$upper,
                    col=col[2],
                    extrapolate=list(data=mdata[,2:ncol(mdata)],
                                     cfact=hsScen$x[,2:ncol(hsScen$x)],
                                     omit.extrapolated=TRUE),
                    plot=1)

collTrace <- lineplot(x=xhyp,
                      y=collSims$pe,
                      lower=collSims$lower,
                      upper=collSims$upper,
                      col=col[3],
                      extrapolate=list(data=mdata[,2:ncol(mdata)],
                                       cfact=collScen$x[,2:ncol(collScen$x)],
                                       omit.extrapolated=TRUE),
                      plot=1)

# Set up traces with labels and legend
labelTrace <- textTile(labels=c("Less than HS", "High School", "College"),
                       x=c( 55,    49,     30),
                       y=c( 0.26,  0.56,   0.87),
                       col=col,
                       plot=1)

legendTrace <- textTile(labels=c("Logit estimates:", "95% confidence", "interval is shaded"),
                        x=c(82, 82, 82),
                        y=c(0.2, 0.15, 0.10),
                        cex=0.9,
                        plot=1)

# Plot traces using tile
tile(nohsTrace,
     hsTrace,
     collTrace,
     labelTrace,
     legendTrace,
     limits=c(18,94,0,1),
     xaxis=list(at=c(20,30,40,50,60,70,80,90)),
     yaxis=list(label.loc=-0.5, major=FALSE),
     xaxistitle=list(labels="Age of Respondent"),
     yaxistitle=list(labels="Probability of Voting"),
     width=list(null=5,yaxistitle=4,yaxis.labelspace=-0.5),
     output=list(width=5.5)
)



################################################################
#
# Now consider a new specification adding the variable
# "ever married", or marriedo
#
# We will estimate this new model with glm(), then
# simulate new scenarios for marrieds and non-marrieds
#
# We could also rerun the age x education scenarios if we wanted


# Estimate logit model using glm()
# Set up model formula and model specific data frame
model2 <- vote00 ~ age + I(age^2) + hsdeg + coldeg + marriedo
mdata2 <- extractdata(model2, data2, na.rm=TRUE)

# Run logit & extract results
logit.m2 <- glm(model2, family=binomial, data=mdata2)
pe.m2 <- logit.m2$coefficients  # point estimates
vc.m2 <- vcov(logit.m2)         # var-cov matrix


# Simulate parameter distributions
sims <- 10000
simbetas.m2 <- mvrnorm(sims, pe.m2, vc.m2)


# Set up counterfactuals:  all ages, each of three educations
xhyp <- seq(18,97,1)
nscen <- length(xhyp)
marriedScen <- notmarrScen <- cfMake(model2, mdata2, nscen)
for (i in 1:nscen) {
  
  # Married (loop over each age)
  # Note below the careful use of before scenarios (xpre) and after scenarios (x)
  #  - we will use the marriedScen counterfactuals in FDs and RRs as well as EVs
  marriedScen <- cfChange(marriedScen, "age", x = xhyp[i], xpre= xhyp[i], scen = i)
  marriedScen <- cfChange(marriedScen, "marriedo", x = 1, xpre= 0, scen = i)
  
  # Married (loop over each age)
  notmarrScen <- cfChange(notmarrScen, "age", x = xhyp[i], scen = i)
  notmarrScen <- cfChange(notmarrScen, "marriedo", x = 0, scen = i)
}

# Simulate expected probabilities for all scenarios
marriedSims <- logitsimev(marriedScen, simbetas.m2, ci=0.95)
notmarrSims <- logitsimev(notmarrScen, simbetas.m2, ci=0.95)

# Simulate first difference of voting wrt marriage
marriedFD <- logitsimfd(marriedScen, simbetas.m2, ci=0.95)

# Simulate relative risk of voting wrt marriage
marriedRR <- logitsimrr(marriedScen, simbetas.m2, ci=0.95)


## Make plots using tile

# Get 3 nice colors for traces
col <- brewer.pal(3,"Dark2")

# Set up lineplot traces of expected probabilities
marriedTrace <- lineplot(x=xhyp,
                         y=marriedSims$pe,
                         lower=marriedSims$lower,
                         upper=marriedSims$upper,
                         col=col[1],
                         extrapolate=list(data=mdata2[,2:ncol(mdata2)],
                                          cfact=marriedScen$x[,2:ncol(marriedScen$x)],
                                          omit.extrapolated=TRUE),
                         plot=1)

notmarrTrace <- lineplot(x=xhyp,
                         y=notmarrSims$pe,
                         lower=notmarrSims$lower,
                         upper=notmarrSims$upper,
                         col=col[2],
                         ci = list(mark="dashed"),
                         extrapolate=list(data=mdata2[,2:ncol(mdata2)],
                                          cfact=notmarrScen$x[,2:ncol(notmarrScen$x)],
                                          omit.extrapolated=TRUE),
                         plot=1)


# Set up traces with labels and legend
labelTrace <- textTile(labels=c("Currently Married", "Not Married"),
                       x=c( 35,    53),
                       y=c( 0.8,  0.56),
                       col=col,
                       plot=1)

legendTrace <- textTile(labels=c("Logit estimates:", "95% confidence", "interval is shaded"),
                        x=c(80, 80, 80),
                        y=c(0.2, 0.15, 0.10),
                        cex=0.9,
                        plot=1)

# Plot traces using tile
tile(marriedTrace,
     notmarrTrace,
     labelTrace,
     legendTrace,
     limits=c(18,94,0,1),
     xaxis=list(at=c(20,30,40,50,60,70,80,90)),
     yaxis=list(label.loc=-0.5, major=FALSE),
     xaxistitle=list(labels="Age of Respondent"),
     yaxistitle=list(labels="Probability of Voting"),
     width=list(null=5,yaxistitle=4,yaxis.labelspace=-0.5),
     output=list(width=5.5)
)



# Plot First Difference

# Set up lineplot trace of relative risk
marriedFDTrace <- lineplot(x=xhyp,
                           y=marriedFD$pe,
                           lower=marriedFD$lower,
                           upper=marriedFD$upper,
                           col=col[1],
                           extrapolate=list(data=mdata2[,2:ncol(mdata2)],
                                            cfact=marriedScen$x[,2:ncol(marriedScen$x)],
                                            omit.extrapolated=TRUE),
                           plot=1)


# Set up baseline: for first difference, this is 0
baselineFD <- linesTile(x=c(18,94),
                        y=c(0,0),
                        plot=1)

# Set up traces with labels and legend
labelFDTrace <- textTile(labels=c("Married compared \n to Not Married"),
                         x=c( 40),
                         y=c( 0.20),
                         col=col[1],
                         plot=1)

legendFDTrace <- textTile(labels=c("Logit estimates:", "95% confidence", "interval is shaded"),
                          x=c(80, 80, 80),
                          y=c(-0.02, -0.05, -0.08),
                          cex=0.9,
                          plot=1)

# Plot traces using tile
tile(marriedFDTrace,
     labelFDTrace,
     legendFDTrace,
     baselineFD,
     limits=c(18,94,-0.1,0.5),
     xaxis=list(at=c(20,30,40,50,60,70,80,90)),
     yaxis=list(label.loc=-0.5, major=FALSE),
     xaxistitle=list(labels="Age of Respondent"),
     yaxistitle=list(labels="Difference in Probability of Voting"),
     width=list(null=5,yaxistitle=4,yaxis.labelspace=-0.5),
     output=list(file="marriedFD",width=5.5)
)


# Plot Relative Risk

# Set up lineplot trace of relative risk
marriedRRTrace <- lineplot(x=xhyp,
                           y=marriedRR$pe,
                           lower=marriedRR$lower,
                           upper=marriedRR$upper,
                           col=col[1],
                           extrapolate=list(data=mdata2[,2:ncol(mdata2)],
                                            cfact=marriedScen$x[,2:ncol(marriedScen$x)],
                                            omit.extrapolated=TRUE),
                           plot=1)


# Set up baseline: for relative risk, this is 1
baselineRR <- linesTile(x=c(18,94),
                        y=c(1,1),
                        plot=1)

# Set up traces with labels and legend
labelRRTrace <- textTile(labels=c("Married compared \n to Not Married"),
                         x=c( 55),
                         y=c( 1.25),
                         col=col[1],
                         plot=1)

legendRRTrace <- textTile(labels=c("Logit estimates:", "95% confidence", "interval is shaded"),
                          x=c(80, 80, 80),
                          y=c(0.98, 0.95, 0.92),
                          cex=0.9,
                          plot=1)

# Plot traces using tile
tile(marriedRRTrace,
     labelRRTrace,
     legendRRTrace,
     baselineRR,
     limits=c(18,94,0.9,1.5),
     xaxis=list(at=c(20,30,40,50,60,70,80,90)),
     yaxis=list(label.loc=-0.5, major=FALSE),
     xaxistitle=list(labels="Age of Respondent"),
     yaxistitle=list(labels="Relative Risk of Voting"),
     width=list(null=5,yaxistitle=4,yaxis.labelspace=-0.5),
     output=list(file="marriedRR",width=5.5)
)



######################################################
# Optional extension:  Side-by-side plots of FD and RR

# Reassign RR traces to plot 2
marriedRRTrace$plot <- labelRRTrace$plot <-
  legendRRTrace$plot <- baselineRR$plot<- 2

# Plot all traces using tile
# NOTE changes to limits, yaxistitles, and output width
tile(marriedFDTrace,
     labelFDTrace,
     legendFDTrace,
     baselineFD,
     marriedRRTrace,
     labelRRTrace,
     legendRRTrace,
     baselineRR,     
     limits=rbind(c(18,94,-0.1,0.5),
                  c(18,94,0.9,1.5)),
     xaxis=list(at=c(20,30,40,50,60,70,80,90)),
     yaxis=list(label.loc=-0.5, major=FALSE),
     xaxistitle=list(labels="Age of Respondent"),
     yaxistitle=list(labels1="Difference in Probability of Voting",
                     labels2="Relative Risk of Voting"),
     width=list(null=5,yaxistitle=4,yaxis.labelspace=-0.5),
     output=list(file="marriedFDRR",width=10)
)


