#### Code to make graphs appearing in "How Newspapers Reveal Political Power"
#### (Ban, Fouirnaies, Hall, Snyder)
#### 
#### Takes the .dta files produced by the do-file make_main_items.do and
#### produces the final graphs (Figures 1-6, A.3-A.4) appearing in the manuscript
####
#### November 20, 2016


rm(list=ls())
library(foreign)
library(zoo)


makeTransparent<-function(someColor, alpha)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


#### Figure 1: Committee Rankings, 1949-1973

data <- read.csv("for_committees_r_graph.csv")
data$gs <- as.numeric(as.vector(data$GS_rank))
data$newspaper <- as.numeric(as.vector(data$rank))
data.81 <- data[1:19,]

reg81 <- lm(data.81$newspaper ~ data.81$gs)

x <- data.81$gs
reg1 <- lm(data.81$newspaper ~ x)
fits1 <- predict(reg1, newdata=data.frame(x=seq(-5,25,length=100)), type="response", interval="confidence")

cor(data.81$newspaper, data.81$gs)

pdf("Fig1_committees_rankings.pdf")
par(mgp=c(3,.5,0), cex.axis=1.2, cex.lab=1.2)
plot(x=data.81$gs, y=data.81$newspaper, col="white", xaxt="n", yaxt="n", xlab="Groseclose-Stewart Ranking", ylab="Coverage-based Ranking", xlim=c(22,-2), ylim=c(22,-2), main="")
grid()
text(x=data.81$gs, y=data.81$newspaper, data.81$committee, col="black")
axis(side=1, at=c(20,15,10,5,1), labels=c(20,15,10,5,1))
axis(side=2, las=1, at=c(20,15,10,5,1), labels=c(20,15,10,5,1))
abline(reg81, col=makeTransparent("springgreen4", 150), lty=2, lwd=2)
polygon(x=c(seq(-5,25,length=100),rev(seq(-5,25,length=100))), y=c(fits1[,2], rev(fits1[,3])), col=makeTransparent("gray70", 70), border=NA)
text(x=18, y=-1, "Correlation = 0.74", col="springgreen4", cex=1.2)
dev.off()


#### Figure 2: News Coverage of Speakers of the House, Before, During, and After Speakership

data <- read.dta("hits_speakers.dta")

pdf(file="Fig2_leaders_before_after.pdf", height=5, width=7)
par(mgp=c(3,.5,0))
plot(x=1:3, y=data[1,2:4], ylim=c(0,2500), col="white", xlab="", ylab="Newspaper Mentions", xaxt="n", yaxt="n")
for (i in 1:nrow(data)) {
  lines(x=1:3, y=data[i, 2:4], col="gray70")
}
lines(x=1:3, y=colMeans(data[,2:4]), col="springgreen4", lwd=5)
axis(side=1, at=1:3, labels=c("Before", "During Leadership", "After"))
axis(side=2, las=1)
dev.off()



#### Figure 3: Relative Coverage of City Offices Over Time

rm(list=ls())
makeTransparent<-function(someColor, alpha)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

data <- read.dta("for_mayor_r_graph.dta")
upper.mayor <- data$r_mayor + 1.96*data$r_mayor_sd
lower.mayor <- data$r_mayor - 1.96*data$r_mayor_sd
upper.mayor.control <- data$r_control_mayor + 1.96*data$r_control_mayor_sd
lower.mayor.control <- data$r_control_mayor - 1.96*data$r_control_mayor_sd

pdf(file="Fig3_mayors_new.pdf", height=6, width=8)

par(mfrow=c(2,2), oma=c(3,3,3,3), mar=c(0.3, 0.1, 1, 1), mgp=c(3,.5,0))

plot(x=data$t, y=data$r_control_mayor, ylim=c(0.3, .8), ylab="Relative Coverage of Mayor", xlab="", xaxt="n", yaxt="n", col="white")
grid()
abline(v=0, lty=2, col="gray40")
polygon(x=c(data$t, rev(data$t)), y=c(lower.mayor.control, rev(upper.mayor.control)), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(data$t, rev(data$t)), y=c(lower.mayor, rev(upper.mayor)), border=NA, col=makeTransparent("springgreen4", 70))
points(x=data$t, y=data$r_mayor, pch=21, col="black", bg="springgreen4")
points(x=data$t, y=data$r_control_mayor, pch=22, col="black", bg="gray80")
axis(side=2, las=1)
legend("topright", pch=c(21, 22), col=c("black", "black"), pt.bg=c("springgreen4", "gray80"), c("Reform Cities", "Control Cities"), bty="n")
mtext(side=3, "Relative Coverage of Mayors", line=.1)

upper.cm <- data$r_city_manager + 1.96*data$r_city_manager_sd
lower.cm <- data$r_city_manager - 1.96*data$r_city_manager_sd
upper.cm.control <- data$r_control_city_manager + 1.96*data$r_control_city_manager_sd
lower.cm.control <- data$r_control_city_manager - 1.96*data$r_control_city_manager_sd

plot(x=data$t, y=data$r_control_city_manager, ylim=c(0, .4), ylab="Relative Coverage of City Manager", xlab="", xaxt="n", yaxt="n", col="white")
grid()
abline(v=0, lty=2, col="gray40")
polygon(x=c(data$t, rev(data$t)), y=c(lower.cm.control, rev(upper.cm.control)), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(data$t, rev(data$t)), y=c(lower.cm, rev(upper.cm)), border=NA, col=makeTransparent("springgreen4", 70))
points(x=data$t, y=data$r_city_manager, pch=21, col="black", bg="springgreen4")
points(x=data$t, y=data$r_control_city_manager, pch=22, col="black", bg="gray80")
axis(side=4, las=1)
legend("topright", pch=c(21, 22), col=c("black", "black"), pt.bg=c("springgreen4", "gray80"), c("Reform Cities", "Control Cities"), bty="n")
mtext(side=3, "Relative Coverage of City Managers", line=.1)

upper.cc <- data$r_city_council + 1.96*data$r_city_council_sd
lower.cc <- data$r_city_council - 1.96*data$r_city_council_sd
upper.cc.control <- data$r_control_city_council + 1.96*data$r_control_city_council_sd
lower.cc.control <- data$r_control_city_council - 1.96*data$r_control_city_council_sd

plot(x=data$t, y=data$r_control_city_council, ylim=c(0.1, .5), ylab="Relative Coverage of City Council", xlab="Years Until Reform", xaxt="n", yaxt="n", col="white")
grid()
abline(v=0, lty=2, col="gray40")
polygon(x=c(data$t, rev(data$t)), y=c(lower.cc.control, rev(upper.cc.control)), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(data$t, rev(data$t)), y=c(lower.cc, rev(upper.cc)), border=NA, col=makeTransparent("springgreen4", 70))
points(x=data$t, y=data$r_city_council, pch=21, col="black", bg="springgreen4")
points(x=data$t, y=data$r_control_city_council, pch=22, col="black", bg="gray80")
axis(side=2, las=1)
axis(side=1)
mtext(side=3, "Relative Coverage of City Council", line=.1)

upper.rel <- data$rel_mayor_council + 1.96*data$rel_mayor_council_sd
lower.rel <- data$rel_mayor_council - 1.96*data$rel_mayor_council_sd
upper.rel.control <- data$rel_mayor_council_control + 1.96*data$rel_mayor_council_control_sd
lower.rel.control <- data$rel_mayor_council_control - 1.96*data$rel_mayor_council_control_sd

plot(x=data$t, y=data$rel_mayor_council, ylim=c(0.4, .9), ylab="Coverage of Mayor Relative to City Council", xlab="Years Until Reform", xaxt="n", yaxt="n", col="white")
grid()
abline(v=0, lty=2, col="gray40")
polygon(x=c(data$t, rev(data$t)), y=c(lower.rel.control, rev(upper.rel.control)), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(data$t, rev(data$t)), y=c(lower.rel, rev(upper.rel)), border=NA, col=makeTransparent("springgreen4", 70))
points(x=data$t, y=data$rel_mayor_council, pch=21, col="black", bg="springgreen4")
points(x=data$t, y=data$rel_mayor_council_control, pch=22, col="black", bg="gray80")
axis(side=4, las=1)
axis(side=1)
mtext(side=3, "Coverage of Mayor Relative to City Council", line=.1)

mtext(side=1, outer=T, line=2, cex=0.8, "Time Until Reform")

dev.off()


#### Figure A.3: Relative Coverage of City Offices Over Time: Filtering Results by City Name
### mayors graph with filtering
rm(list=ls())

makeTransparent<-function(someColor, alpha)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

data <- read.dta("for_mayor_r_graph.dta")

upper.mayor <- data$r_mayor_x + 1.96*data$r_mayor_x_sd
lower.mayor <- data$r_mayor_x - 1.96*data$r_mayor_x_sd
upper.mayor.control <- data$r_control_mayor + 1.96*data$r_control_mayor_sd
lower.mayor.control <- data$r_control_mayor - 1.96*data$r_control_mayor_sd

pdf(file="FigA3_mayors_new_x.pdf", height=6, width=8)

par(mfrow=c(2,2), oma=c(3,3,3,3), mar=c(0.3, 0.1, 1, 1), mgp=c(3,.5,0))

plot(x=data$t, y=data$r_control_mayor, ylim=c(0.2, 1), ylab="Relative Coverage of Mayor", xlab="", xaxt="n", yaxt="n", col="white")
grid()
abline(v=0, lty=2, col="gray40")
polygon(x=c(data$t, rev(data$t)), y=c(lower.mayor.control, rev(upper.mayor.control)), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(data$t, rev(data$t)), y=c(lower.mayor, rev(upper.mayor)), border=NA, col=makeTransparent("springgreen4", 70))
points(x=data$t, y=data$r_mayor_x, pch=21, col="black", bg="springgreen4")
points(x=data$t, y=data$r_control_mayor, pch=22, col="black", bg="gray80")
axis(side=2, las=1)
mtext(side=3, "Relative Coverage of Mayors", line=.1)

upper.cm <- data$r_city_manager_x + 1.96*data$r_city_manager_x_sd
lower.cm <- data$r_city_manager_x - 1.96*data$r_city_manager_x_sd
upper.cm.control <- data$r_control_city_manager + 1.96*data$r_control_city_manager_sd
lower.cm.control <- data$r_control_city_manager - 1.96*data$r_control_city_manager_sd

plot(x=data$t, y=data$r_control_city_manager, ylim=c(0, .5), ylab="Relative Coverage of City Manager", xlab="", xaxt="n", yaxt="n", col="white")
grid()
abline(v=0, lty=2, col="gray40")
polygon(x=c(data$t, rev(data$t)), y=c(lower.cm.control, rev(upper.cm.control)), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(data$t, rev(data$t)), y=c(lower.cm, rev(upper.cm)), border=NA, col=makeTransparent("springgreen4", 70))
points(x=data$t, y=data$r_city_manager_x, pch=21, col="black", bg="springgreen4")
points(x=data$t, y=data$r_control_city_manager, pch=22, col="black", bg="gray80")
axis(side=4, las=1)
legend("topright", pch=c(21, 22), col=c("black", "black"), pt.bg=c("springgreen4", "gray80"), c("Reform Cities", "Control Cities"), bty="n")
mtext(side=3, "Relative Coverage of City Managers", line=.1)

upper.cc <- data$r_city_council_x + 1.96*data$r_city_council_x_sd
lower.cc <- data$r_city_council_x - 1.96*data$r_city_council_x_sd
upper.cc.control <- data$r_control_city_council + 1.96*data$r_control_city_council_sd
lower.cc.control <- data$r_control_city_council - 1.96*data$r_control_city_council_sd

plot(x=data$t, y=data$r_control_city_council, ylim=c(0.1, .6), ylab="Relative Coverage of City Council", xlab="Years Until Reform", xaxt="n", yaxt="n", col="white")
grid()
abline(v=0, lty=2, col="gray40")
polygon(x=c(data$t, rev(data$t)), y=c(lower.cc.control, rev(upper.cc.control)), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(data$t, rev(data$t)), y=c(lower.cc, rev(upper.cc)), border=NA, col=makeTransparent("springgreen4", 70))
points(x=data$t, y=data$r_city_council_x, pch=21, col="black", bg="springgreen4")
points(x=data$t, y=data$r_control_city_council, pch=22, col="black", bg="gray80")
axis(side=2, las=1)
axis(side=1)
mtext(side=3, "Relative Coverage of City Council", line=.1)

upper.rel <- data$rel_mayor_council_x + 1.96*data$rel_mayor_council_x_sd
lower.rel <- data$rel_mayor_council_x - 1.96*data$rel_mayor_council_x_sd
upper.rel.control <- data$rel_mayor_council_control + 1.96*data$rel_mayor_council_control_sd
lower.rel.control <- data$rel_mayor_council_control - 1.96*data$rel_mayor_council_control_sd

plot(x=data$t, y=data$rel_mayor_council, ylim=c(0.2, 1), ylab="Coverage of Mayor Relative to City Council", xlab="Years Until Reform", xaxt="n", yaxt="n", col="white")
grid()
abline(v=0, lty=2, col="gray40")
polygon(x=c(data$t, rev(data$t)), y=c(lower.rel.control, rev(upper.rel.control)), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(data$t, rev(data$t)), y=c(lower.rel, rev(upper.rel)), border=NA, col=makeTransparent("springgreen4", 70))
points(x=data$t, y=data$rel_mayor_council_x, pch=21, col="black", bg="springgreen4")
points(x=data$t, y=data$rel_mayor_council_control, pch=22, col="black", bg="gray80")
axis(side=4, las=1)
axis(side=1)
mtext(side=3, "Coverage of Mayor Relative to City Council", line=.1)

mtext(side=1, outer=T, line=2, cex=0.8, "Time Until Reform")

dev.off()

#### Figure 4: Relative Coverage of the Massachusetts Executive Council Over Time

data <- read.dta("hits_govcouncil.dta")
data <- na.omit(data)

x1 <- data[data$year <= 1959,]$year
x2 <- data[data$year > 1959,]$year

reg0 <- lm(data[data$year <= 1959,]$r_newspaperscom ~ x1)
reg1 <- lm(data[data$year > 1959,]$r_newspaperscom ~ x2)

fits0 <- predict(reg0, newdata=data.frame(x1=seq(1933, 1959, length=100)), type="response", interval="confidence")
fits1 <- predict(reg1, newdata=data.frame(x2=seq(1965, 1980, length=100)), type="response", interval="confidence")

pdf(file="Fig4_ma_gov.pdf", height=7, width=10)
par(mgp=c(3,.5,0), oma=c(2,2,2,2))
plot(x=data$year, y=data$r_newspaperscom, pch=1, cex.lab=1.2, cex=1.5, xlab="", ylab="", yaxt="n", main="", col="gray50", ylim=c(.05, .16), cex.axis=1.6)
grid()
axis(side=2, las=1, cex.axis=1.6)
abline(v=1959, lty=2)
abline(v=1965, lty=2)
lines(x=seq(1933, 1959, length=100), y=fits0[,1], col="springgreen4", lwd=3)
lines(x=seq(1965, 1980, length=100), y=fits1[,1], col="springgreen4", lwd=3)
polygon(x=c(seq(1933, 1959, length=100), rev(seq(1933, 1959, length=100))), y=c(fits0[,2], rev(fits0[,3])), col=makeTransparent("gray70", 90), border=NA)
polygon(x=c(seq(1965, 1980, length=100), rev(seq(1965, 1980, length=100))), y=c(fits1[,2], rev(fits1[,3])), col=makeTransparent("gray70", 90), border=NA)
abline(v=0, lty=2)
mtext(side=2, line=4, "Relative Coverage of MA Executive Council", cex=1.4)

dev.off()

#### Figure A.4: Relative Coverage of the Massachusetts Executive Council Over Time: Boston Globe Coverage

data <- read.dta("hits_govcouncil.dta")
data <- na.omit(data)

x1 <- data[data$year <= 1959,]$year
x2 <- data[data$year > 1959,]$year

reg0 <- lm(data[data$year <= 1959,]$r_Globe ~ x1)
reg1 <- lm(data[data$year > 1959,]$r_Globe ~ x2)

fits0 <- predict(reg0, newdata=data.frame(x1=seq(1933, 1959, length=100)), type="response", interval="confidence")

fits1 <- predict(reg1, newdata=data.frame(x2=seq(1965, 1980, length=100)), type="response", interval="confidence")

pdf(file="FigA4_ma_gov_globe.pdf", height=7, width=10)
par(mgp=c(3,.5,0), oma=c(2,2,2,2))
plot(x=data$year, y=data$r_Globe, pch=1, cex.lab=1.2, cex=1.5, xlab="", ylab="", yaxt="n", main="", col="gray50", ylim=c(.02, .1), cex.axis=1.6)
grid()
axis(side=2, las=1, cex.axis=1.6)
abline(v=1959, lty=2)
abline(v=1965, lty=2)
lines(x=seq(1933, 1959, length=100), y=fits0[,1], col="springgreen4", lwd=3)
lines(x=seq(1965, 1980, length=100), y=fits1[,1], col="springgreen4", lwd=3)
polygon(x=c(seq(1933, 1959, length=100), rev(seq(1933, 1959, length=100))), y=c(fits0[,2], rev(fits0[,3])), col=makeTransparent("gray70", 90), border=NA)
polygon(x=c(seq(1965, 1980, length=100), rev(seq(1965, 1980, length=100))), y=c(fits1[,2], rev(fits1[,3])), col=makeTransparent("gray70", 90), border=NA)
#mtext(side=3, "Relative Coverage of MA Executive Council", font=2, line=.5)
abline(v=0, lty=2)
mtext(side=2, line=4, "Relative Coverage of MA Executive Council", cex=1.4)
dev.off()



#### Figure 5: Relative Coverage of Congress in Tariff Policymaking

data <- read.dta("for_tariff_r_graph.dta")

makeTransparent<-function(someColor, alpha)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

x1 <- data$t[data$post==0]
x2 <- data$t[data$post==1]
reg1 <- lm(data$r[data$post==0] ~ x1)
reg2 <- lm(data$r[data$post==1] ~ x2)

fits1 <- predict(reg1, newdata=data.frame(x1=seq(1870, 1932.5, length=100)), type="response", interval="confidence")
fits2 <- predict(reg2, newdata=data.frame(x2=seq(1932.5, 1985, length=100)), type="response", interval="confidence")

pdf(file="Fig5_tariff.pdf", height=7, width=10)
par(oma=c(2,2,2,2), mgp=c(3,.7,0))
plot(x=data$t, y=data$r, xlab="", ylab="", xaxt="n", yaxt="n", col="gray20", cex.lab=1.2, cex=1.5, pch=1)
grid()
lines(x=seq(1870, 1932.5, length=100), y=fits1[,1], col="springgreen4", lwd=5, lend=2)
lines(x=seq(1932.5, 1985, length=100), y=fits2[,1], col="springgreen4", lwd=5, lend=2)
polygon(x=c(seq(1870, 1932.5, length=100), rev(seq(1870, 1932.5, length=100))), y=c(fits1[,2], rev(fits1[,3])), border=NA, col=makeTransparent("gray70", 90))
polygon(x=c(seq(1932.5, 1985, length=100), rev(seq(1932.5, 1985, length=100))), y=c(fits2[,2], rev(fits2[,3])), border=NA, col=makeTransparent("gray70", 90))
abline(v=1932.5, lty=2)
axis(side=1, cex.axis=1.6)
axis(side=2, las=1, at=seq(0.35,0.65,.1), labels=seq(0.35, 0.65, .1), cex.axis=1.6)
mtext(side=2, line=4, "Relative Coverage of Congress", cex=1.4)
dev.off()


#### Figure 6: Party Committee Power Over Time in Nine U.S. States ****updated version

data <- read.dta("for_partycommittee_r_graph.dta")
data <- na.omit(data)
min <- 0
max <- 2.7

data$party_power_norm <- ave(data$party_power_norm,data$state,FUN=function(x) rollapply(x,2,mean,align="right",fill=NA))

pdf(file="Fig6_state_power.pdf", height=8, width=10)
par(mfrow=c(3,3), oma=c(3,4,3,3), mar=c(1,1,.1,.3), cex.lab=1.3, mgp=c(3,.5,0), cex.axis=1.3)

plot(x=data$year[data$state=="OH"], y=data$party_power_norm[data$state=="OH"], type="l", yaxt="n", xlab="", ylab="", xaxt="n", ylim=c(min, max), xlim=c(1875, 1980))
text(x=1970, y=max-.2, "OH", font=2, cex=1.7)
axis(side=2, las=1, at=seq(0, 3, 1), labels=seq(0,3,1))
mtext(side=2, "Party Committee Power", line=2, cex=1)

plot(x=data$year[data$state=="CA"], y=data$party_power_norm[data$state=="CA"], type="l", yaxt="n", xlab="", ylab="", xaxt="n", ylim=c(min, max), xlim=c(1875, 1980))
text(x=1970, y=max-.2, "CA", font=2, cex=1.7)

plot(x=data$year[data$state=="IL"], y=data$party_power_norm[data$state=="IL"], type="l", yaxt="n", xlab="", ylab="", xaxt="n", ylim=c(min, max), xlim=c(1875, 1980))
text(x=1970, y=max-.2, "IL", font=2, cex=1.7)

plot(x=data$year[data$state=="KS"], y=data$party_power_norm[data$state=="KS"], type="l", yaxt="n", xlab="", ylab="", xaxt="n", ylim=c(min, max), xlim=c(1875, 1980))
axis(side=2, las=1, at=seq(0, 3, 1), labels=seq(0,3,1))
text(x=1970, y=max-.2, "KS", font=2, cex=1.7)
mtext(side=2, "Party Committee Power", line=2, cex=1)

plot(x=data$year[data$state=="WI"], y=data$party_power_norm[data$state=="WI"], type="l", yaxt="n", xlab="", ylab="", xaxt="n", ylim=c(min, max), xlim=c(1875, 1980))
text(x=1970, y=max-.2, "WI", font=2, cex=1.7)

plot(x=data$year[data$state=="PA"], y=data$party_power_norm[data$state=="PA"], type="l", yaxt="n", xlab="", ylab="", xaxt="n", ylim=c(min, max), xlim=c(1875, 1980))
text(x=1970, y=max-.2, "PA", font=2, cex=1.7)

plot(x=data$year[data$state=="NC"], y=data$party_power_norm[data$state=="NC"], type="l", yaxt="n", xlab="", ylab="", ylim=c(min, max), xlim=c(1875, 1980))
axis(side=2, las=1, at=seq(0, 3, 1), labels=seq(0,3,1))
text(x=1970, y=max-.2, "NC", font=2, cex=1.7)
mtext(side=2, "Party Committee Power", line=2, cex=1)

plot(x=data$year[data$state=="TX"], y=data$party_power_norm[data$state=="TX"], type="l", yaxt="n", xlab="", ylab="", ylim=c(min, max), xaxt="n", xlim=c(1875, 1980))
axis(side=1, at=seq(1880, 1980, 20), labels=seq(1880, 1980, 20))
text(x=1970, y=max-.2, "TX", font=2, cex=1.7)

plot(x=data$year[data$state=="NY"], y=data$party_power_norm[data$state=="NY"], type="l", yaxt="n", xlab="", ylab="", ylim=c(min, max), xlim=c(1875, 1980))
text(x=1970, y=max-.2, "NY", font=2, cex=1.7)

dev.off()





