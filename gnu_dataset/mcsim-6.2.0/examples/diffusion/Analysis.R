#------------------------------------------------------------------------------
# Analysis.R
#
# Simple script to look at the diffusion simulation results
#
# Copyright (c) 2017 Free Software Foundation, Inc.
#------------------------------------------------------------------------------

# diffusion_1: compartmental transport

out = read.delim("diffusion_1.out", skip=2)
head(out)
names(out)
dim(out)
nt = dim(out)[1]
nx = dim(out)[2] - 2

ylims = c(-0.1, 1.1)
mycolors = rainbow(nx+1)
i = 2
plot(out$Time, out[,i], type="l", ylim=ylims, col=mycolors[i-1],
     xlab="Time", ylab="Quantity")
par(new=T)
for (i in 1+(2:nx)) {
  plot(out$Time, out[,i], type="l", ylim=ylims, xaxt="n", yaxt="n",
       xlab="", ylab="", col=mycolors[i-1])
  par(new=T)
}
i = nx + 2 # mass balance
plot(out$Time, out[,i], type="l", ylim=ylims, xaxt="n", yaxt="n",
     xlab="", ylab="", col="black")
par(new=F)
# OK


# diffusion_2: compartmental diffusion

out = read.delim("diffusion_2.out", skip=2)
head(out)
names(out)
dim(out)
nt = dim(out)[1]
nx = dim(out)[2] - 2

ylims = c(-0.1, 1.1)
mycolors = rainbow(nx+1)
i = 2
plot(out$Time, out[,i], type="l", ylim=ylims, col=mycolors[i-1],
     xlab="Time", ylab="Quantity")
par(new=T)
for (i in 1+(2:nx)) {
  plot(out$Time, out[,i], type="l", ylim=ylims, xaxt="n", yaxt="n",
       xlab="", ylab="", col=mycolors[i-1])
  par(new=T)
}
i = nx + 2 # mass balance
plot(out$Time, out[,i], type="l", ylim=ylims, xaxt="n", yaxt="n",
     xlab="", ylab="", col="black")
par(new=F)
# OK
tail(out,1)
1/nx

# diffusion_c2: continuous

out = read.delim("diffusion_c2.out", skip=2)
head(out)
names(out)
dim(out)
nt = dim(out)[1]
nx = (dim(out)[2] - 1)

ylims = c(-0.1, 1.1)
mycolors = rainbow(nx+1)
i = 2
plot(out$Time, out[,i], type="l", ylim=ylims, col=mycolors[i-1],
     xlab="Time", ylab="Quantity")
par(new=T)
for (i in 1+(2:nx)) {
  plot(out$Time, out[,i], type="l", ylim=ylims, xaxt="n", yaxt="n",
       xlab="", ylab="", col=mycolors[i-1])
  par(new=T)
}
par(new=F)
# OK

library(rgl)

x = 1:100

plot3d(x=rep(x,nt), y=rep(out$Time, each=nx),
       z=c((t(out[,2:dim(out)[2]]))), type="n",
       xlab="X", ylab="Time", zlab="C")
surface3d(x=x, y=out$Time,
          z=((t(out[,2:dim(out)[2]]))),
          xlab="X", ylab="Time", zlab="C")

