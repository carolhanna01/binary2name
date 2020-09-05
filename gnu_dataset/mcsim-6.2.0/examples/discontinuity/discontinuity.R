# MCSim solution lsodes
out = read.delim("discontinuity_lsodes.out", skip=2)
head(out)
print(out$Q[501],  digits = 20) #  9.99955
print(out$dQ[501], digits = 20) # -0.999955 is the derivative
xlims = c(0, 200)
ylims = c(0, 11)
plot(out$Time, out$Q, type="p", xlim=xlims, ylim=ylims,
     xlab = "Time", ylab = "Q")
par(new=T)

# analytical solution
rate_in = 1;
ke = 0.1;
times = seq(0, 100, 0.2)

# during positive input
Y = rate_in / ke * (1 - exp(-ke * times))
plot(times, Y, type="l", xlim=xlims, ylim=ylims, col="red",
     xlab="", ylab="", xaxt="n", yaxt="n")
par(new=T)
top = Y[501]
# top = 9.9995460007023755367, not exactly rate_in / ke because of limited time

# during zero output
Y = top * exp(-ke * times)
plot(times+100, Y, type="l", xlim=xlims, ylim=ylims, col="red",
     xlab="", ylab="", xaxt="n", yaxt="n")
par(new=T)
# Y[1] = top, as expected
# Y[2] = 9.8015417235583388589 at time 100.02
# dt(Y) at time 100 is -ke * top = -0.99995460007023755367

# MCSim solution cvodes
out2 = read.delim("discontinuity_cvodes.out", skip=2)
head(out2)
print(out2$Q[501],  digits = 20) #  9.99955
print(out2$Q[502],  digits = 20) #  9.80164
print(out2$dQ[501], digits = 20) # -0.999955
xlims = c(0, 200)
ylims = c(0, 11)
plot(out2$Time, out2$Q, type="l", xlim=xlims, ylim=ylims, col="blue",
     xlab = "Time", ylab = "Q")

# check more closely
plot(out$Q, out2$Q, xlim=c(9.8, 10.01), ylim=c(9.8, 10.01))
abline(0,1)

plot(out$dQ, out2$dQ, xlim=c(-1, -0.8), ylim=c(-1, -0.8))
abline(0,1)

# fine!


