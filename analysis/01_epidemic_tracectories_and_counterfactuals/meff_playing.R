mobility <- seq(1, 0, -0.1)
plot(0, 1, xlim = c(2, -1), ylim = c(0, 3), xlab = "mobility", ylab = "Reproduction Number")
for(i in c(-6, -2, 0)){
  lines(mobility - 1, 2 * plogis(-(mobility - 1) * i), col = blues9[((i) * -1) + 3], lwd = 4)
}



R0 <- 3
mobility <- seq(1, -1, -0.1)
plot(0, 1, xlim = c(1, -1), ylim = c(0, 2 * R0), xlab = "mobility", ylab = "Effect Size", cex = 0)
for(i in c(-6, -3, 0)){
  lines(mobility, R0 * 2 * plogis(-mobility * i), col = "blue", lwd=4)
}


mobility <- seq(2, 0, -0.1)
plot(0,1, xlim = c(1,-1), ylim = c(0,2), xlab = "mobility", ylab = "Effect Size")
for(i in c(-6,-2,0)){lines(mobility-1, 2 * plogis(-(mobility-1) * i), col = blues9[((i)*-1)+3], lwd=4)}

t <- seq(-190, 190, 1)
sig <- 1 / (1 + exp(-0.2 * (t - 0)))
plot(t, sig, xlim = c(min(t), max(t)), ylim = c(0, 1))
