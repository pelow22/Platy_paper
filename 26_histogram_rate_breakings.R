#histogram of rate breakings (from table 1)

rate_breaks <- c(27, 21, 28, 20, 19, 8,
                 19, 13, 7, 20, 13, 6,
                 24, 10, 18, 12, 9,
                 6, 13, 7, 6, 6, 6)

png("Histogram_rate_breakings.png", width = 480, height = 480)
hist(rate_breaks, main = "Rate breakings", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,10),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()
