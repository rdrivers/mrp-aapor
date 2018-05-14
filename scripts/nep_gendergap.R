library(tidyverse)

load("~/Dropbox (YouGov Analytics)/Data/frame/NEP/2016/nep16.Rdata")

nep16 <- nep16 %>%
  mutate(state = droplevels(state))
  
fixef <- prop.table(
	xtabs(weight ~ gender + presvote16, data = nep16), 1)[,"clinton"]
fixef[2] <- fixef[2] - fixef[1]

tbl <- prop.table(xtabs(weight ~ state + presvote16 + gender, data = nep16),
	c(1,3))[,'clinton',] 
coefs <- data_frame(male = tbl[,1], gap = tbl[,2] - tbl[,1])
	
plot.gender.by.state <- function(main, coefs, fixef) {
  n <- nrow(coefs)
  x <- rep(c(0,1), rep(n, 2))
  y <- 100 * c(coefs[[1]], coefs[[1]] + coefs[[2]])
  plot(x = x, y = y, pch = 19, cex = 0.5, xlim = c(0, 1),
    ylim = c(0, 100), main = main,xlab = "Gender",
    ylab = "Percent voting for Clinton", axes = FALSE)
  axis(1, at = c(0, 1), labels = c("Male", "Female"))
  axis(2, at = seq(0, 100, 20))
  for (i in seq_len(n)) lines(x = c(0, 1),
    y = c(y[i], y[i+n]), col = "grey", lwd = 0.5)
  abline(100 * fixef, col = "red3", lwd = 2)
}
pdf("~/Projects/mrp-aapor/images/gender-gap-xp.pdf")
plot.gender.by.state("Exit poll", coefs, fixef)
dev.off()




