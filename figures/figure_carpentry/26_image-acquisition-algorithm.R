# From Ecology Letters figure guidelines, to give an example:
# single column (82 mm), two-thirds page width (110 mm) or full page width (173 mm)

dataf <- data.frame(x = seq(-3, 13, by = 0.5), y = 0)
timeWindow <- 2
boxHeight <- 0.25
boxAdj <- 0.1 # 0 means the box will be centered on the timeline, 1/2 and its bottom will be on the timeline, -1/2 and its top will be on the timeline

pdf("figures/image-acquisition-algorithm.pdf", width = 11.0 / 2.54, height = 5.0 / 2.54, pointsize = 10)
par(mar = c(0, 0, 0, 0))
plot(y ~ x, data = dataf, type = "p", axes = FALSE, xlab = NA, ylab = NA, pch = 19, cex = 0.5)
lines(y ~ x, data = dataf)
text(x = 0, y = -0.15, labels = "fire start date", srt = 45, adj = 1)
text(x = 12, y = -0.15, labels = "one year post fire", srt = 45, adj = 1)

symbols(x = -1 * (timeWindow / 2), y = boxHeight * boxAdj, rectangles = matrix(c(timeWindow, boxHeight), nrow = 1), inches = FALSE, add = TRUE)
text(x = -1 * (timeWindow / 2), y = 1.25 * boxHeight, labels = "Pre-fire imagery")

symbols(x = -1 * (timeWindow / 2) + 12, y = boxHeight * boxAdj, rectangles = matrix(c(timeWindow, boxHeight), nrow = 1), inches = FALSE, add = TRUE)
text(x = -1 * (timeWindow / 2) + 12, y = 1.25 * boxHeight, labels = "Post-fire imagery")

legend(x = 5, y = 1, pch = 19, pt.cex = 0.5, legend = "16-day Landsat image\nacquisition schedule", bty = "n", adj = 0, )

points(x = 0, y = 0, cex = 2, pch = 19, col = "red")
points(x = 12, y = 0, cex = 2, pch = 19)

dev.off()
