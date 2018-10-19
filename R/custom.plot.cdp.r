
#x <- mca.ts.partest

#plot.cdp(x, las = 2)
#plot(x, las = 2)

plot.cdp <- function (x, col, numscales = 20, ind, col.signif = 2, main = "", scale.labels, ...) {
    if(missing(col))
        col <- grey(seq(1, 0, length.out = 256))
    if(missing(ind)) ind <- rep(T, ncol(x$data$X))
    if(missing(scale.labels)) scale.labels <- 1L:numscales
    mar <- par()$mar
    z <- log10(x$UpYXcb$CM + 1e-04)[1:numscales, ind]
    par(mar = c(mar[1L], mar[2L], mar[3L], 0.75), fig = c(0, 0.875 - 0.025 * (mar[4L] - 2.1), 0, 1))
    image(y = 1L:sum(ind), x = 1L:numscales, z = z,
          zlim = c(-4, 1e-04), col = col, axes = FALSE, xlab = "", ylab = "",
          main = main, ...)
    box(...)
    axis(1, at = 1L:numscales, labels = scale.labels, ...)
    axis(2, at = 1L:sum(ind), labels = sub("\\." , " ", colnames(x$data$X)[ind]), ...)
    if (!is.null(x$test$signif)) 
        rect(xleft = x$test$signif$U - 0.5, xright = x$test$signif$U + 0.5,
             ybottom = cumsum(ind)[x$test$signif$X] - 0.5, ytop = cumsum(ind)[x$test$signif$X] + 0.5,
             border = col.signif, density = NULL, ...)
    par(mar = c(mar[1L], 0.75, mar[3L], mar[4L]), fig = c(0.875 - 0.025 * (mar[4L] - 2.1), 1, 0, 1), new = TRUE)
    image(z = matrix(seq(-4, 1e-04, length.out = 256), 1L, 256L), x = 0,
          y = seq(-4, 1e-04, length.out = 256), col = col, axes = FALSE,
          xlab = "", ylab = "", main = "", ...)
    box(...)
    axis(4, labels = 10^seq(-4, 0, 1), at = seq(-4, 0, 1), ...)
    par(mar = mar, fig = c(0, 1, 0, 1))
    return(invisible(NULL))
}