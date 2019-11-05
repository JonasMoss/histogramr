context("generics")

set.seed(313)
x = rbeta(10, 2, 7)
ex = histogramr(x, method = "exact", breaks = 5, plot = FALSE)
expect_equal(summary(ex), ex)
expect_equal(logLik(ex), attr(ex, "logLik"))