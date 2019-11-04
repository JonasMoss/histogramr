context("histogramr")

set.seed(313)
x = rbeta(10, 2, 7)

expect_error(histogramr(NA))
expect_error(histogramr(x, breaks = 12))