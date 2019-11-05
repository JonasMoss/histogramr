context("histogramr")

set.seed(313)
x = rbeta(10, 2, 7)

expect_error(histogramr(NA))
expect_error(histogramr(x, breaks = 12))
expect_error(histogramr(c(x, -1), breaks = 5))
expect_error(histogramr(x))


## Test of KL histograms

ex_ea = histogramr(x, method = "exact", constraint = "equal_area",
                   breaks = 5,plot = FALSE)

ex = histogramr(x, method = "exact", breaks = 5, plot = FALSE)

gr_ea = histogramr(x, method = "greedy", constraint = "equal_area",
                   breaks = 5,plot = FALSE)

gr = histogramr(x, method = "greedy", breaks = 5, plot = FALSE)


expect_equal(ex_ea$counts, c(2, 2, 2, 2, 2))
expect_equal(ex_ea$xname, "x")
expect_equal(ex_ea$equidist, FALSE)
expect_equal(attr(ex_ea, "constraint"), "equal_area")
expect_equal(attr(ex_ea, "type"), "KL")
expect_equal(attr(ex_ea, "method"), "exact")
expect_equal(attr(ex_ea, "support"), c(0, 1))

expect_equal(ex$counts, c(0, 7, 1, 1, 1))
expect_equal(ex$xname, "x")
expect_equal(ex$equidist, FALSE)
expect_equal(attr(ex, "constraint"), "none")
expect_equal(attr(ex, "type"), "KL")
expect_equal(attr(ex, "method"), "exact")
expect_equal(attr(ex, "support"), c(0, 1))

expect_equal(gr_ea$counts, c(2, 2, 2, 2, 2))
expect_equal(gr_ea$xname, "x")
expect_equal(gr_ea$equidist, FALSE)
expect_equal(attr(gr_ea, "constraint"), "equal_area")
expect_equal(attr(gr_ea, "type"), "KL")
expect_equal(attr(gr_ea, "method"), "greedy")
expect_equal(attr(gr_ea, "support"), c(0, 1))

expect_equal(gr$counts, c(1, 2, 2, 2, 3))
expect_equal(gr$xname, "x")
expect_equal(gr$equidist, FALSE)
expect_equal(attr(gr, "constraint"), "none")
expect_equal(attr(gr, "type"), "KL")
expect_equal(attr(gr, "method"), "greedy")
expect_equal(attr(gr, "support"), c(0, 1))

## Test of L2 histograms

ex_ea = histogramr(x, method = "exact", constraint = "equal_area",
                   breaks = 5,plot = FALSE, type = "L2")

ex = histogramr(x, method = "exact", breaks = 5, plot = FALSE,
                type = "L2")

gr_ea = histogramr(x, method = "greedy", constraint = "equal_area",
                   breaks = 5, plot = FALSE, type = "L2")

gr = histogramr(x, method = "greedy", breaks = 5, plot = FALSE, type = "L2")

expect_equal(ex_ea$counts, c(2, 2, 2, 2, 2))
expect_equal(ex_ea$xname, "x")
expect_equal(ex_ea$equidist, FALSE)
expect_equal(attr(ex_ea, "constraint"), "equal_area")
expect_equal(attr(ex_ea, "type"), "L2")
expect_equal(attr(ex_ea, "method"), "exact")
expect_equal(attr(ex_ea, "support"), c(0, 1))

expect_equal(ex$counts, c(0, 7, 1, 1, 1))
expect_equal(ex$xname, "x")
expect_equal(ex$equidist, FALSE)
expect_equal(attr(ex, "constraint"), "none")
expect_equal(attr(ex, "type"), "L2")
expect_equal(attr(ex, "method"), "exact")
expect_equal(attr(ex, "support"), c(0, 1))

expect_equal(gr_ea$counts, c(2, 2, 2, 2, 2))
expect_equal(gr_ea$xname, "x")
expect_equal(gr_ea$equidist, FALSE)
expect_equal(attr(gr_ea, "constraint"), "equal_area")
expect_equal(attr(gr_ea, "type"), "L2")
expect_equal(attr(gr_ea, "method"), "greedy")
expect_equal(attr(gr_ea, "support"), c(0, 1))

expect_equal(gr$counts, c(1, 2, 2, 2, 3))
expect_equal(gr$xname, "x")
expect_equal(gr$equidist, FALSE)
expect_equal(attr(gr, "constraint"), "none")
expect_equal(attr(gr, "type"), "L2")
expect_equal(attr(gr, "method"), "greedy")
expect_equal(attr(gr, "support"), c(0, 1))

## Test of initial values

gr = histogramr(x, method = "greedy", breaks = 5, plot = FALSE,
                control = list(modulator = 1,
                               eps = 0.1,
                               init = c(2, 4, 6, 8)))
expect_equal(attr(gr, "control")$modulator, 1)
expect_equal(attr(gr, "control")$eps, 0.1)
expect_equal(unname(attr(gr, "control")$init), c(2, 4, 6, 8))

## Check no return when plot = TRUE
expect_null(histogramr(x, method = "greedy", breaks = 5))

