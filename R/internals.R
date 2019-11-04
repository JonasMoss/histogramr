loglik = function(object, x) {

  breaks = object$breaks
  splits = breaks[2:length(breaks) - 1]
  area = attr(object, "area")
  type = attr(object, "type")
  n_breaks = length(splits)

  n = length(x)

  pn = c(0, sapply(splits, function(j) sum(x <= j) / n), 1)

  if (type == "KL") {

    dis = sapply(1:n_breaks, function(i) {
      - log(breaks[i + 1] - breaks[i]) + log(area[i])
      })

    probs = sapply(1:n_breaks, function(i) pn[i + 1] - pn[i])
    probs[1] = probs[1] - 1 / n
    probs[n_breaks] = probs[n_breaks] + 1 / n
    sum(probs * dis)

  } else {

    dis = sapply(1:n_breaks, function(i) 1 / (breaks[i + 1] - breaks[i]))
    probs = sapply(1:n_breaks, function(i) pn[i + 1] - pn[i])
    probs[1] = probs[1] - 1 / n
    probs[n_breaks] = probs[n_breaks] + 1 / n
    sum(area * (2 * probs - area) * dis)

  }

}


area = function(n, breaks, constraint, vals) {

  if (constraint == "equal_area") {

    w = rep(1 / breaks, breaks)

  } else {

    w = c(sapply(1:breaks, function(i) {
      c(0, vals[-breaks] / n, 1)[i + 1] - c(0, vals[-breaks] / n, 1)[i]
    }))

    w[1] = w[1] - 1 / n
    w[breaks] = w[breaks] + 1 / n

  }

  w

}
