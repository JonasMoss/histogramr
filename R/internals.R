loglik = function(object, x){

  splits = object$splits
  weights = object$density
  breaks = object$breaks
  n = length(x)

  Pn = c(0, sapply(splits, function(j) sum(x<=j)/n), 1)
  splits_aug = c(0, splits, 1)

  if (object$type == "KL") {

    dis = sapply(1:breaks,function(i) - log(splits_aug[i+1] - splits_aug[i]) + log(weights[i]))
    probs = sapply(1:breaks,function(i) Pn[i+1]-Pn[i])
    probs[1] = probs[1] - 1/n
    probs[breaks] = probs[breaks] + 1/n
    sum(probs*dis)

  } else {

    dis   = sapply(1:breaks,function(i) 1/(splits_aug[i+1] - splits_aug[i]))
    probs = sapply(1:breaks,function(i) Pn[i+1]-Pn[i])
    probs[1] = probs[1] - 1/n
    probs[breaks] = probs[breaks] + 1/n
    sum(weights*(2*probs-weights)*dis)

  }
}


calculate_weights = function(n, breaks, weights, vals) {

  if (weights == "equal") {
    w = rep(1/breaks, breaks)
  } else {
    w = c(sapply(1:breaks, function(i) c(0, vals[-breaks]/n, 1)[i + 1] - c(0, vals[-breaks]/n, 1)[i]))
    w[1] = w[1] - 1/n
    w[breaks] = w[breaks] + 1/n
  }

  w

}