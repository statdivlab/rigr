CIhwKM <-
function (z, conf) {
  K <- z$v / (1 + z$v)
  upper <- z$S / sqrt (z$N) / (1 - K) * qSupBrnBrdg (conf)
  list (t=z$t, maxt=z$maxt, lower=z$S-upper, upper=z$S+upper, S=z$S)
}
