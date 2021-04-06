CIptKM <-
function (z, conf) {
  upper <- sqrt (z$v / z$N) * z$S * qnorm ((1 + conf) / 2)
  list (t=z$t, maxt=z$maxt, lower=z$S-upper, upper=z$S+upper, S=z$S)
}
