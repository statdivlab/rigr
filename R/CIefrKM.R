CIefrKM <-
function (z, obs, conf) {
  upper <- sqrt (z$v[sum (z$t <= obs)] / z$N) * z$S * qSupBrnMotn (conf)
  upper[z$t > obs] <- NA
  list (t=z$t, maxt=z$maxt, lower=z$S-upper, upper=z$S+upper, S=z$S)
}
