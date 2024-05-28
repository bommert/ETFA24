# restart R if necessary otherwise just detach package data.table
# detach("package:data.table", unload = TRUE)

setwd("..\\SPARL")
load("data_sensors_oc.RData")

generate_features = function(data, cols, max_lag) {
  d = data[, c("class", "time", cols)]
  print(data$Versuch[1])

  features = lapply(cols, function(col) {
    quants = (0:20) / 20
    n.quants = length(quants)
    n.quants2 = (length(quants) - 1) / 2


    q.abs = matrix(0, nrow = nrow(d) - max_lag, ncol = n.quants)
    for (i in 1:nrow(q.abs)) {
      part = d[(i + 1):(i + max_lag), col]
      q.abs[i, ] = unname(quantile(abs(part), p = quants, type = 2))
    }
    colnames(q.abs) = paste0(col, ".qAbs.", quants * 100)

    q.orig = matrix(0, nrow = nrow(d) - max_lag, ncol = n.quants)
    for (i in 1:nrow(q.orig)) {
      part = d[(i + 1):(i + max_lag), col]
      q.orig[i, ] = unname(quantile(part, p = quants, type = 2))
    }
    colnames(q.orig) = paste0(col, ".qOrig.", quants * 100)

    q.abs.diff = matrix(0, nrow = nrow(d) - max_lag, ncol = n.quants2)
    for (i in 1:nrow(q.abs.diff)) {
      part = d[(i + 1):(i + max_lag), col]
      lower =  unname(quantile(abs(part), p = quants[1:n.quants2], type = 2))
      upper =  unname(quantile(abs(part), p = quants[n.quants:(n.quants2 + 2)], type = 2))
      q.abs.diff[i, ] = upper - lower
    }
    colnames(q.abs.diff) = paste0(col, ".qAbsDiff.", quants[1:n.quants2] * 100)

    q.orig.diff = matrix(0, nrow = nrow(d) - max_lag, ncol = n.quants2)
    for (i in 1:nrow(q.orig.diff)) {
      part = d[(i + 1):(i + max_lag), col]
      lower =  unname(quantile(part, p = quants[1:n.quants2], type = 2))
      upper =  unname(quantile(part, p = quants[n.quants:(n.quants2 + 2)], type = 2))
      q.orig.diff[i, ] = upper - lower
    }
    colnames(q.orig.diff) = paste0(col, ".qOrigDiff.", quants[1:n.quants2] * 100)

    sds = matrix(0, nrow = nrow(d) - max_lag, ncol = 8)
    for (i in 1:nrow(sds)) {
      part = d[(i + 1):(i + max_lag), col]
      sds[i, 1] = sd(abs(part))
      sds[i, 2] = sd(part)
      sds[i, 3] = mad(abs(part))
      sds[i, 4] = mad(part)
      sds[i, 5] = mean(abs(abs(part) - mean(abs(part))))
      sds[i, 6] = mean(abs(part - mean(part)))
      sds[i, 7] = mean(abs(abs(part) - median(abs(part))))
      sds[i, 8] = mean(abs(part - median(part)))
    }
    sds.names.part = paste0(rep(c("Abs", "Orig"), 4),
      rep(c("SD", "MAD", "MDMean", "MDMedian"), each = 2))
    colnames(sds) = paste0(col, ".Deviation", sds.names.part)

    feats = cbind(q.orig, q.abs, q.orig.diff, q.abs.diff, sds)
    return(feats)
  })

  features_all = Reduce(cbind, features)
  exclude = 1:max_lag
  d2 = cbind(d[-exclude, ], features_all)
  d2$class = as.factor(d2$class)
  return(d2)
}

pgf = function(data, cols, max_lag) {
  data_tmp = data[!data$class %in% c("Error", "Synchronization"), ]
  datas = split(data_tmp, data_tmp$Versuch)
  datas = lapply(datas, generate_features, cols = cols, max_lag = max_lag)
  return(datas)
}


datas_mmr_acc = pgf(data_mmr_acc_oc, cols = c("MMR.Acc.x", "MMR.Acc.y", "MMR.Acc.z"), max_lag = 50)
datas_msr = pgf(data_msr_oc, cols = c("MSR.Acc.x", "MSR.Acc.y", "MSR.Acc.z"), max_lag = 25)

save(datas_mmr_acc, datas_msr, file = "datas_features.RData")
