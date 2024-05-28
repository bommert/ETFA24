library(ggplot2)
library(tidyverse)
library(data.table)
library(stringr)
library(cowplot)

load("results_RF.RData")
load("datas_features.RData")


#############################################################
# plot functions

plot_time_series = function(data, col) {
  d = gather(data, key = "variable", value = "value", intersect(col, colnames(data)))
  data_l = d[, c("time", "variable", "value", "class")]

  gg = ggplot(data = data_l, mapping = aes(x = time, y = value, color = class)) +
    scale_color_manual(drop = FALSE, values =
        c("red", "gold", "blue", "darkmagenta", "lawngreen", "turquoise", "black", "darkorange", "maroon1")) +
    geom_path(aes(group = 1)) +
    theme_bw() +
    ylab("sensor value") +
    theme(legend.position = "bottom",
      axis.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16)) +
    guides(color = guide_legend(ncol = 3))

  return(gg)
}



plot_mat = function(dat, color, perfm, title = "") {
  names = rownames(dat)
  plt.data = as.data.frame(dat)
  colnames(plt.data) = c("variable", "measure", "value")

  gg = ggplot(plt.data, aes(measure, variable)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient(low = "white", high = color, limits = c(0, 1), name = perfm) +
    theme_grey() +
    labs(x = "predicted class", y = "true class") +
    scale_x_discrete(expand = c(0, 0), labels = names) +
    scale_y_discrete(expand = c(0, 0), labels = names) +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12)
    ) +
    coord_equal(ratio = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(mapping = aes(color = abs(value) >= 0.75,
      label = sprintf("%.2f", round(value, 2))),
      size = 4) +
    scale_color_manual(guide = "none", values = c("black", "white"))

  if (title != "") {
    if (title == "MMR") title = "MetaMotionS"
    else if (title == "MSR") title = "MSR 145"

    gg = gg +
      ggtitle(title) +
      theme(title = element_text(size = 13))
  }

  print(gg)

  return(NULL)
}


#######################################################################
# renaming for nice names in plots


levels = c("Loading", "Unloading", "Driving (straight)", "Driving (curve)",
  "Lifting (raising)", "Lifting (lowering)", "Standing", "Docking", "Undocking")

rename_classes = function(class) {
  class = as.character(class)
  class[class == "Driving_curve"] = "Driving (curve)"
  class[class == "Driving_straight"] = "Driving (straight)"
  class[class == "Lifting_lowering"] = "Lifting (lowering)"
  class[class == "Lifting_raising"] = "Lifting (raising)"
  class = factor(class, levels = levels)
  return(class)
}

rename_data_classes = function(data) {
  data$class = rename_classes(data$class)
  return(data)
}

datas_mmr_acc = lapply(datas_mmr_acc, rename_data_classes)
datas_msr = lapply(datas_msr, rename_data_classes)


#############################################################
# dureation and proportion of classes

duration = sapply(datas_mmr_acc, function(x) nrow(x) / 100)
sum(duration)

classes = lapply(datas_mmr_acc, function(x) table(x$class))
classes2 = Reduce("+", classes)
baseline = classes2 / sum(classes2)
round(baseline, 2)


# confusion matrices

conf = lapply(result$result, function(r) {
  true = rename_classes(r$true)
  predicted = rename_classes(r$predicted)
  table(true, predicted)
})

result[, conf := conf]
conf.split = split(result$conf, result$problem)
conf.sum = lapply(conf.split, function(co) Reduce("+", co))
conf.rel.row = lapply(conf.sum, function(co) co / rowSums(co))
conf.rel.col = lapply(conf.sum, function(co) t(t(co) / colSums(co)))

pdf("Plots/confusion_matrices_true.pdf", height = 6, width = 7)
for (i in seq_along(conf.rel.row)) {
  plot_mat(conf.rel.row[[i]], title = names(conf.rel.row)[i], color = "darkred",
    perfm = "proportion of \ntrue class")
}
dev.off()

pdf("Plots/confusion_matrices_predicted.pdf", height = 6, width = 7)
for (i in seq_along(conf.rel.col)) {
  plot_mat(conf.rel.col[[i]], title = names(conf.rel.col)[i], color = "darkblue",
  perfm = "proportion of \npredicted class")
}
dev.off()



# precision, recall, F1 score

recall = lapply(conf.rel.row, diag)
precision = lapply(conf.rel.col, diag)
f1 = lapply(seq_along(recall), function(i) {
  2  / (1 / precision[[i]] + 1 / recall[[i]])
})
sapply(f1, mean)

trafo = function(vec, sensor) {
  dat = as.data.frame(matrix(vec, nrow = 1))
  colnames(dat) = names(vec)
  gat = gather(dat)
  res = cbind(gat, sensor = sensor)
  return(res)
}

names(recall) = c("MetaMotionS (random forest)", "MSR 145 (random forest)")

recalls = Reduce(rbind, mapply(trafo, vec = recall, sensor = names(recall), SIMPLIFY = FALSE))
precisions = Reduce(rbind, mapply(trafo, vec = precision, sensor = names(recall), SIMPLIFY = FALSE))
f1s = Reduce(rbind, mapply(trafo, vec = f1, sensor = names(recall), SIMPLIFY = FALSE))

recalls = rbind(recalls, data.frame(key = names(baseline), value = as.numeric(baseline), sensor = "baseline (random classification)"))
precisions = rbind(precisions, data.frame(key = names(baseline), value = as.numeric(baseline), sensor = "baseline (random classification)"))
f1s = rbind(f1s, data.frame(key = names(baseline), value = as.numeric(baseline), sensor = "baseline (random classification)"))


performance = rbind(
  cbind(recalls, measure = "recall"),
  cbind(precisions, measure = "precision"),
  cbind(f1s, measure = "F1 score")
)

performance$sensor = factor(performance$sensor,
  levels = c("MetaMotionS (random forest)", "MSR 145 (random forest)", "baseline (random classification)"))
performance$measure = factor(performance$measure,
  levels = c("recall", "precision", "F1 score"))
performance$key = factor(performance$key, levels = levels)


gg.bar = ggplot(data = performance, mapping = aes(x = key, y = value, fill = sensor)) +
  geom_bar(stat = "identity", position = position_dodge2()) +
  theme_bw() +
  facet_grid(measure ~ "performance") +
  xlab("class") + ylab(element_blank()) +
  scale_fill_manual(values = c("black", "firebrick1", "dodgerblue")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 1))

ggsave(gg.bar, file = "Plots/f1_precision_recall.pdf", height = 8, width = 3.5)




# plot exemplary time series

gg.ts = plot_time_series(datas_msr[[9]], "MSR.Acc.x")
ggsave(gg.ts, file = "Plots/MSR_timeseries.pdf", height = 5, width = 8)

