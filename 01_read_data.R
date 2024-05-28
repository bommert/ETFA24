# General information:
# Create folder SPARL with subfolders MetaMotionS_raw_data, MSR_145_raw_data and Video_Frames_annotated
# containing the sensor data and labels, respectively. Depending on the folder structure you chose,
# you may need to modify the setwd() commands in all files to set your working directory accordingly.


library(stringr)
library(data.table)
setwd("..\\SPARL")


remove_offset = function(dat) {
  cm = apply(dat[, 2:4], 2, median)
  dat[, 2] = dat[, 2] - cm[1]
  dat[, 3] = dat[, 3] - cm[2]
  dat[, 4] = dat[, 4] - cm[3]
  return(dat)
}


# MetaMotionS
path.mmr = "MetaMotionS_raw_data\\"
files.mmr = paste0(path.mmr, list.files(path.mmr, pattern = ".csv"))
res_mmr_acc = lapply(files.mmr, function(file) {
  dat1 = read.csv(file)
  colnames(dat1) = c("epoch.time", "time", "s.elapsed", "MMR.Acc.x", "MMR.Acc.y", "MMR.Acc.z")
  dat = dat1[c("s.elapsed", "MMR.Acc.x", "MMR.Acc.y", "MMR.Acc.z")]
  colnames(dat)[1] = "time"

  dat = remove_offset(dat)
  dat[, 2:4] = 10 * dat[, 2:4]

  v = str_split_1(file, "\\\\")[2]
  v = str_split_1(v, "_")[1]
  v = as.numeric(v)
  dat = cbind(dat, Versuch = v)
  dat = as.data.table(dat)
  return(dat)
})

data_mmr_acc_abs = Reduce(rbind, res_mmr_acc)



# MSR 145
path.msr = "MSR_145_raw_data\\"
files.msr = paste0(path.msr, list.files(path.msr, pattern = ".csv"))
res_msr = lapply(files.msr, function(file) {
  d = read.csv2(file, header = FALSE, skip = 27, dec = ".")
  colnames(d) = c("time", "MSR.Acc.x", "MSR.Acc.y", "MSR.Acc.z")
  d$time = as.POSIXct(d$time)

  t = as.numeric(d$time - min(d$time), unit = "secs")
  t = round(t, 2)
  d$time = t

  d = remove_offset(d)

  v = str_split_1(file, "\\\\")[2]
  v = str_split_1(v, "_")[1]
  v = as.numeric(v)
  dat = cbind(d, Versuch = v)
  dat = as.data.table(dat)
  return(dat)
})

data_msr_abs = Reduce(rbind, res_msr)



# hammer hits
hit_mmr_acc = function(data) abs(data$MMR.Acc.y) > 0.25
hit_msr = function(data) abs(data$MSR.Acc.x) > 0.5

begin_time = function(data, hit_fun) {
  ds = split(data, data$Versuch)
  begins = sapply(ds, function(d) {
    hit = hit_fun(d)
    ind.begin = min(which(hit))
    time.begin = d[ind.begin, ]$time
    return(time.begin)
  })
  res = data.table(Versuch = as.numeric(names(ds)), begin = begins)
  return(res)
}

begin_mmr_acc = begin_time(data_mmr_acc_abs, hit_mmr_acc)
begin_msr = begin_time(data_msr_abs, hit_msr)


# data sets with time relative to beginning
rel_data = function(data, begins) {
  data_rel = merge(data, begins)
  colnames(data_rel)[2] = "time.orig"
  data_rel[, time := time.orig - begin]
  return(data_rel)
}

data_mmr_acc = rel_data(data_mmr_acc_abs, begin_mmr_acc)
data_msr = rel_data(data_msr_abs, begin_msr)



# Labels
path.labels = "Video_Frames_annotated\\"
files.labels = paste0(path.labels, list.files(path.labels, pattern = ".csv"))
res_labels = lapply(files.labels, function(file) {
  d = read.csv(file, header = TRUE)

  colnames(d)[3] = "Driving_straight"
  colnames(d)[4] = "Driving_curve"
  colnames(d)[5] = "Lifting_raising"
  colnames(d)[6] = "Lifting_lowering"
  colnames(d)[13] = "None_Attr"
  colnames(d)[14] = "Error_Attr"

  ind.start = min(which(d$Synchronization == 1))
  time = 1:nrow(d) * 1/30 - ind.start * 1/30
  d$time = time

  v = str_split_1(file, "\\\\")[2]
  v = str_split_1(v, "_")[1]
  v = as.numeric(v)
  dat = cbind(d, Versuch = v)
  dat = as.data.table(dat)
  return(dat)
})

data_labels = Reduce(rbind, res_labels)




# add class information to sensor data
one_class = apply(data_labels[, 1:14], 1, function(li) {
  if (li[14] == 1) {
    return("Error")
  } else {
    class = names(li)[which(li[1:12] == 1)]
    return(class)
  }
})

data_labels_oc = data.table(time = data_labels$time, Versuch = data_labels$Versuch, class = one_class)


add_labels = function(data) {
  versuche = sort(unique(data$Versuch))
  added = lapply(versuche, function(v) {
    data_v = data[data$Versuch == v]
    data_labels_v = data_labels_oc[data_labels_oc$Versuch == v, ]
    row_ind = sapply(data_v$time, function(ti) {
      which.min(abs(ti - data_labels_v$time))
    })
    res = cbind(data_v, data_labels_v[row_ind, 3])
    res = res[res$class != "None", ]
    return(res)
  })
  res = Reduce(rbind, added)
  return(res)
}

data_mmr_acc_oc = add_labels(data_mmr_acc)
data_msr_oc = add_labels(data_msr)


save(data_mmr_acc_oc, data_msr_oc, file = "data_sensors_oc.RData")
