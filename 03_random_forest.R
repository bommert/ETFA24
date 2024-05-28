# preferably run this file on a high performance compute cluster and transfer the result file to your computer when finished.

library(batchtools)
load("datas_features.RData")

reg = makeExperimentRegistry("Pal2Rec_RF", packages = "ranger")

train_test = function(job, data, versuch) {
  test_ind = which(as.numeric(names(data)) == versuch)
  test = data[[test_ind]]
  train = Reduce(rbind, data[-test_ind])
  res = list(train = train, test = test)
  return(res)
}


addProblem("MMR", seed = 1, data = datas_mmr_acc, fun = train_test)
addProblem("MSR", seed = 2, data = datas_msr, fun = train_test)


addAlgorithm("RF", fun = function(job, data, instance) {
  ran = ranger(class ~ ., data = instance$train[, -2], importance = "permutation")
  pred = predict(ran, data = instance$test[, -(1:2)])$predictions
  res = list(predicted = pred, true = instance$test$class, variable.importance = ran$variable.importance)
  return(res)
})




versuche = as.numeric(names(datas_msr))
design = data.frame(versuch = versuche)

design_list = replicate(2, design, simplify = FALSE)
names(design_list) = c("MMR", "MSR")


addExperiments(prob.designs = design_list)




saveResults = function() {
  done = findDone()
  pars = flatten(getJobPars(done))
  res = reduceResultsDataTable(done)
  result = merge(pars, res, by = "job.id")
  save(result, file = "results_RF.RData")
  return(NULL)
}


# submitJobs()
# wait until all jobs have finished
# saveResults()

