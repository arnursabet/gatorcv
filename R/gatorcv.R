# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

data = read.csv("../winequality-red.csv", sep = ";")
data_reg = read.csv("../winequality-red.csv", sep = ";")
data_clas = read.csv("../winequality-red.csv", sep = ";")

old_val = c(3:8)
new_val = factor(c(1, 1, 2, 2, 3, 3))

data_clas$q <- new_val[ match(data$quality, old_val) ]

data_clas = subset(data_clas, select=-c(density,quality))
data_reg = subset(data_reg, select=-c(density))

## set the seed to make your partition reproducible
set.seed(123)

partition_clas = createDataPartition(data_clas$q, times=1, p=0.6)
train_clas = data_clas[partition_clas$Resample1,]
train_clas_X = within(train_clas, rm("q"))
train_clas_Y = train_clas$q
test_clas = data_clas[-partition_clas$Resample1,]
test_clas_X = within(test_clas, rm("q"))
test_clas_Y = test_clas$q

partition_reg = createDataPartition(data_reg$quality, times=1, p=0.6)
train_reg = data_reg[partition_reg$Resample1,]
test_reg = data_reg[-partition_reg$Resample1,]
test_reg_X = within(test_reg, rm("quality"))
test_reg_Y = test_reg$quality


train_Y = train$quality
train_X = within(train, rm("quality"))
test_X = within(test, rm("quality"))


h_parameters_svm = list(
                        list(kernel="linear",
                             cost = c(0.5, 1, 2, 4, 8)),
                        list(kernel="radial",
                             cost = c(0.5, 1, 2, 4, 8),
                             gamma = c(0.01, 0.1, 0.5)),
                        list(kernel="polynomial",
                             cost = c(0.5, 1, 2, 4, 8),
                             gamma = c(0.01, 0.1, 0.5),
                             degree = c(1,2,3))
                        )

h_parameters_svm2 = list(
                        list(kernel="linear",
                             cost = c(0.5, 1)),
                        list(kernel="radial",
                             cost = c(0.5, 1),
                             gamma = c(0.01, 0.1)),
                        list(kernel="polynomial",
                             cost = c(0.5, 1),
                             gamma = c(0.01, 0.1),
                             degree = c(1,2))
                        )

h_parameters_rf = list(list(mtry=c(5:10),
                            ntree=seq(5:25)))

h_parameters_knn = list(list(k=seq(5:50)))

h_parameters_plsr = list(list(n_comp = c(1:10)))

repeats = 30
outer = 10
inner = 5
start.time <- Sys.time()

result = repeatedStratifiedNestedCV(data=train_clas,
                                    target="quality",
                                    model="svm",
                                    h_parameters=h_parameters_svm,
                                    cv_outer=10,
                                    cv_inner=5,
                                    repeats_outer=30,
                                    repeats_inner=30,
                                    outcome="categorical")

end.time <- Sys.time()
time.taken <- end.time - start.time

start.time <- Sys.time()
result.rf = repeatedStratifiedNestedCV(train_reg,
                                     target="quality",
                                     model="randomForest",
                                     h_parameters=h_parameters_rf,
                                     cv_outer=10,
                                     cv_inner=5,
                                     repeats_outer=30,
                                     repeats_inner=30,
                                     outcome="continuous")
end.time <- Sys.time()
time.taken.rf <- end.time - start.time

pred = predict(result.rf$best_model, test_reg_X)
loss(pred=pred,true=test_reg_Y, outcome = "continuous")

pred2 = predict(result4$best_model, test_reg_X, ncomp=10)
loss(pred=pred2,true=test_reg_Y, outcome = "continuous")

pred = predict(result$best, test_clas_X)
loss(pred=pred,true=test_clas_Y, outcome = "categorical")

start.time <- Sys.time()
result3 = repeatedStratifiedNestedCV(train_clas,
                                     target="q",
                                     model="knn",
                                     h_parameters=h_parameters_knn,
                                     cv_outer=10,
                                     cv_inner=5,
                                     repeats_outer=30,
                                     repeats_inner=30,
                                     outcome="categorical")
end.time <- Sys.time()
time.taken.knn <- end.time - start.time

start.time <- Sys.time()
result4 = repeatedStratifiedNestedCV(train_reg,
                                     target="quality",
                                     model="plsr",
                                     h_parameters=h_parameters_plsr,
                                     cv_outer=10,
                                     cv_inner=5,
                                     repeats_outer=30,
                                     repeats_inner=30,
                                     outcome="continuous")
end.time <- Sys.time()
time.taken.plsr <- end.time - start.time

loss = function(pred, true, outcome) {

  if (length(pred) != length(true)) {
    true = true[,1]
  }

  if(outcome == "categorical") {
    loss = mean(pred != true)
  }
  else {
    loss = mean((pred - true)^2)
  }

  return(loss)
}

repeatedGridSearchCV = function(data,
                                target,
                                model,
                                h_parameters,
                                n_folds=5,
                                repeats=1,
                                outcome="categorical")
{
  formula = as.formula(paste(target, "~", "."))

  # total_comb = 0
  # for (i in 1:length(h_parameters)) {
  #   total_comb = total_comb + nrow(expand.grid(h_parameters[[i]]))
  # }
  # pred_ncol = n_folds * (ncol(data) - 1) * total_comb

  cv_error = c()
  fit_index = c()
  for (r in 1:repeats)
  {
    folds = split(data[sample(nrow(data)),], 1:n_folds)

    folds_error = c()

    for (f in 1:n_folds)
    {
      predictions = c()
      train = data.frame(do.call("rbind", folds[-f]))
      test = data.frame(do.call("rbind", folds[f]))
      train_X = subset(train, select=-get(target), drop=FALSE)
      train_Y = subset(train, select=get(target), drop=FALSE)
      test_X = subset(test, select=-get(target), drop=FALSE)
      test_Y = subset(test, select=get(target), drop=FALSE)
      print(nrow(test_Y))
      for (p in 1:ncol(train_X))
      {
        train_X_p = as.data.frame(train_X[, 1:p, drop=FALSE])
        train_p = as.data.frame(cbind(train_X_p, train_Y))
        test_X_p = as.data.frame(test_X[, 1:p, drop=FALSE])

        for (i in 1:length(h_parameters))
        {
          if(model == "svm")
          {
            if (h_parameters[[i]]$kernel == "linear")
            {
              for (c in h_parameters[[i]]$cost)
              {
                fit = svm(formula,
                          data=train_p,
                          kernel="linear",
                          cost=c)

                if (f == 1 && r == 1) {
                  h_name = paste("p:", p, "kernel: linear", "cost:",fit$cost)
                  fit_index = cbind(fit_index, h_name)
                }

                pred = round(predict(fit, test_X_p))
                predictions = cbind(predictions, pred)
              }

            }

            else if (h_parameters[[i]]$kernel == "radial")
            {
              for (c in 1:length(h_parameters[[i]]$cost))
              {
                for (g in h_parameters[[i]]$gamma)
                {
                  fit = svm(formula,
                            data=train_p,
                            kernel="radial",
                            cost=h_parameters[[i]]$cost[c],
                            gamma=g)
                  if (f == 1 && r == 1) {
                    h_name = paste("p:", p, "kernel: radial", "cost:",fit$cost, "gamma:", fit$gamma)
                    fit_index = cbind(fit_index, h_name)
                  }

                  pred = round(predict(fit, test_X_p))
                  predictions = cbind(predictions, pred)
                }

              }
            }

            else # polynomial
            {
              for (c in 1:length(h_parameters[[i]]$cost))
              {
                for (g in 1:length(h_parameters[[i]]$gamma))
                {
                  for (d in h_parameters[[i]]$degree)
                  {
                    fit = svm(formula,
                              data=train_p,
                              kernel="polynomial",
                              cost=h_parameters[[i]]$cost[c],
                              gamma=h_parameters[[i]]$gamma[g],
                              degree=d)

                    if (f == 1 && r == 1) {
                      h_name = paste("p:", p,
                                     "kernel: polynomial",
                                     "cost:",fit$cost,
                                     "gamma:", fit$gamma,
                                     "degree:", fit$degree)

                      fit_index = cbind(fit_index, h_name)
                    }

                    pred = round(predict(fit, test_X_p))
                    predictions = cbind(predictions, pred)
                  }

                }

              }
            }

            #err = mean(test_Y != pred)
            #error = cbind(error, err)

          }
        }

      }

      folds_error = rbind(folds_error, apply(predictions,
                                               2,
                                               loss,
                                               true=test_Y,
                                               outcome=outcome))

    } # Folds

    print(dim(folds_error))
    cv_error = rbind(cv_error, apply(folds_error, 2, mean))
  }
  print(dim(cv_error))
  #print(cv_error)
  #print(fit_index)
  mean_cv_error = apply(cv_error, 2, mean)

  min_error_index = which.min(mean_cv_error)
  min_cv_error = min(mean_cv_error)
  #print(mean_error)
  best = fit_index[min_error_index]
  print(best)
  return(list(best = best,
              best_cv_error = min_cv_error,
              fit_index = fit_index))

}

preds = repeatedGridSearchCV(train,
                             target="quality",
                             model="svm",
                             h_parameters=h_parameters,
                             n_folds=5,
                             repeats=2,
                             outcome="categorical")


innerCVProcedure = function(data,
                            target,
                            model,
                            h_parameters,
                            n_folds=5,
                            repeats=1,
                            outcome="categorical")
{
  formula = as.formula(paste(target, "~", "."))
  cv_error = c()
  fit_index = c()

  for (r in 1:repeats)
  {
    print(r)
    all_predictions = c()
    inner_ind = createFolds(data[,target], k=n_folds, returnTrain = FALSE)
    inner_folds = lapply(inner_ind, function(x, y) y[x,], y = data)
    #print(inner_folds)
    for (f in 1:n_folds)
    {
      predictions = c()
      train = data.frame(do.call("rbind", inner_folds[-f]))
      test = data.frame(do.call("rbind", inner_folds[f]))
      train_X = subset(train, select=-get(target), drop=FALSE)
      train_Y = subset(train, select=get(target), drop=FALSE)
      test_X = subset(test, select=-get(target), drop=FALSE)
      test_Y = subset(test, select=get(target), drop=FALSE)
      #break
      #print(nrow(test_Y))
      for (i in 1:length(h_parameters))
      {
        if (model == "svm")
        {
          if (h_parameters[[i]]$kernel == "linear")
          {
            for (c in h_parameters[[i]]$cost)
            {
              cost = as.numeric(deparse(c))

              fit = eval(substitute(svm(formula,
                        data=train,
                        kernel="linear",
                        cost=cost), list(cost=cost)))
              #print(fit)
              #print(fit)

              if (f == 1 && r == 1) {
                fit_index = cbind(fit_index, list(fit))
              }
              #print(fit)
              pred = predict(fit, test_X)
              predictions = cbind(predictions, pred)
              #print(pred)

            }

          }

          else if (h_parameters[[i]]$kernel == "radial")
          {
            for (c in 1:length(h_parameters[[i]]$cost))
            {
              for (g in h_parameters[[i]]$gamma)
              {
                cost = as.numeric(deparse(h_parameters[[i]]$cost[c]))
                gamma = as.numeric(deparse(g))

                fit = eval(substitute(svm(formula,
                                          data=train,
                                          kernel="radial",
                                          cost=cost,
                                          gamma=gamma), list(cost=cost, gamma=gamma)))


                if (f == 1 && r == 1) {
                  fit_index = cbind(fit_index, list(fit))
                }
                #print(fit)
                pred = predict(fit, test_X)
                predictions = cbind(predictions, pred)
              }

            }
          }

          else # polynomial
          {
            for (c in 1:length(h_parameters[[i]]$cost))
            {
              for (g in 1:length(h_parameters[[i]]$gamma))
              {
                for (d in h_parameters[[i]]$degree)
                {
                  cost = as.numeric(deparse(h_parameters[[i]]$cost[c]))
                  gamma = as.numeric(deparse(h_parameters[[i]]$gamma[g]))
                  degree = as.numeric(deparse(d))
                  fit = eval(substitute(svm(formula,
                                            data=train,
                                            kernel="polynomial",
                                            cost=cost,
                                            gamma=gamma,
                                            degree=degree), list(cost=cost, gamma=gamma, degree=degree)))

                  if (f == 1 && r == 1) {
                    fit_index = cbind(fit_index, list(fit))
                  }
                  #print(fit)
                  pred = predict(fit, test_X)
                  predictions = cbind(predictions, pred)
                }

              }

            }
          }


        }

        else if (model == "randomForest")
        {
          grid = expand.grid(h_parameters[[i]])

          for(row in 1:nrow(grid))
          {
            mtry = grid[row, "mtry"]
            ntree = grid[row, "ntree"]

            fit = eval(substitute(randomForest(formula,
                                               data=train,
                                               mtry=mtry,
                                               ntree=ntree,
                                               importance=TRUE)))

            if (f == 1 && r == 1) {
              fit_index = cbind(fit_index, list(fit))
            }
            pred = round(predict(fit, test_X))
            predictions = cbind(predictions, pred)

          }


        }

        else if (model == "knn")
        {
          neighbors = h_parameters[[i]]$k

          for (k in neighbors)
          {
            if (f == 1 && r == 1) {
              fit_index = cbind(fit_index, list(k))
            }

            predictions = cbind(predictions, knn(train=train_X,
                                                 test=test_X,
                                                 cl=train_Y[,1],
                                                 k=k))

          }
        }

        else if (model == "plsr")
        {
          components = h_parameters[[i]]$n_comp

          for (nc in components)
          {
            #print(nc)
            if (f == 1 && r == 1) {
              fit_index = cbind(fit_index, list(nc))
            }
            # f hat ^ -k (x, alpha)
            fit = plsr(formula,
                       data = train,
                       ncomp = nc,
                       scale=F,
                       validation = "none")
            #print(fit)
            pred = predict(fit, test_X, ncomp=nc)
            predictions = cbind(predictions, pred)

          }
        }

        else {
          print("invalid model.")
        }

      }
      #print(predictions)
      #print(predictions)
      all_predictions = rbind(all_predictions, predictions)


    } #folds
    #print(all_predictions)

    binded_folds = data.frame(do.call("rbind", inner_folds))
    #print(binded_folds)
    #print(binded_folds)
    #print(test_X)
    cv_error = rbind(cv_error, apply(all_predictions,
                                           2,
                                           loss,
                                           true=binded_folds[target],
                                           outcome=outcome))
    #print(cv_error)

  }
  #print(cv_error)
  mean_cv_error = apply(cv_error, 2, mean) # over number of rep and indexed by tuning parameters
  #print(mean_cv_error)
  #print(mean_cv_error)
  min_error_index = which.min(mean_cv_error)
  min_cv_error = min(mean_cv_error)
  #print(mean_error)
  best = fit_index[min_error_index] # optimal cross-validatory choice for tuning parameters / model

  #colnames(cv_error) = fit_index
  #colnames(mean_cv_error) = fit_index
  #print(min_error_index)
  #print(mean_cv_error)
  #print(fit_index)
  #print(best)
  #print(best)
  return(list(best = best,
              best_cv_error = min_cv_error,
              cv_errors = cv_error,
              mean_cv_error = mean_cv_error))

}

repeatedStratifiedNestedCV = function(data,
                                      target,
                                      model,
                                      h_parameters,
                                      cv_outer=5,
                                      cv_inner=5,
                                      repeats_outer=1,
                                      repeats_inner=1,
                                      outcome="categorical")
{

  unique_hp = unique(names(rapply(h_parameters, function(x) head(x,1))))
  formula = as.formula(paste(target, "~", "."))
  nested_cv_error = c()
  fit_index = c()
  best_model = NA

  for (r in 1:repeats_outer)
  {
    print(paste("outer:", r))
    all_predictions = c()
    #inner_cv_error = c()
    outer_ind = createFolds(data[,target], k=cv_outer, returnTrain = FALSE)
    outer_folds = lapply(outer_ind, function(x, y) y[x,], y = data)

    for (of in 1:cv_outer)
    {
      predictions = c()
      train = data.frame(do.call("rbind", outer_folds[-of]))
      test = data.frame(do.call("rbind", outer_folds[of]))
      train_X = subset(train, select=-get(target), drop=FALSE)
      train_Y = subset(train, select=get(target), drop=FALSE)
      test_X = subset(test, select=-get(target), drop=FALSE)
      test_Y = subset(test, select=get(target), drop=FALSE)

      if (of == 1 && r == 1) {
        inner_cv_result = innerCVProcedure(train,
                                           target,
                                           model,
                                           h_parameters,
                                           n_folds=cv_inner,
                                           repeats=repeats_inner,
                                           outcome=outcome)

        best_model = inner_cv_result$best[[1]]
        #print(best_model)
      }

      #print(best_model)
      #print(best_model)
      #best_summary = summary.default(best_model)[,1]
      #optimal_hp = best_summary[match(unique_hp, names(best_summary))]
      if (model == "knn") {

        #fit_index = cbind(fit_index, list(best_model))
        predictions = cbind(predictions, knn(train_X,
                                              test_X,
                                              train_Y[,1],
                                              k = best_model))

      }

      else if (model == "plsr")
      {
        #print(formula)
        #print(train)
        #fit_index = cbind(fit_index, list(best_model))
        #print(fit_index)
        ncomp = best_model
        refitted_model = eval(substitute(plsr(formula,
                              data = train,
                              ncomp=ncomp,
                              scale=F,
                              validation="none"), list(formula=formula,
                                                       ncomp=ncomp)))
        #print(refitted_model)
        predictions = cbind(predictions, predict(refitted_model, test_X, ncomp=ncomp))
        #print(predictions)
      }
      else if (model == "randomForest")
      {
        refitted_model = update(best_model, . ~ ., data = train)
        #fit_index = cbind(fit_index, list(refitted_model))

        predictions = cbind(predictions, round(predict(refitted_model, test_X)))

        best_model = refitted_model
      }

      else {
        refitted_model = update(best_model, . ~ ., data = train)
        #fit_index = cbind(fit_index, list(refitted_model))

        redictions = cbind(predictions, predict(refitted_model, test_X))

        best_model = refitted_model
      }

      all_predictions = rbind(all_predictions, predictions)

      #inner_cv_error = cbind(inner_cv_error, inner_cv_result$best_cv_error)

    } # outer folds
    binded_folds = data.frame(do.call("rbind", outer_folds))

    nested_cv_error = rbind(nested_cv_error, apply(all_predictions,
                                                   2,
                                                   loss,
                                                   true = binded_folds[target],
                                                   outcome = outcome))

  }
  #print(unlist(fit_index))
  #print(length(rep_error))
  #min_fold_error_index = which.min(rep_error)
  #best = fit_index[min_fold_error_index]
  if (model != "knn") {
    best_model = refitted_model
  }

  return(list(best_model=best_model,
              error_est=nested_cv_error))

}



plot(result.rf$error_est, main="Estimates of expected test error.", ylab="expected test error", type="o", col="blue")

plot(result4$error_est, main="Estimates of expected test error.", ylab="expected test error", type="o", col="blue")

plot(result3$error_est, main="Estimates of expected test error.", ylab="expected test error", type="o", col="blue")


split_sample = function(data,
                        train_prop=0.8)
{

  smp_size <- floor(train_prop * nrow(data))
  train_ind <- sample(seq_len(nrow(data)),size=smp_size)

  return(list(train=data[train_ind,],
              test=data[-train_ind,]))
}
