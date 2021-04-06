#library(MLmetrics)
#library(ramify)
#source('get.R')

# visual_json_for_classification
#' @export
visual_classification = function(model_name, test_data){

  # data
  true = trueDummy(model_name, test_data)
  pred = predProb(model_name, test_data)
  visual_data = finalDF(model_name, test_data)

  # labels
  visual_label = roc_pr(visual_data)$visual_label

  # roc & pr
  roc = roc_pr(visual_data)$roc
  pr = roc_pr(visual_data)$pr

  # results - list
  visual_list = list()

  for(i in 1:length(visual_label)){
    visual_list$fpr[[visual_label[i]]] = 1-as.numeric(unlist(roc$Specificity$pr[grepl(visual_label[i], names(roc$Specificity$pr))]))
    visual_list$tpr[[visual_label[i]]] = as.numeric(unlist(roc$Sensitivity$pr[grepl(visual_label[i], names(roc$Sensitivity$pr))]))
    visual_list$recall[[visual_label[i]]] = as.numeric(unlist(pr$Recall$pr[grepl(visual_label[i], names(pr$Recall$pr))]))
    visual_list$precision[[visual_label[i]]] = as.numeric(unlist(pr$Precision$pr[grepl(visual_label[i], names(pr$Precision$pr))]))

    visual_list$legend$roc[[visual_label[i]]] = as.numeric(unlist(roc$AUC$pr[grepl(visual_label[i], names(roc$AUC$pr))]))
    visual_list$legend$'precison-recall'[[visual_label[i]]] = as.numeric(unlist(roc$AUC$pr[grepl(visual_label[i], names(roc$AUC$pr))]))
  }
  for(i in 1:(length(visual_label)-2)){
    visual_list$'confusion_matrix'[[visual_label[i]]] =  as.matrix(confusionMatrix(factor(visual_label[ramify::argmax(true)]), factor(visual_label[ramify::argmax(pred)])))[i,]
  }
  visual_list$chart$AUC = visual_list$legend$roc['micro']
  visual_list$chart$Precision = visual_list$legend$`precison-recall`['micro']
  visual_list$chart$Recall = MLmetrics::Recall(visual_label[ramify::argmax(true)], visual_label[ramify::argmax(pred)])
  visual_list$chart$'F1-score' = MLmetrics::F1_Score(visual_label[ramify::argmax(true)], visual_label[ramify::argmax(pred)])

  return(visual_list)
}

