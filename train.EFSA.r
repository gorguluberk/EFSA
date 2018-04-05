train.EFSA<-function(train_data,as.train_labels,shapelets,shapelet_length,method="RandomForest",ntree=300,k=1){
	distance_matrix = compute_best_matching_distance(train_data,shapelets,shapelet_length)
	
	if(method == "RandomForest"){
		model=randomForest(distance_matrix,as.factor(train_labels),ntree=300)
	}
    train_accuracy = 1-as.numeric(model$err.rate[nrow(model$err.rate),1])
	model = structure(list("model"=model,"train_accuracy"=train_accuracy,"shapelets"=shapelets,"shapelet_length"=shapelet_length),class="EFSA")
	return(model)
}