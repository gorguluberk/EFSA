predict.EFSA<-function(model,dataset){
	distance_matrix = compute_best_matching_distance(test_data,model$shapelets,model$shapelet_length)
	predictions=predict(model$model,distance_matrix)
	return(predictions)
}
