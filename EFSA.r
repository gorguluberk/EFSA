EFSA<-function(train_data,train_labels,test_data=NULL,test_labels,nofsample=50){
	source('shapeletSimilarity.r')
	shapelets_dict = generate_shapelets(train_data,as.factor(train_labels),nofsample)
	shapelets = shapelets_dict$shapelets
	shapelet_length = shapelets_dict$shapelet_length

	model = train.EFSA(train_data,as.factor(train_labels),shapelets,shapelet_length)
	train_accuracy = model$train_accuracy
	print(train_accuracy)
	test_accuracy = NA
	predictions=NA
	if(!is.null(test_data)){
		test_dict = test.EFSA(model,test_data,test_labels)
		test_accuracy = test_dict$test_accuracy
		predictions = test_dict$predictions
	}
	return(structure(list("model"=model,"test_accuracy"=test_accuracy,"predictions"=predictions),class='EFSA'))
}