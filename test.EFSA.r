test.EFSA<-function(model,test_data,label=NULL){
	predictions=predict(model,test_data)
	if(is.null(label)){
	accuracy = NA
	}else{
	accuracy=sum(as.character(predictions)==as.character(label))/length(label)
	}
	return(list("test_accuracy"=accuracy,"predictions"=predictions))
}