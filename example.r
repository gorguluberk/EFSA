#Imports
require(data.table)
require(randomForest)
source('import_all.r')


data_dict = import_data("Yoga")
train_data = data_dict$train_data
test_data = data_dict$test_data
train_labels = data_dict$train_labels
test_labels = data_dict$test_labels

efsa = EFSA(train_data,train_labels,test_data,test_labels)
print(efsa$model$train_accuracy)
print(efsa$test_accuracy)
print(efsa$predictions[1:10])