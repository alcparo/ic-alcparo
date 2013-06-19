classifiers = function(train, test){
	aux = c(calcSVM(train, test), calcKNN(train,test), calcNB(train, test), calcRF(train,test));	
	return (aux);	
}

calcSVM = function(train, test){
	#print("svm");
	model = svm(Class~., train, kernel="polynomial");
	pred = predict(model, test[,-ncol(test)]);	
	acc = sum(test$Class == pred) / nrow(test);
	return(acc);
}

calcKNN = function(train, test){
	#print("knn");
	pred = knn(train[,-ncol(train)], test[,-ncol(test)], cl=train$Class);
	acc = sum(test$Class == pred) / nrow(test);
	return(acc);
}

calcNB = function(train, test){
	#print("nb");
	model = naiveBayes(Class~., train);
	pred = predict(model, test[,-ncol(test)]);
	acc = sum(test$Class == pred) / nrow(test);
	return(acc);
}

calcRF = function(train, test){
	#print("nb");
	model = randomForest(Class~., train);
	pred = predict(model, test[,-ncol(test)]);
	acc = sum(test$Class == pred) / nrow(test);
	return(acc);
}

