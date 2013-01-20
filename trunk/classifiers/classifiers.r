# Classfiers - List of classifiers


CLASS[[1]] = function(tran, test) {

	model = rpart(Class ~ ., tran);
	pred = predict(model, test[,-ncol(test)], type="class");
	return(pred);
}


CLASS[[2]] = function(tran, test) {

	model = MLP(Class ~ ., tran, options="-L 0.3 -M 0.2 -H 1");
	pred = predict(model, test[,-ncol(test)], type="class");
	return(pred);
}


CLASS[[3]] = function(tran, test) {

	model = svm(Class ~ ., tran, scale=FALSE, type="C-classification");
	pred = predict(model, test[,-ncol(test)]);
	return(pred);
}


CLASS[[4]] = function(tran, test) {

	model = naiveBayes(Class ~ ., tran);
	pred = predict(model, test[,-ncol(test)], type="class");
	return(pred);
}


CLASS[[5]] = function(tran, test) {

	model = randomForest(Class ~ ., tran);
	pred = predict(model, test[,-ncol(test)], type="class");
	return(pred);
}


CLASS[[6]] = function(tran, test) {

	model = J48(Class ~ ., tran);
	pred = predict(model, test[,-ncol(test)], type="class");
	return(pred);
}


CLASS[[7]] = function(tran, test) {

	pred = knn(tran[,-ncol(tran)], test[,-ncol(test)], tran$Class, 5);
	return(pred);
}


