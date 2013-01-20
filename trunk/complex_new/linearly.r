# Linear Calc


linearly = function(train) {
	print("linearly");
	#train$Class = as.numeric(train$Class);
	#model = lm(Class~., train);

	model = SMO(Class~., train);

	values = c(l1(model), l2(model, train));
	return(values);
}


l1 = function(model) {

	#aux = sum(model$residuals^2)/length(model$residuals);
	#return(aux);
}


l2 = function(model, train) {

	#pred = predict(model, train[,-ncol(train)]); #dando erroooooo!!!!
	#acc = sum(diag(table(train$Class, round(pred))))/sum(table(train$Class, round(pred)));
	#return(acc);

	return(summary(model)$details[5]); #mean Absoluty Error
	
}

