# Linear Calc


linearly = function(train) {
	print("linearly");
	
	model = SMO(Class~., train);

	values = c(l1(model), l2(model), l3(model));
	return(values);
}


l1 = function(model) { #DCol 

	#aux = sum(model$residuals^2)/length(model$residuals);
	#return(aux);


	#### Precisa conseguir obter os pesos dos atributos da SVM para realizar o cálculo.

	#val = vector();
	#w = vector(); #Pesos
	
	#w = c(0.3653, 1.0019, 1.3507, 0.9636, 0.195, 0.9636, -0.8069)

	#for(i in 1:length(data$Class)){
	#	val[i] = 0;
	#	w[i] = 0;
	#}

	#for(j in 1:length(data$Class)){
	#	for(i in 1:(ncol(data)-1)){
	#		val[j] = val[j] + (w[i]*data[j,i]);
	#
	#	}

	#val[j] = val[j]-16.161; 
	#}

	return(-1);
}


l2 = function(model) {

	return(summary(model)$details[5]); #mean Absoluty Error
	
}

l3=function(model){
	return(-1);
}

