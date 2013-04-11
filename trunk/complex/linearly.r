# Linear Calc


linearly = function(data) {
	print("linearly");
	
	class = data$Class;
	data$Class = as.numeric(data$Class);	
	model = lm(Class~., data);

	values = c(l1(data, model), l2(data, model), l3(data, model));
	return(values);
}


l1 = function(data, model) { #DCol 

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
	
	aux = sum(abs(model$residuals))/nrow(data);
	return(aux);

	#return(-1);
}


l2 = function(data, model) {

	pred = round(predict(model, data[,-ncol(data)]));
	acc = sum(diag(table(data$Class, pred)))/sum(table(data$Class, pred));
	return(1-acc);

	#return(summary(model)$details[5]); #mean Absoluty Error
	
}

l3=function(data, model){

	

	numberOfClasses = length(levels(data$Class));
	numberOfExamples = nrow(data);
	numberOfAttributes = length(data) - 1;
	
	#dataAux = matrix(ncol = ncol(data), nrow = nrow(data));
	dataAux = data;
	
	for(i in 1:numberOfExamples){
	
	repeat{
		  ex1 = sample(rownames(data[data$Class == data[i,]$Class,]), size=1);
		  ex2 = sample(rownames(data[data$Class == data[i,]$Class,]), size=1);
		  
		  if(ex1 != ex2){
		    break;
		    }
		}
	
	  for(j in 1:numberOfAttributes){
		rnd = runif(1);
		dataAux[i, j] = (data[ex1, j]*rnd) + (data[ex2, j]*(1-rnd));
		
	  
	  }
	  
	  dataAux[i,]$Class = data[i,]$Class;
	
	}
	
	pred = round(predict(model, dataAux[,-ncol(dataAux)]));
	acc = sum(diag(table(dataAux$Class, pred)))/sum(table(dataAux$Class, pred));
	#print(c(acc, 1-acc));
	
	
	return(acc);
	
	#return(-1);
}

