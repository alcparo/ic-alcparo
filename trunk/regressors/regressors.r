# R Code
# Classifiers used in techniques and verify the accuracy
# Luis P. Garcia, André Carvalho and Ana C. Lorena


form = function(classifier) {

	#aux = paste(setdiff(METRICS, "SVM"), collapse="+"); #Troquei de T2 pra SVM pra calcular a fórmula corretamente com as medidas
	aux = paste(METRICS, collapse="+"); #Troquei de T2 pra SVM pra calcular a fórmula corretamente com as medidas	
	aux = formula(paste(classifier, "~", aux, sep=""), env=.GlobalEnv);
	return(aux);
}




rg.rf = function(tran, test) {

	pred = lapply(CLASSIFIERS, function(classifier) {
		model = randomForest(form(classifier), tran);
		predict(model, test);
	});

	erro = rg.erro(test, pred);
	return(erro);
}

rg.svm = function(tran, test) {

	pred = lapply(CLASSIFIERS, function(classifier) {
		print(classifier)
		model = svm(form(classifier), tran, scale=FALSE, kernel="polynomial");
		predict(model, test);
	});

	erro = rg.erro(test, pred);
	return(erro);
}


rg.erro = function(test, pred) {
	#print(pred[[1]]);
	pred = matrix(unlist(pred), ncol=length(CLASSIFIERS), dimnames=list(NULL, CLASSIFIERS));
	erro = colMeans((test[,CLASSIFIERS] - pred)^2);
	return(erro);
}

df.erro = function(train, test){
	erro = colMeans((train[,CLASSIFIERS] - test[,CLASSIFIERS])^2);
	return(erro);
}


normalize = function(data) {

	for(i in METRICS) {
		if(min(data[,i]) != max(data[,i])) {
			data[,i] = (data[,i] - min(data[,i]))/(max(data[,i]) - min(data[,i]));
		} else {
			data[,i] = 1;
		}
	}
	return(data);
}