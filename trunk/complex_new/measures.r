# Complexity Metrics
source("Scripts/complex/fisher.r");
source("Scripts/complex/linearly.r");
source("Scripts/complex/mixture.r");


complex = function(train) {
	print("complex");
	aux = c(fisher(train), linearly(train), mixture(train));
	#aux = fisher(train);
	#aux = linearly(train);
	#aux = mixture(train);
	return(aux);
}


setAttr = function(train) {
	print("setAttr");
	train$Class = factor(train$Class); 
	levels(train$Class) = c(1,2);
	return(train);
}


multiclass = function(train) {
	print("multiclass");
	aux = list(); k=1;
	class = levels(train$Class);

	for(i in 1 : (length(class)-1))
		for(j in (i+1) : length(class)) {
			tmp = setAttr(train[train$Class == class[i] | train$Class == class[j],]);
			aux[[k]] = complex(tmp); 
			k = k + 1;
		}
	return(aux);
}


measure = function(train, tec=9) {
	print("measure");
	mat = matrix(unlist(multiclass(train)), ncol=tec, byrow=TRUE);
	for(i in 1 : ncol(mat))
		aux = c(aux, max(mat[,i]));
	return(aux);
}	

