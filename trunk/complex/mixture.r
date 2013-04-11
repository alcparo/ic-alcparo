# Mixture models


mixture = function(train) {
	print("mixture"); #ONDE TEM DISTANCIA, DCOL Normaliza	
	
	
	for(i in 1:(ncol(train)-1)){ #Normalizacao
	  train[,i] = scale(train[,i], center=min(train[,i]), scale = diff(range(train[,i])));
	}
	
	dt = dist(train[,-ncol(train)]);

	#dt = dist(data[, -ncol(data)]);
	#class = data$Class;

	value = c(n1(dt, train$Class), n2(dt, train$Class), n3(train, train$Class), n4(train));
	return(value);
}


n1 = function(dt, class) { #Se eu nao normalizo o dataset, o resultado muda / Diferenças resultantes da implementacao da MST
	tree = mst(dt);
	aux=0;

	for(i in 1 : nrow(tree)){
		for(j in i : ncol(tree)){
			if(tree[i,j] != 0 & (class[i] != class[j])){
				
					aux = aux + 1;			
		
			}
		}		
	}
	
	#print(c(aux, length(class)));
	
	return(aux/length(class));	

}


n2 = function(dt, class) {

	#dt = dist(data[, -ncol(data)]);
	#class = data$Class;
	dt = as.matrix(dt);
	
	interAux = intraAux = c();
	inter = intra = 0;
	
	for(i in 1 : nrow(dt)){
		for(j in 1 : ncol(dt)) {
			if(dt[i,j] != 0){
				if(class[i] != class[j]) {
					interAux[j] = dt[i,j];
				} else {
					intraAux[j] = dt[i,j];
				}
			}
		}
	
		if(!is.null(interAux))
			inter = inter + min(interAux, na.rm=TRUE);
		if(!is.null(intraAux))
			intra = intra + min(intraAux, na.rm=TRUE);

			#print(c(min(interAux, na.rm=TRUE), min(intraAux, na.rm=TRUE)));
			
		interAux = intraAux = c();

	}

	#return(c(mean(intra), mean(inter)));
	
	#print(c(intra,inter));

	return(intra/inter);

}


n3 = function(data, cl){

	train = data[,-ncol(data)];
	#train = data;
	#cl = data$Class;

	result = knn.cv(train, cl, k=1, use.all=F);
	#print(result);
	#print(cl);
	
	error = as.numeric(result) - as.numeric(cl);
	qnt = 0;
	#print(error);
	for(i in 1:length(error)){
	  if(error[i] != 0){
	    increment(qnt);
	  }
	}
	
	

	#aux = sum(abs(as.numeric(result) - as.numeric(cl)));
	
	#print(aux);

	return (qnt / length(cl));	

}

n4 = function(train){
  
	#baseado no L3	
	
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
		dataAux[i, j] = data[ex1, j]*rnd + data[ex2, j]*(1-rnd);		
	  
	  }
	  
	  dataAux[i,]$Class = data[i,]$Class;
	
	}
	
	return(n3(dataAux, dataAux$Class));	
	
}

