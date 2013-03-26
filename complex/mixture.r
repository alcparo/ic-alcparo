# Mixture models


mixture = function(train) {
	print("mixture");
	dt = dist(train[,-ncol(train)]);

	#dt = dist(data[, -ncol(data)]);
	#class = data$Class;

	value = c(n1(dt, train$Class), n2(dt, train$Class), n3(train, train$Class), n4());
	return(value);
}


n1 = function(dt, class) {

	#dt = dist(data[, -ncol(data)]);
	#class = data$Class;
	tree = mst(dt);
	aux=0;

	for(i in 1 : nrow(tree)){
		for(j in i : ncol(tree)){
			if(tree[i,j] != 0 & (class[i] != class[j])){
				
					aux = aux + 1;			
		
			}
		}		
	}
	
	print(c(aux, length(class)));
	
	return(aux/length(class));	

}


n2 = function(dt, class) {

	#dt = dist(data[, -ncol(data)]);
	#class = data$Class;
	dt = as.matrix(dt);
	
	interAux = intraAux = c();
	inter = intra = 0;
	
	for(i in 1 : nrow(dt)){
		for(j in i : ncol(dt)) {
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

			print(c(min(interAux, na.rm=TRUE), min(intraAux, na.rm=TRUE)));
			
		interAux = intraAux = c();

	}

	#return(c(mean(intra), mean(inter)));
	
	print(c(intra,inter));

	return(intra/inter);

}


n3 = function(data, cl){

	train = data[,-ncol(data)];
	#train = data;
	#cl = data$Class;

	result = knn.cv(train, cl, k=1, use.all=F);

	aux = sum(abs(as.numeric(result) - as.numeric(cl)));

	return (aux / length(cl));	

}

n4 = function(){
  
	#baseado no L3

	return(-1);
}

