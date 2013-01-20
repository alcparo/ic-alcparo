# Mixture models


mixture = function(train) {
	print("mixture");
	dt = dist(train[,-ncol(train)]);
	value = c(n1(dt, train$Class), n2(dt, train$Class), dim(train));
	return(value);
}


n1 = function(dt, class, aux=0) {

	tree = mst(dt);
	for(i in 1 : nrow(tree))
		for(j in i : ncol(tree))
			if(tree[i,j] != 0 & class[i] != class[j])
				aux = aux + 1;

	return(aux/length(class));
}


n2 = function(dt, class) {

	dt = as.matrix(dt);
	inter = intra = c();

	for(i in 1 : nrow(dt))
		for(j in i : ncol(dt)) {
			if(dt[i,j] != 0 & class[i] != class[j]) {
				inter = c(inter, dt[i,j]);
			} else {
				intra = c(intra, dt[i,j]);
			}
		}

	return(c(mean(intra), mean(inter)));
}


dim = function(train) {
	
	return(log10((ncol(train)-1)/nrow(train)));
}
