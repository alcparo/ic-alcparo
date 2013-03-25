# Ensemble - Define the classifiers to compose the ensemble


similarity = function(pred) {

	tab = table(pred[1,], pred[2,]) + table(pred[1,], pred[3,]) + table(pred[2,], pred[3,]);
	aux = sum(diag(tab));
	return(aux);
}


difficulty = function(preds) {

	dist = lapply(1:7, function(x) {

		aux = matrix(0, nrow=7, ncol=7);
		for(y in 1:7)
			for(z in 1:7)
				if(x!=y & x!=z & y!=z)
					aux[y,z] = aux[y,z] + similarity(preds[c(x,y,z),]);
		return(aux);
	});
	return(dist);
}


first = function(dist) {

	vet = vector("numeric", 7);
	for(i in 1 : 7)
		vet[i] = max(dist[[i]]);
	tmp = which.max(vet);
	return(tmp);
}


order = function(dist, bin=rep(0,7)) {
	
	f = first(dist);	
	s = which.max(apply(dist[[f]], 2, max));
	t = which.max(dist[[f]][,s]);

	bin[c(f,s,t)] = 1;
	return(bin);
}


ensamble = function(train, noise) {
	
	dist = difficulty(th.models(train, train));
	return(order(dist));
}
