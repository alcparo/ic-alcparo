# Ensamble voting - Techniques of ensambles of classifiers


th.models = function(train, test, bin=rep(1,7)) {

	preds = NULL;
	for(i in 1 : 7)
		if(bin[i] == 1)
			preds = rbind(preds, CLASS[[i]](train, test));	
		
	return(preds);
}


cs = function(preds, class) {
	print("cs");
	for(i in 1 : ncol(preds))
		if((preds[1,i] == preds[2,i]) & (preds[2,i] == preds[3,i]))
			class[i] = preds[1,i];
	return(class);
}


mt = function(preds, class) {
	print("mt");
	for(i in 1 : ncol(preds)) 
		class[i] = which.max(tabulate(preds[,i]));
	return(class);
}


th.methods = function(train, noise, bin, func) {

	vote = do.call(func, list(th.models(train, train, bin), train$Class));
	aux = values(train, noise, vote);
	return(aux);
}


technique = function(train, noise, alg=c("cs", "mt")) {
	print("technique");
	bin = ensamble(train, noise);

	aux = mclapply(1:length(alg), function(x) {
		th.methods(train, noise, bin, alg[x]);
	});
	return(aux);
}


