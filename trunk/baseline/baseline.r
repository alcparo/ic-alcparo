# Baseline - Techniques from drop to compare with ensemble


voting = function(preds, class) {
	print("voting");
	for(i in 1 : ncol(preds))
		if(any(preds[,i] != class[i]))
			class[i] = preds[which(preds[,i] != class[i])[1],i];
	return(class);
}


redefine = function(train, temp) {

	diff = setdiff(row.names(train), row.names(temp));
	for(i in 1 : length(diff))
		train[diff,]$Class[i] = setdiff(levels(train$Class), train[diff,]$Class[i])[1] 
	return(train$Class);
}


remove = function(temp, vote) {
	temp = temp[temp$Class == vote,];
	return(temp);
}


xnn = function(train, test, x) {
	print("xnn");
	pred = knn(train[,-ncol(train)], test[,-ncol(test)], train$Class, x);
	return(pred);
}


ednn = function(train) {
	print("ednn");
	pred = xnn(train, train, 5);
	return(pred);	
}


renn = function(train) {
	print("renn");
	temp = train;
	pred = xnn(temp, temp, 5);

	while(any(temp$Class != pred) & 
			(length(unique(pred)) == length(levels(train$Class)))) {
		temp = remove(temp, pred);
		pred = xnn(temp, temp, 5);
	} 	

	pred = redefine(train, temp);
	return(pred);
}


aenn = function(train) {
	print("aenn");
	preds = matrix(0, nrow=5, ncol=nrow(train));
	for(i in 1 : 5)
		preds[i,] = xnn(train, train, i);
	pred = voting(preds, train$Class);
		
	return(pred);
}


bs.methods = function(train, noise, func) {

	vote = do.call(func, list(train));
	aux = values(train, noise, vote);
	return(aux);
}


baseline = function(train, noise, alg=c("ednn", "renn", "aenn")) {

	aux = mclapply(1:length(alg), function(x) {
		bs.methods(train, noise, alg[x]);
	});
	return(aux);
}


