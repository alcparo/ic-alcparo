

default = function(tran, test) {

	mean = colMeans(tran[,TECHS]);
	pred = matrix(mean, ncol=length(TECHS), nrow=LOO, byrow=TRUE);
	erro = colMeans((test[,TECHS] - pred)^2);
	return(erro);
}


rand = function(tran, test) {

	unif = sample(seq(0,1, by=STEP), LOO*length(TECHS), replace=TRUE);
	pred = matrix(unif, ncol=length(TECHS), nrow=LOO);
	erro = colMeans((test[,TECHS] - pred)^2);
	return(erro);
}


threshold = function(tran, test) {

	mse = mclapply(REGRESSORS, function(reg) {
		err = matrix(unlist(mclapply(1:length(tran), function(i) {
			do.call(reg, list(tran[[i]], test[[i]]));
		})), ncol=5, byrow=TRUE);
	});

	return(mse);
}


