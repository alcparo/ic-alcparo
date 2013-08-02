

loo = function(data, k=10) {

	fold = list();
	tmp = lapply(1:(nrow(data)/LOO), function(i) {
		idx = (((i-1)*LOO)+1):(i*LOO);
		data[idx,];
	});

	for(i in 1:k) {

		test = tran = data.frame();

		for(j in 1:(nrow(data)/LOO)) {

			if(sample(c(0,1), 1)) {
				tran = rbind(tran, tmp[[j]]);
			} else {
				test = rbind(test, tmp[[j]]);
			}
		}
	
		fold$tran[[i]] = tran
		fold$test[[i]] = test
	}
	return(fold);
}


