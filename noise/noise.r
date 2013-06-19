index = function(rate, num) {

	value = trunc(rate*num);
	noise = sample(num, value, replace=FALSE);
	return(noise);
	
}

pollution = function(train, noise) {	

	tmp = intersect(row.names(train), noise);
	
	for(i in 1:length(tmp)){		
		train[row.names(train)==tmp[i], ncol(train)] = sample(setdiff(levels(train$Class), train[tmp[i],]$Class), 1, replace=FALSE);
	}
	
	return(train);
}
