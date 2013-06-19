kcv = function(data, k) {

	tmp = fold = list();
	class = levels(data$Class);

	for(i in 1 : length(class))
		tmp[[i]] = data[data$Class == class[i],]; 

	for(i in 1 : k) {

		ssmp = c();  
		for(j in 1 : length(class))
			ssmp = c(ssmp, row.names(tmp[[j]][(((i-1)*(nrow(tmp[[j]])/k))+1):(i*(nrow(tmp[[j]])/k)),]));

		fold$test[[i]] = data[sample(ssmp, length(ssmp)),];
		fold$train[[i]] = data[setdiff(row.names(data), ssmp),];
	}

	return(fold);
}