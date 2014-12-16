# Creation of k fold used by the k-Fold cross validation method

kcv = function(data, k) {

	tmp = fold = list();
	class = levels(data$Class); # Classes of the dataset

	for(i in 1 : length(class))	# Creates a sudset where all the instances have the same Class = class[i]
		tmp[[i]] = data[data$Class == class[i],]; 

	for(i in 1 : k) { 

		ssmp = c();  
		for(j in 1 : length(class))
			ssmp = c(ssmp, row.names( tmp[[j]][(((i-1)*(nrow(tmp[[j]])/k))+1):(i*(nrow(tmp[[j]])/k)),]) );

		fold$test[[i]] = data[sample(ssmp, length(ssmp)),];
		fold$train[[i]] = data[setdiff(row.names(data), ssmp),];

		#resample to add missing classes to the training set ## VERIFY
		for(j in 1:length(class)){
			if(sum(which(fold$train[[i]]$Class == class[j]))==0){
				ssmp = ssmp[!ssmp==row.names(fold$test[[i]][fold$test[[i]]$Class==class[j],])];
			}
		}

		fold$test[[i]] = data[sample(ssmp, length(ssmp)),];
		fold$train[[i]] = data[setdiff(row.names(data), ssmp),];
	}

	return(fold);
}