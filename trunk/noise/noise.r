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

preprocessing = function(data) {
		
	nMeasures = 13;
	nClassifiers = 4;	
	k = 10; # K-fold cross validation	
	
	table = matrix(0, nrow=(EPOCHS*length(RATES)), ncol=(nMeasures+nClassifiers));
	tableTmp = matrix(0, nrow=k, ncol=(nMeasures+nClassifiers));
		
	#colnames(table) = c("F1", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2", "SVM", "kNN", "NaiveBayes", "randomForest");
	listTmp = list();	
	
	for(i in 1 : length(RATES)) {
		
		tableRows=mclapply(1:EPOCHS, function(x){
		
			noise = index(RATES[i], nrow(data))
			dataPolluted = pollution(data, noise);
			
			#GERAR INDICES e armazenar as classes que serão mudadas
			dataKCV = kcv(data, k); #k fold cross validation

			for(l in 1:k) {
			
				dataKCVTrainTmp = dataKCV$train[[l]];
				dataKCVTestTmp = dataKCV$test[[l]];
				intersectionTmp = intersect(row.names(dataKCVTrainTmp), noise);
				
				for(m in 1:length(intersectionTmp)){ #Polui o conjunto de treinamento
					dataKCVTrainTmp[which(row.names(dataKCVTrainTmp)==intersectionTmp[m]),]$Class = dataPolluted$Class[intersectionTmp[m]];
				}
					
				complexMeasures = complex(dataKCVTrainTmp);
				tableTmp[l,1:nMeasures] = complexMeasures;
				tableTmp[l,(nMeasures+1):(nMeasures+nClassifiers)] = classifiers(dataKCVTrainTmp, dataKCVTestTmp); #train = dataKCV$train[[l]] ? Poluir test set ?
			} #do.call e rbind 
				
			#table[(x + ((i-1)*EPOCHS)), ] = colMeans(tableTmp);
			return (colMeans(tableTmp));		
		});
		
		listTmp[[i]] = do.call(rbind, tableRows);
	}
	
	table = do.call(rbind, listTmp);
	colnames(table) = c("F1", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2", "SVM", "kNN", "NaiveBayes", "randomForest");
	
	
	return (table);
}

classifiers = function(train, test){
	aux = c(calcSVM(train, test), calcKNN(train,test), calcNB(train, test), calcRF(train,test));	
	return (aux);	
}

calcSVM = function(train, test){
	#print("svm");
	model = svm(Class~., train, kernel="polynomial");
	pred = predict(model, test[,-ncol(test)]);	
	acc = sum(test$Class == pred) / nrow(test);
	return(acc);
}

calcKNN = function(train, test){
	#print("knn");
	pred = knn(train[,-ncol(train)], test[,-ncol(test)], cl=train$Class);
	acc = sum(test$Class == pred) / nrow(test);
	return(acc);
}

calcNB = function(train, test){
	#print("nb");
	model = naiveBayes(Class~., train);
	pred = predict(model, test[,-ncol(test)]);
	acc = sum(test$Class == pred) / nrow(test);
	return(acc);
}

calcRF = function(train, test){
	#print("nb");
	model = randomForest(Class~., train);
	pred = predict(model, test[,-ncol(test)]);
	acc = sum(test$Class == pred) / nrow(test);
	return(acc);
}

