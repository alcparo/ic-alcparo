#Cria uma metabase dado um dataset "data"

metabase = function(data) { 
	
	nMeasures = 14;
	nClassifiers = 4;	
	k = 10; # K-fold cross validation	
	
	table = matrix(0, nrow=(EPOCHS*length(RATES)), ncol=(nMeasures+nClassifiers));
	tableTmp = matrix(0, nrow=k, ncol=(nMeasures+nClassifiers));
		
	#colnames(table) = c("F1", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2", "SVM", "kNN", "NaiveBayes", "randomForest");
	listTmp = list();		
	
	for(i in 1 : length(RATES)) {
		#print(c("RATE: ", RATES[i]));
		tableRows=lapply(1:EPOCHS, function(x){
			cat(c("RATE: ", RATES[i],"\tEPOCHS: ", x, "\n"));
			
			noise = index(RATES[i], nrow(data))
			dataPolluted = pollution(data, noise); #Poluí o dataset
			
			#GERAR INDICES e armazenar as classes que serão mudadas
			dataKCV = kcv(data, k); #k fold cross validation

			for(l in 1:k) {
			
				dataKCVTrainTmp = dataKCV$train[[l]];
				dataKCVTestTmp = dataKCV$test[[l]];
				intersectionTmp = intersect(row.names(dataKCVTrainTmp), noise);
				
				for(m in 1:length(intersectionTmp)){ #Polui o conjunto de treinamento
					dataKCVTrainTmp[which(row.names(dataKCVTrainTmp)==intersectionTmp[m]),]$Class = dataPolluted[intersectionTmp[m],]$Class;
				}
					
				#complexMeasures = complex(dataKCVTrainTmp);
				complexMeasures = measures(dataKCVTrainTmp); #Calcula as medidas de complexidade
				tableTmp[l,1:nMeasures] = unlist(complexMeasures);
				tableTmp[l,(nMeasures+1):(nMeasures+nClassifiers)] = classifiers(dataKCVTrainTmp, dataKCVTestTmp); #train = dataKCV$train[[l]] ? Poluir test set ?
				
				
				
			} #do.call e rbind 
				
			#print("return");
			
			
			#table[(x + ((i-1)*EPOCHS)), ] = colMeans(tableTmp);
			return (colMeans(tableTmp));		
		});
		#print("TERMINOU");
		listTmp[[i]] = do.call(rbind, tableRows);
	}
	#print("FIM");
	table = do.call(rbind, listTmp);
	
	
	colnames(table) = c("F1", "F1v", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2", "SVM", "kNN", "NaiveBayes", "randomForest");
	
	
	return (table);
}