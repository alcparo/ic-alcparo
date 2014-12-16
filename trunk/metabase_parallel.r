#Cria uma metabase dado um dataset "data"

library(doMC)
library(foreach)
registerDoMC()

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
		noiseParallel = list();
		for(n in 1:EPOCHS) noiseParallel[[n]] = index(RATES[i], nrow(data)); 

		#tableRows=lapply(1:EPOCHS, function(x){
		tableRows=foreach(x=1:EPOCHS) %dopar% {
			cat(c("RATE: ", RATES[i],"\tEPOCHS: ", x, "\n"));
			
			noise = noiseParallel[[x]];
			dataPolluted = pollution(data, noise); #Poluí o dataset
			
			#GERAR INDICES e armazenar as classes que serão mudadas
			dataKCV = kcv(data, k); #k fold cross validation

			for(l in 1:k) {
			
				dataKCVTrainTmp = dataKCV$train[[l]];
				dataKCVTestTmp = dataKCV$test[[l]];
				intersectionTmp = intersect(row.names(dataKCVTrainTmp), noise);

				
				
				for(m in 1:length(intersectionTmp)){ #Polui o conjunto de treinamento TRAIN
					if(sum(dataKCVTrainTmp$Class == dataKCVTrainTmp[which(row.names(dataKCVTrainTmp)==intersectionTmp[m]),]$Class) != 1){
						dataKCVTrainTmp[which(row.names(dataKCVTrainTmp)==intersectionTmp[m]),]$Class = dataPolluted[intersectionTmp[m],]$Class;
					}
					else{
						print(paste("Poluição da instância não executada, único exemplo da classe no conjunto de treinamento. Rate: ", RATES[i], " Epoca: ", x, "Fold: ", k));
					}
				}
					
				
				complexMeasures = measures(dataKCVTrainTmp); #Calcula as medidas de complexidade
				tableTmp[l,1:nMeasures] = unlist(complexMeasures);
				tableTmp[l,(nMeasures+1):(nMeasures+nClassifiers)] = classifiers(dataKCVTrainTmp, dataKCVTestTmp); #train = dataKCV$train[[l]] ? Poluir test set ?
				
				print(tableTmp[l,]);
				
				
			} #do.call e rbind 
				
			
			return (colMeans(tableTmp));		
		}
		#print("TERMINOU");
		listTmp[[i]] = do.call(rbind, tableRows);
	}
	#print("FIM");
	table = do.call(rbind, listTmp);
	
	
	colnames(table) = c("F1", "F1v", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2", "SVM", "kNN", "NaiveBayes", "randomForest");
	
	
	return (table);
}