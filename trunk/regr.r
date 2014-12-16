list.metabases = list();
for(i in 1:length(FILES.METABASES)){
	list.metabases[[i]] = read.table(FILES.METABASES[i]);
}

table.metabases = do.call(rbind, list.metabases); # Cria a metabase com os meta atributos de todos os datasets

table.metabases = normalize(table.metabases);

row.names(table.metabases) = 1:2640; ### 2640 entradas da metabase (40 * 66)

results.svm = list();
results.rf = list();
results.default = list();

for(k in 1:EXAMPLES){

	index = sample(c(1:66), 66, replace=F); 
	index = index*40-39;

	index.train = c();
	index.test = c();

	for(i in 1:33){
		index.train = append(index.train, seq(index[i], index[i]+39, 1));
	}

	for(i in 34:66){
		index.test = append(index.test, seq(index[i], index[i]+39, 1));
	}

	results.svm[[k]] = rg.svm(table.metabases[index.train,], table.metabases[index.test,]);
	results.rf[[k]] = rg.rf(table.metabases[index.train,], table.metabases[index.test,]);
	results.default[[k]] = df.erro(table.metabases[index.train,], table.metabases[index.test,]);
}

aux.svm = data.frame(do.call(rbind, results.svm));
#aux.svm = data.frame(aux.svm);


aux.rf = data.frame(do.call(rbind, results.rf));
#aux.rf = data.frame(aux.rf);

aux.default = data.frame(do.call(rbind, results.default));
#aux.default = data.frame(aux.default);


results.final = lapply(CLASSIFIERS, function(x){ # Resultado dos meta-regressores 

	aux = matrix(nrow = EXAMPLES+2, ncol=3, dimnames=list(c(seq(1, EXAMPLES, 1), "mean", "sd"), c("rg.SVM", "rg.RF", "default")));
	aux[1:EXAMPLES, "rg.SVM"] = aux.svm[, x];
	aux[1:EXAMPLES, "rg.RF"] = aux.rf[, x];
	aux[1:EXAMPLES, "default"] = aux.default[, x];
	aux["mean",] = colMeans(aux[1:EXAMPLES,]);
	aux["sd",] = apply(aux[1:EXAMPLES,], 2, sd);
	
	return(aux);
});

names(results.final) = CLASSIFIERS;

require(ggplot2);
require(reshape2);

colnames(results.final$SVM) = c("SVM", "RF", "Default");
box.svm=results.final$SVM[1:5,];
box.svm.melt=melt(box.svm)
colnames(box.svm.melt) = c("", "Regressores", "MSE");
p = ggplot(data.frame(box.svm.melt), aes(Regressores, MSE))
p+geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=3) + ggtitle("SVM")

colnames(results.final$randomForest) = c("SVM", "RF", "Default");
box.rf=results.final$randomForest[1:5,];
box.rf.melt=melt(box.rf)
colnames(box.rf.melt) = c("", "Regressores", "MSE");
p = ggplot(data.frame(box.rf.melt), aes(Regressores, MSE))
p+geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=3) + ggtitle("RF")

colnames(results.final$kNN) = c("SVM", "RF", "Default");
box.knn=results.final$kNN[1:5,];
box.knn.melt=melt(box.knn)
colnames(box.knn.melt) = c("", "Regressores", "MSE");
p = ggplot(data.frame(box.knn.melt), aes(Regressores, MSE))
p+geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=3) + ggtitle("kNN")

colnames(results.final$NaiveBayes) = c("SVM", "RF", "Default");
box.naive=results.final$NaiveBayes[1:5,];
box.naive.melt=melt(box.naive)
colnames(box.naive.melt) = c("", "Regressores", "MSE");
p = ggplot(data.frame(box.naive.melt), aes(Regressores, MSE))
p+geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=3) + ggtitle("Naive Bayes")


boxplot(box.svm[,1], box.svm[,2], box.svm[,3], box.rf[,1], box.rf[,2], box.rf[,3], box.knn[,1], box.knn[,2], box.knn[,3], box.naive[,1], box.naive[,2], box.naive[,3])

group = [1,1,1,2,2,2,3,3,3,4,4,4]


