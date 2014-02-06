#FILES.METABASES = paste("/home/andrecatini/metabases/", list.files("/home/andrecatini/metabases/"), sep="");
#EXAMPLES = 5; #2 ou mais

list.metabases = list();
for(i in 1:length(FILES.METABASES)){
	list.metabases[[i]] = read.table(FILES.METABASES[i]);
}

table.metabases = do.call(rbind, list.metabases);

table.metabases = normalize(table.metabases);

row.names(table.metabases) = 1:2640;

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


results.final = lapply(CLASSIFIERS, function(x){

	aux = matrix(nrow = EXAMPLES+2, ncol=3, dimnames=list(c(seq(1, EXAMPLES, 1), "mean", "sd"), c("rg.SVM", "rg.RF", "default")));
	aux[1:EXAMPLES, "rg.SVM"] = aux.svm[, x];
	aux[1:EXAMPLES, "rg.RF"] = aux.rf[, x];
	aux[1:EXAMPLES, "default"] = aux.default[, x];
	aux["mean",] = colMeans(aux[1:EXAMPLES,]);
	aux["sd",] = apply(aux[1:EXAMPLES,], 2, sd);
	
	return(aux);
});

names(results.final) = CLASSIFIERS;

