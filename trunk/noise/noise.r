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

kcv = function(data, k=10) {

	tmp = fold = list();
	class = levels(data$Class);

	for(i in 1 : length(class))
		tmp[[i]] = data[data$Class == class[i],]; 

	for(i in 1 : k) {

		ssmp = c();  
		for(j in 1 : length(class))
			ssmp = c(ssmp, row.names(tmp[[j]][(((i-1)*(nrow(tmp[[j]])/k))+1):(i*(nrow(tmp[[j]])/k)),]));

		fold$test[[i]] = data[sample(ssmp, length(ssmp)),];
		fold$tran[[i]] = data[setdiff(row.names(data), ssmp),];
	}

	return(fold);
}

preprocessing = function(data) {
	
	# construir uma tabela com 8*30 linhas e Y+1 colunas (atributos - numero de medidas de complexidade e classe) 
	table = matrix(0, nrow=(8*30), ncol=11);
	
	tecnicas=c();	

	for(i in 1 : length(RATES)) {

		for(j in 1 : EPOCHS) {

			noise = index(RATES[i], nrow(data));
			aux = kcv(data); #k fold cross validation
					
			vet = matrix(0, nrow=10, ncol=10);
			
			for(l in 1 : 10) {

				tmp = aux$tran[[l]];

				# aqui precisamos calcular as medidas de complexidade para o conjunto sem ruido	
				#vet[i,1:5] = complexity(tmp);	
				#vet[i,1:5] = measure(tmp);

				measures_tmp = measure(tmp);
				vet[i,1] = measures_tmp[[3]];
				vet[i,2] = measures_tmp[[4]];
				vet[i,3] = measures_tmp[[5]];
				vet[i,4] = measures_tmp[[6]];
				vet[i,5] = measures_tmp[[7]];

				tmp = pollution(aux$tran[[l]], noise);
				
				tecnicas[i] = run(tmp, noise);
				
				print(names(tecnicas[l]), tecnicas[i])
				
				# aqui precisamos calcular as medidas de complexidade para o conjunto com ruido
				#vet[i,6:10] = complexity(tmp);	

				#vet[i,6:10] = measure(tmp);	

				measures_tmp = measure(tmp);
				vet[i,6] = measures_tmp[[7]];
				vet[i,7] = measures_tmp[[8]];
				vet[i,8] = measures_tmp[[9]];
				vet[i,9] = measures_tmp[[10]];

				print(vet);		
				
			}

			#Calcular media das colunas, exceto a ultima
			#Ultima fazer por voto de maioria (moda)
			#Jogar linha resultante na 
		}

		#table[((i-1)*EPOCHS)+j] = rowMeans(vet);
	}
}


main = function() {

	# Database diretory
	DIR = "Datasets_processados/";

	# Files
	FILES = list.files(DIR);


	table = mclapply(1:length(FILES), function(x) {

		data = read.arff(paste(DIR, FILES[x], sep=""));
		return (preprocessing(data));
	});
}

