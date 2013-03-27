# Fisher


fisher = function(train) {
	print("fisher");

	values = c(f1(train), f2(train), f3(train));
	return(values);
}


f1 = function(data) { #m-class

	numberOfAttributes = ncol(data)-1;
	numberOfClasses = length(levels(data[,ncol(data)]));
	numberOfExamplesPerClass=c();
	numberOfExamples = length(data$Class);

	f = den = 0;

	atLeastOne = FALSE;
	maxF1 = -1;
	whichAttribute = -1;

	if(numberOfClasses == 2){

		for(i in 1:numberOfAttributes){
			if( (mean(data[data$Class == 1, i]) - mean(data[data$Class == 2, i]) != 0) && (sqrt(var(data[data$Class == 1, i])) + sqrt(var(data[data$Class == 2, i])) != 0) ){

				atLeastOne = TRUE;

				f = (mean(data[data$Class == 1, i]) - mean(data[data$Class == 2, i]))^2 / (var(data[data$Class == 1, i]) + var(data[data$Class == 2, i]));
				#print(f);
				if(f > maxF1){
					maxF1 = f;
					whichAttribute = i;
				}
			}
		}
		#print(c("maxF1", maxF1));
		if(!atLeastOne){
			print("Error F!");
		}

		return(maxF1);

	} else{

		#data <- data[order(data$Class),];		
		
		for(i in 1:numberOfClasses){
			numberOfExamplesPerClass[i] = length(which(data$Class==i));
		}

		for( att in 1:numberOfAttributes ){ #Percorre todos atributos
			
			f = den = 0;

			for(c1 in 1:numberOfClasses){#Primeiro somatorio
				
				varAux = var(data[data$Class==c1,att]);

				if(is.na(varAux))
					varAux = 0;

				den = den + (numberOfExamplesPerClass[c1] / numberOfExamples) * varAux;

				if(c1!=numberOfClasses){
					for(c2 in (c1+1):numberOfClasses){
						if(c1!=c2){
							f = f + ( (numberOfExamplesPerClass[c1]/numberOfExamples) * (numberOfExamplesPerClass[c2]/numberOfExamples) * ((mean(data[data$Class==c1,att]) - mean(data[data$Class==c2,att]))^2));							
						}
					}
				}
			}

			if(f != 0 && den != 0){
				atLeastOne = TRUE;
				f = f/den;
				#print(f);
				if(f > maxF1){
					maxF1 = f;
					whichAttribute = att;
				}
			}	

		}

		if(!atLeastOne){
			print("Error F!");
		}

		return(maxF1);
	}
}


f2 = function(data) { #m-class

	numberOfAttributes = ncol(data)-1;
	numberOfClasses = length(levels(data[,ncol(data)]));
	numberOfExamplesPerClass=c();
	numberOfExamples = length(data$Class);

	F2 = 0;

	for(c1 in 1:numberOfClasses){
		if(c1!=numberOfClasses){
			for(c2 in (c1+1):numberOfClasses){
				twoClassF2 = 1;

				for(att in 1:numberOfAttributes){
					minMax = min(max(data[data$Class == c1, att]), max(data[data$Class == c2, att]));
					maxMin = max(min(data[data$Class == c1, att]), min(data[data$Class == c2, att]));		
					maxMax = max(max(data[data$Class == c1, att]), max(data[data$Class == c2, att]));
					minMin = min(min(data[data$Class == c1, att]), min(data[data$Class == c2, att]));		

					if(maxMax != minMin){
						twoClassF2 = twoClassF2 * ((minMax - maxMin)/(maxMax - minMin));
					}
					else{
						print("Divisao por zero");
					}
				}

				F2 = F2 + abs(twoClassF2);

			}	
		}	
	}

	return(F2);
}


f3OLD = function(data) { #funciona pra 2-class. verificar as médias utilizadas pra m-class
	
	dataOriginal = data;
	dataAux=data;
	F3 = F4 = 0;

	primeiraIteracao = TRUE;
	attrRemain = ncol(data)-1;
	exRemain = nrow(data);

	nrowC1 = nrowC2 = 0;
	nrowC1Aux = nrowC2Aux = c();

	
	while(attrRemain != 0 && exRemain != 0){
     # print(c("attrRemain", attrRemain));
     # print(c("exRemain", exRemain));
      
		data = multiclass2(data);

		values=c();	
		index1 = index2 = index=  c();	
		attr=c();

		for(j in 1 : length(data)) {
	
			c1 = data[[j]][data[[j]]$Class == 1,];
			c2 = data[[j]][data[[j]]$Class == 2,];
			
			#print(data[[j]]);

			if(primeiraIteracao){
				#c1Original = c1;
				#c2Original = c2;

				nrowC1 = nrowC1Aux[j] = nrow(c1);
				nrowC2 = nrowC2Aux[j] = nrow(c2);
			}

			aux = rep(0, (ncol(c1)-1));

			for(i in 1 : (ncol(c1)-1)){
				aux[i] = (sum(c2[,i] < min(c1[,i]) | c2[,i] > max(c1[,i])) + sum(c1[,i] < min(c2[,i]) | c1[,i] > max(c2[,i])))/(nrowC1 + nrowC2);

				#print(aux[i]);
			}

			#aux = sort(aux, decreasing = T);
			#print(aux);
			
			values[j] = max(aux);	 #--- CORRETO
			attr = which(aux == max(aux));

			#values[j] = aux[j];
			#attr = which(aux == aux[j]);

			if(length(attr) != 1){
				#print(attr);
				#attr = attr[1]; ## DESCOBRIR QUAL CRITERIO DE DESEMPATE
				attr=max(attr);	
			}

			#if(primeiraIteracao){
			#	F3 = values;
			#	print(F3);
				#primeiraIteracao=FALSE;
			#}
		
			#F4 = F4 + values[j];
			
			cumulDiscPower = sum(c2[,attr] < min(c1[,attr]) | c2[,attr] > max(c1[,attr])) + sum(c1[,attr] < min(c2[,attr]) | c1[,attr] > max(c2[,attr]));
			#print(cumulDiscPower);
			
			index1 = (c2[,attr] < min(c1[,attr]) | c2[,attr] > max(c1[,attr]));
			index2 = (c1[,attr] < min(c2[,attr]) | c1[,attr] > max(c2[,attr]));

			nomesc2 = rownames(c2[index1,]);
			nomesc1 = rownames(c1[index2,]);

			index = sort(as.integer(c(nomesc1, nomesc2)),method="quick");
			#index = union(index, sort(as.integer(c(nomesc1, nomesc2)),method="quick"));	
			
			#print(index);
		} 
			if(primeiraIteracao){
				#print(values);
				#print(sum(values));
				F3 = sum(values) / length(data);
				#lengthPrimeiraIteracao = length(data);

				F4 = F3;

				nrowC1 = sum(nrowC1Aux)/length(data);
				nrowC2 = sum(nrowC2Aux)/length(data);
				
				primeiraIteracao=FALSE;
			} else{
				F4 = F4 + (sum(values)/length(data));
			}
			

			if(cumulDiscPower==0)
				break;
		
			data = dataAux[-index,-attr];
			dataAux = data;

			attrRemain = attrRemain - 1;
			exRemain = exRemain - length(index);
			

	}

	return(c(F3, F4));
}

f3 = function(data) {
	
	mdata=multiclass2(data);
	resultados = matrix(nrow=length(mdata), ncol=2);
	for(j in 1:length(mdata)){
	
	  data=mdata[[j]];
	  
	  dataOriginal = data;
	  indexTotal = c();
	  attrTotal = c();
	  
	  F3 = F4 = 0;
	
	  primeiraIteracao = TRUE;
	  attrRemain = ncol(data)-1;
	  exRemain = nrow(data);
	  #cumulDiscPower = rep(0, attrRemain);
	  
	  atributos = 1:attrRemain;

	  while((attrRemain != 0) && (exRemain > 0)){
		  index = attr = c();
	  
		  c1 = data[data$Class == 1,];
		  c2 = data[data$Class == 2,];
		  
		  if(primeiraIteracao){
		    nrowC1 = nrow(c1);
		    nrowC2 = nrow(c2);
		    
		  }
		  
		  aux = c();
		  
		  for(i in 1:attrRemain){
		    aux[i] = (sum(c2[,atributos[i]] < min(c1[,atributos[i]]) | c2[,atributos[i]] > max(c1[,atributos[i]])) + sum(c1[,atributos[i]] < min(c2[,atributos[i]]) | c1[,atributos[i]] > max(c2[,atributos[i]])))/(nrowC1+nrowC2);
		    
		  }
		  
		  value = max(aux);
		  attrTemp = which(aux == max(aux));
		  attr = atributos[max(attrTemp)];	
		  
		  
		  cumulDiscPower = sum(c2[,attr] < min(c1[,attr]) | c2[,attr] > max(c1[,attr])) + sum(c1[,attr] < min(c2[,attr]) | c1[,attr] > max(c2[,attr]));
				  
		  indexC1 = (c1[,attr] < min(c2[,attr]) | c1[,attr] > max(c2[,attr]));
		  indexC2 = (c2[,attr] < min(c1[,attr]) | c2[,attr] > max(c1[,attr]));
		  
		  nomesC1 = rownames(c1[indexC1,]);
		  nomesC2 = rownames(c2[indexC2,]);	
		  
		  index = sort(as.integer(c(nomesC1, nomesC2)));
		  
		  indexTotal = union(indexTotal, index);
		  
		  indexTotal = sort(indexTotal);
			  
		  if(primeiraIteracao){
		    F3 = value;
		    F4 = value;
		    
		    primeiraIteracao = FALSE;
		    
		  } else{
		    F4 = F4 + value;
		  }
		  
		  if(cumulDiscPower==0)
		      break;
		  
		  data = dataOriginal[-indexTotal, ];
		  
		  #aux1 = atributos[attrRemain];
		  #atributos[attrRemain] = attr;
		  #atributos[attr] = aux1;
		  
		  atributos = atributos[-max(attrTemp)];
		  
		  
		  
		  attrRemain = attrRemain - 1;
		  
		  #atributos[1:attrRemain] = sort(atributos[1:attrRemain]);
		  #atributos[attrRemain:(ncol(dataOriginal)-1)] = sort(atributos[attrRemain:(ncol(dataOriginal)-1)]);
		  
		  exRemain = exRemain - length(index);
		  #print(c("ex remain: ", exRemain));
	  
	  }
	  
	  print(c(F3,F4));
	  
	  resultados[j, 1] = F3;
	  resultados[j, 2] = F4;
	  
	}
	
	return(c(mean(resultados[,1]), mean(resultados[,2])));
	#return(c(F3, F4));
}

