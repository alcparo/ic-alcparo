# Fisher


fisher = function(train) {
	print("fisher");

	#c = list();
	#for(i in 1 : length(class))
	#	c[[i]] = train[train$Class == class[i], -ncol(train)];

	values = c(f1(train));
	return(values);
}


f1 = function(data) {

	#aux = rep(0, ncol(c1));
	#for(i in 1 : ncol(c1))
	#	aux[i] = ((mean(c1[,i])-mean(c2[,i]))^2)/(var(c1[,i])^2 + var(c2[,i])^2);
	#return(max(aux));

	

	numberOfAttributes = ncol(data)-1;
	numberOfClasses = length(levels(data[,ncol(data)]));
	numberOfExamplesPerClass=c();
	numberOfExamples = length(data$Class);

	f = den = 0;

	atLeastOne = FALSE;
	maxF1 = -1;
	whichAttribute = -1;

	for(i in 1:numberOfClasses){
		numberOfExamplesPerClass[i] = length(which(data$Class==i));
	}


	for( att in 1:numberOfAttributes ){ #Percorre todos atributos
		#print("Attr");
		f = den = 0;

		for(c1 in 1:numberOfClasses){#Primeiro somatorio
			den = den + ((numberOfExamplesPerClass[c1] / numberOfExamples) * (var(data[data$Class==c1,att])^2));
			#print(c("den", den));

			if(c1!=numberOfClasses){
				for(c2 in (c1+1):numberOfClasses){
					if(c1!=c2){
						f = f + ( (numberOfExamplesPerClass[c1]/numberOfExamples) * (numberOfExamplesPerClass[c2]/numberOfExamples) * ( mean(data[data$Class==c1,att]) - mean(data[data$Class==c2,att])) ^ 2);		
						#print(c("f",f));
				
					}
				}
			}
		}

		if(f != 0 && den != 0){
			atLeastOne = TRUE;
			f = f/den;
			print(c("f", f));
			
			if(f > maxF1){
				maxF1 = f;
				whichAttribute = att;
			}
		}	

	}

	print(c("maxF1", maxF1));

	if(!atLeastOne){
		print("Error F!");
	}

	return(maxF1);
}


f2 = function(data) {

	#for(i in 1 : ncol(c1))
	#	aux = aux * (min(max(c1[,i]), max(c2[,i])) - max(min(c1[,i]), min(c2[,i])))/(max(max(c1[,i]), max(c2[,i])) - min(min(c1[,i]), min(c2[,i])));
	#return(aux);

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


f3 = function(c1, c2) {

	aux = rep(0, ncol(c1));
	for(i in 1 : ncol(c1))
		aux[i] = (sum(c2[,i] < min(c1[,i]) | c2[,i] > max(c1[,i])) + sum(c1[,i] > max(c2[,i]) | c1[,i] < min(c2[,i])))/(nrow(c1)+ nrow(c2));
	return(max(aux));
}
