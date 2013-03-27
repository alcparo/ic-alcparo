tmeasures = function(data){

    return(c(t1(data), t2(data)));

}



t1 = function(data){

   

    #// 3. Search the nearest neighbors for each example.
   
   # data=read.arff("/home/andrecatini/IC/Datasets_processados/AcuteInflammations.arff");
    class = data$Class;
    
      #Normalizacao
   # dataAux = data[,-ncol(data)];
    #dataTemp = matrix(nrow=nrow(dataAux), ncol=ncol(dataAux));
    
    #for(i in 1:nrow(dataAux)){
    #  for(j in 1:ncol(dataAux)){
	#dataTemp[i,j] = ((dataAux[i,j] - min(dataAux[,j]))/(max(dataAux[,j]) - min(dataAux[,j])));
    #  }
    #}
    for(i in 1:(ncol(data)-1)){
	  data[,i] = scale(data[,i], center=min(data[,i]), scale = diff(range(data[,i])));
	}
    
    
    dt=dist(data[,-ncol(data)]);
    dt=as.matrix(dt);
    
    
    EPSILON_SPHERES = 0.55;
	
	
    #Variaveis
    interAux = c();
    inter = intra = 0;
    distNeigh = rep(c(DBL_MAX), times = nrow(dt)); #inicializacao matriz com valores maximos
    globalMinDist = DBL_MAX;
    overlappedExamples = FALSE;
    interClasses = c();
    aux = 1;
	print("Search the nearest neighbors for each example.");
	for(i in 1 : nrow(dt)){
		for(j in 1 : ncol(dt)) {
			#if(dt[i,j] != 0){ #tirar essa verificacao -> para poder ocorrer overlap
				if(class[i] != class[j]) { #TENHO QUE LIMITAR APENAS PARA OS EXEMPLOS INTER
					interAux[aux] = dt[i,j]; #eu quero o minimo disso
					increment(aux);
				} 
			#}
		}
	
		if(!is.null(interAux)){
			distNeigh[i] = min(interAux, na.rm=TRUE);
			#interClasses[aux] = which(interAux == min(interAux, na.rm=TRUE));
			#aux = aux + 1;
		}
		
		if(distNeigh[i] == 0){
		  overlappedExamples = TRUE;
		} else if(globalMinDist > distNeigh[i]){
		  globalMinDist = distNeigh[i];
		}

			#print(c(min(interAux, na.rm=TRUE), min(intraAux, na.rm=TRUE)));
			
		interAux = c();
		aux = 1;

	}
	

    #// 4. Define the maximum separation permitted, epsilon.
   
    print("Define the maximum separation permitted, epsilon.");
    epsilon = EPSILON_SPHERES*globalMinDist;

    #// 5. Search for the adherence subsets.
   
    print("Search for the adherence subsets.");
    maxAdherenceOrder = 0;
    adherenceOrder = c();
    
    for (i in 1:nrow(dt)){
      if(overlappedExamples && (distNeigh[i] == 0)){
	adherenceOrder[i] = 0;
      } else{
	adherenceOrder[i] = as.integer((distNeigh[i] / epsilon)) - 1;
      }
      
      if(adherenceOrder[i] > maxAdherenceOrder){
	maxAdherenceOrder = adherenceOrder[i]; 
      }
    
    }

    #// 6. Eliminate adherence subsets strictly included in another.
    
    print("Eliminate adherence subsets strictly included in another.");
    maximum = maxAdherenceOrder;
    
    while(maximum >=  0){
    
	for(i in 1:length(adherenceOrder)){
	
	    if(adherenceOrder[i] == maximum){
	    
		for(j in 1:length(adherenceOrder)){
		    difOfOrder = (adherenceOrder[i] - adherenceOrder[j]) * epsilon;
		    distAux = dt[i,j];
		    
		    if(distAux < difOfOrder){
			adherenceOrder[j] = -1;
		    }
		
		}
	    
	    }
	
	}
	
	nextMaximum = -1;
	
	for(i in 1:length(adherenceOrder)){ #ver como otimizar isso (max ?)
	    if(adherenceOrder[i] != -1 && (adherenceOrder[i] < maximum) && (adherenceOrder[i] > nextMaximum)){
		nextMaximum = adherenceOrder[i];
	    }
	}
	
	maximum =  nextMaximum;
    
    }

    #// 7. Get statistics for the fraction of maximum covering spheres measure.
    
    print("Get statistics for the fraction of maximum covering spheres measure.");
    sum = sumsqr = numOrders = 0;
    stats = c();
    
    
    for(i in 1:length(adherenceOrder)){
	if(adherenceOrder[i] >= 0){
	    sum = sum + adherenceOrder[i];
	    sumsqr = sumsqr + (adherenceOrder[i] * adherenceOrder[i]);
	    numOrders = numOrders + 1;
	  
	}
    
    }

    stats[1] = numOrders;
    stats[2] = sum / numOrders;
    stats[3] = sqrt( ( sumsqr - sum * sum / numOrders ) / ( numOrders - 1 ) );
    
    
    print(stats);
    
    return(stats[1]/nrow(data));

}

t2 = function(data){
	return(nrow(data)/(ncol(data)-1));
}

