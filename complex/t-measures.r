t1 = function(data){

   

    #// 3. Search the nearest neighbors for each example.
    #searchNearestNeighborsOfAnotherClass ( neigh, distNeigh, globalMinDist, overlappedExamples );
    
    #LEMBRETE: se a distancia for 0, overlap . overlappedExamples = TRUE;
    #Calcular distancia maxima esferas
    #Verificar as distancias que sao != DBL_MAX
    
    dt = dist(data[, -ncol(data)]);
    class = data$Class;
    dt = as.matrix(dt);
    
    EPSILON_SPHERES = 0.55;
	
	
    #Variaveis
    interAux = c();
    inter = intra = 0;
    distNeigh = rep(c(DBL_MAX), times = nrow(dt)); #inicializacao matriz com valores maximos
    globalMinDist = DBL_MAX;
    overlappedExamples = FALSE;
    interClasses = c();
    aux = 1;
	
	for(i in 1 : nrow(dt)){
		for(j in 1 : ncol(dt)) {
			#if(dt[i,j] != 0){ #tirar essa verificacao -> para poder ocorrer overlap
				if(class[i] != class[j]) { #TENHO QUE LIMITAR APENAS PARA OS EXEMPLOS INTER
					interAux[j] = dt[i,j]; #eu quero o minimo disso
					
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

	}
	

    #// 4. Define the maximum separation permitted, epsilon.
    #epsilon = ( ComplexityMeasures::EPSILON_SPHERES * globalMinDist );
    
    epsilon = EPSILON_SPHERES*globalMinDist;

    #// 5. Search for the adherence subsets.
    #calculateAdherenceSubsets ( adherenceOrder, maxAdherenceOrder, distNeigh, overlappedExamples, epsilon );
    
    maxAdherenceOrder = 0;
    adherenceOrder = c();
    
    for (i in 1:nrow(dt)){
      if(overlappedExamples && (distNeigh[i] == 0)){
	adherenceOrder[i] = 0;
      } else{
	adherenceOrder[i] = (distNeigh[i] / epsilon) - 1;
      }
      
      if(adherenceOrder[i] > maxAdherenceOrder){
	maxAdherenceOrder = adherenceOrder[i]; 
      }
    
    }

    #// 6. Eliminate adherence subsets strictly included in another.
    #eliminateAdherenceSetsIncluded ( adherenceOrder, maxAdherenceOrder, epsilon );
    
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
	
	for(i in 1:length(adherenceOrder)){
	    if(adherenceOrder[i] != -1 && (adherenceOrder[i] < maximum) && (adherenceOrder[i] > nextMaximum)){
		nextMaximum = adherenceOrder[i];
	    }
	}
	
	maximum =  nextMaximum;
    
    }

    #// 7. Get statistics for the fraction of maximum covering spheres measure.
    #float* valuesReturn = getStatisticsFractMaxCoveringSpheres ( adherenceOrder, maxAdherenceOrder );
    
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

}

t2 = function(data){
	return(nrow(data)/(ncol(data)-1));
}

