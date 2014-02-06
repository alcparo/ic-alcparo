multiclass = function(data) {

	cl = levels(data$Class);
        aux = 1;
	listAux = list();

	for(i in 1 : (length(cl)-1)) {
		for(j in (i+1) : length(cl)) {

			temp = data[data$Class == cl[i] | data$Class == cl[j],];
			temp = setAttr(binarize(temp));

			listAux[[aux]] = temp;
			aux = aux + 1;
		}
	}
	return(listAux);
}

multiclass2 = function(data) {

	cl = levels(data$Class);
        aux = 1;
	listAux = list();

	for(i in 1 : length(cl)) {
		

			temp = data[data$Class == cl[i] | data$Class != cl[i],];
			
			if(i != length(cl)){
			    temp[temp$Class != cl[i],ncol(temp)] = cl[i+1];
			}else{
			    temp[temp$Class != cl[i],ncol(temp)] = cl[i-1];
			}
			  
			temp = setAttr(binarize(temp));

			listAux[[aux]] = temp;
			aux = aux + 1;
		
	}
	return(listAux);
}


setAttr = function(data) {

	n = ncol(data);
	rownames(data) = NULL;
	colnames(data) = c(paste(rep("V", n-1), 1:(n-1), sep=""), "Class");
	
	
	
	data$Class = factor(data$Class); levels(data$Class) = 1:length(levels(data$Class));
	return(data);
}


binarize = function(data) {
 	temp = NTB(Class ~ ., data, control="N=FALSE, A=TRUE");
	return(temp);
}
