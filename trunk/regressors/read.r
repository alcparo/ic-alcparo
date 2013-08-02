# R Code
# Reading base files 
# Luis P. Faina Garcia, André Carvalho and Ana C. Lorena
# Read the base files with complexity measures and fscore for techniques


remove = function(data) {

	data = data.frame(data);
	data = data[,setdiff(COLNAMES, "T2")];
	return(data);
}


normalize = function(data) {

	for(i in METRICS) {
		if(min(data[,i]) != max(data[,i])) {
			data[,i] = (data[,i] - min(data[,i]))/(max(data[,i]) - min(data[,i]));
		} else {
			data[,i] = 1;
		}
	}
	return(data);
}


make = function(dir) {

	tab = lapply(list.files(dir, full.names=TRUE), function(file) {
		aux = matrix(unlist(dget(file)),  ncol=length(COLNAMES), byrow=TRUE);
		colnames(aux) = COLNAMES;
		normalize(aux);
	});
	return(tab);
}


read = function(type, load) {

	dir = paste(DATABASE, type, "/", load, "/", sep="");
	data = do.call("rbind", make(dir));
	data = remove(data);
	return(data);
}




