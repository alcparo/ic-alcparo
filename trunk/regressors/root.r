# R Code
# Artificial Noise Meta-Learning 
# Luis P. Faina Garcia, André Carvalho and Ana C. Lorena
# Artificial regression for Meta-Learning


write = function(mse, type) {

	name = paste(type, sep="");
	dump("mse", name);
	return(0);
}


step = function(data, alpha=0.05) {

	for(i in TECHS)
		data[,i] = round(data[,i]/alpha)*alpha;
	return(data);
}


root = function(type, load) {

	data = loo(read(type, load));
	mse = threshold(data$tran, data$test);
	write(mse, type);
	return(0);
}


