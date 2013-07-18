# Complexity Metrics
source("complex/fisher.r");
source("complex/linearly.r");
source("complex/mixture.r");
source("complex/t-measures.r");

complex = function(train) {
	#print("complex");
	aux = c(fisher(train), linearly(train), mixture(train), tmeasures(train));
	#aux = fisher(train);
	#aux = linearly(train);
	#aux = mixture(train);
	return(aux);
}