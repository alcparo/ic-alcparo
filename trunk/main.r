# Directories
setwd("~/ic-alcparo");


#source("Scripts/config.r");
#source("Scripts/classifiers.r");
#source("Scripts/baseline.r");
#source("Scripts/scores.r");
#source("Scripts/ensamble.r");
#source("Scripts/technique.r");
#source("Scripts/noise.r");


source("processing/multiclass.r");
source("config.r");



run = function(train, noise) {

	aux = c(unlist(baseline(train, noise)), unlist(technique(train, noise)));
	
	#baseline: ednn", "renn", "aenn"
	#technique: "cs", "mt"
	#cada tecnica retorna dois valores, fazendo com que o aux fique com o dobro do tamanho, tendo assim componentes NA
	
	names(aux) = c("ednn", "renn", "aenn", "cs", "mt");
	
	print(aux); 
	print(names(sort(aux, decreasing=TRUE)[1]));
	
	
	return(sort(aux, decreasing=TRUE)[1]);	 #Retorna o nome da melhor tecnica de deteccao de ruido junto com a sua acuracia ?
}


#trocar lapply por mclapply em technique e baseline

