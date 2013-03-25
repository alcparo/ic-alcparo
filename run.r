# Directories
setwd("~/IC/");

source("Scripts/config.r");
source("Scripts/classifiers/classifiers.r");
source("Scripts/baseline/baseline.r");
source("Scripts/fscore/fscore.r");
source("Scripts/technique/ensamble.r");
source("Scripts/technique/technique.r");
source("Scripts/noise/noise.r");
source("Scripts/complex/measures.r");


run = function(train, noise) {
	print("run");
	aux = c(unlist(baseline(train, noise)), unlist(technique(train, noise)));
	names(aux) = c("ednn", "renn", "aenn", "cs", "mt");
	return(sort(aux, decreasing=TRUE)[1]);	
}
