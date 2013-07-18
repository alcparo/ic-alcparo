# R Code
# DCoL Library
# Ho and Basu (2002), Ho et al. (2006) and (Orriols-Puig et al., 2010)
# A set of measures designed to characterize the apparent complexity of data sets

# Directory
DIR = ("/home/andrecatini/");


# Complexity metrics
METRICS = c("F1", "F1v", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2");

# Re-implementation of three meansure metrics
REPLACE = c("F1v", "L1", "L2", "L3");

measures = function(data) {

	name = paste(".", paste(sample(letters, 50, replace=TRUE), collapse=""), sep="");
	write.arff(data, paste(DIR, name, ".arff", sep=""));

	exe = paste(DIR, "DCoL-v1.1/Source/./dcol", sep="");
	inp = paste(" -i ", DIR, name, ".arff", sep="");
	out = paste(" -o ", DIR, name, " -A", sep="");

	system(paste(exe, inp, out, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE);
	vet = read.table(paste(DIR, name, ".txt", sep=""), skip=18)[,-1];
	names(vet) = METRICS;

	if(length(levels(data$Class)) != 2) {

		out = paste(" -o ", DIR, name, " -F 1v -L 1 -L 2 -L 3 -d", sep="");
		system(paste(exe, inp, out, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE);
		vet[,REPLACE] = colMeans(read.table(paste(DIR, name, ".txt", sep=""), skip=8)[,-1]);
	}

	system(paste("rm ", DIR, name, ".* ", sep=""));
	return(vet);
}

