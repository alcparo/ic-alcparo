source("config.r");

for(i in 1:length(FILES)){	
	dataAux = read.arff(FILES[i]);
	tabelaAux = metabase(dataAux);
	#tabelaAux;
	write.table(data.frame(tabelaAux), paste(path.expand("~/metabases/"),FILES.BASES.OK.NAMES[i],".metabase",sep=""));	# Salva as metabases de cada dataset
}