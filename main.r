source(path.expand("~/ic-alcparo/config.r"));

for(i in 1:length(FILES)){	
	if(!any(FILES.METABASES.NAMES == paste(FILES.NAMES[i],".metabase", sep=""))){ #Caso algum erro, não repetir para as bases já computadas
		print(FILES.NAMES[i]);
		dataAux = read.arff(FILES[i]);
		tabelaAux = metabase(dataAux);
		#tabelaAux;
		write.table(data.frame(tabelaAux), paste(path.expand("~/metabases_NEW/"),FILES.NAMES[i],".metabase",sep=""));	# Salva as metabases de cada dataset
	}
}