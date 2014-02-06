

# Nominal to binary
NTB = make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary");

binarize = function(data) {
 	temp = NTB(Class ~ ., data, control="N=FALSE, A=TRUE");
	return(temp);
}

setAttr = function(data) {

	n = ncol(data);
	rownames(data) = NULL;
	colnames(data) = c(paste(rep("V", n-1), 1:(n-1), sep=""), "Class");
	
	
	
	data$Class = factor(data$Class); levels(data$Class) = 1:length(levels(data$Class));
	return(data);
}

processing = function(data, name) {

	cl = levels(data$Class);

	for(i in 1 : (length(cl)-1)) {
		for(j in (i+1) : length(cl)) {

			name = unlist(strsplit(name, "\\."))[1];
			temp = data[data$Class == cl[i] | data$Class == cl[j],];
			temp = setAttr(binarize(temp));

			write.arff(temp, paste("./tratados/", name,cl[i],cl[j], ".arff", sep=""));
		}
	}
	return(1);
}

tratamento <- function(){

	setwd("~/Documentos/IC/Datasets_ARFF")

	# Database diretory
	#DIR = "Datasets_ARFF/";

	# Files
	files.arff = list.files(DIR);
	
	files.length <- length(files.arff)
	
	controle <- 1
	
	while(controle <= files.length){
	
		base <- read.arff(files.arff[controle])
		
		print(files.arff[controle])

		length <- length(base)
		nrows <- nrow(base)

		for(i in 1:length){
			
			na.rows <- which(is.na(base[,i]))
		
			if(!is.na(na.rows[1])){ #Trata somente as colunas que possuem valores NA
			
				if(!is.factor(base[,i])){ #Atributos numericos (media)
				
					for(j in 1:length(na.rows)){
										
						mean <- mean(base[which(base[,length] == base[na.rows[j],length]),i], na.rm=T)
						
						base[na.rows[j],i] <- mean
					}
					
				}
				else{ #Atributos categoricos (moda)
					
					for(j in 1:length(na.rows)){
									
						mode <- mfv(as.numeric(base[which(base[,length] == base[na.rows[j],length]),i]))
						
						base[na.rows[j],i] <- levels(base[,i])[mode[1]] #Pega sempre a primeira moda
					}					
					
				}					
				
			}			
			
		}
		
		base <- setAttr(binarize(setAttr(base)))
		
		#base <- setAttr		
		#name <- strsplit(files.arff[controle], ".arff")[[1]]	
		#processing(base, name)
		
		write.arff(base, paste(c("./tratados_new/", files.arff[controle]), collapse=""))
		
		print("OK!")
		
		controle <- controle+1		
	}
}
