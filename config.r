#install.packages("ape");
#install.packages("e1071");
#install.packages("multicore");
#install.packages("class");
#install.packages("randomForest");
#install.packages("rpart");
#install.packages("RWeka");

# Packages
library(ape)
library(e1071);
library(class);
#library(multicore);
library(randomForest);
library(rpart); 
library(RWeka);
library(modeest);

# List of functions
CLASS = list();

# Rates
RATES = c(0.05, 0.1, 0.2, 0.4);

# Epochs
EPOCHS = 10;

# Directory
DIR = (path.expand("~/"));

# Complexity metrics
METRICS = c("F1", "F1v", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "T2");

# Re-implementation of four measure metrics
REPLACE = c("F1v", "L1", "L2", "L3");

# Classifiers names
CLASSIFIERS = c("SVM", "kNN", "NaiveBayes", "randomForest");

COLNAMES = c(METRICS, CLASSIFIERS);

# Weka Classifiers
MLP = make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron");

# Weka Nominal to binary
NTB = make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary");

#Directories
setwd(path.expand("~/ic-alcparo/"));

# Files
#FILES.NAMES = list.files(path.expand("~/Datasets_processados/"));
FILES.NAMES = as.matrix(read.table("files.bases.ok.names.txt"))[,1];
FILES = paste(path.expand("~/Datasets_Processados_NEW/"), FILES.NAMES, sep="");

DBL_MAX = .Machine$double.xmax;
DBL_MIN = .Machine$double.xmin;

# Metabases file names
#FILES.METABASES.NAMES = list.files(path.expand("~/metabases_NEW"));
#FILES.METABASES = paste(path.expand("~/metabases_NEW/"), list.files(path.expand("~/metabases_NEW/")), sep="");

FILES.METABASES.NAMES = list.files(path.expand("~/metabases"));
FILES.METABASES = paste(path.expand("~/metabases/"), list.files(path.expand("~/metabases/")), sep="");

# Number of regressors iterations
EXAMPLES = 5; #2 ou mais

set.seed(1);


source("others/multiclass.r");
source("others/increment.r");
source("others/kcv.r");
source("classifiers/classifiers.r");
source("noise/noise.r");
source("measures.r");
source("metabase.r");
source("regressors/regressors.r");


#FILES.METABASES.NAMES = list.files(path.expand("~/metabases/"));
#FILES.METABASES.NAMES = strsplit(FILES.METABASES.NAMES, split=".metabase");
#FILES.METABASES.NAMES = do.call(rbind, FILES.METABASES.NAMES);

#FILES.BASES.OK.NAMES = intersect(FILES.NAMES,FILES.METABASES.NAMES);
#FILES.BASES.OK = paste(path.expand("~/Datasets_processados/"), FILES.BASES.OK.NAMES, sep="");
#write(FILES.BASES.OK.NAMES, "files.bases.ok.names.txt");

