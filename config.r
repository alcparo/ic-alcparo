#install.packages("ape");
#install.packages("e1071");
#install.packages("multicore");
#install.packages("randomForest");
#install.packages("rpart");
#install.packages("RWeka");

# Packages
library(ape)
library(e1071);
library(multicore);
library(randomForest);
library(rpart); 
library(RWeka);

# List of functions
CLASS = list();

# Rates
#RATES = seq(0.05, 0.40, 0.05);

RATES = c(0.05, 0.1, 0.2, 0.4);

#
#RATES2 = rep(RATES, each=10);

# Epochs
EPOCHS = 10;

# Classifiers
MLP = make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron");

# Nominal to binary
NTB = make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary");

# Directories
#setwd("~/IC/");

# Files
FILES = list.files("Datasets_processados/");


##TESTE SVN


DBL_MAX = .Machine$double.xmax;
DBL_MIN = .Machine$double.xmin;
