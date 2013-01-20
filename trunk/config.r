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
RATES = seq(0.05, 0.40, 0.05);

# Epochs
EPOCHS = 30;

# Classifiers
MLP = make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron");

# Nominal to binary
NTB = make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary");

# Directories
setwd("~/IC/");

# Files
FILES = list.files("Datasets_processados/");


##TESTE SVN

