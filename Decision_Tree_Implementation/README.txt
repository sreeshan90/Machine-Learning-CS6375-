Sreesha Nagaraj
sxn146630

Steps to compile and run the java project


Compilation:
javac Parameter.java

javac Node.java

javac Heuristic.java

javac DecisionTree.java

javac Index.java


Running the program:

with the tree
java Index  10 15 training_set.csv validation_set.csv test_set.csv yes

without the tree (just the accuracies)
java Index  10 15 training_set.csv validation_set.csv test_set.csv no