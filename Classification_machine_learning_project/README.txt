DIRECTORY IN CUI INSERIRE IL FILE test set.csv
Il file test set.csv deve essere inserito in una castelle presente in MyDrive chiamata Project ML, quindi il path sarà /MyDrive/Project\ ML
ovviamente il tutto deve essere caricato su google drive

LIBRERIE DA INSTALLARE
Tutte le librerie sono presenti nel codice, quindi non c'è bisogno di inserirle manualmente, basterà solamente eseguire lo script.
In particolare le librerie usate sono:

-mlxtend
-sklearn
-pandas
-numpy
-joblib
-sys
-train_test_split, GridSearchCV (sklearn.model_selection)
-enable_iterative_imputer (sklearn.experimental)
-SimpleImputer, IterativeImputer, KNNImputer sklearn.impute
-StandardScaler, MinMaxScaler, MaxAbsScaler, RobustScaler, OneHotEncoder, OrdinalEncoder (sklearn.preprocessing)
-RandomUnderSampler, ClusterCentroids (imblearn.under_sampling)
-RandomOverSampler, SMOTE (imblearn.over_sampling)
-IsolationForest (sklearn.ensemble)
-ColumnTransformer (sklearn.compose)
-f_classif, mutual_info_classif, SelectKBest, SelectFromModel (sklearn.feature_selection)
-ExhaustiveFeatureSelector (mlxtend.feature_selection)
-Pipeline (sklearn.pipeline)
-RandomForestClassifier, GradientBoostingClassifier, AdaBoostClassifier (sklearn.ensemble)
-SVC, LinearSVC (sklearn.svm)
-DecisionTreeClassifier,export_graphviz (sklearn.tree)
-tree (sklearn)
-graphviz
-matplotlib.pyplot
-accuracy_score, confusion_matrix, precision_score, recall_score, f1_score (sklearn.metrics)

INFO
Il file Routine_test.ipynb esegue la routine di test mentre il file Routine_training è quello usato per il treining