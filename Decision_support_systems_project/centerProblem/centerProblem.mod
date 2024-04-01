param I integer > 0;	# insieme dei domand point
param J integer > 0;	# insieme dei possibili location per i facility
param d {i in 1..I, j in 1..J}; 	# distanza tra il domand point i e il possibile facility j
param p integer > 0; 	# numero totale dei facility da allocare
param h {i in 1..I}; 	# importanza associata ad ogni demand point
param m {j in 1..J};	# max cunstumer che il facility j può gestire
param c {j in 1..J}; 	# costo di apertura del facility nella posizione j

var x {i in 1..I, j in 1..J} binary; 	# 1 se il domand point i è servito dal facility j, o altrimenti
var y {j in 1..J} binary; 	# 1 se nella posizione j è stato aperto un facility
var D; #massima distanza tra il domand point e il facility vicino

minimize costi_totali: D+sum{j in 1..J}c[j]*y[j]; 

s.t. vinc_servizio{i in 1..I}: sum{j in 1..J}x[i,j]=1;	# tutti i domand point devono esere serviti
s.t. vinc_attivazione{i in 1..I, j in 1..J}: x[i,j]-y[j]<=0;	#un domand point può essere servito soltanto da un facility aperto
s.t. vinc_numero_totale: sum{j in 1..J}y[j]=p;	# i facility aperti devono essere p
s.t. vinc_max_cuntomer{j in 1..J}: sum{i in 1..I}x[i,j]<=m[j];	# il numero di domand point serviti da un facility deve essere <= del numero massimo consentito
s.t. D_def{i in 1..I}: D >= h[i]*sum{j in 1..J}d[i,j]*x[i,j]; # devinizione di D (massima distanza tra il domand point e il facility vicino)