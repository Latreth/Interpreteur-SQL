SELECT e.dpt, MAX(e.ide) FROM "employes.csv" e WHERE e.nom=e.nom GROUP BY e.dpt

(* Resultat : 
e.max(ide) e.dpt 
91	1	
98	2	
95	3	
89	4	
96	5	
77	6	
100	7	
87	8	
81	9	
70	10	
99	20	*)
