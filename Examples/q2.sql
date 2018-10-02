SELECT e.dpt, MIN(e.ide) FROM "employes.csv" e WHERE e.nom=e.nom GROUP BY e.dpt

(* Resultat :
e.min(ide) e.dpt 
1	1	
18	2	
15	3	
10	4	
4	5	
5	6	
2	7	
3	8	
30	9	
17	10	
45	20*)
