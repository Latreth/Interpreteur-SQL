SELECT e.dpt, SUM(e.ide) FROM "employes.csv" e WHERE e.nom=e.nom GROUP BY e.dpt

(* Réponse :
e.sum(ide) e.dpt 
636	1	
408	2	
444	3	
918	4	
463	5	
238	6	
596	7	
397	8	
353	9	
185	10	
412	20	*)
