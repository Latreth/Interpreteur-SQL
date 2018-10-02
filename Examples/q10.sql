SELECT e.dpt, e.nom FROM "employes.csv" e WHERE e.dpt IN ( SELECT s.dpt FROM "employes.csv" s, "departements.csv" ds WHERE ds.directeur = s.ide AND e.dpt = ds.idd)

(* Réponse :
e.dpt e.nom 
1	Thor Bush	
1	Iris Mcbride	
1	Kameko Short	
1	Clark Henson	
1	Fredericka Alexander	
1	Austin Mueller	
1	Eliana Santos	
1	Vivian Gregory	
1	Tana Thomas	
1	Kieran Weaver	
1	McKenzie Jensen	
1	Devin Bolton	
1	Gemma Calhoun	
1	Hamish Fulton	
2	Lillian Berg	
2	Ora Grimes	
2	Forrest Harrison	
2	Vernon Nieves	
2	Diana Peterson	
2	Oliver Lowe	
2	Kenyon Hood	
10	Alexander Sullivan	
10	Melinda Lott	
10	Ray Tran	
10	Hoyt Alston	
10	Tatyana Becker	*)
