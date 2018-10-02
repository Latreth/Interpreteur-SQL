#Projet de DBDM

## Rendu 1
### GUEPIN Florent & RODRIGUEZ Emmanuel

Compilation
===========

Pour compiler : make. L’exécutable créé s’appelle `exec`.

Détail du travail
=================

Nous avons implémenté la totalité du travail demander en Partie 1 du projet. Cependant, il reste une incertitude sur le Not In : en effet, nous n'avons pas reussi a finir de faire tourner la querie3 du sample donner. Cela sera corriger pour le prochain rendu. 
Enfin, nous avons toutefois passer les queries du sample, ainsi qu'un certain nombres de tests que vous pouvez trouver dans les fichiers. 
Les tables à Load ne sont pas nécessaires à marquer dans l'execution du .exec

==================
Dans ce deuxième rendu, nous avons implémenter la plupart des questions. En effet, nous avons implémenter : 
Les optimisations (avec la création d'un nouveau fichier d'interpretation optimizer)
Les queries ORDER BY et GROUP BY ainsi que les fonctions d'agregations max min et sum ont aussi été implémentés. 
Enfin, les comparaisons via des égalités à des string ou a des entiers (ou des inégalités) ont aussi été traités. 
Pour des soucis d'implémentation, (pour le OR optimiser) le '>' ainsi que le '!=' apparaissent dans les types, cependant, puisqu'il est possible de faire une demande avec uniquement des '=' ainsi que des '<' nous n'avons pas jugés nécessaires d'alourdir outre mesure la syntaxe : c'est pourquoi ceci n'est pas implémenter dans le parser (grammaire). 
Les ajouts particuliers, propre à l'ORDER BY ont elles été cependant ajoutés : les options DESC et ASC ont été implémentés afin de classés dans un ordre croissants ou décroissants. 
Vous pouvez trouvés dans le dossier examples une liste d'examples que nous avons jugés interessants de tester pour observer la justesse de nos fonctions. Les résultats produits par nos queries ont été fournis avec la querie dans chaque fihiers '.sql' .

Quelques problèmes subsistants : 
=================================
Nous n'arrivons pas à faire passer la queries 3 de votre sample. Bien que nous ayons tout de même implémenter la fonction qui permetes de modifier les NOT IN et les IN. De ce  fait, certaine queries peuvent surement ne pas passer. 
# Compilateur-Interpreteur
