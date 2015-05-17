reset

set term svg
set output "perfs.svg" # le nom du fichier qui est engendre

set title "Influence du nombre de clauses et de variables sur le temps d'execution (avec WL)"
set xlabel "Nombre de variables"
set ylabel "Nombre de clauses"
set zlabel "Temps d'exécution median en s"

set view 60,40

splot "comparaison3d.dat" using 1:2:3 with pm3d

