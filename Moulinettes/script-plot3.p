# pour visualiser le dessin trace par ce script gnuplot, taper
# gnuplot -persist script-plot.p
#  (en s`assurant que le fichier comparaison.dat est present dans le repertoire)

reset

### decommenter les 2 lignes ci-dessous pour engendrer un fichier pdf
### plutot qu`un dessin a l`ecran
#set term pdfcairo
#set output "heuristiquesWL.pdf" # le nom du fichier qui est engendre

set title "Comparaison heuristiques avec WL"
set xlabel "Nombre de littéraux"
set ylabel "Temps d'execution"


# Dessin en joignant des points
set style data linespoints

set pointsize 2   # la taille des points


# on trace deux courbes: avec les colonnes 1 et 2, avec les colonnes 1 et 3
# a chaque fois, le nom de la courbe est lu en tete de colonne
plot "comparaison.dat" using 1:8 title columnheader(8), \
     "comparaison.dat" using 1:9 title columnheader(9), \
     "comparaison.dat" using 1:10 title columnheader(10), \
     "comparaison.dat" using 1:11 title columnheader(11)

