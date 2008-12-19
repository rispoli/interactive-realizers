#set xrange [0:5]; replot


set term png size 1024, 768

set xrange[-5:5]
set yrange[-10:10]
set xtics 1
set ytics 1
set grid

set output "of.png"
plot (x + 1) * (x - 2) * ((x - 4) / 2)




#h(x) = sin(x)
#plot h(x)
#replot h(24)
#replot h(25)
#replot h(26)
#replot h(48)
#set xrange[23:49]; replot
#
