# run in C:\gnuplot-2.0\bin\wgnuplot  ## 2.4 has bugs!

set terminal eepic color # latex
### set term epslatex color


### not used
set output "growth1.tex"
unset label
set size 0.3,0.5
set key off
set xtic nomirror scale 1 offset 0 1,1,100
set ytic axis nomirror scale 1 offset -1 0,5,10000
set border 1
set pointsize 0.3
set xlabel "$n$" # font "TimesRoman"
set samples 5
set label "$n+12$" at 5.2, 17.3
set label "$3n$" at 5.2, 15.1
set label "$n^2$" at 5.2, 25.3
set label "$Fibo(n)$" at 5.2, 5.3
set yrange [0:25]
plot [1:5]    x+12 with linespoints pt 2, \
               3*x with linespoints pt 5, \
               x*x with linespoints pt 4, \
               "fib.dat" using 1:2 with linespoints pt 6

unset label

set pointsize 1
set output "growth2.tex"
set size 0.45,0.65
set format y "$%g$"
set format x "$%g$"
set key off
set xlabel "$n$" # font "TimesRoman"
set xtic border nomirror 2,2,100
set ytic border nomirror 0,20,1000
set yrange [0:100]
set samples 10
set label "$3n$" at 10.5, 30.6
set label "$n^2$" at 10.5, 105
set label "$Fibo(n)$" at 10.5,55.6
plot [1:10]  3*x t '$3n$' with points ps 0.3 pt 7, x*x t '$n^2$' with points pt 12 ps 0.3, "fib.dat" with points pt 6 ps 0.3

unset label              
set output "growth3.tex"
set size 0.45,0.65
set format y "$%g$" # font "Palatino"
set format x "$%g$" # font "Palatino"
set xlabel "$n$" # font "Palatino"
set ylabel "" # font "Palatino"
set key off
set xtic border nomirror 4,4,100
set ytic border nomirror 0,1000,6000
set samples 20
set yrange [0:6900]
#set label "$n+12$" at 22.5, 32
#set label "$3n$" at 21, 60
set label "$n^2$" at 21, 500
#set label "$Fibo(n)$" at 20, 100
#set label "$Fibo(n)$" at 21, 5500
set label "$Fibo(n)$" at 21, 6765
plot [1:20] 3*x t '$3n$' with points ps 0.3 pt 7, x*x t '$n^2$' with points pt 12, "fib.dat" with points pt 6

unset output
set term windows
               

