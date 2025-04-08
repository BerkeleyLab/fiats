set logscale y 10
set key font ",20"
set title "Input-Tensor Histograms Mapped to the Interval [0,1]" font "Arial,20"

plot for [col=2:8] 'inputs_stats.plt' using 1:col with linespoints title columnheader
pause mouse any "Plot 1 of 2. Press any key to continue.\n"

set title "Output-Tensor Histograms Mapped to the Interval [0,1]" font "Arial,20"

plot for [col=2:6] 'outputs_stats.plt' using 1:col with linespoints title columnheader
pause mouse any "Plot 2 of 2. Press any key to exit.\n"
