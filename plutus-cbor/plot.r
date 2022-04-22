plot   (f$n,f$cpu/1e9,col=4, xlab="Number of assets", ylab="Time (ms)")
lines  (f$n,f$cpu/1e9, col=4)
points (f$n,f$cpu8/1e9,col=3)
lines  (f$n,f$cpu8/1e9, col=3)
points (f$n,f$t/1e9,col=2)
lines  (f$n,f$t/1e9,col=2)
legend (x="topleft", legend=c("Predicted time (CEK step cost = 35000)", "Predicted time (CEK step cost = 2000)", "Actual time"),
        pch=20,col=c(4,3,2),inset=.01,text.col=1)
