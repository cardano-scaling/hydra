liCol <- "green"
lbCol <- "red"
ltCol <- "yellow"
lmCol <- "blue"

f <- rf("EqualsData.frame")
li <- f[f$name=="listI",]
lb <- f[f$name=="listB",]
lt <- f[f$name=="listTree",]
lm <- f[f$name=="mapTree",]

plot1 <- function() {
    plot  (li$x, li$t*1e6, col=liCol, main="EqualsData", xlab="size", ylab="time (µs)",ylim=c(0,400))
    points(lb$x, lb$t*1e6, col=lbCol)
    points(lt$x, lt$t*1e6, col=ltCol)
    points(lm$x, lm$t*1e6, col=lmCol)
    legend(x="topleft", inset=.05,
           legend=c("List(I)","List(B)", "Tree(List)", "Tree(Map)"),
           col=c(liCol, lbCol, ltCol, lmCol), lty=1, lw=2)

}
       
plot2 <- function() {
    plot  (li$x, li$t*1e6, col=liCol, main="EqualsData", xlab="size", ylab="time (µs)",ylim=c(0,25))
    points(lb$x, lb$t*1e6, col=lbCol)
    points(lt$x, lt$t*1e6, col=ltCol)
    points(lm$x, lm$t*1e6, col=lmCol)
    legend(x="bottomright", inset=.05,
           legend=c("List(I)","List(B)", "Tree(List)", "Tree(Map)"),
           col=c(liCol, lbCol, ltCol, lmCol), lty=1, lw=2)
}

