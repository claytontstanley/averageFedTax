library(data.table)
library(ggplot2)
library(cumstats)
library(scales)

aTbl = data.table(rate=c(.1, .12, .22, .24, .32, .35, .37),
		  inc=c(0, 9525, 38700, 82500, 157500, 200000, 500000))

aTbl[, .SD
     ][.(inc=0:1000000), on=.(inc), roll=Inf
     ][, copy(.SD)[aTbl, bracketP := T, on=.(inc)]
     ][inc == 0, bracketP := NA
     ][, inc := inc + 1
     ][, rateMean := cummean(rate)
     ][, incEarned := inc - inc * rateMean
     ][, rowid := 1:.N
     ][, {bTbl <<- copy(.SD); .SD}
     ]

bTbl[, .SD
     ][rowid %in% sample(.N, 10000) | bracketP
     ][, ggplot(.SD, aes(inc, rateMean))
     + geom_line()
     + geom_point(data=.SD[bracketP == T])
     + geom_point(data=.SD[bracketP == T], aes(inc, rate), color='blue')
     + xlab('Income')
     + ylab('Average Tax')
     + scale_x_continuous(label=comma, breaks=pretty_breaks(n=10))
     + scale_y_continuous(label=percent, breaks=pretty_breaks(n=10))
     + theme_bw()
     ]

bTbl[, .SD
     ][, dIncEarned := incEarned - shift(incEarned)
     ][rowid %in% sample(.N, 10000) | bracketP
     ][, ggplot(.SD, aes(inc, dIncEarned))
     + geom_point()
     ]

bTbl[, .SD
     ][inc %in% c(
		  246957
		  , 246957 - 12550 		
		  , 246957 - 12550 + 5000 - 19500
		  )
     ]

#(160000 -19500) * x + 28475 * .22 + 58482 * .22 + 5000 * 0 = 219950 * .2395 

160000 - 19500 + 28475 + 58482 + 5000 -12550

219950 * .2395
52678 - 58482*.22 - 28475*.22
33547/(160000 - 19500)
.2388

.25 * 219950
.2388 * 219950
