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

