library("ggplot2")
#dev.off(dev.list()["RStudioGD"]) 
rm(list = ls())
rows <- 75
cols <-75
iter <- 100
alive <- 0.9

rownsnames = ""
for (i in 1:(rows - 1)) {
    rownsnames <- c(rownsnames, "")
}
colsnames = ""
for (i in 1:(cols - 1)) {
    colsnames <- c(colsnames, "")
}

world <- matrix(0, nrow = rows, ncol = cols, dimnames = list(rownsnames, colsnames))
for (i in 1:rows) {
    for (j in 1:cols) {
        r <- runif(1,0,1)
        if (r > alive) {
            world[i,j] <- 1
        }

    }
}

for (k in 1:iter) {
    set <- matrix(NA, nrow= 1, ncol = 2)
    for (i in 1:rows) {
        for (j in 1:cols) {
            vlb <- i - 1
            vub <- i + 1
            hlb <- j - 1
            hub <- j + 1

            if (i == 1) {
                vlb <- 1
            } 
            if (i == rows) {
                vub <- rows
            } 
            if (j == 1) {
                hlb <- 1
            } 
            if (j == cols) {
                hub <- cols
            } 

            s <- 0
            for (v in vlb:vub) {
                for (h in hlb:hub) {
                    s <- s + world[v, h]
                }
            }
            
            if (world[i,j] == 1 & (s < 3 | s > 4)) {
                world[i,j] <- 0
            }
            if (world[i,j] == 0 & s == 3) {
                world[i,j] <- 1
            }

            if (world[i,j] == 1) {
                set <- rbind(set, c(i,j))
            }
        }
    }
    sets <- data.frame(x = set[2:nrow(set),1], y = set[2:nrow(set),2])
    print(ggplot(sets, aes(x = sets[,1],y = sets[,2])) + 
        geom_point(show.legend = FALSE, colour = "black") +
        xlab(" ") +
        ylab(" ") +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank()))
    Sys.sleep(0.05)
}

