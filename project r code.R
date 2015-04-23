GDP <- read.table("C:/Data/gdpeducationdata.txt",col.names=c("country","gdp","continent","secondary"))
n <- 166

for (i in 1:n) {
  if (GDP$secondary[i] == 4 || GDP$secondary[i] == 5 || GDP$secondary[i] == 6) {
    GDP$secondary[i] <- 0
  } else if (GDP$secondary[i] == 7 || GDP$secondary[i] == 8 || GDP$secondary[i] == 9) {
    GDP$secondary[i] <- 1
  }
}

secondary <- as.factor(GDP$secondary)
continent <- as.factor(GDP$continent)
data.table <- table(continent,secondary); data.table

value.matrix <- matrix(c(rep(0,12)),nrow=6,byrow=TRUE)
for (i in 1:n) {
  for (j in 1:6) {
    if (GDP$continent[i] == j) {
      if (GDP$secondary[i] == 0) {
        value.matrix[j,1] <- value.matrix[j,1] + 1
      } else if (GDP$secondary[i] == 1) {
        value.matrix[j,2] <- value.matrix[j,2] + 1
      }
    }
  }
} 

rowSums(value.matrix)
colSums(value.matrix)

x1 <- c(rep(0,n))
x2 <- c(rep(0,n))
x3 <- c(rep(0,n))
x4 <- c(rep(0,n))
x5 <- c(rep(0,n))
x6 <- c(rep(0,n))
for (i in 1:n) {
  if (GDP$continent[i] == 1) {
    x1[i] <- 1
  } else if (GDP$continent[i] == 6) {
    x1[i] <- -1
  } else {
    x1[i] <- 0
  }
  
  if (GDP$continent[i] == 2) {
    x2[i] <- 1
  } else if (GDP$continent[i] == 6) {
    x2[i] <- -1
  } else {
    x2[i] <- 0
  }
  
  if (GDP$continent[i] == 3) {
    x3[i] <- 1
  } else if (GDP$continent[i] == 6) {
    x3[i] <- -1
  } else {
    x3[i] <- 0
  }
  
  if (GDP$continent[i] == 4) {
    x4[i] <- 1
  } else if (GDP$continent[i] == 6) {
    x4[i] <- -1
  } else {
    x4[i] <- 0
  }
  
  if (GDP$continent[i] == 5) {
    x5[i] <- 1
  } else if (GDP$continent[i] == 6) {
    x5[i] <- -1
  } else {
    x5[i] <- 0
  }
  
  if (GDP$secondary[i] == 0) {
    x6[i] <- 1
  } else if (GDP$secondary[i] == 1) {
    x6[i] <- -1
  }
}

x1x6 <- x1*x6
x2x6 <- x2*x6
x3x6 <- x3*x6
x4x6 <- x4*x6
x5x6 <- x5*x6

cbind(x1,x2,x3,x4,x5,x6,x1x6,x2x6,x3x6,x4x6,x5x6)[1:25,]

GDP.mod1 <- lm(GDP$gdp~x1+x2+x3+x4+x5+x6+x1x6+x2x6+x3x6+x4x6+x5x6)
summary(GDP.mod1)
anova(GDP.mod1)

GDP.mod2 <- lm(GDP$gdp~x1+x2+x3+x4+x5+x6)
summary(GDP.mod2)
anova(GDP.mod2)

GDP.mod3 <- lm(GDP$gdp~x6+x1x6+x2x6+x3x6+x4x6+x5x6)
summary(GDP.mod3)
anova(GDP.mod3)

GDP.mod4 <- lm(GDP$gdp~x1+x2+x3+x4+x5+x1x6+x2x6+x3x6+x4x6+x5x6)
summary(GDP.mod4)
anova(GDP.mod4)

anova(GDP.mod2,GDP.mod1)
anova(GDP.mod3,GDP.mod1)
anova(GDP.mod4,GDP.mod1)

qf(0.95,5,154)
qf(0.95,1,154)

plot(GDP$country,GDP$gdp,xlab="Country",ylab="GDP per capita",main="GDP per capita by country")
plot(GDP$continent,GDP$gdp,xlab="Continent",ylab="GDP per capita",main="GDP per capita by continent")
plot(GDP$secondary,GDP$gdp,xlab="Secondary school education",ylab="GDP per capita",main="GDP per capita by secondary school education")

europe <- 0; e <- 0
africa <- 0; a <- 0
for (i in 1:n) {
  if (GDP$continent[i] == 3) {
    europe <- europe + GDP$gdp[i]
    e <- e + 1
  } else if (GDP$continent[i] == 4) {
    africa <- africa + GDP$gdp[i]
    a <- a + 1
  }
}
europe.mean <- europe/e; europe.mean
africa.mean <- africa/a; africa.mean
tapply(GDP$gdp,GDP$continent,mean)
cbind(value.matrix[1,1]+value.matrix[1,2],value.matrix[2,1]+value.matrix[2,2],value.matrix[3,1]+value.matrix[3,2],value.matrix[4,1]+value.matrix[4,2],value.matrix[5,1]+value.matrix[5,2],value.matrix[6,1]+value.matrix[6,2])
shorter <- 0; longer <- 0
for (i in 1:n) {
  if (GDP$secondary[i] == 0) {
    shorter <- shorter + 1
  } else if (GDP$secondary[i] == 1) {
    longer <- longer + 1
  }
}
shorter; longer
tapply(GDP$gdp,GDP$secondary,mean)

g.sums <- c(rep(0,12))
g.means <- c(rep(0,12))
g.counts <- c(value.matrix[1,],value.matrix[2,],value.matrix[3,],value.matrix[4,],value.matrix[5,],value.matrix[6,])
interaction.means <- function(cont,sec,iter) {
  g.sum.iter <- 0
  for (i in 1:n) {
    if (GDP$secondary[i] == sec) {
      if (GDP$continent[i] == cont) {
        g.sum.iter <- g.sum.iter + GDP$gdp[i]
      }
    }
  }
  return(g.sum.iter)
}

k <- 1
for (i in 1:6) {
  for (j in 0:1) {
    g.sums[k] <- interaction.means(i,j,k)
    k <- k + 1
  }
}
for (i in 1:12) {
  g.means[i] <- (g.sums[i])/(g.counts[i])
}
cbind(g.counts,round(g.means,2))

interaction.plot <- function(cont,sec,iter,d) {
  result <- c(rep(0,d))
  t <- 1
  for (i in 1:n) {
    if (GDP$secondary[i] == sec) {
      if (GDP$continent[i] == cont) {
        result[t] <- iter
        t <- t + 1
        result[t] <- GDP$gdp[i]
        t <- t + 1
      }
    }
  }
  return(result)
}

g.interact <- c()
s <- 1
for (i in 1:6) {
  for (j in 0:1) {
    u <- 2*(g.counts[s])
    g.interact <- c(g.interact,interaction.plot(i,j,s,u))
    s <- s + 1
  }
}

plot.matrix <- matrix(c(rep(0,2*n)),nrow=n,byrow=TRUE)
x <- 1
for (i in 1:n) {
  plot.matrix[i,1] <- g.interact[x]
  x <- x + 1
  plot.matrix[i,2] <- g.interact[x]
  x <- x + 1
}

plot(plot.matrix[,1],plot.matrix[,2],xlab="Continent by Education interaction",ylab="GDP per capita",main="GDP per capita by interaction")

GDP.mod5 <- aov(GDP$gdp~factor(GDP$continent)+factor(GDP$secondary))
anova(GDP.mod5)
TukeyHSD(GDP.mod5)

secondary.mean <- as.vector(tapply(GDP$gdp,GDP$secondary,mean)); secondary.mean
continent.mean <- as.vector(tapply(GDP$gdp,GDP$continent,mean)); continent.mean
