required_packages <-
    c("raster") # "tidyverse"
install.packages(setdiff(required_packages, rownames(installed.packages())))

#  načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


# linear gradient, elevation raster (predictor)
r <- raster(ncol = 100, nrow = 100, xmn = 0, ymn = 0, xmx = 99, ymx = 99)
values(r) <- rep(seq(0, 999, 10), 100)
# plot(r)

# Lowland species (linear dependency of distribution on elevation)
# y = -0.001*x + 1
x <- seq(0, 999)
y <- -0.001 * x + 1

# chart
# plot(x,y, main="Lowland species (y = -0.001*x + 1)", xlab="elevation", ylab="probab. of occurrence")

# teoretical occurrence probabilities based on elevation
d <- -0.001 * r + 1
# plot(d, main="teoretical probab. of occurrence")


vs <- function(rep = 1, n = 1000, r = r, d = d) {
    res.points <- SpatialPoints(data.frame(x = 0, y = 0))[-1, ]
    res.elevations <- c()
    res.x <- c()
    res.y <- c()

    # generate random points
    for (i in 1:rep) {
        sample.random <- sampleRandom(d, size = n, cells = TRUE, sp = TRUE)
        # plot(sample.random, pch=1, cex=1.5, col="red", add=TRUE)

        # undelying probabilities
        # print(sample.random@data$layer)

        # true/false (1/0)
        tf <- rbinom(n, size = 1, prob = sample.random@data$layer)

        # remove false (0) points
        sample.real <- sample.random
        sample.real@data <- sample.random@data[-which(tf == 0), ]
        sample.real@coords <- sample.random@coords[-which(tf == 0), ]

        # par(new=TRUE)
        # plot(sample.real, pch=3, cex=1.5, col="green", add=TRUE)

        # elevation values
        r.real <- extract(r, sample.real)

        # check number of points per elevation
        # hist(r.real, breaks=10)

        res.points <- rbind(res.points, sample.real)
        res.elevations <- append(res.elevations, r.real)
    }

    return(list(res.points = res.points, res.elevations = res.elevations))
}

elevations1 <- vs(3, 1000, r, d)
elevations10 <- vs(10, 1000, r, d)

# check number of unique cell per elevation (per_pixel)
elevations1.per_pixel <- extract(r, SpatialPoints(unique(elevations1$res.points@coords)))
elevations10.per_pixel <- extract(r, SpatialPoints(unique(elevations10$res.points@coords)))


# hist(elevations1$res.elevations, right=FALSE, breaks=10)
# hist(elevations1.per_pixel, right=FALSE, breaks=10)

# hist(elevations10$res.elevations, right=FALSE, breaks=10)
# hist(elevations10.per_pixel, right=FALSE, breaks=10)

# plot(elevations1$res.points)
# plot(elevations10$res.points)


# as.vector(table(elevations10$res.elevations))
# as.vector(table(elevations10.per_pixel))

# ks.test(as.vector(table(elevations10$res.elevations)), as.vector(table(elevations10.per_pixel)))
# cor.test(as.vector(table(elevations10$res.elevations)), as.vector(table(elevations10.per_pixel)), method = "spearman")


# plot(SpatialPoints(unique(elevations10$res.points@coords)))





# Předpoklad je, že 1) každá buňka má stejnou šanci být vybrána (prozkoumána) a 2) je prozkoumána se stejnou intenzitou.

# Při použitém groupování per_pixel: čím více vzorkuju, tím více mi v absolutních číslech
# přibývá pixelů s více a více okrajovými (méně vhodnými) hodnotami, které postupně
# zkreslují teoretickou pravděpodobnost distribuce druhu v prostoru.
# Jenže jak poznám správnou míru provzorkování území? Musel bych to udělat najednou (v jeden okamžik)
# a už to neopakovat. (jenže reálně do doho vstupuje navíc detaktabilita, takže nemodeluju nikdy
# pravděpodobnost výskytu, ale pravděpodobnost detektability)