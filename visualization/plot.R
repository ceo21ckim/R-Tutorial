# plot ----
# bty ----

op <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))

plot(0:6, 0:6, bty = "C", main = ' bty = \"c\" ')

plot(0:6, 0:6, bty = "n", main = ' bty = \"n\" ')

plot(0:6, 0:6, bty = "o", main = ' bty = \"o\" ')

plot(0:6, 0:6, bty = "7", main = ' bty = \"o\" ')

plot(0:6, 0:6, bty = "u", main = ' bty = \"u\" ')

plot(0:6, 0:6, bty = "l", main = ' bty = \"l\" ')
par(op)


# type ----
op <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))

plot(0:6, 0:6, main = 'default')

plot(0:6, 0:6, type = 'b', main = 'type = \"b\" ')

plot(0:6, 0:6, type = 'c', main = 'type = \"c\" ')

plot(0:6, 0:6, type = 'o', main = 'type = \"o\" ')

plot(0:6, 0:6, type = 's', main = 'type = \"s\" ')

plot(0:6, 0:6, type = 'S', main = 'type = \"S\" ')
par(op)


# cex ----
op <- par(no.readonly = TRUE)
par(mfrow = c(2,2), oma = c(2, 2, 2, 2), cex = 1)
plot(0:6, 0:6, type = 'n', main = 'cex in text')
text(1:3, 1:3, labels = LETTERS[1:3], cex = 1:3)
plot(0:6, 0:6, type = 'n', main = 'cex in plot')
text(1:3, 1:3, labels = LETTERS[1:3], cex = 1:3)
par(cex = 2)

plot(0:6, 0:6, type = 'n', main = 'cex in par')
text(1:3, 1:3, labels = LETTERS[1:3], cex = 1:3)
plot(0:6, 0:6, type = 'n', main = 'cex in par')
text(1:3, 1:3, labels = c('가', '나', '다'), cex = 1:3)
points(3:5, 1:3, pch = 1:3, cex = 1:3)
# title(main = 'cex', line = 0, outer = T)
par(op)


# srt ----
par('srt')
op <- par(no.readonly = TRUE)
par(mar = c(2, 2, 2, 2))
plot(0:6, 0:6, type = 'n', axes = F, xlab = "", ylab = "")
text(3, 5, 'srt = 0', srt = 0, cex = 2)
text(1, 3, 'srt = 90', srt = 90, cex = 2)
text(3, 1, 'srt = 180', srt = 180, cex = 2)
text(5, 3, 'srt = 270', srt = 270, cex = 2)
text(5, 5, 'srt = -45', srt = -45, cex = 2)
text(1, 5, 'srt = 45', srt = 45, cex = 2)
par(op)


op <- par(no.readonly = TRUE)
par(mfrow = c(3, 3), oma = c(0, 0, 2, 0), mar = c(2, 2, 2, 2))
plot(0:4, 0:4, tck = -0.2, main = 'tck = -0.2')
plot(0:4, 0:4, tck = -0.1, main = 'tck = -0.1')
plot(0:4, 0:4, tck = 0, main = 'tck = 0')
plot(0:4, 0:4, tck = 0.3, main = 'tck = 0.3')
plot(0:4, 0:4, tck = 0.5, main = 'tck = 0.5')
plot(0:4, 0:4, tck = 0.7, main = 'tck = 0.7')
plot(0:4, 0:4, tck = 1, main = 'tck = 1')
par(tck = 0.2)
plot(0:4, 0:4, main = 'tck defined in par')
plot(0:4, 0:4,tck = -0.1, main = 'tck defined in both')
title(main = 'tck', line = 0, outer = T)
par(op)


op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
par('mar')
par(mar = c(0, 0, 0, 0))
plot(0:4, 0:4)
title('mar = c(0, 0, 0, 0)')
par(mar = c(2, 2, 2, 2))
plot(0:4, 0:4, main = 'mar = c(2, 2, 2, 2)')
par(mar = c(5, 5, 5, 5))
plot(0:4, 0:4, main = 'mar = c(5, 5, 5, 5)')
par(mar = c(1, 2, 3, 4))
plot(0:4, 0:4, main = 'mar = c(1, 2, 3, 4)')
par(op)