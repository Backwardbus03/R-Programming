for (i in 1:5) {
  print(i ^ 2)
}

for(i in c(2, 4, 6, 8)) {
  print(i ^ 2)
}

x <- 0
for(i in c(2, 4, 6, 7, 8)) {
  if(i %% 2 == 0) {
    x <- x + 1
  }
}
print(x)

# while is like normal while and repeat requires a break statement

abc = function(x, y) {x ^ 2 + y ^ 2}

for(i in 1:3) {
  for(j in 1:3) {
    print(abc(i, j))
  }
}

print(seq(from=-4, length=4, by=2))

for(i in 5:7) {
  print(i + 3*i - 2 * i ** 2)
}

print(c("Mooc course", "are helpful"))

print(-seq(to=-8, length=6))

print(seq(along=c(25, 77, 25, 140, 8, 20, 120, 56, 19)))

x = c(34, 154, 176, 43, 88, 92, 37, 65, 59, 26, 38, 74, 66)
print(x[(sqrt(x^2 - 10*x) < 30)])

y = 15:5
print(y[(2:5)])