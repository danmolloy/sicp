require 'irb'
# Exercise 1.11

def ifunc(n, i=4, a=2, b=1, c=0)
  return n if n < 3
  total = a+2*b+3*c
  return total if i > n
  ifunc(n, i + 1, total, a, b)
end

IRB.start