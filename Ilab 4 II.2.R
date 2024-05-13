

medie=function(N)
  {
  pr1=3/4
  pr2=1/4
  l1=4
  l2=12
  s=0
  for (i in 1:N)
  {
    x=p1*rexp(1, l1)+p2*rexp(1, l2)
    s=s+x
  }
  medie=s/N
  return(medie)
}

medie(10000)
