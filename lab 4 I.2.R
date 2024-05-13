
aria=function(N)
{
  N_C=0;
  for(i in 1:N)
  {
    x=runif(1,0,2)
    y=runif(1,0,2)
    if(y<=-2*x*x+5*x-2)
    {
      N_C=N_C+1
    }
  }
  return(N_C/N*4);
}
arie_aprox=aria(10000)
arie_exact=integrate(function(x) -2*x*x+5*x-2, 0.5, 2)$value
x=arie_aprox-arie_exact
eroare_relativa=abs(x)/arie_exact
arie_exact
arie_aprox
eroare_relativa
