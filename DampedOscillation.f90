program damped
  implicit none
  real a , v , x , t , dt , c1 , c2
  integer j
  write(6,*)'input c1,c2(a=-c1*x+c2*v)'
  read(5,*)c1,c2
  write(6,*)'oscillation orbital'
  x=10
  t=0
  v=0
  dt=0.01
  do j=1,1000
    t= t + dt
    a= 0-c1 * x - c2 * v
    v= v + a * dt
    x= x + v * dt
    write(6,*)t,x
  enddo
endprogram damped
