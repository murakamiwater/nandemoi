program circularMotion
  implicit none
  real,parameter::pi=3.1415926
  real x , y , vx , vy , v0 , a , ax , t , dt
  integer n
  x=3
  v0 = 6*pi
  t=0
  dt=0.08
  write(6,*)'input acceleration'
  read(5,*)a
  write(6,*)'circular orbital'
vx=v0
vy=0
  do n=1,1000
    write(6,*)x,y
    t= t + dt

    vx= vx - a * ( vy / sqrt( vx**2 + vy**2 ) ) * dt
    x= x + vx * dt
    vy= vy + a * ( vx / sqrt( vx**2 + vy**2 ) ) * dt
    y= y + vy * dt
  end do

endprogram circularMotion
