program circularMotion
  implicit none
  real,parameter::pi=3.1415926
  real(8) x , y , vx , vy , v0 , a , ax , t , dt
  integer n, Openstatus

  open(25,file = "result", status = "replace", action ="write",&
  access = "sequential", position = "rewind",&
  iostat = Openstatus)
  if ( Openstatus > 0 ) stop "Error opening file.25"
  x=3
  v0 = pi
  t=0
  dt=0.00001
  y = 0
  write(6,*)'input acceleration'
  read(5,*)a
  write(6,*)'circular orbital'
vx=v0
vy=0
  do n=1,1000000
    write(25,*)x,y
    t= t + dt

    vx= vx - a * ( vy / sqrt( vx**2 + vy**2 ) ) * dt
    x= x + vx * dt
    vy= vy + a * ( vx / sqrt( vx**2 + vy**2 ) ) * dt
    y= y + vy * dt
  end do

  close(25)

endprogram circularMotion
