program RK4

	implicit none
	integer, parameter :: n_points=400 !n_points=int((x_max-x_min)/h)
	real(kind=8) :: h,min_x,max_x,k1,k2,k3,k4,average_k
	real(kind=8), dimension(n_points) :: x,y
	real(kind=8), external :: f_prime !Declares the function
	integer :: i
	open (unit = 1, file = "results.txt")	
	
	h=0.01
	min_x=0.0
	max_x=4.0
	
	x(1)=min_x
	y(1)=1.0
	
	do i=2,n_points
		k1=h*f_prime(x(i-1)      ,y(i-1))
		k2=h*f_prime(x(i-1)+0.5*h,y(i-1)+0.5*k1)
		k3=h*f_prime(x(i-1)+0.5*h,y(i-1)+0.5*k2)
		k4=h*f_prime(x(i-1)+h,y(i-1)+k3)
		
		!Calculates de slope
		average_k=(1.0/6.0)*(k1+2.0*k2+2.0*k3+k4)
		
		x(i)=x(i-1)+h
		y(i)=y(i-1)+average_k
	enddo
	
	write (1,*) x
	write (1,*) y
	close(1)

end program RK4

!Initialization of function f_prime
real(kind=8) function f_prime(x,y)
	implicit none
	real(kind=8), intent (in) :: x,y
	f_prime=-y
end function f_prime
