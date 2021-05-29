      SUBROUTINE  init(x,u,dx)
      IMPLICIT NONE
      INCLUDE 'common.inc' 
      DOUBLE PRECISION x(*),u(*),dx
      INTEGER i
      DOUBLE PRECISION U0
      EXTERNAL U0
C
      DO i=1,n+2
         x(i)=(i-1.0d0)*dx
         u(i)=u0(x(i))
      ENDDO
      RETURN
      END
cccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE  calcul_u(u,uold,dx,dt)
      IMPLICIT NONE
      INCLUDE 'common.inc' 
      DOUBLE PRECISION u(*),uold(*),dx,dt
      INTEGER i
C
      DO i=2,n+1
c        u(i) = uold(i) - c*dt/dx*0.5d0*(uold(i+1)-uold(i-1))
c     & +0.5d0*(c*dt/dx)**2.0*(uold(i+1)-2*uold(i)+uold(i-1))
c------------------------S1--------------------------
		 u(i) = uold(i) -c*dt/dx*(uold(i+1)-uold(i))
c------------------------S2--------------------------
c		 u(i) = uold(i) -c*dt/(2.0d0*dx)*(uold(i+1)-uold(i-1))	
      ENDDO
      RETURN
      END

