cccccccccccccccccccccccccccccccccccccccccc
      FUNCTION u0(x)
      IMPLICIT NONE
      DOUBLE PRECISION u0,x
c
c     if ((x.gt.1.).and.(x.lt.2.)) u0=1.
	  u0 = dexp(-50.0d0*(x-3.0d0)**2.0d0)

      RETURN
      END
cccccccccccccccccccccccccccccccccccccccccc
      FUNCTION uex(t,x)
      IMPLICIT NONE
      INCLUDE 'common.inc' 

      DOUBLE PRECISION t,uex,x,u0
      EXTERNAL U0
c 
      UEX = U0(X-C*T)
      RETURN
      END
        
        

