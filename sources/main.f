      PROGRAM transport
c Resolution de l'equation de transport sur [0,L]
c avec des conditions aux limites periodiques

      IMPLICIT NONE
      INCLUDE 'common.inc' 
C     Declaration
      DOUBLE PRECISION deltax,deltat
      DOUBLE PRECISION Tf,t,CFL,L
      DOUBLE PRECISION u(1100),uold(1100)
      DOUBLE PRECISION x(1100)
      INTEGER CPT
	  integer J
	  double precision uex
	  external uex
	  
	  CHARACTER(100) :: num1char
      CHARACTER(100) :: num2char
	  double precision num1
	  integer num2, le

      open(1,file='trace.plt')
C UNE FOIS LE CALCUL TERMINE, LANCER GNUPLOT PUIS LA COMMANDE
C load 'trace.plt' pour visualiser la solution en temps


	  

	  !First, make sure the right number of inputs have been provided
	  IF(COMMAND_ARGUMENT_COUNT().NE.2)THEN
		WRITE(*,*)'ERROR, TWO COMMAND-LINE ARGUMENTS REQUIRED, STOPPING'
		STOP
	  ENDIF

	  CALL GET_COMMAND_ARGUMENT(1,num1char)   !first, read in the two values
	  CALL GET_COMMAND_ARGUMENT(2,num2char, le)

	  READ(num1char,*)num1                    !then, convert them to REALs
	  READ(num2char,*)num2
	 
	 
	  print*,le

	  open(13,FILE= "scriptgnu2")
	  write(13, *) "set terminal png"
	  write(13,*) 'set output "S1_n=',num2char(1:le),
     &  '_CFL=', num1char(1:1), 'p', num1char(3:3), '.png"' 
	  write(13,*) 'set title "CFL=', num1char(1:le), ', N=',
     &  num2char(1:le), '"'
	  write(13,*) 'plot "last" u 1:2 w l t "u", "" u 1:3 w l t "uex"'
	  close(13)
	  
	  
      t = 0.0d0
      Tf= 5.0d0
      c = 1.0d0

c      n = 500
	  n = num2 
c      cfl = 0.5d0 ! c'est alpha
 	  cfl = num1

      L = 10.0D0
      deltax = L/dble(n+1)

      cpt = 0

c Initialisation 
      call init(x,uold,deltax)

      DO WHILE (t .LT. Tf)
c Ecriture des donnees en temps dans data/tmp.numero
c        CALL ecrit(x,uold,t,cpt)
		 open(12,FILE="last")
		 DO J = 1,N+1
			WRITE(12,*) x(J), uold(J),uex(t,x(j))
			WRITE(12,*) x(J+1), uold(J),uex(t,x(j))
		 ENDDO	
		 close(12)
         cpt = cpt+1

c Calcul du pas de temps (Cond de stabilité)
         deltat = cfl*deltax/abs(c)
         if (t+deltat.ge.Tf) deltat = Tf-t
         t =t + deltat
          print*,'t = ', t

c Calcul de la solution a l'instant t_{n+1}
         CALL calcul_u(u,uold,deltax,deltat)

c Mise à jour des conditions aux limites
c        a l'aval : cond de Neumann homogene: cond de sortie libre
         u(N+2) = u(N+1)

c MAJ de uold
         uold = u
      ENDDO
	  

      close(1)
	  call system("gnuplot scriptgnu2")
      stop 'Execution avec succes!!!'     
      end program
