      subroutine ecrit(x,u,t,i)
      IMPLICIT NONE
      INCLUDE 'common.inc' 
      double precision u(*),x(*),t,uex
      external uex
C NE PAS TOUCHER
C***********************************************************************************************************************
      INTEGER NNN,INDEX,J,Ra(1:1000),QU
      INTEGER DIR1,DIR2,INDEXMAX,I,K,YFILE
      CHARACTER*72 NUMERO,TMP,NOM1
      CHARACTER    VIRG,BACK
C***********************************************************************************************************************
      VIRG= ',' 
      BACK='\\'
      INDEXMAX = 10
c
c Decomposition d'un entier en decimale.
c
      NNN   = I
      INDEX = 1
c
10    IF(NNN .LT. 10) THEN
         Ra(INDEX)=NNN
      ELSE
        QU = NNN / 10
        Ra(INDEX) = NNN-10 * QU
        NNN = QU
        INDEX = INDEX + 1
        GO TO 10
      ENDIF
      DO 15 J=INDEX+1,INDEXMAX
           Ra(J) = 0
15    CONTINUE
c
c Transcription des decimales en caracteres
c
      NUMERO(1:1) = CHAR(ICHAR('0')+Ra(INDEXMAX))
      DO 20 J = 2,INDEXMAX
        K= J - 1
        TMP(1:J)=NUMERO(1:K)//CHAR(ICHAR('0')+Ra(INDEXMAX-J+1))
        NUMERO = TMP
 20   CONTINUE
c
c L'entier est maintenant en caracteres.
c
c Ouverture des fichiers ou sont stockee y et u
c
      NOM1     = 'sources/data/tmp.'//NUMERO(1:INDEXMAX)
      OPEN(11,FILE=NOM1)	
      DIR1 = 17
c
c On prepare les fichiers Gnuplot; on rajoute des guillemets aux noms de fichiers
c
      NOM1   = '"'//NOM1(1:INDEXMAX+DIR1)//'"'
c
c Remplissage des fichiers gnuplot ou une seule courbe est tracee et on attend une seconde.
c
      DIR1 = DIR1 + 2


c
c Ecriture de la hauteur d'eau dans le fichier y.plt
c
      WRITE(1,98) ABS(t)
98    FORMAT('set title "T = ',F8.3)
      WRITE(1,*) 'set autoscale y'
      WRITE(1,*) 'set ylabel  "u"  font "Times-Roman, 18' 


      WRITE(1,*) 'plot ',NOM1(1:INDEXMAX+DIR1),
     & '     u 1:2 w l lw 3 title "u",',
     & '""   u 1:3 w l lw 3 title "uex"'
      IF (i.EQ.0) THEN 
        WRITE(1,*) 'pause -1 "Hit return to continue"'
      ENDIF
c On ecrit dans les fichiers 
c
      DO J = 1,N+1
        WRITE(11,*) x(J), u(J),uex(t,x(j))
        WRITE(11,*) x(J+1), u(J),uex(t,x(j))
      ENDDO

      RETURN
      END

