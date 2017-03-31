    INTEGER Function GFN(Fileunit)
        ! Get File Lines
        INTEGER , INTENT(IN) :: Fileunit
        CHARACTER(Len=4) :: cDummy
        INTEGER :: ierr
        GFN = 0
        REWIND(Fileunit)
        Do WHILE(.TRUE.)
            READ(Fileunit,*,ioStat = ierr) cDummy
!           Format ( A4,A2,I5,1X,A4,A1,A3,1X,A1,I4,A1,3X,3F8.3,2F6.2,6X,A4,A2,A2 )
            IF(ierr /= 0) EXIT
            GFN = GFN  + 1
        END DO
        REWIND(Fileunit)
    END Function GFN
!###################################################################################
!   The program help you to find unproper water molercules and deletthem
!###################################################################################
    PROGRAM fcw
    IMPLICIT NONE
    INTEGER i, n, err_mesg, ierr, ckn, j, GFN, nfn
    CHARACTER*6, DIMENSION(:), ALLOCATABLE:: rectyp
    CHARACTER*1, DIMENSION(:), ALLOCATABLE:: altloc,chainid,icode
    CHARACTER*2, DIMENSION(:), ALLOCATABLE:: element,charge
    CHARACTER*3, DIMENSION(:), ALLOCATABLE:: resname
    CHARACTER*4, DIMENSION(:), ALLOCATABLE:: atmname,segid
    CHARACTER*32,DIMENSION(:), ALLOCATABLE:: args
    INTEGER*8, DIMENSION(:), ALLOCATABLE:: atmsn,ressq,awts
    REAL, DIMENSION(:), ALLOCATABLE:: xx,yy,zz,ocq,bf,lens
    CHARACTER*32 arg,x,fname
    REAL dx,dy,dz,minlens,crits
!   read argvs
    crits = 0.5
    n = iargc()
    IF (n > 1) THEN
        ALLOCATE(args(1:n), stat = err_mesg)
        DO i=1,n
            CALL getarg(i,arg)
            READ(arg,*)args(i)
            WRITE(*,*)'arg',i,':',args(i)
        END DO
        fname = args(1)
        READ(args(n),*)crits
        GO TO 10
    ELSE 
        CALL getarg(1,arg)
        READ(arg,*)fname
        fname = fname
        GO TO 10
    ENDIF
!   rd wats
    
!   rd atms
10  OPEN(100,file=fname,status='old')
    PRINT *,"PDB File name:",fname
    n = GFN(100)
    !n = GFL(100)
    !PRINT *,"total atoms:",n
    ALLOCATE(rectyp(1:n),atmname(1:n),altloc(1:n),resname(1:n),chainid(1:n),icode(1:n),segid(1:n),element(1:n),charge(1:n))
    ALLOCATE(atmsn(1:n),ressq(1:n),xx(1:n),yy(1:n),zz(1:n),ocq(1:n),bf(1:n),awts(1:n),lens(1:n))
    PRINT *,"Start Read!"
    i = 1
    DO WHILE(.TRUE.)
        READ(100,1,ioStat=ierr)rectyp(i),atmsn(i),atmname(i) ,altloc(i),resname(i),chainid(i),ressq(i),icode(i),xx(i),yy(i),zz(i),ocq(i),bf(i),segid(i),element(i),charge(i)
1       Format ( A6,I5,1X,A4,A1,A3,1X,A1,I4,A1,3X,3F8.3,2F6.2,6X,A4,A2,A2 )
        awts(i) = 0
        IF(resname(i) .eq. "WAT" ) THEN
            IF(atmname(i) .eq. " O  ") THEN
                awts(i) = 1
            END IF
            IF(atmname(i) .eq. " H1 ") THEN
                awts(i) = 3
            END IF
            IF(atmname(i) .eq. " H2 ") THEN
                awts(i) = 3
            END IF
        END IF
        IF(rectyp(i) .eq."TER   ") awts(i) = 2
        IF(rectyp(i) .eq. "END   ") EXIT
        i = i + 1
    END DO
    PRINT *,"Read over!"
!    PRINT *,rectyp(n)
    ckn = 0
    DO i=1,n
        IF(awts(i).eq.1)THEN
            minlens = 9999
            DO j=1,n
                IF(awts(j) .eq. 0)THEN
                    dx = xx(i) - xx(j)
                    dy = yy(i) - yy(j)
                    dz = zz(i) - zz(j)
                    lens(j) = sqrt( (dx * dx + dy * dy + dz * dz ) )
                    IF(minlens.gt.lens(j))minlens = lens(j)
                 END IF
            END DO
            IF(crits .gt. minlens) THEN
                awts(i) = -1
                awts(i+1) = -1
                awts(i+2) = -1
                awts(i+3) = -1
            END IF
        END IF
    END DO
    REWIND(100)
    nfn = 1
    DO WHILE(fname(nfn:nfn) .ne. " ")
        nfn = nfn + 1
    END DO
    fname = fname(1:nfn)
    fname(nfn:nfn+4) = ".out"
    OPEN(101,file=fname,status="unknown")
    PRINT *,"OUTPUT File:",fname
    PRINT *,"Distance",crits
    DO i=1,n
!        READ(100,3,ioStat=ierr)rectyp(i),atmsn(i),atmname(i),altloc(i),resname(i),chainid(i),ressq(i),icode(i),xx(i),yy(i),zz(i),ocq(i),bf(i),segid(i),element(i),charge(i)
!3       Format ( A6,I5,1X,A4,A1,A3,1X,A1,I4,A1,3X,3F8.3,2F6.2,6X,A4,A2,A2 )
        IF(awts(i) .gt. -1 )THEN
            IF(resname(i) .ne. "   ") THEN
                WRITE(101,4)rectyp(i),atmsn(i),atmname(i),altloc(i),resname(i),chainid(i),ressq(i),icode(i),xx(i),yy(i),zz(i),ocq(i),bf(i),segid(i),element(i),charge(i)
            ELSE
                WRITE(101,5)rectyp(i)
            END IF
4           Format ( A6,I5,1X,A4,A1,A3,1X,A1,I4,A1,3X,3F8.3,2F6.2,6X,A4,A2,A2 )
5           Format ( A6 )
        END IF
    END DO
    END PROGRAM fcw