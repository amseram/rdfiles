!  rdcsv.f90 
!
!  FUNCTIONS:
!  rdcsv - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: rdcsv
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
    INTEGER Function GFL(Fileunit)
        ! Get File Lines
        INTEGER , INTENT(IN) :: Fileunit
        CHARACTER(Len=1) :: cDummy
        INTEGER :: ierr
        GFL = 0
        REWIND(Fileunit)
        Do WHILE(.TRUE.)
            READ(Fileunit,*,ioStat = ierr) cDummy
            IF(ierr /= 0) EXIT
            GFL = GFL + 1
        END DO
        REWIND(Fileunit)
    END Function GFL
    program rdcsv
!*****************************************************************************
!   Rdcsv and inset into mysql
!*****************************************************************************
    implicit none
    ! Variables
	INTEGER i, n, err_mesg, GFL, ierr
    CHARACTER(Len=256), DIMENSION(:), ALLOCATABLE:: args, cont !Dynamic arrays
	CHARACTER*32 arg,x,fname
    ! Body of rdcsv
    ! print *, 'Hello World'
	n = iargc()
	IF (n > 1) THEN
        ALLOCATE(args(1:n), stat = err_mesg)
		DO i=1,n
			CALL getarg(i,arg)
			READ(arg,*)x
            args(i) = x
			WRITE(*,*)'arg',i,':',x
        END DO
        fname = ".\"//args(1)
		GO TO 10
    ELSE 
        CALL getarg(1,arg)
        READ(arg,*)fname
        fname = ".\"//fname
        GO TO 10
    ENDIF
10  OPEN(100, file=fname, status='old')
    PRINT *,"File name:",fname
    n = GFL(100)
    PRINT *,"total line:",n
    ALLOCATE(cont(1:n))
    DO i = 1,n
        READ(100,*,iostat = ierr)cont(i)
        IF(ierr /= 0) EXIT
    END DO
    DO i = 1,n
        PRINT*,cont(i)
    END DO
    end program rdcsv

