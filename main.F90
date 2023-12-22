!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-30
! update: 2021-11-30
! summary: This code reads a markdown file and extracts the fortran code

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(HDF5File_) :: hdf5file
!! hdf5 file
TYPE(MSHFile_) :: mshfile
!! msh file
CHARACTER(LEN=99) :: mshfilename
!! name of mshfile
CHARACTER(LEN=99) :: hdf5filename
!! name of hdf5 file
CHARACTER(LEN=*), PARAMETER :: modname = "msh2hdf"
!! name of program
CHARACTER(LEN=*), PARAMETER :: myname = "main"
!! name of the program
TYPE(CommandLineInterface_) :: cli
!! command line interface
INTEGER(I4B) :: error
!! main
! initializing Command Line Interface
CALL cli%initiate( &
     & progname='msh2hdf', &
     & version='v21.11.0', &
     & authors='Vikas Sharma, Ph.D.', &
     & license='MIT', &
     & description='Convert mshFile to hdf5file',&
     & examples=[ &
     & 'msh2hdf                                           ', &
     & 'msh2hdf -h                                        ', &
     & 'msh2hdf --input mshfile.msh --output hdf5file.h5  ', &
     & 'msh2hdf -i mshfile.msh -o hdf5file.h5             ', &
     & 'msh2hdf --version                                 ', &
     & 'msh2hdf -v                                        '])
!!
CALL cli%add(switch='--input', switch_ab='-i', help='name of input file',&
     & required=.TRUE., act='store', error=error)
!!
IF (error .NE. 0) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'cannot add value of --input from CLI')
!!
!! handling output
CALL cli%add(switch='--output', switch_ab='-o', help='name of output file',&
     & required=.FALSE., act='store', def='default', error=error)
IF (error .NE. 0) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'cannot add value of --output from CLI')
!!
!!
CALL cli%get(switch='-i', val=mshfilename, error=error)
IF (error .NE. 0) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'cannot get value of --input from CLI')
!!
CALL cli%get(switch='-o', val=hdf5filename, error=error)
IF (error .NE. 0) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'cannot get value of --output from CLI')

CALL mshfile%Initiate(filename=mshfilename, STATUS="OLD", ACTION="READ")
CALL mshfile%OPEN()
CALL mshfile%READ()

IF (TRIM(hdf5filename) .EQ. 'default') THEN
hdf5filename = TRIM(mshfile%getFilePath())//TRIM(mshfile%getFileName())//".h5"
END IF

CALL hdf5file%Initiate(hdf5filename, "NEW")
CALL hdf5file%OPEN()
CALL mshfile%Export(hdf5file, "")
CALL hdf5file%DEALLOCATE()
CALL mshfile%DEALLOCATE()

END PROGRAM main
