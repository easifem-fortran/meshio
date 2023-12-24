! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-24
! summary:  This program converts mesh to easifem mesh format
!
!# Introduction
!
! In easifem to create a domain we need mesh in HDF5File_ format.
! This is a command line interface to convert different mesh format
! to easifem mesh format.
!
! - Gmsh MSH4 to easifem

PROGRAM main
USE GlobalData, ONLY: I4B, DFP, LGT
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE MSHFile_Class, ONLY: MSHFile_
USE CommandLineInterface_Class, ONLY: CommandLineInterface_
USE String_Class, ONLY: String

IMPLICIT NONE

CHARACTER(*), PARAMETER :: default_output = "super-mario-is-super-cool"
CHARACTER(LEN=*), PARAMETER :: modname = "meshio"
!! name of program
CHARACTER(LEN=*), PARAMETER :: myname = "main"
!! name of the program
TYPE(HDF5File_) :: hdf5file
!! hdf5 file
TYPE(MSHFile_) :: mshfile
!! msh file
CHARACTER(1024) :: longString
CHARACTER(:), ALLOCATABLE :: mshfilename
!! name of mshfile
CHARACTER(:), ALLOCATABLE :: hdf5filename
!! name of hdf5 file
TYPE(String) :: path, name
TYPE(CommandLineInterface_) :: cli
!! command line interface
INTEGER(I4B) :: error

! initializing Command Line Interface
CALL cli%initiate( &
     & progname='msh2hdf', &
     & version='v23.10.2', &
     & authors='Vikas Sharma, Ph.D.', &
     & license='MIT', &
     & description='Convert mesh to easifem mesh format.',&
     & examples=[ &
     & 'meshio                                           ', &
     & 'meshio -h                                        ', &
     & 'meshio --input mshfile.msh --output hdf5file.h5  ', &
     & 'meshio -i mshfile.msh -o hdf5file.h5             ', &
     & 'meshio --version                                 ', &
     & 'meshio -v                                        '])

CALL cli%Add(switch='--input', switch_ab='-i', help='Name of input file  ',&
     & required=.TRUE., act='store', error=error)

IF (error .NE. 0) &
     & CALL e%RaiseError(modName//"::"//myName//" - "// &
     & '[INTERNAL ERROR] :: Cannot Add value of --input from CLI')

!! handling output
CALL cli%Add(switch='--output', switch_ab='-o', help='name of output file',&
     & required=.FALSE., act='store', def=default_output, error=error)
IF (error .NE. 0) &
     & CALL e%RaiseError(modName//"::"//myName//" - "// &
     & '[INTERNAL ERROR] :: Cannot Add value of --output from CLI')

CALL cli%Get(switch='-i', val=longString, error=error)
IF (error .NE. 0) &
     & CALL e%RaiseError(modName//"::"//myName//" - "// &
     & '[INTERNAL ERROR] :: Cannot Get value of --input from CLI')
mshfilename = TRIM(longString)

CALL cli%Get(switch='-o', val=longString, error=error)
IF (error .NE. 0) &
     & CALL e%RaiseError(modName//"::"//myName//" - "// &
     & 'cannot Get value of --output from CLI')
hdf5filename = TRIM(longString)

CALL mshfile%Initiate(filename=mshfilename, STATUS="OLD", ACTION="READ")
CALL mshfile%OPEN()
CALL mshfile%READ()

IF (hdf5filename .EQ. default_output) THEN
  path = mshfile%GetFilePath()
  name = mshfile%GetFileName()
  hdf5filename = path%chars()//name%chars()//".h5"
END IF

CALL hdf5file%Initiate(hdf5filename, "NEW")
CALL hdf5file%OPEN()
CALL mshfile%Export(hdf5file, "")
CALL hdf5file%DEALLOCATE()
CALL mshfile%DEALLOCATE()

END PROGRAM main
