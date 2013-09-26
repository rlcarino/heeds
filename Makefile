#======================================================================
#
#    HEEDS (Higher Education Enrollment Decision Support) - A program
#      to create enrollment scenarios for 'next term' in a university
#    Copyright (C) 2012, 2013 Ricolindo L. Carino
#
#    This file is part of the HEEDS program.
#
#    HEEDS is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    HEEDS is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program (see the file COPYING.TXT). If not, see
#    <http://www.gnu.org/licenses/>.
#
#    Send inquiries about HEEDS to:
#      Ricolindo L Carino
#      E-mail: Ricolindo.Carino@AcademicForecasts.com
#      Address: 600 Cruise St., Starkville, MS 39759, U.S.A.
#
#======================================================================

# Makefile for HEEDS

#
# On Windows, the build-PC must have MinGW/MSYS installed, and the
#   HEEDS directory must be C:\HEEDS\
#

#---------------------------------------------------------------
# raw data format (RAWDATA)
#---------------------------------------------------------------
#RAWDATA=REGIST
RAWDATA=CUSTOM

#---------------------------------------------------------------
# operating system (OS) and destination of binary (BIN)
#---------------------------------------------------------------
#OS=GLNX
#BIN=~/HEEDS/bin/$(OS)
OS=MSWIN
BIN=/c/HEEDS/bin/$(OS)

#---------------------------------------------------------------
# debugging flags 
#---------------------------------------------------------------
DEBUG=-Dno_password_check 
#-DDBsubst -DDBprereq -DDBcoreq -DDBconcpreq
#-DBperformance -DBunits -DBsimplify -DBpriority

#---------------------------------------------------------------
# Fortran compiler and flags
#---------------------------------------------------------------
FFLAGS=-Wunused -ffree-form
#-fbounds-check
OPTIONS = -D$(OS) -D$(RAWDATA) $(DEBUG) 
FC = gfortran $(FFLAGS) $(OPTIONS)

#---------------------------------------------------------------
# object files
#---------------------------------------------------------------

OBJ = UTILITIES.o UNIVERSITY.o INITIALIZE.o Input$(RAWDATA).o XMLIO.o HTML.o EditUNIVERSITY.o \
	DisplayROOMS.o DisplayTEACHERS.o DisplaySUBJECTS.o DisplayCURRICULA.o DisplaySECTIONS.o DisplayBLOCKS.o \
	EditROOMS.o EditTEACHERS.o EditSUBJECTS.o EditCURRICULA.o EditSECTIONS.o EditBLOCKS.o SERVER.o

#---------------------------------------------------------------
# targets
#---------------------------------------------------------------
all:	INTERACTIVE

INTERACTIVE:	$(OBJ)
	$(FC) $(OBJ) -o $(BIN)/HEEDS_SERVER -lfcgi -Wl,--rpath -Wl,/usr/local/lib

UTILITIES.o:	Makefile

UNIVERSITY.o:	UTILITIES.o

InputCUSTOM.o:	UNIVERSITY.o

XMLIO.o:	UNIVERSITY.o  Input$(RAWDATA).o

INITIALIZE.o:	XMLIO.o

HTML.o:	XMLIO.o

EditUNIVERSITY.o:	HTML.o

DisplayROOMS.o:	HTML.o

EditROOMS.o:	DisplayROOMS.o

DisplayTEACHERS.o:	HTML.o

EditTEACHERS.o:	DisplayTEACHERS.o

DisplaySUBJECTS.o:	HTML.o

EditSUBJECTS.o:	DisplaySUBJECTS.o

DisplayCURRICULA.o:	HTML.o

EditCURRICULA.o:	DisplayCURRICULA.o

DisplaySECTIONS.o:	HTML.o

EditSECTIONS.o:	DisplaySECTIONS.o

DisplayBLOCKS.o:	HTML.o

EditBLOCKS.o:	DisplayBLOCKS.o

.F90.o:
	$(FC) -c $<

%.o:	%.mod

.SUFFIXES:	 .F90

clean:
	rm -f *.mod *.o *~ 
