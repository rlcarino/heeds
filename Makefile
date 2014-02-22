#======================================================================
#
#    HEEDS (Higher Education Enrollment Decision Support) - A program
#      to create enrollment scenarios for 'next term' in a university
#    Copyright (C) 2012-2014 Ricolindo L. Carino
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
RAWDATA=SIAS

#---------------------------------------------------------------
# operating system (OS)
# directory for library (LIBPATH)
# directory for binary (BINPATH)
#---------------------------------------------------------------

#OS=GLNX
#LIBPATH=-L/usr/lib -lfcgi
#BINPATH=.

OS=MSWIN
LIBPATH=-lfcgi -Wl,--rpath -Wl,/usr/local/lib
BINPATH=/c/HEEDS/bin/$(OS)

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

BASE = UTILITIES.o UNIVERSITY.o INITIALIZE.o XMLIO.o

WEB = $(BASE) HTML.o ADVISING.o EditUNIVERSITY.o EditTEACHERS.o EditSECTIONS.o EditBLOCKS.o \
	EditSTUDENT.o EditCHECKLIST.o EditENLISTMENT.o 
#TIMETABLING.o

#---------------------------------------------------------------
# targets
#---------------------------------------------------------------
#all:	INTERACTIVE 
all:	BATCH INTERACTIVE 

INTERACTIVE:	$(WEB) SERVER.o 
	$(FC) $(WEB) SERVER.o  -o $(BINPATH)/HEEDS_SERVER $(LIBPATH)

BATCH:	$(BASE) STATIC.o
	$(FC) $(BASE) STATIC.o  -o $(BINPATH)/HEEDS_STATIC $(LIBPATH)

STATIC.o:	$(BASE) Input$(RAWDATA).F90

#UTILITIES.o:	Makefile

UNIVERSITY.o:	UTILITIES.o

INITIALIZE.o:	UNIVERSITY.o

XMLIO.o:	UNIVERSITY.o XMLIO-OTHER.F90

HTML.o:	$(BASE)

EditUNIVERSITY.o:	HTML.o

EditROOMS.o:	HTML.o

EditTEACHERS.o:	HTML.o

EditSUBJECTS.o:	HTML.o

EditCURRICULA.o:	HTML.o

EditSECTIONS.o:	HTML.o

EditBLOCKS.o:	HTML.o

ADVISING.o:	HTML.o

EditSTUDENT.o:	ADVISING.o

EditCHECKLIST.o:	HTML.o

EditENLISTMENT.o:	HTML.o

TIMETABLING.o:	HTML.o

SERVER.o:	$(WEB)

.F90.o:
	$(FC) -c $<

%.o:	%.mod

.SUFFIXES:	 .F90

clean:
	rm -f *.mod *.o *~ 
