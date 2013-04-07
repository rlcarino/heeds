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
# raw data format (RAWDATA) and operating system (OS)
#---------------------------------------------------------------
RAWDATA = SIAS
#UPLB
#SIAS
OS = MSWIN
#GLNX
#MSWIN

#---------------------------------------------------------------
# debugging flags 
#---------------------------------------------------------------
DEBUG = -DPRODUCTION
#-DPRODUCTION
#-DDBsubst 
#-DDBprereq 
#-DDBcoreq 
#-DDBconcpreq

#---------------------------------------------------------------
# Fortran compiler and flags
#---------------------------------------------------------------
FFLAGS = -Wunused -ffree-form
#-fbounds-check
OPTIONS =-D$(OS) -D$(RAWDATA) -I$(RAWDATA) $(DEBUG) 
FC = gfortran $(FFLAGS) $(OPTIONS)

#---------------------------------------------------------------
# object files
#---------------------------------------------------------------

COMMON = BASE.o XML.o TIMES.o CGI.o UNIVERSITY.o COLLEGES.o DEPARTMENTS.o \
	ROOMS.o TEACHERS.o SUBJECTS.o SECTIONS.o \
	TIMETABLES.o CURRICULA.o STUDENTS.o GRADES.o \
	CHECKLISTS.o PRE_ENLISTMENT.o WAIVERS.o BLOCKS.o \
	ADVISING.o 
#	SCHEDULING.o

INTERACTIVE = HTML.o \
	EditSUBJECTS.o EditSECTIONS.o EditBLOCKS.o EditCURRICULA.o \
	EditROOMS.o EditTEACHERS.o EditPREDICTIONS.o EditENLISTMENT.o \
	REPORTS.o DEMAND.o \
	WEBSERVER.o MAIN.o


#---------------------------------------------------------------
# targets
#---------------------------------------------------------------

help:
	echo 'Usage: make HEEDS OS=MSWIN|GLNX RAWDATA=SIAS|UPLB'

# default target
all:	$(RAWDATA)_$(OS)

# MS Windows
$(RAWDATA)_MSWIN:
	make HEEDS OS=MSWIN BIN=/C/HEEDS/bin

# GNU/Linux
$(RAWDATA)_GLNX:
	make HEEDS OS=GLNX BIN=$(HOME)/HEEDS/bin

#	
HEEDS:	$(COMMON) $(INTERACTIVE)
	$(FC) $(COMMON) $(INTERACTIVE) -o $(BIN)/HEEDS_$(OS)_$(RAWDATA) -lfcgi -Wl,--rpath -Wl,/usr/local/lib

BASE.o:	Makefile

CGI.o:	BASE.o

XML.o:	BASE.o

TIMES.o:	BASE.o

UNIVERSITY.o:	XML.o

COLLEGES.o:	UNIVERSITY.o  $(RAWDATA)/custom_read_colleges.F90

DEPARTMENTS.o:	COLLEGES.o $(RAWDATA)/custom_read_departments.F90

ROOMS.o:	DEPARTMENTS.o  $(RAWDATA)/custom_read_rooms.F90

TEACHERS.o:	DEPARTMENTS.o  $(RAWDATA)/custom_read_teachers.F90

SUBJECTS.o:	DEPARTMENTS.o TIMES.o $(RAWDATA)/custom_read_subjects.F90

CURRICULA.o:	SUBJECTS.o  $(RAWDATA)/custom_read_curricula.F90

SECTIONS.o:	SUBJECTS.o ROOMS.o TEACHERS.o  $(RAWDATA)/custom_read_classes.F90

TIMETABLES.o:	SECTIONS.o

STUDENTS.o:	CURRICULA.o  $(RAWDATA)/custom_read_students.F90

PRE_ENLISTMENT.o:	SECTIONS.o STUDENTS.o GRADES.o

WAIVERS.o:	STUDENTS.o SECTIONS.o

BLOCKS.o:	SECTIONS.o CURRICULA.o 

CHECKLISTS.o:	PRE_ENLISTMENT.o $(RAWDATA)/custom_checklists.F90

ADVISING.o:	WAIVERS.o CHECKLISTS.o CGI.o $(RAWDATA)/custom_advising.F90

SCHEDULING.o:	ADVISING.o TIMETABLES.o

HTML.o:	ADVISING.o TIMETABLES.o BLOCKS.o

EditSUBJECTS.o:	HTML.o

EditSECTIONS.o:	HTML.o

EditBLOCKS.o:	HTML.o

EditCURRICULA.o:	HTML.o

EditPREDICTIONS.o:	HTML.o

EditENLISTMENT.o:	HTML.o

DEMAND.o:	HTML.o $(RAWDATA)/custom_demand.F90

REPORTS.o:	HTML.o $(RAWDATA)/custom_reports.F90

WEBSERVER.o:	EditSUBJECTS.o EditSECTIONS.o EditBLOCKS.o EditCURRICULA.o \
	EditPREDICTIONS.o EditENLISTMENT.o \
	REPORTS.o  DEMAND.o 

MAIN.o:	WEBSERVER.o

.F90.o:
	$(FC) -c $<

%.o:	%.mod

.SUFFIXES:	 .F90

clean:
	rm -f *.mod *.o *~ 
