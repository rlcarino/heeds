#======================================================================
#
#    HEEDS (Higher Education Enrollment Decision Support) - A program
#      to create enrollment scenarios for 'next term' in a university
#    Copyright (C) 2012 Ricolindo L Carino
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

#---------------------------------------------------------------
# OS dependent symbols 
#---------------------------------------------------------------

#OS = WINDOWS
#DESTDIR = /C/HEEDS/bin
#EXT = WIN.EXE
#DELCMD = del
#URLENCODE = -DDO_NOT_ENCODE

OS = GNULINUX
DESTDIR = /home/heeds/HEEDS/bin
EXT = GLNX
DELCMD = rm -f
URLENCODE = -DDO_NOT_ENCODE

#---------------------------------------------------------------
# raw data format & debugging flags 
#---------------------------------------------------------------
RAWDATA = CUSTOM
DEBUG = 
#-DDBsubst 
#-DDBprereq -DDBcoreq -DDBconcpreq

#---------------------------------------------------------------
# Fortran compiler and flags
#---------------------------------------------------------------
FFLAGS = -ffree-form -fbounds-check
OPTIONS = $(DEBUG) $(URLENCODE) -D$(OS) -I$(RAWDATA)

FC = gfortran -Wunused $(FFLAGS) $(OPTIONS) 
#FC = g95 -Wunused-vars -ftrace=frame -ftrace=full $(FFLAGS) $(OPTIONS) 

#---------------------------------------------------------------
# object files
#---------------------------------------------------------------

COMMON = BASE.o XML.o TIMES.o CGI.o COLLEGES.o DEPARTMENTS.o \
	ROOMS.o TEACHERS.o SUBJECTS.o SECTIONS.o \
	TIMETABLES.o CURRICULA.o STUDENTS.o GRADES.o \
	CHECKLISTS.o PRE_ENLISTMENT.o BLOCKS.o 

INTERACTIVE = USERFUNCTIONS.o HTML.o \
	EditSUBJECTS.o EditSECTIONS.o EditBLOCKS.o EditCURRICULA.o \
	EditROOMS.o EditTEACHERS.o EditENLISTMENT.o \
	REPORTS.o DEMAND.o \
	SERVER.o MAIN.o

#---------------------------------------------------------------
# targets
#---------------------------------------------------------------

all:	HEEDS

help:
	echo 'Usage: make HEEDS RAWDATA=data_format_code OS=GNULINUX|WINDOWS'

HEEDS:	$(COMMON) $(INTERACTIVE)
	$(FC) $(COMMON) $(INTERACTIVE) -o $(DESTDIR)/HEEDS-$(EXT)

BASE.o:	Makefile

CGI.o:	BASE.o

XML.o:	BASE.o

TIMES.o:	BASE.o

COLLEGES.o:	BASE.o  $(RAWDATA)/custom_read_colleges.F90

DEPARTMENTS.o:	COLLEGES.o $(RAWDATA)/custom_read_departments.F90

ROOMS.o:	DEPARTMENTS.o  $(RAWDATA)/custom_read_rooms.F90

TEACHERS.o:	DEPARTMENTS.o  $(RAWDATA)/custom_read_teachers.F90

SUBJECTS.o:	DEPARTMENTS.o TIMES.o $(RAWDATA)/custom_read_subjects.F90

CURRICULA.o:	SUBJECTS.o  $(RAWDATA)/custom_read_curricula.F90

SECTIONS.o:	SUBJECTS.o ROOMS.o TEACHERS.o  $(RAWDATA)/custom_read_classes.F90

TIMETABLES.o:	SECTIONS.o

STUDENTS.o:	CURRICULA.o  $(RAWDATA)/custom_read_students.F90

PRE_ENLISTMENT.o:	SECTIONS.o STUDENTS.o GRADES.o

BLOCKS.o:	SECTIONS.o CURRICULA.o 

CHECKLISTS.o:	PRE_ENLISTMENT.o $(RAWDATA)/custom_checklists.F90

USERFUNCTIONS.o:	CHECKLISTS.o CGI.o TIMETABLES.o BLOCKS.o

HTML.o:	USERFUNCTIONS.o 

EditSUBJECTS.o:	HTML.o

EditSECTIONS.o:	HTML.o

EditBLOCKS.o:	HTML.o

EditCURRICULA.o:	HTML.o

EditENLISTMENT.o:	HTML.o

DEMAND.o:	HTML.o $(RAWDATA)/custom_demand.F90

REPORTS.o:	HTML.o $(RAWDATA)/custom_reports.F90

SERVER.o:	EditSUBJECTS.o EditSECTIONS.o EditBLOCKS.o EditCURRICULA.o \
	EditENLISTMENT.o REPORTS.o  DEMAND.o

MAIN.o:	SERVER.o

.F90.o:
	$(FC) -c $<

%.o:	%.mod

.SUFFIXES:	 .F90

clean:
	$(DELCMD) *.mod *.o *~ *.exe

