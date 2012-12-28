@echo off
rem ======================================================================
rem 
rem     HEEDS (Higher Education Enrollment Decision Support) - A program
rem       to create enrollment scenarios for 'next term' in a university
rem     Copyright (C) 2012 Ricolindo L Carino
rem 
rem     This file is part of the HEEDS program.
rem 
rem     HEEDS is free software: you can redistribute it and/or modify
rem     it under the terms of the GNU General Public License as published by
rem     the Free Software Foundation, either version 3 of the License, or
rem     (at your option) any later version.
rem 
rem     HEEDS is distributed in the hope that it will be useful,
rem     but WITHOUT ANY WARRANTY; without even the implied warranty of
rem     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem     GNU General Public License for more details.
rem 
rem     You should have received a copy of the GNU General Public License
rem     along with this program (see the file COPYING.TXT). If not, see
rem     <http://www.gnu.org/licenses/>.
rem 
rem     Send inquiries about HEEDS to:
rem       Ricolindo L Carino
rem       E-mail: Ricolindo.Carino@AcademicForecasts.com
rem       Address: 600 Cruise St., Starkville, MS 39759, U.S.A.
rem 
rem ======================================================================

echo g95 -v -o C:\HEEDS\bin\HEEDS-GROUP-MSWIN-APACHE-PHP.EXE -Wunused-vars -ftrace=frame -ftrace=full -ffree-form -fbounds-check -DDO_NOT_ENCODE -DMSWIN -DGROUP -DAPACHE -DPHP -DCUSTOM -ICUSTOM BASE.F90 XML.F90 TIMES.F90 CGI.F90 COLLEGES.F90 DEPARTMENTS.F90 ROOMS.F90 TEACHERS.F90 SUBJECTS.F90 SECTIONS.F90 TIMETABLES.F90 CURRICULA.F90 STUDENTS.F90 GRADES.F90 PRE_ENLISTMENT.F90 CHECKLISTS.F90 WAIVERS.F90 BLOCKS.F90 ADVISING.F90 USERFUNCTIONS.F90 HTML.F90 EditSUBJECTS.F90 EditSECTIONS.F90 EditBLOCKS.F90 EditCURRICULA.F90 EditROOMS.F90 EditTEACHERS.F90 EditPREDICTIONS.F90 EditENLISTMENT.F90 REPORTS.F90 DEMAND.F90 SERVER.F90 SCHEDULING.F90 MAIN.F90 

g95 -v -o C:\HEEDS\bin\HEEDS-GROUP-MSWIN-APACHE-PHP.EXE -Wunused-vars -ftrace=frame -ftrace=full -ffree-form -fbounds-check -DDO_NOT_ENCODE -DMSWIN -DGROUP -DAPACHE -DPHP -DCUSTOM -ICUSTOM BASE.F90 XML.F90 TIMES.F90 CGI.F90 COLLEGES.F90 DEPARTMENTS.F90 ROOMS.F90 TEACHERS.F90 SUBJECTS.F90 SECTIONS.F90 TIMETABLES.F90 CURRICULA.F90 STUDENTS.F90 GRADES.F90 PRE_ENLISTMENT.F90 CHECKLISTS.F90 WAIVERS.F90 BLOCKS.F90 ADVISING.F90 USERFUNCTIONS.F90 HTML.F90 EditSUBJECTS.F90 EditSECTIONS.F90 EditBLOCKS.F90 EditCURRICULA.F90 EditROOMS.F90 EditTEACHERS.F90 EditPREDICTIONS.F90 EditENLISTMENT.F90 REPORTS.F90 DEMAND.F90 SERVER.F90 SCHEDULING.F90 MAIN.F90 

pause
