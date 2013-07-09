!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012, 2013 Ricolindo L. Carino
!
!    This file is part of the HEEDS program.
!
!    HEEDS is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    HEEDS is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program (see the file COPYING.TXT). If not, see
!    <http://www.gnu.org/licenses/>.
!
!    Send inquiries about HEEDS to:
!      Ricolindo L Carino
!      E-mail: Ricolindo.Carino@AcademicForecasts.com
!      Address: 600 Cruise St., Starkville, MS 39759, U.S.A.
!
!======================================================================


module EditROOMS

    use DisplayROOMS

    implicit none

contains

    subroutine room_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_ROOM_CODE) :: tRoom, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, rdx, j
        character (len=255) :: mesg, remark
        type (TYPE_ROOM) :: wrk
        logical :: isDirtyROOMS

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tRoom, rdx)
        if (rdx/=0) tRoom = 'TBA'

        call html_comment('room_edit('//trim(tRoom)//') ')
        rdx = index_to_room(tRoom)
        targetDepartment = Room(rdx)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        wrk = Room(rdx) ! make a working copy
        isDirtyROOMS = .false.
        remark = SPACE
        mesg = SPACE

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info

            if (trim(tRoom)=='TBA') then
                mesg = 'Add new room'
                tAction = 'Add'
            else
                mesg = 'Edit info for room '//tRoom
                tAction = 'Update'
            end if

            call room_info(device, wrk, mesg, remark, tAction)

        else ! action is Add or Update; collect changes


            call cgi_get_named_integer(QUERY_STRING, 'MaxCapacity', wrk%MaxCapacity, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxCapacity=', wrk%MaxCapacity
            if (ierr/=0) wrk%MaxCapacity = Room(rdx)%MaxCapacity

            call cgi_get_named_integer(QUERY_STRING, 'Cluster', wrk%Cluster, ierr)
            !write(*,*) 'ierr=', ierr, ', Cluster=', wrk%Cluster
            if (ierr/=0) wrk%Cluster = Room(rdx)%Cluster

            call cgi_get_named_string(QUERY_STRING, 'Code', mesg, ierr)
            wrk%Code = trim(mesg)
            !write(*,*) 'ierr=', ierr, ', Code=', wrk%Code
            if (ierr/=0) wrk%Code = Room(rdx)%Code

            call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
            wrk%DeptIdx = index_to_dept(tDepartment)
            !write(*,*) 'ierr=', ierr, ', DeptIdx=', wrk%DeptIdx
            if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Room(rdx)%DeptIdx

            if (wrk%Code /= Room(rdx)%Code) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Code changed to '//wrk%Code
            end if

            if (wrk%DeptIdx /= Room(rdx)%DeptIdx) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Department changed to '//Department(wrk%DeptIdx)%Code
            end if

            if ( wrk%MaxCapacity /= Room(rdx)%MaxCapacity) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Max seating capacity changed to '//itoa(wrk%MaxCapacity)
            end if

            if ( wrk%Cluster /= Room(rdx)%Cluster) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Cluster changed to '//itoa(wrk%Cluster)
            end if

            if (isDirtyROOMS) then ! some changes

                if (wrk%Code /= Room(rdx)%Code) then  ! new code; check if room already exists

                    j = index_to_room(wrk%Code)
                    if (j==0) then ! not used

                        if (trim(tAction)=='Add') then
                            NumAdditionalRooms = NumAdditionalRooms+1
                            Room(NumRooms+NumAdditionalRooms) = wrk
                            rdx = NumRooms+NumAdditionalRooms
                            tRoom = wrk%Code
                            remark = ': Added new room '//wrk%Code
                        else
                            ! update existing
                            Room(rdx) = wrk
                        end if

                    else

                        remark = ': Add/edit room failed; "'//trim(wrk%Code)//'" already exists.'
                        isDirtyROOMS = .false.

                    end if
                else
                    ! same code; update other fields
                    Room(rdx) = wrk
                end if

                if (isDirtyROOMS) call xml_write_rooms(trim(pathToYear)//'ROOMS.XML')

            else ! Add or Update clicked, but no changes made
                remark = ': No changes made?'

            end if

            call html_college_links(device, Department(wrk%DeptIdx)%CollegeIdx, &
                trim(tRoom)//remark)

        end if

    end subroutine room_edit


end module EditROOMS
