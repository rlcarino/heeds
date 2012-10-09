!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012 Ricolindo L Carino
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


subroutine custom_read_rooms(path, errNo)

    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    character (len=MAX_LEN_ROOM_CODE) :: tCode
    integer :: i, j, k, locx, seats
    character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

    fileName = trim(dirRAW)//trim(path)//'ROOMS.CSV'
    open (unit=unitNum, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return
    call file_log_message('Retrieving room codes from '//fileName)

    ! skip first line
    read(unitNum, AFORMAT) line

    do
        read(unitNum, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (line==SPACE .or. line(1:1)=='#') cycle

        call index_to_delimiters('"', line, ndels, pos)
        !id,code,name,type
        !8,"A101","A101","[CED]"
        !9,"A102","A102","[CED]"
        !10,"A103","A103","[CED]"
        !1  2    3 4    5 6     7

        tCode = line(pos(2)+1:pos(3)-1)
        call upper_case(tCode)
        j = index_to_room(tCode)
        if (j>0) cycle ! already encountered

        tDepartment = line(pos(6)+2:pos(7)-2)
        k = index_to_dept(tDepartment)
        if (k==0) k = NumDepartments ! refers to REGISTRAR
        Department(k)%hasInfo = .true.

        locx = 0 ! all rooms are close to each other?

        seats = 50 ! all rooms have the same capacity?

        call check_array_bound (NumRooms+NumAdditionalRooms+1, MAX_ALL_ROOMS, 'MAX_ALL_ROOMS')
        do i=NumRooms,1,-1
            if (tCode<Room(i)%Code) then
                Room(i+1) = Room(i)
            else
                j = i
                exit
            end if
        end do
        call initialize_room (Room(j+1), tCode, k, locx, seats)
        NumRooms = NumRooms + 1

    end do

    return
end subroutine custom_read_rooms

