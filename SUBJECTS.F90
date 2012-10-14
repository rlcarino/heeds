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


module SUBJECTS

    use DEPARTMENTS
    use TIMES

    implicit none

    ! subject variables
    integer, parameter :: &
    MAX_ALL_SUBJECTS = 4500, & ! max no. of subjects
    MAX_LEN_SUBJECT_CODE = 20, &         ! length of subject names
    MAX_LEN_SUBJECT_TITLE=100, &         ! length of subject titles
    MAX_ALL_SUBJECT_PREREQ = 50, &         ! max number of tokens in prereq of a subject
    MAX_ALL_SUBJECT_COREQ = 5, &         ! max number of tokens in corequisite of a subject
    MAX_ALL_SUBJECT_CONCURRENT = 5, &       ! max number of tokens in concurrencies
    MAX_ALL_SUBJECT_CONCPREQ = 5, &      ! max number of tokens in concurrent prereq of a subject
    MAX_ALL_DUMMY_SUBJECTS = -50            ! max no. of dummy subjects

    type :: TYPE_SUBJECT
        character (len=MAX_LEN_SUBJECT_CODE) :: Name
        character (len=MAX_LEN_SUBJECT_TITLE) :: Title
        integer :: DeptIdx
        real :: Units
        integer :: TermOffered
        real :: Tuition, LabFee, LectHours, LectLoad
        integer :: MinLectSize, MaxLectSize
        real :: LabHours, LabLoad
        integer :: MaxLabSize, MinLabSize, &
            lenPreq, Prerequisite(MAX_ALL_SUBJECT_PREREQ), &
            lenCoreq, Corequisite(MAX_ALL_SUBJECT_COREQ), &
            lenConc, Concurrent(MAX_ALL_SUBJECT_CONCURRENT), &
            lenConcPreq, ConcPrerequisite(MAX_ALL_SUBJECT_CONCPREQ)
    end type TYPE_SUBJECT

    type (TYPE_SUBJECT), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Subject
    integer :: NumSubjects, NumDummySubjects, NumAdditionalSubjects
    integer :: INDEX_TO_NONE

    real, dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS,0:2) :: FailRate

    integer, dimension (0:MAX_ALL_SUBJECTS) :: SubjectRank

    ! subject areas
    type :: TYPE_SUBJECT_AREA
        character (len=MAX_LEN_SUBJECT_CODE) :: Code
        integer :: CollegeIdx
    end type TYPE_SUBJECT_AREA
    type (TYPE_SUBJECT_AREA), dimension (0:MAX_ALL_SUBJECTS/3) :: SubjectArea
    integer :: NumSubjectAreas

    ! substitutions/equivalencies
    integer, parameter :: &
        MaxAllSubstitutions = 700, &                ! max no. of substitutions
        MaxSubsArraySize = MaxAllSubstitutions*10   ! max size of substitutions array
    integer :: NumSubst
    integer, dimension(MaxAllSubstitutions) :: SubstIdx
    integer, dimension(MaxSubsArraySize) :: Substitution

    ! renamed subjects
    integer, parameter :: MAX_ALL_RENAMES = 500
    integer :: NumRenames
    integer, dimension(MAX_ALL_RENAMES,2) :: Renamed

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line
    integer, private :: unitNo=2, eof, ndels, pos(50)


contains

#include "custom_read_subjects.F90"

    subroutine initialize_subject (wrkSubject)

        type(TYPE_SUBJECT), intent (out) :: wrkSubject

        wrkSubject = TYPE_SUBJECT ( &
            SPACE, & ! character (len=MAX_LEN_SUBJECT_CODE) :: Name
            SPACE, & ! character (len=MAX_LEN_SUBJECT_TITLE) :: Title
            NumDepartments, & ! integer :: DeptIdx
            0, & ! integer :: Units
            0, & ! integer :: TermOffered
            0.0, 0.0, & ! real :: Tuition, LabFee
            0.0, 0.0, & ! real :: LectHours, LectLoad
            0, 0, & ! integer :: MinLectSize, MaxLectSize
            0.0, 0.0, & ! real :: LabHours, LabLoad
            0, 0, & ! integer :: MaxLabSize, MinLabSize, &
            1, INDEX_TO_NONE, & !     lenPreq, Prerequisite(MAX_ALL_SUBJECT_PREREQ), &
            1, INDEX_TO_NONE, & !     lenCoreq, Corequisite(MAX_ALL_SUBJECT_COREQ), &
            1, INDEX_TO_NONE, & !     lenConc, Concurrent(MAX_ALL_SUBJECT_CONCURRENT), &
            1, INDEX_TO_NONE) !     lenConcPreq, ConcPrerequisite(MAX_ALL_SUBJECT_CONCPREQ)

        return
    end subroutine initialize_subject


    subroutine xml_write_subjects(path)

        character(len=*), intent(in) :: path

        integer :: subj, i, j
        character(len=255) :: mesg1, mesg2, mesg3, mesg4

        ! training only?
        if (noWrites) return

        ! basic information about subjects
        fileName = trim(dirXML)//trim(path)//'SUBJECTS.XML'
        call xml_open_file(unitNo, XML_ROOT_SUBJECTS, fileName, i)

        write(unitNo,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '        Code - subject identifier', &
        '        Title - subject title', &
        '        Department - responsible department', &
        '        Units - credit to the student', &
        '        TermOffered - 1 for first semester, 2 for second semester, S for summer; or combinations of 1, 2, and S', &
        '        LectHours - no. of lecture hours; 0 if the subject is lab-only', &
        '        MinLectSize - minimum size to open lecture class; 0 if the subject is lab-only', &
        '        MaxLectSize - maximum size to create another lecture class; 0 if the subject is lab-only', &
        '        LabHours - laboratory/recitation/computation class hours; 0 if the subject is lecture-only', &
        '        MinLabSize - minimum size to open lab class; 0 if the subject is lecture-only', &
        '        MaxLabSize - maximum size to create another lab class; 0 if the subject is lecture-only', &
        '    </comment>'

        do subj=NumDummySubjects,NumSubjects+NumAdditionalSubjects
            if (subj==-1 .or. subj==0) cycle

            call xml_write_character(unitNo, indent0, 'Subject')
            call xml_write_character(unitNo, indent1, 'Code', Subject(subj)%Name)
            call xml_write_character(unitNo, indent1, 'Title', Subject(subj)%Title)
            call xml_write_character(unitNo, indent1, 'Department', Department(Subject(subj)%DeptIdx)%Code)
            call xml_write_character(unitNo, indent1, 'TermOffered', text_term_offered(Subject(subj)%TermOffered))
            if (Subject(subj)%Units/=0.0) call xml_write_float(unitNo, indent1, 'Units', Subject(subj)%Units,2)
            if (Subject(subj)%LectHours/=0.0) call xml_write_float(unitNo, indent1, 'LectHours', Subject(subj)%LectHours,2)
            if (Subject(subj)%MinLectSize/=0) call xml_write_integer(unitNo, indent1, 'MinLectSize', Subject(subj)%MinLectSize)
            if (Subject(subj)%MaxLectSize/=0) call xml_write_integer(unitNo, indent1, 'MaxLectSize', Subject(subj)%MaxLectSize)
            if (Subject(subj)%LabHours/=0.0) call xml_write_float(unitNo, indent1, 'LabHours', Subject(subj)%LabHours,2)
            if (Subject(subj)%MinLabSize/=0) call xml_write_integer(unitNo, indent1, 'MinLabSize', Subject(subj)%MinLabSize)
            if (Subject(subj)%MaxLabSize/=0) call xml_write_integer(unitNo, indent1, 'MaxLabSize', Subject(subj)%MaxLabSize)
            call xml_write_character(unitNo, indent0, '/Subject')

        end do

        call xml_close_file(unitNo, XML_ROOT_SUBJECTS)

        ! additional information
        fileName = trim(dirXML)//trim(path)//'SUBJECTS-OTHER.XML'
        call xml_open_file(unitNo, XML_ROOT_SUBJECTS, fileName, i)

        write(unitNo,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '        Code - subject identifier', &
        '        DisplayUnits - how the number of units will be displayed (i.e., 3, 2.0, (1.5) )', &
        '        Tuition - how much a student will pay if he takes the subject', &
        '        LabFee - how much (in addn to Tuition) a student will pay for laboratory fee', &
        '        LectLoad - workload credit to teacher of lecture class', &
        '        LabLoad - workload credit to teacher of laboratory/recitation/computation class', &
        '        Prerequisite - subject prerequisite in prefix format, tokens being separated by "+"', &
        '          possible values are', &
        '            1. NONE', &
        '            2. COI', &
        '            3. Student classification (FRESHMAN, SOPHOMORE, JUNIOR, SENIOR)', &
        '            4. year level in curriculum (FIRST,SECOND,THIRD,FOURTH,FIFTH,GRADUATING)', &
        '            5. subject code - another subject', &
        '            6. OR+2+5, OR+3+5, OR+4+5', &
        '            7. OR+5+5, AND+5+5', &
        '            8. OR+6+6, OR+6+7, OR+7+7, AND+7+7', &
        '        Corequisite - subject co-requisite', &
        '        Concurrent - subject to be concurrently registered', &
        '        ConcurrentPrerequisite - prerequisite that can be registered concurrently', &
        '    </comment>'

        do subj=NumDummySubjects,NumSubjects+NumAdditionalSubjects
            if (subj==-1 .or. subj==0) cycle
            ! prerequisite
            i = Subject(subj)%Prerequisite(1)
            mesg1 = Subject(i)%Name
            do j=2,Subject(subj)%lenPreq
                i = Subject(subj)%Prerequisite(j)
                mesg1 = trim(mesg1)//'+'//Subject(i)%Name
            end do
            ! corequisite
            i = Subject(subj)%Corequisite(1)
            mesg2 = Subject(i)%Name
            do j=2,Subject(subj)%lenCoreq
                i = Subject(subj)%Corequisite(j)
                mesg2 = trim(mesg2)//'+'//Subject(i)%Name
            end do
            ! concurrent
            i = Subject(subj)%Concurrent(1)
            mesg3 = Subject(i)%Name
            do j=2,Subject(subj)%lenConc
                i = Subject(subj)%Concurrent(j)
                mesg3 = trim(mesg3)//'+'//Subject(i)%Name
            end do
            ! prerequisite that can be taken concurrently
            i = Subject(subj)%ConcPrerequisite(1)
            mesg4 = Subject(i)%Name
            do j=2,Subject(subj)%lenConcPreq
                i = Subject(subj)%ConcPrerequisite(j)
                mesg4 = trim(mesg4)//'+'//Subject(i)%Name
            end do

            call xml_write_character(unitNo, indent0, 'Subject')
            call xml_write_character(unitNo, indent1, 'Code', Subject(subj)%Name)
            if (Subject(subj)%Units>0.0) call xml_write_character(unitNo, indent1, 'DisplayUnits', ftoa(Subject(subj)%Units,2))
            if (Subject(subj)%Tuition/=0.0) call xml_write_float(unitNo, indent1, 'Tuition', Subject(subj)%Tuition,2)
            if (Subject(subj)%LabFee/=0.0) call xml_write_float(unitNo, indent1, 'LabFee', Subject(subj)%LabFee,2)
            if (Subject(subj)%LectLoad/=0.0) call xml_write_float(unitNo, indent1, 'LectLoad', Subject(subj)%LectLoad,2)
            if (Subject(subj)%LabLoad/=0.0) call xml_write_float(unitNo, indent1, 'LabLoad', Subject(subj)%LabLoad,2)
            if (trim(mesg1)/='NONE') call xml_write_character(unitNo, indent1, 'Prerequisite', mesg1)
            if (trim(mesg2)/='NONE') call xml_write_character(unitNo, indent1, 'Corequisite', mesg2)
            if (trim(mesg3)/='NONE') call xml_write_character(unitNo, indent1, 'ConcurrentWith', mesg3)
            if (trim(mesg4)/='NONE') call xml_write_character(unitNo, indent1, 'ConcurrentPrerequisite', mesg4)
            call xml_write_character(unitNo, indent0, '/Subject')

        end do

        call xml_close_file(unitNo, XML_ROOT_SUBJECTS)

        return
    end subroutine xml_write_subjects


    subroutine read_subjects(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        character (len=MAX_LEN_SUBJECT_CODE) :: token
        integer :: i, j
        logical :: noXML

        NumDummySubjects = -1
        NumSubjects = 0
        NumAdditionalSubjects = 0
        NumRenames = 0
        Renamed = 0
        Failrate = 0.0

        noXML = .false.
        call initialize_subject (Subject(0))
        Subject = Subject(0)
        Subject(-1)%Name = '(dummy)'
        Subject(-1)%Title = '(dummy)'
        INDEX_TO_NONE = -1 ! fix after reading in all subject codes

        call xml_read_subjects(path, errNo) ! try the XML file
        if (errNo/=0) then ! something wrong with XML file
            noXML = .true.
            call  custom_read_subjects(path, errNo) ! try custom format
            if (errNo/=0) return ! something still wrong
        end if

        ! read additional info
        call xml_read_subjects_other(path, errNo)
        if (errNo/=0) then
            call custom_read_subjects_prerequisites(path, errNo)
            noXML = errNo==0
            call SIAS_read_assessment(path, errNo)
            errNo = 0
        end if

        ! write the XML subjects file?
        if (noXML) call xml_write_subjects(path)

        ! subject areas
        NumSubjectAreas = 0
        SubjectArea = TYPE_SUBJECT_AREA (SPACE, 0)
        do i=1,NumSubjects
            token = Subject(i)%Name
            j = index(token, SPACE)
            token(j:) = SPACE
            if (SubjectArea(NumSubjectAreas)%Code/=token) then
                call check_array_bound (NumSubjectAreas+1, MAX_ALL_SUBJECTS/3, 'NumSubjectAreas')
                NumSubjectAreas = NumSubjectAreas+1
                SubjectArea(NumSubjectAreas) = TYPE_SUBJECT_AREA(token, Department(Subject(i)%DeptIdx)%CollegeIdx)
            end if
        end do

        ! fail rates
        call xml_read_failrates(path, eof) ! try the XML file
        if (eof/=0) then ! try custom format
            call custom_read_failrates(path, eof)
        end if

        return
    end subroutine read_subjects


    subroutine xml_read_subjects(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        integer :: i, j
        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type(TYPE_SUBJECT) :: wrkSubject
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept
        character (len=MAX_LEN_SUBJECT_CODE) :: token

        ! open file for basic info on subjects, return on any error
        fileName = trim(dirXML)//trim(path)//'SUBJECTS.XML'
        call xml_open_file(unitNo, XML_ROOT_SUBJECTS, fileName, errNo, forReading)
        if (errNo/=0) return

        ! read subject code & responsible department
        do
            read(unitNo, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('Subject') ! initialize temporary subject data
                    call initialize_subject (wrkSubject)

                case ('Code')
                    wrkSubject%Name = adjustl(value)

                case ('Department')
                    tDept = adjustl(value)
                    j = index_to_dept(tDept)
                    if (j/=0) wrkSubject%DeptIdx = j

                case ('/Subject') ! add temporary subject data to Subject()
                    ! duplicate subject ?
                    i = index_to_subject(wrkSubject%Name)
                    if (i/=0) cycle ! duplicate

                    ! not a dummy subject
                    Department(wrkSubject%DeptIdx)%hasInfo = .true.

                    if (wrkSubject%DeptIdx>1) then
                        NumSubjects = NumSubjects + 1
                        call check_array_bound (NumSubjects, MAX_ALL_SUBJECTS, 'MAX_ALL_SUBJECTS')
                        i = NumSubjects
                        do while (wrkSubject%Name<Subject(i-1)%Name)
                            Subject(i)%Name = Subject(i-1)%Name
                            i = i-1
                        end do
                        Subject(i)%Name = wrkSubject%Name

                    elseif (wrkSubject%DeptIdx==1) then ! dummy subject
                        NumDummySubjects = NumDummySubjects-1
                        Subject(NumDummySubjects)%Name = wrkSubject%Name

                    end if

                case default
                ! do nothing

            end select

        end do
        call xml_close_file(unitNo)

        ! fix INDEX_TO_NONE
        token = 'NONE'
        INDEX_TO_NONE = index_to_subject(token)
        Subject(:)%Prerequisite(1) = INDEX_TO_NONE
        Subject(:)%Corequisite(1) = INDEX_TO_NONE
        Subject(:)%Concurrent(1) = INDEX_TO_NONE
        Subject(:)%ConcPrerequisite(1) = INDEX_TO_NONE

        ! read the rest of subject basic info
        call xml_open_file(unitNo, XML_ROOT_SUBJECTS, fileName, eof, forReading)
        do
            read(unitNo, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))
                case ('Subject') ! initialize temporary subject data
                    call initialize_subject (wrkSubject)

                case ('Code')
                    wrkSubject%Name = adjustl(value)

                case ('Title')
                    wrkSubject%Title = adjustl(value)

                case ('Department')
                    tDept = adjustl(value)
                    wrkSubject%DeptIdx = index_to_dept(tDept)

                case ('Units')
                    wrkSubject%Units = atof(value)

                case ('TermOffered')
                    wrkSubject%TermOffered = 0
                    if (index(value, '1')>0) wrkSubject%TermOffered = wrkSubject%TermOffered+1
                    if (index(value, '2')>0) wrkSubject%TermOffered = wrkSubject%TermOffered+2
                    if (index(value, 'S')>0) wrkSubject%TermOffered = wrkSubject%TermOffered+4

                case ('LectHours')
                    wrkSubject%LectHours = atof(value)

                case ('MinLectSize')
                    wrkSubject%MinLectSize = atoi(value)

                case ('MaxLectSize')
                    wrkSubject%MaxLectSize = atoi(value)

                case ('LabHours')
                    wrkSubject%LabHours = atof(value)

                case ('MinLabSize')
                    wrkSubject%MinLabSize = atoi(value)

                case ('MaxLabSize')
                    wrkSubject%MaxLabSize = atoi(value)

                ! read the additional info also, in case the file has the old format
                case ('LabFee')
                    wrkSubject%LabFee = atof(value)

                case ('Tuition')
                    wrkSubject%Tuition = atof(value)

                case ('LectLoad')
                    wrkSubject%LectLoad = atof(value)

                case ('LabLoad')
                    wrkSubject%LabLoad = atof(value)

                case ('Prerequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_PREREQ, &
                        wrkSubject%lenPreq, wrkSubject%Prerequisite, eof)
                    if (eof>0) exit

                case ('Corequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_COREQ, &
                        wrkSubject%lenCoreq, wrkSubject%Corequisite, eof)
                    if (eof>0) exit
                    if (wrkSubject%lenCoreq>0 .and. Subject(wrkSubject%Corequisite(1))%Name/='NONE') then
                        write(*,*) trim(wrkSubject%Name)//' has co-requisite ', &
                            (Subject(wrkSubject%Corequisite(j))%Name, j=1,wrkSubject%lenCoreq)
                    end if

                case ('ConcurrentWith')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_CONCURRENT, wrkSubject%lenConc, wrkSubject%Concurrent, eof)
                    if (eof>0) exit
                    if (wrkSubject%lenConc>0 .and. Subject(wrkSubject%Concurrent(1))%Name/='NONE') then
                        write(*,*) trim(wrkSubject%Name)//' is concurrent with ', &
                            (Subject(wrkSubject%Concurrent(j))%Name, j=1,wrkSubject%lenConc)
                    end if

                case ('ConcurrentPrerequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_CONCPREQ, wrkSubject%lenConcPreq, &
                    wrkSubject%ConcPrerequisite, eof)
                    if (eof>0) exit
                    if (wrkSubject%lenConcPreq>0 .and. Subject(wrkSubject%ConcPrerequisite(1))%Name/='NONE') then
                        write(*,*) trim(wrkSubject%Name)//' has pre-req that can be concurrent: ', &
                            (Subject(wrkSubject%ConcPrerequisite(j))%Name, j=1,wrkSubject%lenConcPreq)
                    end if

                case ('/Subject') ! add temporary subject data to Subject()
                    i = index_to_subject(wrkSubject%Name)
                    Subject(i) = wrkSubject

                case default
                    ! do nothing

            end select

        end do

        call xml_close_file(unitNo)

        return
    end subroutine xml_read_subjects


    subroutine xml_read_subjects_other(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        integer :: i, j
        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type(TYPE_SUBJECT) :: wrkSubject

        ! additional subject info
        errNo = 0
        fileName = trim(dirXML)//trim(path)//'SUBJECTS-OTHER.XML'
        call xml_open_file(unitNo, XML_ROOT_SUBJECTS, fileName, errNo, forReading)
        if (errNo/=0) return

        do
            read(unitNo, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))
                case ('Subject') ! initialize temporary subject data
                    ! ignore

                case ('Code') ! copy existing subject info to wrk
                    wrkSubject%Name = adjustl(value)
                    i = index_to_subject(wrkSubject%Name)
                    wrkSubject = Subject(i)

                case ('LabFee')
                    wrkSubject%LabFee = atof(value)

                case ('Tuition')
                    wrkSubject%Tuition = atof(value)

                case ('LectLoad')
                    wrkSubject%LectLoad = atof(value)

                case ('LabLoad')
                    wrkSubject%LabLoad = atof(value)

                case ('Prerequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_PREREQ, &
                        wrkSubject%lenPreq, wrkSubject%Prerequisite, eof)
                    if (eof>0) exit

                case ('Corequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_COREQ, &
                        wrkSubject%lenCoreq, wrkSubject%Corequisite, eof)
                    if (eof>0) exit
                    if (wrkSubject%lenCoreq>0 .and. Subject(wrkSubject%Corequisite(1))%Name/='NONE') then
                        write(*,*) trim(wrkSubject%Name)//' has co-requisite ', &
                            (Subject(wrkSubject%Corequisite(j))%Name, j=1,wrkSubject%lenCoreq)
                    end if

                case ('ConcurrentWith')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_CONCURRENT, wrkSubject%lenConc, wrkSubject%Concurrent, eof)
                    if (eof>0) exit
                    if (wrkSubject%lenConc>0 .and. Subject(wrkSubject%Concurrent(1))%Name/='NONE') then
                        write(*,*) trim(wrkSubject%Name)//' is concurrent with ', &
                            (Subject(wrkSubject%Concurrent(j))%Name, j=1,wrkSubject%lenConc)
                    end if

                case ('ConcurrentPrerequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_CONCPREQ, wrkSubject%lenConcPreq, &
                    wrkSubject%ConcPrerequisite, eof)
                    if (eof>0) exit
                    if (wrkSubject%lenConcPreq>0 .and. Subject(wrkSubject%ConcPrerequisite(1))%Name/='NONE') then
                        write(*,*) trim(wrkSubject%Name)//' has pre-req that can be concurrent: ', &
                            (Subject(wrkSubject%ConcPrerequisite(j))%Name, j=1,wrkSubject%lenConcPreq)
                    end if

                case ('/Subject') ! add temporary subject data to Subject()
                    i = index_to_subject(wrkSubject%Name)
                    Subject(i) = wrkSubject

                case default
                    ! do nothing

            end select

        end do

        call xml_close_file(unitNo)

        return
    end subroutine xml_read_subjects_other


    function index_to_subject(token)
        integer :: index_to_subject
        character (len=MAX_LEN_SUBJECT_CODE), intent(in) :: token
        integer :: i, j, cdx

        ! try the dummy subjects
        index_to_subject = 0
        do cdx=NumDummySubjects,-1
            if (token==Subject(cdx)%Name) then
                index_to_subject = cdx
                return
            end if
        end do

        ! try the newly added subjects
        do cdx=NumSubjects+1,NumSubjects+NumAdditionalSubjects
            if (token==Subject(cdx)%Name) then
                index_to_subject = cdx
                return
            end if
        end do

        ! the named subjects
        i = 1
        j = NumSubjects
        do
            if (i>j) then ! not found
                cdx = 0
                exit
            else
                cdx = (i + j)/2
                if (token==Subject(cdx)%Name) then
                    !           write(*,*)  '... found '//Subject(cdx)%Name
                    exit
                else if (token<Subject(cdx)%Name) then
                    j = cdx-1
                !           write(*,*)  '...<'//Subject(cdx)%Name
                else
                    i = cdx+1
                !           write(*,*)  '...>'//Subject(cdx)%Name
                end if
            end if
        end do
        index_to_subject = cdx
        return
    end function index_to_subject


    function index_to_new_subject(subj)
        integer :: index_to_new_subject
        integer, intent(in) :: subj
        integer :: i, j, cdx
        i = 1
        j = NumRenames
        do
            if (i>j) then ! not found
                cdx = subj
                exit
            else
                cdx = (i + j)/2
                if (subj==Renamed(cdx,1)) then
                    cdx = Renamed(cdx,2)
                    exit
                else if (subj<Renamed(cdx,1)) then
                    j = cdx-1
                else
                    i = cdx+1
                end if
            end if
        end do
        index_to_new_subject = cdx
        return
    end function index_to_new_subject


    subroutine tokenize_subjects(subline, symbol, maxTokens, nTokens, tokenArray, ier)
        integer, intent (in) :: maxTokens
        integer, intent (out) :: nTokens, tokenArray(maxTokens), ier
        character, intent (in) :: symbol ! delimiter
        character(len=*), intent (in) :: subline
        integer :: i, j, k, ndelsub, posub(30)
        character (len=MAX_LEN_SUBJECT_CODE) :: token

        !write(*,*) 'String is : '//subline
        ier = 0
        k = 0
        call index_to_delimiters(symbol, subline, ndelsub, posub)
        do j=1,ndelsub
            token = adjustl(subline(posub(j)+1:posub(j+1)-1))
            i = index_to_subject(token)
            !write(*,*) i, token
            if (i==0) then
                ier = 113 ! not listed
                call file_log_message ('Tokenize(): '//subline, trim(token)//' is not in catalog?')
            else
                k = k+1
                tokenArray(k) = i
            end if
        end do
        nTokens = ndelsub
        return
    end subroutine tokenize_subjects


    function is_graduate_level_subject(subj)
        logical :: is_graduate_level_subject
        integer, intent (in) :: subj
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        integer :: i,f,l,num
        tSubject = Subject(subj)%Name
        i = index(tSubject,dash)
        if (i>0) then
            l = i-1
        else
            l = len_trim(tSubject)
        end if
        i = index(tSubject,'.')
        if (i>0) l = i-1
        f = index(tSubject, SPACE)+1
        tSubject = tSubject(f:l)
        num = atoi(tSubject)
        is_graduate_level_subject = num>200
        return
    end function is_graduate_level_subject


    function is_offered(subj, term)
        logical :: is_offered
        integer, intent (in) :: subj, term
        integer :: j

        ! offered?
        j = Subject(subj)%TermOffered
        is_offered = &
        (j==7) .or. & ! offered 12S
        (term==1 .and. mod(j,2)==1) .or. & ! offered 1,12,1S
        (term==2 .and. (j==2 .or. j==3 .or. j==6) ) .or. & ! offered 2,12,2S
        (term==0 .and. (j>3) ) ! offered S,1S,2S
        return
    end function is_offered


    function is_lecture_lab_subject(subj)
        integer, intent (in) :: subj
        logical :: is_lecture_lab_subject
        ! returns true if subj is a lecture-lab/recit subject
        is_lecture_lab_subject = Subject(subj)%LectHours*Subject(subj)%LabHours/=0.0
        return
    end function is_lecture_lab_subject


    function text_prerequisite_of_subject(subj,href)
        ! returns the text representation of the prerequisite of subj
        character (len=255) :: text_prerequisite_of_subject, displayStr
        integer, intent (in) :: subj, href
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=255) :: str127(2*MAX_ALL_SUBJECT_PREREQ)
        integer :: j, prereq!, ddx1, ddx2

        !ddx1 = Subject(subj)%DeptIdx

        ! corequisite
        displayStr = SPACE
        str127 = SPACE
        do j=Subject(subj)%lenCoreq,1,-1
            tSubject = Subject(Subject(subj)%Corequisite(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1) /= 'NONE' .and. str127(1)/=SPACE) displayStr = 'Co-req: '//trim(str127(1))
        ! concurrencies
        str127 = SPACE
        do j=Subject(subj)%lenConc,1,-1
            tSubject = Subject(Subject(subj)%Concurrent(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1) /= 'NONE' .and. str127(1)/=SPACE) then
            if (displayStr/=SPACE) then
                displayStr = 'Conc. with: '//trim(str127(1))//'. '//displayStr
            else
                displayStr = 'Conc. with: '//str127(1)
            end if
        end if
        str127 = SPACE
        do j=Subject(subj)%lenConcPreq,1,-1
            tSubject = Subject(Subject(subj)%ConcPrerequisite(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1) /= 'NONE' .and. str127(1)/=SPACE) then
            if (displayStr/=SPACE) then
                displayStr = 'Conc. preq: '//trim(str127(1))//'. '//displayStr
            else
                displayStr = 'Conc. preq: '//str127(1)
            end if
        end if

        str127 = SPACE
        do j=Subject(subj)%lenPreq,1,-1
            prereq = Subject(subj)%Prerequisite(j)
            tSubject = Subject(prereq)%Name
            if (tSubject == 'OR') then
                if (j /= 1) then
                    str127(j) = trim(str127(j+2))//' or '//trim(str127(j+1))//SPACE
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//' or '//trim(str127(j+1))
                end if
            else if (tSubject == 'AND') then
                if (j /= 1) then
                    str127(j) = trim(str127(j+2))//' and '//trim(str127(j+1))//SPACE
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//' and '//trim(str127(j+1))
                end if
            else
                !ddx2 = Subject(prereq)%DeptIdx
                !if (Department(ddx2)%Code=='(dummy)' .or. ddx1==ddx2 .or. href==0) then
                !  str127(j) = tSubject
                !else
                !  !str127(j) = HREFToSubject(prereq)
                !  str127(j) = cgi_make_href(not_cgi, fnSubjectList, Subject(prereq)%Name, &
                !    A1=Department(Subject(prereq)%DeptIdx)%Code, anchor=Subject(prereq)%Name)
                !end if
                str127(j) = tSubject
            end if
        end do

        if (str127(1)/=SPACE) then
            if (displayStr/=SPACE) then
                displayStr = trim(str127(1))//'. '//displayStr
            else
                displayStr = str127(1)
            end if
        end if
        text_prerequisite_of_subject = displayStr
        return
    end function text_prerequisite_of_subject



    subroutine custom_read_failrates(path, eof)
        character (len=*), intent (in) :: path
        integer, intent (out) :: eof

        integer :: cdx, i, j, k, l
        character (len=MAX_LEN_SUBJECT_CODE) :: token

        fileName = trim(dirRAW)//trim(path)//'FAILRATES'
        open (unit=unitNo, file=fileName, form='formatted', status='old', iostat=eof)
        if (eof/=0) return

        call file_log_message('Retrieving subject passing rates from '//fileName)
        !write(unitNo,AFORMAT) '### Based on data from '//itoa(startYear)//' to '//itoa(currentYear)
        !write(unitNo,AFORMAT) '### Format is: Subject,#(Fail Sum.),#(Enrolled Sum.),#(Fail 1st),'// &
        !  '#(Enrolled 1st),#(Fail 2nd),#(Enrolled 2nd)'
        !write(unitNo,AFORMAT) '### #(Fail) = #(5.00) + #(DRP) + #(U) + #(LOA)'
        do
            read(unitNo, AFORMAT, iostat=eof) line
            if (eof<0) exit
            if (line(1:1)=='#' .or. line(1:3)=='   ') cycle
            !write(*,*) trim(line)
            call index_to_delimiters(comma, line, ndels, pos)
            if (ndels<7) cycle ! not enough data on line
            token = line(:pos(2)-1)
            cdx = index_to_subject(token)
            l = 2
            do k=0,2
                i = atoi(line(pos(l)+1:pos(l+1)-1))
                j = atoi(line(pos(l+1)+1:pos(l+2)-1))
                Failrate(cdx,k) = (1.0*i)/max(1,j)
                !write(*,*), txtSemester(k), i, j, Failrate(cdx,k)
                l = l+2
            end do
            !write(*,*) token, Failrate(cdx,0:2)
        end do
        close(unitNo)

        return
    end subroutine custom_read_failrates


    subroutine xml_read_failrates(path, eof)

        character(len=*), intent(in) :: path
        integer, intent (out) :: eof

        integer :: cdx, i, j, k, l
        character (len=MAX_LEN_SUBJECT_CODE) :: token
        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag

        ! open file, return on any error
        fileName = trim(dirXML)//trim(path)//'FAILRATES.XML'
        call xml_open_file(unitNo, XML_FAILRATES, fileName, eof, forReading)
        if (eof/=0) return

        ! read line by line
        do
            read(unitNo, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('Failrate')
                    call index_to_delimiters(comma, value, ndels, pos)
                    if (ndels<7) cycle ! not enough data on line
                    token = value(:pos(2)-1)
                    cdx = index_to_subject(token)
                    l = 2 ! start with 2nd token
                    do k=0,2 ! summer, 1st term, 2nd term
                        i = atoi(value(pos(l)+1:pos(l+1)-1))
                        j = atoi(value(pos(l+1)+1:pos(l+2)-1))
                        Failrate(cdx,k) = (1.0*i)/max(1,j)
                        ! write(*,*), txtSemester(k), i, j, Failrate(cdx,k)
                        l = l+2
                    end do

                case default
                ! do nothing

            end select

        end do
        call xml_close_file(unitNo)

        return
    end subroutine xml_read_failrates


    subroutine xml_write_failrates(path, GrandTotal)

        character(len=*), intent(in) :: path
        integer, intent (in), dimension(MAX_ALL_SUBJECTS,2,0:2) :: GrandTotal

        integer :: cdx

        ! training only?
        if (noWrites) return

        fileName = trim(dirXML)//trim(path)//'FAILRATES.XML'
        call xml_open_file(unitNo, XML_FAILRATES, fileName, cdx)
        write(unitNo,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '        Failrate - Subject,#(Fail Sum.),#(Enrolled Sum.),#(Fail 1st),#(Enrolled 1st),#(Fail 2nd),#(Enrolled 2nd)', &
        '                  #(Pass) = #(Grade>=75) + #(Grade<=3.0); #(Fail) = not Pass', &
        '    </comment>'

        do cdx=1,NumSubjects
            if (sum(GrandTotal(cdx,1:2,0:2))==0) cycle
            call xml_write_character(unitNo, indent0, 'Failrate', &
                trim(Subject(cdx)%Name)//COMMA// &
                trim(itoa(GrandTotal(cdx,2,0)))//COMMA// &
                trim(itoa(sum(GrandTotal(cdx,1:2,0))))//COMMA// &
                trim(itoa(GrandTotal(cdx,2,1)))//COMMA// &
                trim(itoa(sum(GrandTotal(cdx,1:2,1))))//COMMA// &
                trim(itoa(GrandTotal(cdx,2,2)))//COMMA// &
                trim(itoa(sum(GrandTotal(cdx,1:2,2)))) )
        end do
        call xml_close_file(unitNo, XML_FAILRATES)

        return
    end subroutine xml_write_failrates



end module SUBJECTS
