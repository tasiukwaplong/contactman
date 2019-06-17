 program contact_man
 implicit none
 
print*, ' '
print*, '+---------------------------------------------------------+'
print*, '|    ______                                               |'
print*, '|   / ____/    Welcome to Contact Man. A CLI program to   |'
print*, '|  / /           help save phone contacts and also do     |'
print*, '| / /____              some manipulation on them          |'
print*, '| \_____/MAN                                              |'
print*, '+---------------------------------------------------------+'
print*, ' '


 ! MENU OPTIONS
  print *, "1. Add <phone, name> --To add new contact"
  print *, "2. List --List all contacts"
  print *, "3. Export <CSV | JSON | TXT> --To export contacts in either CSV, JSON or TXT"
  print *, "4. Help --To get help on how to use the software"
  print *, "5. About --To get about software and how to contribute on GitHub"
  print *, "6. Exit --Exit Contact Man "

  ! INIT
  call init_program()

 end program contact_man
 

recursive subroutine init_program()
   implicit none

     ! VARIABLES
    character (len=200)::response
     ! GET USER RESPONSE 
    print *, ' '
    write(*,'("<[ENTER COMMAND:]>: ",\)')
    read(*,'(A)') response
    
    if (len_trim(response) .ne. 0) then
     call take_response(response)
    else
      call init_program()
    end if
end subroutine init_program


subroutine take_response(response)
 implicit none

! VARIABLES
 character (len=200)::response
  
! GET KEYWORD TO PROCESS COMMAND
 if ((response(1:4) .eq. 'Add ') .or. (response(1:4) .eq. 'add ')) then
   call add_contact(response(5:200))
 else if((response(1:5) .eq. 'List ') .or. (response(1:5) .eq. 'list ')) then
   call list_contacts()
 else if((response(1:5) .eq. 'Find ') .or. (response(1:5) .eq. 'find ')) then
   call find_contacts(trim(response(6:200)))
 else if((response(1:7) .eq. 'Export ') .or. (response(1:7) .eq. 'export ')) then
    print *, '<[INFO]> Contacts exorted successfully......'
    print *, "<[INFO]> Kindly check the directory this program is currently being executed." 
     call init_program()
 else if((response(1:6) .eq. 'About ') .or. response(1:6) .eq. 'about ') then
  print *, 'Contactman V0.1. Created by Tasiu Kwaplong (TK)'
  print *, 'Kindly give me a star and also contribute to this work on GitHub:'
  print *, 'www.github.com/tasiukwaplong/contactman'
  call init_program()
 else if((response(1:5) .eq. 'Exit') .or. (response(1:5) .eq. 'exit')) then
  print *, 'Kindly give me a star and also contribute to this work on GitHub:'
  print *, 'www.github.com/tasiukwaplong/contactman'
  print *, 'Thanks for using Conatact MAN...!!'
 else
  print *, '<[ERROR]> Invalid command entered.'
  print *, '<[TIP]>Enter any of Add, List, Find, Export, Help, About or Exit'  
  call init_program()  
 end if
 
end subroutine take_response


subroutine add_contact(contact)
   
   ! SUB TO ADD CONTACT TO FILE
   implicit none
   
   character(len=200)::contact
   character(len=200)::contactNumber
   character(len=200)::contactName
   integer::contactNumberIndex
   logical :: exist
   character(len=600)::textToSaveJSON, textToSaveCSV, textToSaveTXT

  ! print *, scan(contact, ',') !DOES SAME THING
   contactNumberIndex = index(contact, ',')
   if (contactNumberIndex == 0) then
     print *, '<[ERROR]> Sorry incorrect command entered'
     print *, "<[HINT]> To save TK's number use command: Add 09031514346 TK" 
     call init_program()
   end if
   
    contactNumber = trim(adjustl(contact(1:contactNumberIndex-1)))
    contactName = trim(adjustr(contact(contactNumberIndex+1: 200)))
  
   ! VALIDATE NUMBER
   if ((len_trim(contactNumber) < 10) .or. (len_trim(contactNumber) > 15) ) then
    print *, '<[ERROR]> Incorrect phone number provided'
    print *, '<[HINT]> Phone number must not be greater than 15 or less than 11 digits e.g. 09031514346, TK'    
    call init_program()
   end if

   ! WRITE TO FILE OR UPDATE CONTACT FILE
    ! output data into a file 
    inquire(file="contacts.json", exist=exist)
    if (exist) then
      open(20, file="contacts.json", status="old", position="append", action="write")
      open(21, file="contacts.csv", status="old", position="append", action="write")
      open(22, file="contacts.txt", status="old", position="append", action="write")

      textToSaveJSON = ',{"'//contactNumber(1:len_trim(contactNumber))//'":"'//contactName(1:len_trim(contactName))//'"}'
      textToSaveCSV = contactNumber(1:len_trim(contactNumber))//','//contactName(1:len_trim(contactName))

      print *, 'Saving '//contactName
      write(20, *) textToSaveJSON(1:len_trim(textToSaveJSON))
      write(21, *) textToSaveCSV(1:len_trim(textToSaveCSV))
      write(22, *) textToSaveCSV

    else
      open(20, file="contacts.json", status="new")
      open(21, file="contacts.csv", status="new")
      open(22, file="contacts.txt", status="new")

      textToSaveJSON = '{"'//contactNumber//'":"'//contactName//'"}'
      textToSaveCSV = contactNumber//','//contactName

      print *, 'Saving '//contactName
      write(20, *) textToSaveJSON
      write(21, *) textToSaveCSV      
      write(22, *) textToSaveCSV
    end if

    print *, 'Operation completed !!'
    close(20)
    close(21)
    close(22)
    call init_program()

end subroutine


subroutine find_contacts(contactToSearch)
  implicit none
  character (len=1000) :: allContacts
  character (len=40) :: contactToSearch, searchMsg 
  integer:: i, t, io
  logical :: exist
  inquire(file="contacts.txt", exist=exist)

    if (.not. exist) then
      print *, "No contacts yet. Add new contact" 
      call init_program()
    end if
      open (22,file="contacts.txt", action="read", status='old')

      do
        read (22,"(a)", IOSTAT=io) allContacts ! read line into character variable
        if (io .lt. 0) then
          exit
        endif

        t = scan(allContacts, contactToSearch)

        if (t == 0) then
          searchMsg = '<[ERROR]> Sorry no such number exist'
        else 
          searchMsg = 'One match found'// allContacts
        endif

      end do
      close(22)

      print *, searchMsg
      call init_program()
      !print *, end(t)
end subroutine find_contacts


subroutine list_contacts()
  implicit none
  character (len=1000) :: allContacts
  integer:: i, t, io
  logical :: exist, exist2

  inquire(file="contacts.txt", exist=exist)
    
    if (.not. exist) then
      print *, "No contacts yet. Add new contact" 
      call init_program()
    end if

      open (22,file="contacts.txt", action="read", status='old')

      do
        read (22,"(a)", IOSTAT=io) allContacts ! read line into character variable
        if (io .lt. 0) then
          exit
        endif

        print*, adjustr(allContacts(1:len_trim(allContacts)))
        
      end do
      close(22)
!print *, end(t)
 call init_program()
end subroutine list_contacts