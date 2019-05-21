program outputdata
 implicit none
 logical :: exist

 ! output data into a file 
  inquire(file="test.txt", exist=exist)
  if (exist) then
    open(12, file="test.txt", status="old", position="append", action="write")
    write(12, *) "Appended Text"  
  else
    open(12, file="test.txt", status="new", action="write")
    write(12, *) "New Text"

  end if

  close(12)

end program outputdata