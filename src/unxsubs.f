      subroutine fabort
      i = ieee_handler ('clear', 'all', ' ')
      return
      end
c
c=================================================================
c
      subroutine clfile (lun)
      call flush (lun)
      close (lun)
      end
c
c=======================================================================c
c
      subroutine  delete  (name, i)
c
c This function operates on a filename.  The file is assumed to be
c closed already.
c
      character name*132
c
      open (19, file=name, status='old')
      close (19, status='delete', iostat=i)
      return
      end
c
c=========================================================================
c
      subroutine byebye
      character out*80
      call ieee_flags ('clear', 'exception', 'underflow', out)
      call ieee_flags ('clear', 'exception', 'inexact', out)
      write (6,*)
      write (6,*) 'Good bye.'
      write (6,*)
      stop
      end
c
c==========================================================================
c
      subroutine  oops
      character out*80
      call ieee_flags ('clear', 'exception', 'underflow', out)
      call ieee_flags ('clear', 'exception', 'inexact', out)
      write (6,*)
      write (6,*) 'Sorry about that.'
      write (6,*)
      stop
      end
c
c==============================================================================
c
      subroutine infile (lun, file, istat)
      character expand*132
      character file*(*)
      if (file .eq. ' ') go to 999
  100 if (file(1:1) .eq. ' ') then
         file = file(2:len(file))
         go to 100
      end if
      open (lun, file=expand(file), status='old', err=999)
      istat = 0
      return
  999 istat = -1
      return
      end
c
c========================================================
c
      character*(*) function expand(file)
      character file*(*)
      k = 0
      if ((file(1:1) .eq. '$') .or. (file(1:1) .eq. '~')) then
         do i=2,29
            if (file(i:i) .eq. '/') then
               call getenv (file(2:i-1), expand)
               do j=1,len(expand)
                  if (expand(j:j) .ne. ' ') k=j
               end do
               if (k .ne. 0) then
                  expand = expand(1:k)//'/'//file(i+1:30)
                  return
               end if
            end if
         end do
      end if
c
      do i=2,29
         if (file(i:i) .eq. ':') then
            call getenv (file(1:i-1), expand)
            do j=1,len(expand)
               if (expand(j:j) .ne. ' ') k=j
            end do
            if (k .ne. 0) then
               expand = expand(1:k)//'/'//file(i+1:30)
               return
            end if
         end if
      end do
      expand = file
      return
      end
c
c======================================================
c
      subroutine outfil (lun, file, istat)
      character expand*132, string*132
      character file*40, answer*40
c     logical exist
      if (file .eq. ' ') then
         istat = -1
         return
      end if
 1000 if (file(1:1) .eq. ' ') then
         file = file(2:len(file))
         go to 1000
      end if
 2000 call infile (lun, file, istat)
      if (istat .eq. 0) then
         call stupid ('This file already exists: '//file)
         answer = 'OVERWRITE'
         call getnam ('New output file name:', answer)
         if (answer .eq. 'OVERWRITE') then
            close (lun, status='delete')
	         open (lun, file=expand(file), status='new', 
     .           iostat=istat)
         else if (answer .eq. 'END-OF-FILE') then
            call tblank
            call stupid ('Appending to existing file.')
            file = 'APPEND'
 1100       read (lun,110,end=9000)
  110       format (a)
            go to 1100
         else
            file = answer
            go to 2000
         end if
      else
         string = expand(file)
         open (lun, file=expand(file), status='new',
     .        iostat=istat)
      end if
 9000 return
      end
c
c====================================================
c
      subroutine  delfil  (lun, file, istat)
      character expand*132
      character file*(*)
c
c Delete a disk file.  File is assumed to be closed before it
c gets this far.
c
      open (lun, file=expand(file), status='old', iostat=istat)
      if (istat .eq. 0) close (lun, status='delete')
      return
      end
c
c====================================================
c
      character*(*) function case (string)
      character string*(*)
c
c For UNIX, leave the cases of the characters alone!
c
      case = string
      return
      end
c
c====================================================
c
      character*(*) function caps (string)
      character string*(*)
c
c Convert lower case letters to upper case.
c
      l = length(string)
      do i=1,l
         j = ichar(string(i:i))
         if ((j .ge. 97) .and. (j .le. 122)) then
            j = j-32
            string(i:i) = char(j)
         end if
      end do
      caps = string
      return
      end
c
c=======================================================================
c
      subroutine ovrwrt (line, iwhich)
      character line*(*)
      if (iwhich .eq. 1) then
         write (6,1) line
    1    format (/a)
      else if (iwhich .eq. 2) then
         write (6,2) line, char(13)
    2    format (a, a1, $)
      else if (iwhich .eq. 3) then
         write (6,3) line
    3    format (a)
      else
         write (6,4) line, char(13)
    4    format (/a, a1, $)
      end if
      call flush (6)
      return
      end
