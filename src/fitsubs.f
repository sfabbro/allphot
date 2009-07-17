c
c
c#######################################################################
c#######################################################################
c
c              Official DAO version:  2004 April 16
c
c#######################################################################
c#######################################################################
c
      subroutine  delpic  (jmno, ier)
c
c This routine operates on the filename contained in the array element
c pictur(jmno).  The image is assumed to be closed before it is deleted.
c
      include 'arrays.inc'
c
      character expand*132, name*132, message*80, extend*40
c
      ier = 0
      name = expand(extend(pictur(jmno), imgtyp(jmno)))
      if (imgtyp(jmno) .eq. 'imh') then
         call imdele (name, ier)
      else
         call ftgiou (j, ier)
         call delfil (j, name, ier)
         call ftfiou (j, ier)
      end if
      if (ier .ne. 0) then
         call stupid ('delpic: '//message(imgtyp(jmno), ier))
c     print*,ier
      else
         imid(jmno) = 0
      end if
      return
      end
c
c=======================================================================
c
      subroutine  object  (imno, name)
      include 'arrays.inc'
      character name*(*), commnt*72, switch*40
c
      call sget (imno, 'title   ', name, commnt)
      if (name.eq.'ERROR!') call sget (imno, 'TARGNAME', name, commnt)
      if (name.eq.'ERROR!') call sget (imno, 'OBJECT  ', name, commnt)
      if (name.eq.'ERROR!') call sget (imno, 'COMMENT ', name, commnt)
      write (6,608) switch(pictur(imno), '...')
  608 format (/5x, a)
      if (name .ne. 'ERROR!') write (6,608) name(1:length(name))
      write (6,610) ncol(imno), nrow(imno)
  610 format (/38x, 'Picture size: ', 2i6/)
      return
      end
c
c#######################################################################
c#######################################################################
c
c           The rest of this file contains all the calls to 
c                     FITSIO and IMFORT routines.
c
c#######################################################################
c#######################################################################
c
      character*80 function  message  (imgtyp, ier)
      character error*80, imgtyp*4
      if (imgtyp .eq. 'imh') then
         call imemsg (ier, error)
      else
         call ftgerr (ier, error)
      end if
      message = error
      return
      end
c
c=================================================================
c
      subroutine  jget  (imno, keywrd, j, commnt)
      include 'arrays.inc'
      character message*80, commnt*(*), keywrd*8
      integer j
c
      ier = 0
      if (imgtyp(imno) .eq. 'imh') then
         call imgkwi (imid(imno), keywrd, j, ier)
         commnt = ' '
      else
         call ftgkyj (imid(imno), keywrd, j, commnt, ier)
      end if
      if (ier .ne. 0) then
         call stupid ('jget: '//message(imgtyp(imno), ier))
         j = -1073741824
      end if
      return
      end
c
c=================================================================
c
      subroutine  rget  (imno, keywrd, r, commnt)
      include 'arrays.inc'
      character commnt*(*), keywrd*8
c
      ier = 0
      if (imgtyp(imno) .eq. 'imh') then
         call imgkwr (imid(imno), keywrd, r, ier)
         commnt = ' '
      else
         call ftgkye (imid(imno), keywrd, r, commnt, ier)
      end if
c
      if (ier .ne. 0) r = -1.1E19
      return
      end
c
c=================================================================
c
      subroutine  dget  (imno, keywrd, r, commnt)
      include 'arrays.inc'
      character message*80, commnt*(*), keywrd*8
      double precision r
c
      ier = 0
      if (imgtyp(imno) .eq. 'imh') then
         call imgkwd (imid(imno), keywrd, r, ier)
         commnt = ' '
      else
         call ftgkyd (imid(imno), keywrd, r, commnt, ier)
      end if
c
      if (ier .ne. 0) then
         call stupid ('dget: '//message(imgtyp(imno), ier))
         r = -1.1D38
      end if
      return
      end
c
c=================================================================
c
      subroutine  sget  (imno, keywrd, s, commnt)
      include 'arrays.inc'
      character s*(*), commnt*(*), keywrd*8, message*80
c
      ier = 0
      if (imgtyp(imno) .eq. 'imh') then
         call imgkwc(imid(imno), keywrd, s, ier)
         commnt = ' '
      else
         call ftgkys (imid(imno), keywrd, s, commnt, ier)
      end if
c
      if (ier .ne. 0) s = 'ERROR!'
      return
      end
c
c=================================================================
c
      subroutine  sput  (imno, keywrd, s, commnt, ier)
      include 'arrays.inc'
      character s*(*), commnt*(*), keywrd*(*)
c
      ier = 0
c     print*,'a ',imgtyp(imno)
      if (imgtyp(imno) .eq. 'imh') then
         call imakwc (imid(imno), keywrd, s, commnt, ier)
      else
c     print*,'aa ',imid(imno), keywrd, ier
         call ftdkey (imid(imno), keywrd, ier)
c     print*,'b',ier, s
      ier = 0
         call ftpkys (imid(imno), keywrd, s, commnt, ier)
c     print*,'c',ier
      end if
c
      if (ier .ne. 0) s = 'ERROR!'
      return
      end
c
c=======================================================================
c
      real  function  rdpixl (imno, ix, iy)
      include 'arrays.inc'
      character message*80
      logical bad
c
      n = ncol(imno)
      if (imgtyp(imno) .eq. 'imh') then
         ier = 0
         call imgs2r (imid(imno), rdpixl, ix, ix, iy, iy, ier)
         if (ier .ne. 0) then
            call stupid ('rdpixl: '//message(imgtyp(imno), ier))
            write (6,*) ' imgs2r in rdpixl', imno, imid(imno), ix, iy
            write (6,*) pictur(imno)
            rdpixl = -1.01e19
         end if
      else
         ifirst = (iy-1)*n + ix
         ier = 0
         call ftgpve (imid(imno), 1, ifirst, 1, -1.01e19, 
     .        rdpixl, bad, ier)
         if (ier .ne. 0) then
            call stupid ('ftgpve: '//message(imgtyp(imno), ier))
            write (6,*) ' ftgpve in rdpixl', imno, imid(imno), ix, iy
            write (6,*) pictur(imno), ier
            rdpixl = -1.01e19
            CALL OOPS
         end if
      end if
      return
      end
c     
c=======================================================================
c
      subroutine  rdrow  (imno, iy, row, show, ier)
      include 'arrays.inc'
c
      character message*80
      real row(*)
      integer ier
      logical bad, show
c
      ier =0
c     if (show) print*,'in rdrow',ier
      n = ncol(imno)
      if (imgtyp(imno) .eq. 'imh') then
c     if (show) print*,'imh'
         call imgl2r (imid(imno), row, iy, ier)
c     if (show) print*,'X:',ier
         if (ier .ne. 0) then
            call stupid ('rdrow: '//message(imgtyp(imno), ier))
            write (6,*) ' imgl2r in rdrow',imno,imid(imno),ncol(imno),iy
            CALL OOPS
         end if
      else
         ifirst = (iy-1)*n+1
c     if (show) print*,'y',imno, imid(imno), ifirst, n, ier
         call ftgpve (imid(imno), 1, ifirst, n, -1.01e15, row, 
     .        bad, ier)
c     if (show) print*,'Y ',bad,ier
         if (ier .ne. 0) then
            write (6,*) ' ftgpve in rdrow',imno,imid(imno),ncol(imno),
     .           iy, ier
            call stupid ('rdrow: '//message(imgtyp(imno), ier))
            CALL OOPS
         end if
      end if
c
      return
      end
c     
c=======================================================================
c
      subroutine  wrrow  (imno, iy, row, ier)
      include 'arrays.inc'
c
      character message*80
      real row(*)
c     print*,'wrrow'
c
      ier = 0
      n = ncol(imno)
c
      if (dattyp(imno) .eq. 'SHRT') then
         do i=1,ncol(imno)
            row(i) = max(-32768., min(32767., anint(row(i))))
         end do
      else if (dattyp(imno) .eq. 'LONG') then
         do i=1,ncol(imno)
            row(i) = anint(row(i))
         end do
      end if
c
      if (imgtyp(imno) .eq. 'imh') then
         call impl2r (imid(imno), row, iy, ier)
      else
         ifirst = (iy-1)*n+1
         call ftppre (imid(imno), 1, ifirst, n, row, ier)
      end if
c
      if (ier .ne. 0) then
         call stupid ('wrrow: '//message(imgtyp(imno), ier))
         write (6,*) ' wrrow', iy
      end if
      return
      end
c
c=======================================================================
c
      subroutine rdaray (imno, lx, ly, mx, my, maxbox, func, ier)
c
c lx, ly are the pixel coordinates of the corner of the subarray
c     func in the image
c mx, my are the dimensions of the subarray func to be extracted from
c     from the image
c maxbox is the stated x-dimension of the subarray func in the calling
c     routine
c
      include 'arrays.inc'
      character message*80
      real func(maxbox,*)
      logical bad
c     print*,'rdaray'
c
c Make sure the subarray is wholly contained within the image.
c Redefine array corners if necessary.
c
      ier = 0
      id = imid(imno)
      mx = lx+mx-1
      my = ly+my-1
      lx = max(1,lx)
      ly = max(1,ly)
c
c lx, ly are now the lower limits of the subarray in the big image
c
      mx = min(ncol(imno),mx)
      my = min(nrow(imno),my)
c
c mx, my are now the upper limits of the subarray in the big image
c
      my = my-ly+1
c
c my is now the vertical size of the subarray
c
      if (imgtyp(imno) .eq. 'imh') then
         jy = ly - 1
         do j=1,my
            jy = jy + 1
            call imgs2r (id, func(1,j), lx, mx, jy, jy, ier)
            if (ier .ne. 0) then 
               call stupid ('rdaray: '//message(imgtyp(imno), ier))
               write (6,*) 'rdaray:  x =', lx, ' to', mx, 
     .              '  y =', jy
            end if
         end do 
         mx = mx-lx+1
c
c mx is now the horizontal size of the subarray
c
      else
         mx = mx-lx+1
c
c mx is now the horizontal size of the subarray
c
         n = ncol(imno)
         jy = ly-1
         ifirst = jy*n + lx
         do j=1,my
            call ftgpve (id, 1, ifirst, mx, -1.01e19, func(1,j),
     .           bad, ier)
            if (ier .ne. 0) then 
               call stupid ('rdaray: '//message(imgtyp(imno),ier))
               write (6,*) 'rdaray ftgpve:  x =', lx, ' to', mx, 
     .              '  y =', jy+j
            end if
            ifirst = ifirst + n
         end do 
      end if
      return
      end
c
c=======================================================================
c
      subroutine wraray (jmno, lx, ly, mx, my, maxbox, func, ier)
c
c lx, ly are the pixel coordinates of the corner of the subarray
c     func in the image
c mx, my are the dimensions of the subarray func to be inserted into
c     the image
c maxbox is the stated x-dimension of the subarray func in the calling
c     routine
c
      include 'arrays.inc'
      character message*80
      real func(maxbox,*)
c     print*,'wraray'
c
      ier = 0
      if (dattyp(jmno) .eq. 'SHRT') THEN
         do j=1,my
            do i=1,mx
               func(i,j) = max(-32768., min(32767., anint(func(i,j))))
            end do
         end do
      else if (dattyp(jmno) .eq. 'LONG') THEN
         do j=1,my
            do i=1,mx
               func(i,j) = anint(func(i,j))
            end do
         end do
      end if
c
c Make sure the subarray is wholly contained within the image.
c Redefine array corners if necessary.
c
      id = imid(jmno)
      mx = lx+mx-1
      my = ly+my-1
      lx = max(1,lx)
      ly = max(1,ly)
      mx = min(ncol(jmno),mx)
      my = min(nrow(jmno),my)
      my = my-ly+1
c
      if (imgtyp(jmno) .eq. 'imh') then
         jy = ly - 1
         do j=1,my
            jy = jy + 1
            call imps2r (id, func(1,j), lx, mx, jy, jy, ier)
            if (ier .ne. 0) then 
               call stupid ('wraray: '//message(imgtyp(jmno), ier))
               write (6,*) 'wraray imps2r:  x =', lx, ' to', mx, 
     .              '  y =', jy
            end if
         end do 
         mx = mx-lx+1
      else
         mx = mx-lx+1
         n = ncol(jmno)
         ifirst = (ly-2)*n + lx
         jy = ly-1
         do j=1,my
            ifirst = ifirst + n
            jy = jy + 1
            call ftppre (id, 1, ifirst, mx, func(1,j), ier)
            if (ier .ne. 0) then 
               call stupid ('wraray: '//message(imgtyp(jmno),ier))
               write (6,*) 'wraray ftppre:  x =', lx, ' to', mx, 
     .              '  y =', jy
            end if
         end do 
      end if
      return
      end
c
c=======================================================================
c
      subroutine  rdsect  (imno, ly, my, dat, maxcol, ier)
      include 'arrays.inc'
c
c maxcol is the x dimension of the data array, which can be larger than
c the image.
c
      real dat(maxcol,*)
c
      character message*80
      logical bad
c
      ier = 0
      ly = max0(1,ly)
      my = min0(nrow(imno),my)
      nx = ncol(imno)
c
      if (imgtyp(imno) .eq. 'imh ') then
         call imgs2r (imid(imno), dat(1,ly), 1, nx, ly, my, ier)
      else if (imgtyp(imno) .eq. 'fits') then
         i = (ly-1)*nx+1
         n = (my-ly+1)*nx
         bad = .false.
         call ftgpve (imid(imno), 1, i, n, -1.01e19, 
     .        dat(1,ly), bad, ier)
      else
         call stupid ('Invalid image type: '//imgtyp(imno))
         ier = -1
         return
      end if
c
      if (ier .ne. 0) then
         call stupid ('rdsect: '//message(imgtyp(imno), ier))
         write (6,*) pictur(imno), ' imgs2r in rdsect',
     .        ly, my, nx, maxcol
         call oops
      end if
c
c Unwrap the data
c
      if ((my .gt. ly) .and. (nx .lt. maxcol)) then
         do j=my,ly+1,-1
            do i=nx,1,-1
               dat(i,j) = dat((j-ly)*nx+i,ly)
            end do
         end do
      end if
      return
      end
c
c=======================================================================
c
      subroutine  wrsect  (imno, ly, my, dat, maxcol, ier)
      include 'arrays.inc'
c
c maxcol is the x-dimension of the data array in the calling program,
c which can be different from the x-dimension of the image.
c
      real dat(maxcol,*)
c
      character message*80
c     print*,'wrsect'
c
      ier = 0
      nx = ncol(imno)
      ly = max0(1,ly)
      my = min0(nrow(imno), my)
c
c ly, my are the vertical limits of the image section.  Complete
c rows will be written.
c
      if (dattyp(imno) .eq. 'SHRT') then
         do j=ly,my
            do i=1,nx
               dat(i,j)=anint(amax1(-32768.,min(32767.,dat(i,j))))
            end do
         end do
      else if (dattyp(imno) .eq. 'LONG') then
         do j=ly,my
            do i=1,nx
               dat(i,j) = anint(dat(i,j))
            end do
         end do
      end if
c
c Wrap the data.
c
      if ((my .gt. ly) .and. (nx .lt. maxcol)) then
         do j=ly+1,my
            do i=1,nx
               k = (j-ly)*nx + i
               dat(k,ly) = dat(i,j)
            end do
         end do
      end if
c
      if (imgtyp(imno) .eq. 'imh') then
         call imps2r (imid(imno), dat(1,ly), 1, nx, ly, my, ier)
      else
         i = (ly-1)*nx+1
         n = (my-ly+1)*nx
c     PRINT *,'before ftppre',imno,imid(imno),ly,my,nx,i,n,ier
         call ftppre (imid(imno), 1, i, n, dat(1,ly), ier)
c     PRINT *,'after ftppre',ier,ncol(imno),nrow(imno)
      end if
c
      if (ier .ne. 0) then
         call stupid ('wrsect: '//message(imgtyp(imno), ier))
         write (6,*) 'wrsect ', imno, ly, my, nx, maxcol
         call oops
      end if
      return
      end
c
c=======================================================================
c
      subroutine  attach  (image, nx, ny)
      include 'arrays.inc'
      character switch*40, image*40
      character coofile*40, magfile*40, psffile*40, profile*40, 
     .     grpfile*40
      common /filnam/ coofile, magfile, psffile, profile, grpfile
      ier = 0
      if (switch(image, ' ') .eq. ' ') 
     .     call getnam ('Input image name:', image)
      write (6,610) switch(image, '...')
  610 format (/5x, a/)
      if (imid(1) .gt. 0) call clpic (1, ier)
      pictur(1) = image
      imgtyp(1) = ' '
      dattyp(1) = ' '
      call opnpic (1, 'R', .false., ier)
      if (ier .eq. 0) then
         nx = ncol(1)
         ny = nrow(1)
         write (6,611) ncol(1), nrow(1)
  611    format (/38x, 'Picture size: ', 2i6/)
         coofile = switch(image, '.coo')
         magfile = switch(image, '.ap')
         psffile = switch(image, '.psf')
         profile = switch(image, '.nst')
         grpfile = switch(image, '.grp')
      else
         nx = 0
         ny = 0
      end if
      return
      end
c
c=======================================================================
c
      subroutine  opnpic  (imno, mode, show, ier)
c
c Open the pre-existing image whose filename is contained in the array
c element PICTUR(IMNO)
c
      include 'arrays.inc'
c
c Arrays and functions
c
      character expand*132, switch*40, extend*40
      integer axlen(7), access
c
c Variables
c
      character name*132, mode*1
      character message*80
      character commnt*72
      integer lblock
      logical show
      common /block/ lblock
c
c Executable
c
c
c REMEMBER TO UNCOMMENT CHARACTER MESSAGE ABOVE
c
c     if(show)print*,'opnpic ',pictur(imno),mode
      ier = 0
c
c If image is already open (i.e., it has a logical unit number) return
c
      if (imid(imno) .ne. 0) then
         return
      end if
c 
c Get a free LUN and define R/W mode just in case it is a 
c FITS file.
c
      call ftgiou (imid(imno), ier)
c     print*,'ftgiou',imid(imno),ier
      if ((imid(imno) .le. 0) .or. (ier .ne. 0)) then
         call stupid ('Unable to assign unit number.')
         ier = -7
         return
      end if
c     if(show)print*,'ftgiou',imid(imno),ier
      if (mode .eq. 'W') then
         j = 1
      else
         j = 0
      end if
c     print*,'mode ',j,'  ',imgtyp(imno)
c
c Consider five possibilities:
c
c     1. filename extension is .imh
c     2. filename extension is .fit
c     3. filename extension is .fits
c     4. filename extension is something else ==> fits format
c     5. filename extension is not specified
c        a.  try '.fits'
c        b.  try '.fit'
c        c.  try '.imh'
c
c If all this fails, give up.
c
c If imgtyp is non-blank, then the image has been opened before.
c
c     if(show)print*,'imgtyp ',imgtyp(imno), ichar(imgtyp(imno)(1:1)),
c    .     ichar(imgtyp(imno)(2:2)),ichar(imgtyp(imno)(3:3)),
c    .     ichar(imgtyp(imno)(4:4))
      ier = 0
      if (imgtyp(imno) .ne. ' ') then
         if (imgtyp(imno) .eq. 'imh ') then
            name = extend(pictur(imno), 'imh')
c     if(show)print*,'go to 4000 ',name
            go to 4000
         else if (imgtyp(imno) .eq. 'fit ') then
            name = extend(pictur(imno), 'fit')
c     if(show)print*,'go to 3000 ',name
            go to 3000
         else if (imgtyp(imno) .eq. 'fits') then
            name = extend(pictur(imno), 'fits')
c     print*,'go to 2000 ',imid(imno),name
c     if(show)print*,'go to 2000 ',name
            go to 2000
         else
            name = extend(pictur(imno), imgtyp(imno))
c     if(show)print*,'go to 1000 ',name
            go to 1000
         end if
      end if
c
c Never before opened.  Look for a filename extension.
c
      name = pictur(imno)
c     if(show)print*,'A ',name
      l = length(name)
      if (name(l-3:l) .eq. '.imh') go to 4000
      if (name(l-3:l) .eq. '.fit') go to 3000
      if (name(l-4:l) .eq. '.fits') go to 2000
c
c Not a standard filename extension.  Try to find  a non-standard one.
c
      do i=l,1,-1
         if (name(i:i) .eq. '.') then
c
c Period was found.  Treat it as a FITS file.
c
            go to 1000
         end if
      end do
c
c No period was found in the filename, so there is no filename
c extension specified.  Try appending '.fits' and see whether
c that file exisits.
c
      name = name(1:l)//'.fits'
      if (access(expand(name), 'r') .eq. 0) go to 2000
      name = switch(name, '.imh')
      if (access(expand(name), 'r') .eq. 0) go to 4000
      go to 1100
c
 1000 continue
c     if(show)print*,1000
      call ftopen (imid(imno), expand(name), j, lblock, ier)
c     if(show)print*,'ftopen1',ier
      if (ier .eq. 0) then
         imgtyp(imno) = name(i+1:l)
         go to 5000
      end if
 1100 call stupid ('Apparently not a standard image format:  '//
     .     pictur(imno))
      ier = 7
      return
c
 2000 continue
c     if(show)print*,'2000',imid(imno),j,ier
c     if(show)print*,name,ier
c     if(show)print*,expand(name),ier
c     print*,'ftopen ', pictur(imno), j
c     print*,'ftopen ',imid(imno),j,lblock,ier
c     print*,expand(name)
      call ftopen (imid(imno), expand(name), j, lblock, ier)
c     print*,'ftopen', ier
      if (ier .ne. 0) then
         call stupid ('Unable to open '//pictur(imno))
         write (6,*) message('fits', ier)
         call oops
      end if
c     if(show)print*,'ftopen',j,lblock,ier
      if (ier .eq. 0) then
c     if(show)print*,'a fits worked'
c
c '.fits' worked
c
         imgtyp(imno) = 'fits'
         go to 5000
      else
c     if(show)print*,'A fits didn''t work',ier
c     if(show)print*,'B'
      end if
      if (imgtyp(imno) .ne. ' ') then
         imid(imno) = 0
         return
      end if
c
c '.fits' didn't work.  Try '.fit'
c
      name = switch(name, '.fit')
c     if(show)print*,'Trying ',name
c
 3000 continue
c     if(show)print*,3000
      ier = 0
      call ftopen (imid(imno), expand(name), j, lblock, ier)
c     if(show)print*,'ftopen2',ier
      if (ier .eq. 0) then
c
c '.fit' worked
c
         imgtyp(imno) = 'fit'
         go to 5000
      end if
      if (imgtyp(imno) .ne. ' ') return
c
c '.fit' didn't work.  Try '.imh' and redefine R/W mode.
c
      name = switch(name, '.imh')
c
 4000 continue
c     if(show)print*,4000
c
c Give back the FITS LUN.
c
      call ftfiou (imid(imno), ier)
c     if(show)print*,'ftfiou', ier
      imid(imno) = 0
      if (mode .eq. 'W') then
         j = 3
      else
         j = 1
      end if
      call imopen (expand(name), j, imid(imno), ier)
      if (ier .eq. 0) then
c
c '.imh' worked.
c
c     if(show)print*,'imh worked', imid(imno)
         imgtyp(imno) = 'imh '
         call imgsiz (imid(imno), axlen, naxis, itype, ier)
         if (ier .eq. 0) then
            ncol(imno) = axlen(1)
            nrow(imno) = axlen(2)
            if (itype .eq. 3) then
               dattyp(imno) = 'SHRT'
            else if (itype .eq. 5) then
               dattyp(imno) = 'LONG'
            else if (itype .eq. 6) then
               dattyp(imno) = 'REAL'
            else
               call stupid ('Unsupported data type')
               ier = 1
            end if
            ier = 0
            return
         end if
      else
         call imemsg (ier, commnt)
         call stupid (commnt)
         ncol(imno) = -1
         return
      end if
c
 5000 continue
c     if(show)print*,5000
      call jget (imno, 'NAXIS   ', naxis, commnt)
      if (naxis .le. 0) then
         ier = 2
         return
      end if
c
      call jget (imno, 'NAXIS1  ', ncol(imno), commnt)
      if (ncol(imno) .le. 0) then
         ier = 3
         return
      end if
c
      if (naxis .eq. 1) then
         nrow(imno) = 1
      else
         call jget (imno, 'NAXIS2  ', nrow(imno), commnt)
         if (nrow(imno) .le. 0) then
            ier = 4
            return
         end if
      end if
c
      call jget (imno, 'BITPIX  ', naxis, commnt)
      if (naxis .eq. 16) then
         dattyp(imno) = 'SHRT'
      else if (naxis .eq. 32) then
         dattyp(imno) = 'LONG'
      else if (naxis .eq. -32) then
         dattyp(imno) = 'REAL'
      else
         call stupid ('Unsupported data type.')
         ier = 5
         return
      end if
c     if(show)print*,'opnpic returning ier ',ier
      ier = 0
      return
      end
c
c=======================================================================
c
      subroutine  coppic  (file, ier)
      include 'arrays.inc'
      character file*40
c     print*,'coppic ', file
c
      ier = 0
      pictur(2) = file
      ncol(2) = ncol(1)
      nrow(2) = nrow(1)
      imgtyp(2) = imgtyp(1)
      dattyp(2) = dattyp(1)
c     print*,'a'
      call crepic (1, 2, ier)
c     print*,'leaving coppic',ier
      return
      end
c
c=======================================================================
c
      subroutine  crepic  (imno, jmno, ier)
      include 'arrays.inc'
c
      character expand*132, name*132, extend*40
      integer axlen(2), access
c
      character message*80, commnt*80
      common /block/ lblock
c
c Tuck the image dimensions away someplace safe, lest the delpic
c erase them.
c
c     print*,'crepic'
      ier = 0
      axlen(1) = ncol(jmno)
      axlen(2) = nrow(jmno)
      if (imid(jmno) .gt. 0) then
         call stupid ('The output image is already open!')
         call oops
      end if
c
c If an image of this name exists, delete it.
c
c     print*,'crepic opnpic', jmno, imid(jmno), imgtyp(jmno), ier
      name = expand(extend(pictur(jmno), imgtyp(jmno)))
      if (access(name, 'w') .eq. 0) then
c     print*,'delpic in crepic A'
c     print*,name
         call delpic (jmno, ier)
         if (ier .ne. 0) call stupid ('delpic error')
      end if
      ier = 0
      ncol(jmno) = axlen(1)
      nrow(jmno) = axlen(2)
c
      if (imgtyp(jmno) .eq. 'imh') then
         if (dattyp(jmno) .eq. 'SHRT') then
            i = 3
         else if (dattyp(jmno) .eq. 'LONG') then
            i = 5
         else if (dattyp(jmno) .eq. 'REAL') then
            i = 6
         else
            ier = 2
            return
         end if
         name = extend(pictur(jmno), 'imh')
c     print*,'before imcrea', axlen, i
c     print*,expand(name)
         call imcrea (expand(name), axlen, 2, i, ier)
c     print*,'crepic imcrea',ier
         if (ier .ne. 0) then
            call stupid ('crepic A: '//message('imh', ier))
c     print*,'crepic imcrea ',expand(name)
            call oops
         end if
         i = 3
         call imopen (expand(name), i, imid(jmno), ier)
c     print*,'crepic imopen',ier
         if (ier .ne. 0) then
            call stupid ('crepic B: '//message('imh', ier))
c     print*,'crepic imopen ', expand(name)
            call oops
         end if
         call imhcpy (imid(imno), imid(jmno), ier)
         if (ier .ne. 0) then
            call stupid ('crepic C: '//message('imh', ier))
c     print*,'crepic imhcpy '
            call oops
         end if
c     print*,'crepic imhcpy',ier
      else
         call ftgiou (j, ier)
c     print*,'crepic ftgiou',j,ier
         if (ier .ne. 0) then
            call stupid ('crepic D: '//message('fits', ier))
c     print*,'crepic ftgiou'
            return
         end if
         imid(jmno) = j
         name = expand(extend(pictur(jmno), imgtyp(jmno)))
c     print*,'crepic ftinit', j, '"'//name//'"'
         call ftinit (j, name, lblock, ier)
c     print*,'after ftinit',lblock,ier
         if (ier .ne. 0) then
            call stupid ('crepic E: '//message('fits', ier))
c     print*,'crepic ftinit',j,name,lblock,ier
            return
         end if
c     print*,'crepic ftcopy',imid(imno),j
c     print*,imno,' "'//pictur(imno)//'"'
         name = expand(extend(pictur(imno), imgtyp(imno)))
         if (access(name, 'r') .ne. 0) then
            call stupid ('Error copying '//pictur(imno)//
     .           ' to '//pictur(jmno))
            ier = 99
            return
         end if
         ier = 0
         call ftcopy (imid(imno), j, 0, ier)
c     print*,'after ftcopy', ier
         if (ier .ne. 0) then
            call stupid ('crepic F: '//message('fits', ier))
c     print*, pictur(imno), j
c     print*,'crepic ftcopy',imid(imno),j,ier
c           return
         end if
c
c Do we need to change the image dimensions or data type?
c
         ier = 0
         if ((ncol(imno) .ne. ncol(jmno)) .or.
     .       (nrow(imno) .ne. nrow(jmno)) .or.
     .       (dattyp(imno) .ne. dattyp(jmno))) then
c     print*,'need to change something'
            if (dattyp(jmno) .eq. 'SHRT') then
               i = 16
            else if (dattyp(jmno) .eq. 'LONG') then
               i = 32
            else if (dattyp(jmno) .eq. 'REAL') then
               i = -32
            else
               ier = 1
               return
            end if
            call ftrsim (j, i, 2, axlen, ier)
            if (ier .ne. 0) then
               call stupid ('crepic F: '//message('fits', ier))
               call oops
            end if
c           call ftppre (j, 1, axlen(1)*axlen(2), 1, x, ier)
c     print*,'after ftppre',j,i,axlen, ' ier ', ier
            call ftflus (j, ier)
            if (ier .ne. 0) then
               call stupid ('crepic G: '//message('fits', ier))
               call oops
            end if
         end if
      end if
c
c     print*,'leaving crepic'
      if (ier .ne. 0) then
         call stupid ('crepic H: '//message(imgtyp(imno), ier))
      end if
      return
      end
c
c=======================================================================
c
      subroutine  clpic  (imno, ier)
c
c This routine closes the image file associated with the LUN contained 
c in the array element imid(imno).
c
      include 'arrays.inc'
      character message*80
c     print*,'clpic'
c
      ier = 0
      i = imid(imno)
      if (i .le. 0) return
      if (imgtyp(imno) .eq. 'imh') then
         call imclos (i, ier)
         if (ier .ne. 0) then
            call stupid ('clpic: '//message('imh', ier))
c     print*,'clpic imclos ', pictur(imno)
            call oops
         end if
      else
         call ftclos (i, ier)
         if (ier .ne. 0) then
            call stupid ('clpic: '//message('fits', ier))
c     print*,'clpic ftclos ', pictur(imno)
            call oops
         end if
         call ftfiou (i, ier)
         if (ier .ne. 0) then
            call stupid ('clpic: '//message('fits', ier))
c     print*,'clpic ftfiou ', pictur(imno)
            call oops
         end if
      end if
c
      if (ier .ne. 0) then
         call stupid ('clpic: '//message(imgtyp(imno), ier))
c     write (6,*), 'leaving clpic', pictur(imno)
      end if
      imid(imno) = 0
      return
      end
c
c=======================================================================
c
      subroutine  imdele  (name, ier)
      character name*(*), keywrd*(*), commnt*(*)
      double precision d
      real row(*)
      integer axlen(*)
      entry imgl2r (i, row, j, k)
      entry imclos (i, j)
      entry imgkwr (i, keywrd, x, j)
      entry imgkwd (i, keywrd, d, j)
      entry imgkwc (i, keywrd, name, j)
      entry imakwc (i, keywrd, name, commnt, j)
      entry imgkwi (i, keywrd, j, k)
      entry imcrea (name, axlen, i, j, k)
      entry imgsiz (i, axlen, j, k, l)
      entry imgs2r (i, row, j, k, l, m, n)
      entry imps2r (i, row, j, k, l, m, n)
      entry impl2r (i, row, j, k)
      entry imhcpy (i, j, k)
      entry imemsg (i, name)
      call stupid ('IRAF format not supported')
      call oops
      entry imopen (name, i, j, k)
      call stupid ('FITS image not found.')
      k = -1
      return
      end
