C
C#######################################################################
C#######################################################################
C
C              Official DAO version:  2004 April 16
C
C#######################################################################
C#######################################################################
C
      subroutine  delpic  (jmno, ier)
C
C This routine operates on the filename contained in the array element
C pictur(jmno).  The image is assumed to be closed before it is deleted.
C
      include 'arrays.inc'
C
      character expand*132, name*132, message*80, extend*40
C
      ier = 0
      name = expand(extend(pictur(jmno), imgtyp(jmno)))
      call ftgiou (j, ier)
      call delfil (j, name, ier)
      ier = 0
      call ftfiou (j, ier)
      if (ier .ne. 0) then
         call stupid ('delpic: '//message(imgtyp(jmno), ier))
c     print*,ier
      else
         imid(jmno) = 0
      end if
      return
      end
C
C=======================================================================
C
      subroutine  object  (imno, name)
      include 'arrays.inc'
      character name*(*), commnt*72, switch*40
C
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
C
C#######################################################################
C#######################################################################
C
C           The rest of this file contains all the calls to 
C                     FITSIO and IMFORT routines.
C
C#######################################################################
C#######################################################################
C
      character function  message  *80  (imgtyp, ier)
      character error*80, imgtyp*4
      call ftgerr (ier, error)
      message = error
      return
      end
C
C=================================================================
C
      subroutine  jget  (imno, keywrd, j, commnt)
      include 'arrays.inc'
      character message*80, commnt*(*), keywrd*8
      integer j
C
      ier = 0
      call ftgkyj (imid(imno), keywrd, j, commnt, ier)
      if (ier .ne. 0) then
         call stupid ('jget: '//message(imgtyp(imno), ier))
         j = -1073741824
      end if
      return
      end
C
C=================================================================
C
      subroutine  rget  (imno, keywrd, r, commnt)
      include 'arrays.inc'
      character commnt*(*), keywrd*8
C
      ier = 0
      call ftgkye (imid(imno), keywrd, r, commnt, ier)
C
      if (ier .ne. 0) r = -1.1E19
      return
      end
C
C=================================================================
C
      subroutine  dget  (imno, keywrd, r, commnt)
      include 'arrays.inc'
      character message*80, commnt*(*), keywrd*8
      double precision r
C
      ier = 0
      call ftgkyd (imid(imno), keywrd, r, commnt, ier)
C
      if (ier .ne. 0) then
         call stupid ('dget: '//message(imgtyp(imno), ier))
         r = -1.1D38
      end if
      return
      end
C
C=================================================================
C
      subroutine  sget  (imno, keywrd, s, commnt)
      include 'arrays.inc'
      character s*(*), commnt*(*), keywrd*8
C
      ier = 0
      call ftgkys (imid(imno), keywrd, s, commnt, ier)
C
      if (ier .ne. 0) s = 'ERROR!'
      return
      end
C
C=================================================================
C
      subroutine  sput  (imno, keywrd, s, commnt, ier)
      include 'arrays.inc'
      character s*(*), commnt*(*), keywrd*(*)
C
      ier = 0
c     print*,'a ',imgtyp(imno)
c     print*,'aa ',imid(imno), keywrd, ier
      call ftdkey (imid(imno), keywrd, ier)
c     print*,'b',ier, s
      ier = 0
      call ftpkys (imid(imno), keywrd, s, commnt, ier)
c     print*,'c',ier
C
      if (ier .ne. 0) s = 'ERROR!'
      return
      end
C
C=======================================================================
C
      real  function  rdpixl (imno, ix, iy)
      include 'arrays.inc'
      character message*80
      logical bad
C
      n = ncol(imno)
      ifirst = (iy-1)*n + ix
      ier = 0
      call ftgpve (imid(imno), 1, ifirst, 1, -1.01e19, 
     .     rdpixl, bad, ier)
      if (ier .ne. 0) then
         call stupid ('ftgpve: '//message(imgtyp(imno), ier))
         write (6,*) ' ftgpve in rdpixl', imno, imid(imno), ix, iy
         write (6,*) pictur(imno), ier
         rdpixl = -1.01e19
         CALL OOPS
      end if
      return
      end
C     
C=======================================================================
C
      subroutine  rdrow  (imno, iy, row, show, ier)
      include 'arrays.inc'
C
      character message*80
      real row(*)
      integer ier
      logical bad, show
C
      ier =0
c     if (show) print*,'in rdrow',ier
      n = ncol(imno)
      ifirst = (iy-1)*n+1
c     if (show) print*,'y',imno, imid(imno), ifirst, n, ier
      call ftgpve (imid(imno), 1, ifirst, n, -1.01e15, row, 
     .     bad, ier)
c     if (show) print*,'Y ',bad,ier
      if (ier .ne. 0) then
         write (6,*) ' ftgpve in rdrow',imno,imid(imno),ncol(imno),
     .        iy, ier
         call stupid ('rdrow: '//message(imgtyp(imno), ier))
         CALL OOPS
      end if
C
      return
      end
C     
C=======================================================================
C
      subroutine  wrrow  (imno, iy, row, ier)
      include 'arrays.inc'
C
      character message*80
      real row(*)
c     print*,'wrrow'
C
      ier = 0
      n = ncol(imno)
C
      if (dattyp(imno) .eq. 'SHRT') then
         do i=1,ncol(imno)
            row(i) = max(-32768., min(32767., anint(row(i))))
         end do
      else if (dattyp(imno) .eq. 'LONG') then
         do i=1,ncol(imno)
            row(i) = anint(row(i))
         end do
      end if
C
      ifirst = (iy-1)*n+1
      call ftppre (imid(imno), 1, ifirst, n, row, ier)
C
      if (ier .ne. 0) then
         call stupid ('wrrow: '//message(imgtyp(imno), ier))
         write (6,*) ' wrrow', iy
      end if
      return
      end
C
C=======================================================================
C
      subroutine rdaray (imno, lx, ly, mx, my, maxbox, func, ier)
C
C lx, ly are the pixel coordinates of the corner of the subarray
C     func in the image
C mx, my are the dimensions of the subarray func to be extracted from
C     from the image
C maxbox is the stated x-dimension of the subarray func in the calling
C     routine
C
      include 'arrays.inc'
      character message*80
      real func(maxbox,*)
      logical bad
c     print*,'rdaray'
C
C Make sure the subarray is wholly contained within the image.
C Redefine array corners if necessary.
C
      ier = 0
      id = imid(imno)
      mx = lx+mx-1
      my = ly+my-1
      lx = max(1,lx)
      ly = max(1,ly)
C
C lx, ly are now the lower limits of the subarray in the big image
C
      mx = min(ncol(imno),mx)
      my = min(nrow(imno),my)
C
C mx, my are now the upper limits of the subarray in the big image
C
      my = my-ly+1
C
C my is now the vertical size of the subarray
C
      mx = mx-lx+1
C
C mx is now the horizontal size of the subarray
C
      n = ncol(imno)
      jy = ly-1
      ifirst = jy*n + lx
      do j=1,my
         call ftgpve (id, 1, ifirst, mx, -1.01e19, func(1,j),
     .        bad, ier)
         if (ier .ne. 0) then 
            call stupid ('rdaray: '//message(imgtyp(imno),ier))
            write (6,*) 'rdaray ftgpve:  x =', lx, ' to', mx, 
     .           '  y =', jy+j
         end if
         ifirst = ifirst + n
      end do 
      return
      end
C
C=======================================================================
C
      subroutine wraray (jmno, lx, ly, mx, my, maxbox, func, ier)
C
C lx, ly are the pixel coordinates of the corner of the subarray
C     func in the image
C mx, my are the dimensions of the subarray func to be inserted into
C     the image
C maxbox is the stated x-dimension of the subarray func in the calling
C     routine
C
      include 'arrays.inc'
      character message*80
      real func(maxbox,*)
c     print*,'wraray'
C
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
C
C Make sure the subarray is wholly contained within the image.
C Redefine array corners if necessary.
C
      id = imid(jmno)
      mx = lx+mx-1
      my = ly+my-1
      lx = max(1,lx)
      ly = max(1,ly)
      mx = min(ncol(jmno),mx)
      my = min(nrow(jmno),my)
      my = my-ly+1
C
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
     .           '  y =', jy
         end if
      end do 
      return
      end
C
C=======================================================================
C
      subroutine  rdsect  (imno, ly, my, dat, maxcol, ier)
      include 'arrays.inc'
C
C maxcol is the x dimension of the data array, which can be larger than
C the image.
C
      real dat(maxcol,*)
C
      character message*80
      logical bad
C
      ier = 0
      ly = max0(1,ly)
      my = min0(nrow(imno),my)
      nx = ncol(imno)
C
      i = (ly-1)*nx+1
      n = (my-ly+1)*nx
      bad = .false.
      call ftgpve (imid(imno), 1, i, n, -1.01e19, 
     .     dat(1,ly), bad, ier)
C
      if (ier .ne. 0) then
         call stupid ('rdsect: '//message(imgtyp(imno), ier))
         write (6,*) pictur(imno), ' imgs2r in rdsect',
     .        ly, my, nx, maxcol
         call oops
      end if
C
C Unwrap the data
C
      if ((my .gt. ly) .and. (nx .lt. maxcol)) then
         do j=my,ly+1,-1
            do i=nx,1,-1
               dat(i,j) = dat((j-ly)*nx+i,ly)
            end do
         end do
      end if
      return
      end
C
C=======================================================================
C
      subroutine  wrsect  (imno, ly, my, dat, maxcol, ier)
      include 'arrays.inc'
C
C maxcol is the x-dimension of the data array in the calling program,
C which can be different from the x-dimension of the image.
C
      real dat(maxcol,*)
C
      character message*80
c     print*,'wrsect'
C
      ier = 0
      nx = ncol(imno)
      ly = max0(1,ly)
      my = min0(nrow(imno), my)
C
C ly, my are the vertical limits of the image section.  Complete
C rows will be written.
C
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
C
C Wrap the data.
C
      if ((my .gt. ly) .and. (nx .lt. maxcol)) then
         do j=ly+1,my
            do i=1,nx
               k = (j-ly)*nx + i
               dat(k,ly) = dat(i,j)
            end do
         end do
      end if
C
      i = (ly-1)*nx+1
      n = (my-ly+1)*nx
c     PRINT *,'before ftppre',imno,imid(imno),ly,my,nx,i,n,ier
      call ftppre (imid(imno), 1, i, n, dat(1,ly), ier)
c     PRINT *,'after ftppre',ier,ncol(imno),nrow(imno)
C
      if (ier .ne. 0) then
         call stupid ('wrsect: '//message(imgtyp(imno), ier))
         write (6,*) 'wrsect ', imno, ly, my, nx, maxcol
         call oops
      end if
      return
      end
C
C=======================================================================
C
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
C
C=======================================================================
C
      subroutine  opnpic  (imno, mode, show, ier)
C
C Open the pre-existing image whose filename is contained in the array
C element PICTUR(IMNO)
C
      include 'arrays.inc'
C
C Arrays and functions
C
      character expand*132, switch*40, extend*40
      integer axlen(7), access
C
C Variables
C
      character name*132, mode*1
      character message*80
      character commnt*72
      integer lblock
      logical show
      common /block/ lblock
C
C Executable
C
C
C REMEMBER TO UNCOMMENT CHARACTER MESSAGE ABOVE
C
      if(show)print*,'opnpic ',pictur(imno),mode
      ier = 0
C
C If image is already open (i.e., it has a logical unit number) return
C
      if (imid(imno) .ne. 0) then
         return
      end if
C 
C Get a free LUN and define R/W mode just in case it is a 
C FITS file.
C
      call ftgiou (imid(imno), ier)
      if(show)print*,'ftgiou',imid(imno),ier
      if ((imid(imno) .le. 0) .or. (ier .ne. 0)) then
         call stupid ('Unable to assign unit number.')
         ier = -7
         return
      end if
      if(show)print*,'ftgiou',imid(imno),ier
      if (mode .eq. 'W') then
         j = 1
      else
         j = 0
      end if
      if(show)print*,'mode ',j,'  ',imgtyp(imno)
C
C Consider five possibilities:
C
C     1. filename extension is .imh
C     2. filename extension is .fit
C     3. filename extension is .fits
C     4. filename extension is something else ==> fits format
C     5. filename extension is not specified
C        a.  try '.fits'
C        b.  try '.fit'
C        c.  try '.imh'
C
C If all this fails, give up.
C
C If imgtyp is non-blank, then the image has been opened before.
C
      if(show)print*,'imgtyp ',imgtyp(imno), ichar(imgtyp(imno)(1:1)),
     .     ichar(imgtyp(imno)(2:2)),ichar(imgtyp(imno)(3:3)),
     .     ichar(imgtyp(imno)(4:4))
      ier = 0
      if (imgtyp(imno) .ne. ' ') then
         if(show)print*,'imgtyp defined:', imgtyp(imno)
         if (imgtyp(imno) .eq. 'imh ') then
            name = extend(pictur(imno), 'imh')
      if(show)print*,'go to 4000 ',name
            go to 4000
         else if (imgtyp(imno) .eq. 'fit ') then
            name = extend(pictur(imno), 'fit')
      if(show)print*,'go to 3000 ',name
            go to 3000
         else if (imgtyp(imno) .eq. 'fits') then
            name = extend(pictur(imno), 'fits')
      if(show)print*,'go to 2000 ',imid(imno),name
      if(show)print*,'go to 2000 ',name
            go to 2000
         else
            name = extend(pictur(imno), imgtyp(imno))
      if(show)print*,'go to 1000 ',name
            go to 1000
         end if
      end if
C
C Never before opened.  Look for a filename extension.
C
      name = pictur(imno)
      if(show)print*,'A ',name
      l = length(name)
      if (name(l-3:l) .eq. '.imh') then
         if (show) print*,'imh'
         go to 4000
      else
         if (show) print*,'not imh'
      end if
      if (name(l-3:l) .eq. '.fit') then
         if (show) print*,'fit'
         go to 3000
      else
         if (show) print*,'not fit'
      end if
      if (name(l-4:l) .eq. '.fits') then
         if (show) print*, 'fits go to 2000'
         go to 2000
      else
         if (show) print*, 'not fits'
      end if
C
C Not a standard filename extension.  Try to find  a non-standard one.
C
      do i=l,1,-1
         if (name(i:i) .eq. '.') then
C
C Period was found.  Treat it as a FITS file.
C
            if (show) print*,'extension found: '//name(i+1:l)
            go to 1000
         end if
      end do
C
C No period was found in the filename, so there is no filename
C extension specified.  Try appending '.fits' and see whether
C that file exisits.
C
      name = name(1:l)//'.fits'
      if (access(expand(name), 'r') .eq. 0) then
         if (show) print*,'fits works'
         go to 2000
      else
         if (show) print*,name(1:l)//'.fits doesn''t work'
      end if
      name = switch(name, '.imh')
      if (access(expand(name), 'r') .eq. 0) go to 4000
      go to 1100
C
 1000 continue
      if(show)print*,1000
      call ftopen (imid(imno), expand(name), j, lblock, ier)
      if(show)print*,'ftopen1',ier
      if (ier .eq. 0) then
         imgtyp(imno) = name(i+1:l)
         go to 5000
      end if
 1100 call stupid ('Apparently not a standard image format:  '//
     .     pictur(imno))
      ier = 7
      return
C
 2000 continue
      if(show)print*,'2000',imid(imno),j,ier
      if(show)print*,name,ier
      if(show)print*,expand(name),ier
      if(show)print*,'ftopen ', pictur(imno), j
      if(show)print*,'ftopen ',imid(imno),j,lblock,ier
      if(show)print*,expand(name)
      call ftopen (imid(imno), expand(name), j, lblock, ier)
      if(show)print*,'ftopen', ier
      if (ier .ne. 0) then
         call stupid ('Unable to open '//pictur(imno))
         write (6,*) message('fits', ier)
         call oops
      end if
      if(show)print*,'ftopen',j,lblock,ier
      if (ier .eq. 0) then
      if(show)print*,'a fits worked'
C
C '.fits' worked
C
         imgtyp(imno) = 'fits'
         go to 5000
      else
      if(show)print*,'A fits didn''t work',ier
      if(show)print*,'B'
      end if
      if (imgtyp(imno) .ne. ' ') then
         imid(imno) = 0
         return
      end if
C
C '.fits' didn't work.  Try '.fit'
C
      name = switch(name, '.fit')
      if(show)print*,'Trying ',name
C
 3000 continue
      if(show)print*,3000
      ier = 0
      call ftopen (imid(imno), expand(name), j, lblock, ier)
      if(show)print*,'ftopen2',ier
      if (ier .eq. 0) then
C
C '.fit' worked
C
         imgtyp(imno) = 'fit'
         go to 5000
      end if
      if (imgtyp(imno) .ne. ' ') return
C
C '.fit' didn't work.  Try '.imh' and redefine R/W mode.
C
      name = switch(name, '.imh')
C
 4000 continue
      if(show)print*,4000
C
C Give back the FITS LUN.
C
      call ftfiou (imid(imno), ier)
      if(show)print*,'ftfiou', ier
      imid(imno) = 0
      if (mode .eq. 'W') then
         j = 3
      else
         j = 1
      end if
      if (show)print*,'imh unsuported in this version'
      call stupid (commnt)
      ncol(imno) = -1
      return
C
 5000 continue
      if(show)print*,5000
      call jget (imno, 'NAXIS   ', naxis, commnt)
      if (naxis .le. 0) then
         ier = 2
         return
      end if
C
      call jget (imno, 'NAXIS1  ', ncol(imno), commnt)
      if (ncol(imno) .le. 0) then
         ier = 3
         return
      end if
C
      if (naxis .eq. 1) then
         nrow(imno) = 1
      else
         call jget (imno, 'NAXIS2  ', nrow(imno), commnt)
         if (nrow(imno) .le. 0) then
            ier = 4
            return
         end if
      end if
C
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
      if(show)print*,'opnpic returning ier ',ier
      ier = 0
      return
      end
C
C=======================================================================
C
      subroutine  coppic  (file, ier)
      include 'arrays.inc'
      character file*40
c     print*,'coppic ', file
C
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
C
C=======================================================================
C
      subroutine  crepic  (imno, jmno, ier)
      include 'arrays.inc'
C
      character expand*132, name*132, extend*40
      integer axlen(2), access
C
      character message*80
      common /block/ lblock
C
C Tuck the image dimensions away someplace safe, lest the delpic
C erase them.
C
c     print*,'crepic'
      ier = 0
      axlen(1) = ncol(jmno)
      axlen(2) = nrow(jmno)
      if (imid(jmno) .gt. 0) then
         call stupid ('The output image is already open!')
         call oops
      end if
C
C If an image of this name exists, delete it.
C
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
C
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
     .        ' to '//pictur(jmno))
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
c     return
      end if
C
C Do we need to change the image dimensions or data type?
C
      ier = 0
      if ((ncol(imno) .ne. ncol(jmno)) .or.
     .     (nrow(imno) .ne. nrow(jmno)) .or.
     .     (dattyp(imno) .ne. dattyp(jmno))) then
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
         call ftflus (j, ier)
         if (ier .ne. 0) then
            call stupid ('crepic G: '//message('fits', ier))
            call oops
         end if
      end if
C
c     print*,'leaving crepic'
      if (ier .ne. 0) then
         call stupid ('crepic H: '//message(imgtyp(imno), ier))
      end if
      return
      end
C
C=======================================================================
C
      subroutine  clpic  (imno, ier)
C
C This routine closes the image file associated with the LUN contained 
C in the array element imid(imno).
C
      include 'arrays.inc'
      character message*80
c     print*,'clpic'
C
      ier = 0
      i = imid(imno)
      if (i .le. 0) return
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
C
      if (ier .ne. 0) then
         call stupid ('clpic: '//message(imgtyp(imno), ier))
c     write (6,*), 'leaving clpic', pictur(imno)
      end if
      imid(imno) = 0
      return
      end
