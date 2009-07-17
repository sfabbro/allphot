c
c#######################################################################
c#######################################################################
c
c              Official DAO version:  2004 January 9
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
      character expand*132, name*132, message*80, extend*30
c
      ier = 0
      name = expand(extend(pictur(jmno), imgtyp(jmno)))
      if (imgtyp(jmno) .eq. 'imh') then
         call imdele (name, ier)
      else
         call delfil (imid(imno), name, ier)
      end if
      if (ier .ne. 0) then
         call stupid (message(imgtyp(jmno), ier))
      else
         ncol(jmno) = 0
      end if
      return
      end
c
c=======================================================================
c
      subroutine  object  (imno, name)
      include 'arrays.inc'
      character*(*) name
c
      call sget (imno, 'title', name)
      if (name .eq. 'ERROR!') call sget (imno, 'TARGNAME', name)
      if (name .eq. 'ERROR!') call sget (imno, 'OBJECT', name)
      if (name .eq. 'ERROR!') call sget (imno, 'COMMENT', name)
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
      character function message*80 (imgtyp, ier)
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
      subroutine  jget  (imno, keywrd, j)
      include 'arrays.inc'
      character comm*80, keywrd*8
      integer j
c
      if (imgtyp(imno) .eq. 'imh') then
         call imgkwi (imid(imno), keywrd, j, ier)
      else
         ier = 0
         call ftgkyj (imid(imno), keywrd, j, comm, ier)
      end if
      if (ier .ne. 0) j = -1073741824
      return
      end
c
c=================================================================
c
      subroutine  rget  (imno, keywrd, r)
      include 'arrays.inc'
      character comm*80, keywrd*8
c
      if (imgtyp(imno) .eq. 'imh') then
         call imgkwr (imid(imno), keywrd, r, ier)
      else
         ier = 0
         call ftgkye (imid(imno), keywrd, r, comm, ier)
      end if
c
      if (ier .ne. 0) r = -1.1E19
      return
      end
c
c=================================================================
c
      subroutine  sget  (imno, keywrd, s)
      include 'arrays.inc'
      character s*80, comm*80, keywrd*8
c
      if (imgtyp(imno) .eq. 'imh') then
         call imgkwc(imid(imno), keywrd, s, ier)
      else
         ier = 0
         call ftgkys (imid(imno), keywrd, s, comm, ier)
      end if
c
      if (ier .ne. 0) s = 'ERROR!'
      return
      end
c
c=================================================================
c
      subroutine  sput  (imno, keywrd, s, comm, ier)
      include 'arrays.inc'
      character s*(*), comm*(*), keywrd*(*)
c
      if (imgtyp(imno) .eq. 'imh') then
         call imakwc (imid(imno), keywrd, s, comm, ier)
      else
         ier = 0
         call ftpkys (imid(imno), keywrd, s, comm, ier)
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
         call imgs2r (imid(imno), rdpixl, ix, ix, iy, iy, ier)
      else
         ier = 0
         ifirst = (iy-1)*n + ix
         call ftgpve (imid(imno), 1, ifirst, 1, -1.01e19, 
     .        rdpixl, bad, ier)
      end if
      if (ier .ne. 0) then
         call stupid (message(imgtyp(imno), ier))
         write (6,*) ' imgs2r in rdpixl', ix, iy
         rdpixl = -1.01e19
      end if
      return
      end
c     
c=======================================================================
c
      subroutine  rdrow  (imno, iy, row, ier)
      include 'arrays.inc'
c
      character message*80
      real row(*)
      logical bad
c
      n = ncol(imno)
      if (imgtyp(imno) .eq. 'imh') then
         call imgl2r (imid(imno), row, iy, ier)
      else
         ier = 0
         ifirst = (iy-1)*n+1
         call ftgpve (imid(imno), 1, ifirst, n, -1.01e19, row, bad, ier)
      end if
c
      if (ier .ne. 0) then
         call stupid (message(imgtyp(imno), ier))
         write (6,*) ' imgl2r in rdrow',imno,imid(imno),ncol(imno),iy
      end if
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
      logical bad
c
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
         ier = 0
         ifirst = (iy-1)*n+1
         call ftppre (imid(imno), 1, ifirst, n, -1.01e19, row, bad, ier)
      end if
c
      if (ier .ne. 0) then
         call stupid (message(imgtyp(imno), ier))
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
c
c Make sure the subarray is wholly contained within the image.
c Redefine array corners if necessary.
c
      id = imid(imno)
      mx = lx+mx-1
      my = ly+my-1
      lx = max(1,lx)
      ly = max(1,ly)
      mx = min(ncol(imno),mx)
      my = min(nrow(imno),my)
      my = my-ly+1
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
      else
         mx = mx-lx+1
         n = ncol(imno)
         ifirst = (j-2)*n + lx
         jy = ly-1
         do j=1,my
            ifirst = ifirst + n
            jy = jy + 1
            call ftgpve (id, 1, ifirst, mx, -1.01e19, func(1,j),
     .           bad, ier)
            if (ier .ne. 0) then 
               call stupid ('rdaray: '//message(imgtyp(imno),ier))
               write (6,*) 'rdaray:  x =', lx, ' to', mx, 
     .              '  y =', jy
            end if
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
      logical bad
c
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
               write (6,*) 'wraray:  x =', lx, ' to', mx, 
     .              '  y =', jy
            end if
         end do 
         mx = mx-lx+1
      else
         mx = mx-lx+1
         n = ncol(jmno)
         ifirst = (j-2)*n + lx
         jy = ly-1
         do j=1,my
            ifirst = ifirst + n
            jy = jy + 1
            call ftppre (id, 1, ifirst, mx, -1.01e19, func(1,j),
     .           bad, ier)
            if (ier .ne. 0) then 
               call stupid ('wraray: '//message(imgtyp(jmno),ier))
               write (6,*) 'wraray:  x =', lx, ' to', mx, 
     .              '  y =', jy
            end if
         end do 
      end if
      return
      end
c
c=======================================================================
c
      subroutine rdsect (imno, ly, my, dat, maxdat, ier)
      include 'arrays.inc'
c
c maxdat is the x dimension of the data array, which can be larger than
c the image.
c
      real dat(maxdat,*)
c
      character message*80
      logical bad
c
      ly = max0(1,ly)
      my = min0(nrow(imno),my)
      nx = ncol(imno)
c
      if (imgtyp(imno) .eq. 'imh') then
         call imgs2r (imid(imno), dat(1,ly), 1, nx, ly, my, ier)
      else
         ier = 0
         i = (ly-1)*nx+1
         n = (my-ly+1)*nx
         call ftgpve (imid(imno), 1, i, n, -1.01e19, 
     .        dat(1,ly), bad, ier)
      end if
c
      if (ier .ne. 0) then
         call stupid (message(imgtyp(imno), ier))
         write (6,*) pictur(imno), ' imgs2r in rdsect',
     .        ly, my, nx, maxdat
         call oops
      end if
c
c Unwrap the data
c
      if ((my .gt. ly) .and. (nx .lt. maxdat)) then
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
      subroutine wrsect (imno, ly, my, dat, maxdat, ier)
      include 'arrays.inc'
c
c maxdat is the x-dimension of the data array in the calling program,
c which can be different from the x-dimension of the image.
c
      real dat(maxdat,*)
c
      character message*80
      logical bad
c
      nx = ncol(imno)
      ly = max0(1,ly)
      my = min0(nrow(imno), my)
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
      if ((my .gt. ly) .and. (nx .lt. maxdat)) then
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
         ier = 0
         i = (ly-1)*nx+1
         n = (my-ly+1)*nx
         call ftppre (imid(imno), 1, i, n, -1.01e19, 
     .      dat(1,ly), bad, ier)
      end if
c
      if (ier .ne. 0) then
         call stupid (message(imgtyp(imno), ier))
         write (6,*) 'wrsect ', imno, ly, my, nx, maxdat
         call oops
      end if
      return
      end
c
c=======================================================================
c
      subroutine  attach  (name, nx, ny)
      include 'arrays.inc'
      character switch*30, name*30
      if (switch(name, ' ') .eq. ' ') 
     .     call getnam ('Input image name:', name)
      pictur(1) = name
      imgtyp(1) = ' '
      dattyp(1) = ' '
      call opnpic (1, 'R', ier)
      if (ier .eq. 0) then
         nx = ncol(1)
         ny = nrow(1)
      else
         nx = 0
         ny = 0
      end if
      return
      end
c
c=======================================================================
c
      subroutine  opnpic  (imno, mode, ier)
c
c Open the pre-existing image whose filename is contained in the array
c element PICTUR(IMNO)
c
      include 'arrays.inc'
c
c Arrays and functions
c
      character expand*132, switch*30, extend*30
      integer axlen(7)
c
c Variables
c
      character name*132, mode*1
      integer lblock
      common /block/ lblock
c
c Executable
c
c If image is already open (i.e., it has a logical unit number) return
c
      if (imid(imno) .ne. 0) return
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
      if (imgtyp(imno) .ne. ' ') then
         if (imgtyp(imno) .eq. 'imh') then
            name = extend(pictur(imno), 'imh')
            go to 4000
         else if (imgtyp(imno) .eq. 'fit') then
            name = extend(pictur(imno), 'fit')
            go to 3000
         else if (imgtyp(imno) .eq. 'fits') then
            name = extend(pictur(imno), 'fits')
            go to 2000
         else
            name = extend(pictur(imno), imgtyp(imno))
            go to 1000
         end if
      end if
c
c Never before opened.  Look for a filename extension.
c
      name = pictur(imno)
      l = length(name)
      if (name(l-3:l) .eq. '.imh') go to 4000
c 
c It could be a FITS file.  Get a free LUN just in case, and
c define R/W mode.
c
      call ftgiou (imid(imno), ier)
      if (mode .eq. 'W') then
         j = 1
      else
         j = 0
      end if
c
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
      go to 2000
c
 1000 continue
      call ftopen (imid(imno), expand(name), mode, lblock, ier)
      if (ier .eq. 0) then
         imgtyp(imno) = name(i+1:l)
         go to 5000
      end if
      call stupid ('Apparently not a standard image format:  '//
     .     pictur(imno))
      ier = 7
      return
c
 2000 continue
      call ftopen (imid(imno), expand(name), mode, lblock, ier)
      if (ier .eq. 0) then
c
c '.fits' worked
c
         imgtyp(imno) = 'fits'
         go to 5000
      end if
c
c '.fits' didn't work.  Try '.fit'
c
      name = switch(name, '.fit')
c
 3000 continue
      call ftopen (imid(imno), expand(name), mode, lblock, ier)
      if (ier .eq. 0) then
c
c '.fit' worked
c
         imgtyp(imno) = 'fit'
         go to 5000
      end if
c
c '.fit' didn't work.  Try '.imh' and redefine R/W mode.
c
      name = switch(name, '.imh')
c
 4000 if (mode .eq. 'W') then
         j = 3
      else
         j = 1
      end if
      call imopen (expand(name), j, imid(imno), ier)
      if (ier .eq. 0) then
c
c '.imh' worked
c
         imgtyp(imno) = 'imh'
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
         ier = 6
         return
      end if
c
 5000 call jget (imno, 'NAXIS', naxis)
      if (naxis .le. 0) then
         ier = 2
         return
      end if
c
      call jget (imno, 'NAXIS1', ncol(imno))
      if (ncol(imno) .le. 0) then
         ier = 3
         return
      end if
c
      if (naxis .eq. 1) then
         nrow(imno) = 1
      else
         call jget (imno, 'NAXIS2', nrow(imno))
         if (nrow(imno) .le. 0) then
            ier = 4
            return
         end if
      end if
c
      call jget (imno, 'BITPIX', naxis)
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
      ier = 0
      return
      end
c
c=======================================================================
c
      subroutine  coppic  (file, ier)
      include 'arrays.inc'
      character file*30
      pictur(2) = file
      ncol(2) = ncol(1)
      nrow(2) = nrow(1)
      imgtyp(2) = imgtyp(1)
      dattyp(2) = dattyp(1)
      call crepic (1, 2, ier)
      return
      end
c
c=======================================================================
c
      subroutine  crepic  (imno, jmno, ier)
      include 'arrays.inc'
c
      character expand*132, name*132, extend*30
      integer axlen(2)
c
      character message*80
      common /block/ lblock
c
c Tuck the image dimensions away someplace safe, lest the delpic
c erase them.
c
      axlen(1) = ncol(jmno)
      axlen(2) = nrow(jmno)
      if (imid(jmno) .ne. 0) then
         call stupid ('The output image is already open!')
         call oops
      end if
c
c If an image of this name exists, delete it.
c
      call opnpic (jmno, 'W', ier)
      if (ier .eq. 0) then
         call clpic (jmno, ier)
         call delpic (jmno, ier)
      end if
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
         call imcrea (expand(name), axlen, 2, i, ier)
         if (ier .ne. 0) then
            call stupid (message('imh', ier))
            call oops
         end if
         i = 3
         call imopen (expand(name), i, imid(jmno), ier)
         if (ier .ne. 0) then
            call stupid (message('imh', ier))
            call oops
         end if
         call imhcpy (imid(imno), imid(jmno), ier)
      else
         k = 0
         call ftgiou (j, ier)
         imid(jmno) = j
         k = 0
         name = expand(extend(pictur(2), imgtyp(1)))
         call ftinit (j, name, lblock, ier)
         call ftcopy (imid(imno), j, 0, ier)
c
c Do we need to change the image dimensions or data type?
c
         if ((ncol(imno) .ne. ncol(jmno)) .or.
     .       (nrow(imno) .ne. nrow(jmno)) .or.
     .       (dattyp(imno) .ne. dattyp(jmno))) then
            if (dattyp(jmno) .eq. 'SHRT') then
               i = 16
            else if (dattyp(jmno) .eq. 'LONG') then
               i = 32
            else if (dattyp(jmno) .eq. 'REAL') then
               i = -32
            else
               ier = 1
            end if
            call ftrsim (j, i, 2, axlen, ier)
         end if
c        call ftphpr (j, .true., bitpix, 2, axlen, 0, 1, 
c    .        .false., ier)
      end if
c
      if (ier .ne. 0) then
         call stupid (message(imgtyp(imno), ier))
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
c
      i = imid(imno)
      if (i .le. 0) return
      if (imgtyp(imno) .eq. 'imh') then
         call imclos (i, ier)
      else
         call ftclos (i, ier)
      end if
c
      if (ier .ne. 0) then
         call stupid (message(imgtyp(imno), ier))
         write (6,*), pictur(imno), ' imclos in clpic'
      end if
      imid(imno) = 0
      return
      end
c
c=======================================================================
c
      subroutine ftgerr (i, comm)
      character comm*(*), comm2*(*), key*(*)
      real dat(*)
      integer idat(*)
      logical bad
      entry ftgkyj (i, key, j, comm, k)
      entry ftgkye (i, key, a, comm, j)
      entry ftgkys (i, key, comm, comm2, j)
      entry ftpkys (i, key, comm, comm2, j)
      entry ftgpve (i, j, k, l, a, b, bad, m)
      entry ftppre (i, j, k, l, a, dat, bad, m)
      entry ftinit (i, comm, j, k)
      entry ftcopy (i, j, k, l)
      entry ftrsim (i, j, k, idat, l)
      entry ftclos (i, j)
      call stupid ('FITS format not supported')
      call oops
      end
      subroutine ftopen (i, comm, key, j, k)
      character comm*(*), key*(*)
      entry ftgiou (i, k)
      k = -1
      return
      end
