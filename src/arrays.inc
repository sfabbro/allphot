      integer maxpic
      parameter (maxpic=301)
      character pictur(maxpic)*30, dattyp(maxpic)*4, imgtyp(maxpic)*4
      integer imid(maxpic), ncol(maxpic), nrow(maxpic)
      common /strings/ pictur, imgtyp, dattyp
      common /values/ imid, ncol, nrow
