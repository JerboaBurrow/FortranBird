# Linux building

Build with ```make```

Clean build files with ```make clean```

[](screen.png)

Standard OpenGL libs will be needed, and also free GLUT. On Ubuntu you can do

```apt-get install freeglut3-dev```

___

# Windows (mingw from linux) building

### Good luck...

- You may need to symlink some gl libs in ```/usr/x86_64-w64-mingw32/lib```

```sudo apt-get install gfortran-mingw-w64-x86-64-posix```

```make clean && COMPILER=x86_64-w64-mingw32-gfortran-posix make```

___

### Thanks

- Anthony Stone and Aleksandar Donev for f03gl https://www-stone.ch.cam.ac.uk/pub/f03gl/index.xhtml