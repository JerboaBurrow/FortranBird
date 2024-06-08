#  Copyright 2009 Anthony Stone and Aleksandar Donev

#  This file is part of f03gl.
#
#  f03gl is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  any later version.
#
#  f03gl is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with f03gl (see file COPYING). If not, see
#  <http://www.gnu.org/licenses/>.



#  Comment out for 32-bit architecture
BITS      := 64

#  The directories containing the OpenGL libraries and GLUT libraries
# OGLLIBDIR := -L/usr/lib/x86_64-linux-gnu
# OGLLIBDIR := -L/usr/lib${BITS}

#  The directory containing the X11 libraries
# X11LIBDIR := -L/usr/X11R6/lib${BITS}
X11LIBDIR := -L/usr/lib/x86_64-linux-gnu

#  Libraries for OpenGL, including GLUT, GLU and OpenGL
#F90GLUTLIB := -lfreeglut -lGL -lGLU

# The X11 libraries
X11LIB     := -lX11 -lm

ifndef COMPILER
  COMPILER := gfortran
endif
FC=${COMPILER}
#  Gfortran
FFLAGS    := $(DEBUG) -fno-range-check -DOPENGL
LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR}
ifeq (${COMPILER},gfortran)
  LIBS := -lglut -lGL -lGLU ${X11LIB} -lpthread -ldl -lgfortran
  FONTS := GLUT_fonts.o
else
  LIBS := -lfreeglut -lGL -lGLU ${X11LIB} -lpthread -ldl -lgfortran
endif

#  Choose the appropriate definition of the GLUT variable to choose
#  GLUT, OpenGLUT or FreeGlut. It may be necessary to adjust the libraries.
# GLUT      := openglut
GLUT      := freeglut
# GLUT      := glut

VPATH=gl

LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib
MODULES    =  ${FONTS} OpenGL_gl.o OpenGL_${GLUT}.o OpenGL_glu.o perlin.o quads.o worlds.o

all: fortranBird

OpenGL_${GLUT}.o: OpenGL_gl.o

%.o: %.F90
	${FC} ${FFLAGS} -O3 -c $<

fortranBird : %: %.F90 ${MODULES} force
	${FC} ${FFLAGS} -O3 -c $<
	${FC} $@.o ${MODULES} ${LIBRARIES} ${LIBS} -o $@

OpenGL%.mod: OpenGL%.F90
	${FC} ${FFLAGS} -O3 -c $<

force:

OpenGL_${GLUT}.o OpenGL_glu.o: OpenGL_gl.o


clean:
	-rm -f *.mod *.o fortranBird
