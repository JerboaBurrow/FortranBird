Module gl_f90_mod

    Use opengl_gl
    Use opengl_glu
    Use opengl_glut

    Use Perlin

    ! Google it, it's a lovely feature of Fortran...
    Implicit None
    
    Integer(GLint)       :: wW = 512, wH = 512
    Integer(glcint)      :: window
    Integer, Allocatable :: cells(:,:)
    Integer              :: i, j, ni, nj, h, px = 0, py = 0

    Real                 :: tic, toc

    Type(PerlinGenerator) :: gen
    
    Contains

    Subroutine updateBackground()
        Call glBegin(GL_POINTS)
        Do i = 1,wW
            Do j = 1,wH
                !cells(i, j) = Sin(gen%sample(i+px, j+py)) > 0.5
                If (cells(i,j) == 1) Then
                    Call glColor3f(0.7,0.7,0.7)
                Else
                    Call glColor3f(0.0,0.0,0.0)
                End if
                Call glVertex2f(i-1.0,j-1.0)
            End Do
        End Do
        Call glEnd()
    End Subroutine
    
    Subroutine display() bind(c)
        Call cpu_time(toc)
        Call glViewport(0_GLint,0_GLint,wW,wH)
        Call glMatrixMode(GL_PROJECTION)
        Call glLoadIdentity()
        Call gluOrtho2D(0.0_gldouble,wW+0.0_gldouble,0.0_gldouble,wH+0.0_gldouble)
        Call glClearColor(1.0,1.0,1.0,1.0)
        Call glClear(GL_COLOR_BUFFER_BIT)
        Call glColor3f(1.0,0.0,0.0)
        
        Call updateBackground()

        Call glFlush()

        Call glutPostRedisplay()

        Call cpu_time(tic)
        Print *, tic-toc
        Do while (tic-toc < 1.0/60.0)
            Call cpu_time(tic)
        End Do
        
    End Subroutine display
    
    Subroutine key(ikey, x, y) bind(c)
        Integer(GLbyte), VALUE :: ikey
        Integer(GLint), VALUE :: x, y
        Select Case(ikey)
          Case (119) 
            ! w
            py = py + 1
          Case (97)
            ! a
            px = px - 1
          Case (115)
            ! s
            py = py - 1
          Case (100)
            ! d  
            px = px + 1
          Case (27)
            Stop
        End Select
    End Subroutine
    
    Subroutine init()
        Integer(GLenum) type
        
        Call gen%init()

        Call glutInitWindowSize(wW,wH)
        Call glutInit()
        type = GLUT_RGB
        type = ior(type,GLUT_SINGLE)
        Call glutInitDisplayMode(type)
        window = glutCreateWindow("GL and Fortran!")
        Call glutDisplayFunc(display)
        Call glutKeyboardFunc(key)
           
        Call glutMainLoop()
    
    End Subroutine init
    
End module gl_f90_mod
    
Program main
    Use opengl_gl
    Use opengl_glut
    Use gl_f90_mod

    Allocate(cells(wW,wH))

    Do i = 1,wW
        Do j = 1,wH
            cells(i,j) = rand()>0.5
        End Do
    End Do

    Call init()
End Program main
    