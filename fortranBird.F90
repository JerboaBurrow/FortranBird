Module gl_f90_mod

    ! NB this version of GL has no shaders, I think... it some bizzare subset of ~2003 era GL.
    !   we are making a game in Fortran, all bets are off
    Use opengl_gl
    Use opengl_glu
    Use opengl_glut

    Use Quads, Only: Quad
    Use Worlds
    Use Players, Only: Player

    ! Google it, it's a lovely feature of Fortran...
    Implicit None
    
    Integer(GLint), Parameter :: wW = 512, wH = 512
    Integer(glcint)           :: window
    Integer                   :: frame = 0, score = 0
    Logical                   :: w_down = .false., a_down = .false.,&
                                 s_down = .false., d_down = .false.,&
                                 game_over = .false.
    
    Real                      :: vx = 10, vy = 10, ph = wW/8.0, pw = wH/8.0, &
                                 px = wW/8.0, py = wH/2.0, wvx = 2, wvy = 0
    Real                      :: tic, toc
    Real                      :: gy = 1.0, difficulty = 1.0

    Type(Player)              :: bird
    Type(World)               :: w
    
    Contains

    Subroutine draw_text(text, x, y, scale)
    
      Character(Len=*), Intent(In   ) :: text
      Real,             Intent(In   ) :: x, y, scale

      Integer :: i, p

      call glscalef(scale, scale, scale)
      Call glColor4f(0.0,0.0,0.0,0.0)
      Call glTranslatef(x, y, 0.0_glfloat)
      i=1
      Do while (i <= len(text))
        p = Ichar(text(i:i))
        If (i < len(text)+1 .and. text(i:i+1) == '\n') Then
            Call glTranslatef(-(i+1)*scale, -scale*1.5, 0.0_glfloat)
            i = i+2
            Cycle
        End If
        i = i+1
        Call glutStrokeCharacter(GLUT_STROKE_MONO_ROMAN, p)
      End Do
  
    End Subroutine draw_text

    Subroutine move(v)
        Real, Intent(In   ) :: v(1:2)

        Real :: new(1:2)

        new = (/px, py/) + v

        new(1) = Max(Min(new(1), Real(wW)-pw/2.0), pw/2.0)
        new(2) = Max(Min(new(2), Real(wH)-ph/2.0), ph/2.0)

        px = new(1)
        py = new(2)
    End Subroutine move

    Subroutine end_game()
      Character(Len=16) :: s
      Write (s, '(a, i0)') "Game over!", score
      Call w%draw()
      Call bird%draw()
      Call draw_text(s, 0.0, wH*0.5, 0.25)
    End Subroutine end_game

    Subroutine update()

      Real :: v(1:2)

      v = (/0.0, -Min(vy-1,gy*difficulty)/)

      If (w_down) Then
        v(2) = v(2) + vy
      End If

      If (a_down) Then
        v(1) = v(1) - vx
      End If

      If (s_down) Then
        v(2) = v(2) - vy
      End If

      If (d_down) Then
        v(1) = v(1) + vx
      End If

      If (Sum(v) /= 0.0) Then
        Call move(v)
      End If

      Call bird%set_position((/px, py/))
      Call w%update((/wvx*difficulty, wvy/))

      Call w%draw()
      Call bird%draw()
      game_over = w%collided(bird%get_bb())

    End Subroutine update
    
    Subroutine display() bind(c)

        Call glViewport(0_GLint,0_GLint,wW,wH)
        Call glMatrixMode(GL_PROJECTION)
        Call glLoadIdentity()
        Call glOrtho(0.0_gldouble,wW+0.0_gldouble,0.0_gldouble,wH+0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
        Call glClearColor(1.0,1.0,1.0,1.0)
        Call glClear(GL_COLOR_BUFFER_BIT)
        Call glColor3f(1.0,0.0,0.0)
        
        If (game_over) Then
          Call end_game()
        Else
          Call update()
          If (frame == 59) score = score + 1
        End If

        Call glFlush()

        Call glutPostRedisplay()

        Call cpu_time(tic)
        Do while (tic-toc < 1.0/60.0)
            Call cpu_time(tic)
        End Do
        Call cpu_time(toc)
        frame = Mod(frame + 1, 60)
        
    End Subroutine display

    Subroutine keyUp(ikey, x, y) bind(c)
      Integer(GLbyte), VALUE :: ikey
      Integer(GLint), VALUE :: x, y
      Select Case(ikey)
        Case (119) 
          ! w
          w_down = .false.
        Case (97)
          ! a
          a_down = .false.
        Case (115)
          ! s
          s_down = .false.
        Case (100)
          ! d  
          d_down = .false.
      End Select
    End Subroutine
    
    Subroutine keyDown(ikey, x, y) bind(c)
        Integer(GLbyte), VALUE :: ikey
        Integer(GLint), VALUE :: x, y
        Select Case(ikey)
          Case (119) 
            ! w
            w_down = .true.
          Case (97)
            ! a
            a_down = .true.
          Case (115)
            ! s
            s_down = .true.
          Case (100)
            ! d  
            d_down = .true.
          Case (27)
            Stop
        End Select
    End Subroutine
    
    Subroutine init()
        Integer(GLenum)      :: type
        Real                 :: t
        Integer, Allocatable :: seed(:)
        Integer              :: n

        Call random_seed(size=n)
        Allocate(seed(n))
        Call random_seed(get=seed)

        Call glutInitWindowSize(wW,wH)
        Call glutInit()
        type = GLUT_RGB
        type = ior(type,GLUT_SINGLE)
        Call glutInitDisplayMode(type)
        window = glutCreateWindow("Fortran Bird!")
        Call glutDisplayFunc(display)
        Call glutKeyboardFunc(keyDown)
        Call glutKeyboardUpFunc(keyUp)
           
        Call glutMainLoop()
    
    End Subroutine init
    
End module gl_f90_mod
    
Program main
    Use opengl_gl
    Use opengl_glut
    Use gl_f90_mod

    Call bird%init((/px, py/), (/pw, ph/), 0.0)
    Call w%generate(0.0, pw*2, Real(wH), pw, pw*3, 0.1*Real(wH), 0.75*Real(wH))

    Call init()
End Program main
    