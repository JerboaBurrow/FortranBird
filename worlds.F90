Module worlds

    Use Quads

    Implicit None

    Integer, Parameter                 :: cache = 16
    Real,    Parameter, Dimension(1:4) :: colour = (/1.0,0.0,0.0,1.0/)

    Type, Public :: World
        Real                                    :: minimum_space, height,&
                                                   min_width, max_width,&
                                                   min_height, max_height
        Type(Quad), Private, Dimension(1:cache) :: obstacles
        Contains

            Procedure, Public :: collided, generate, &
              update => update_world, draw => draw_world
    End Type

    Contains

        Subroutine draw_world(w)
            Class(World), Intent(InOut) :: w

            Integer :: i

            Do i = 1, cache
                Call w%obstacles(i)%draw()
            End Do
        End Subroutine draw_world

        Type(Quad) Function generate_obstacle(neighbour, space, height, &
          min_width, max_width, min_height, max_height)
            Type(Quad), Intent(In   ) :: neighbour
            Real,       Intent(In   ) :: space, height,&
                                         min_width, max_width, min_height, max_height

            Real, Dimension(1:2) :: c, wh

            c = neighbour%get_c()

            c(1) = c(1) + space + Rand()*space + neighbour%get_w()
            c(2) = Rand()*height

            wh(1) = Rand()*(max_width-min_width)+min_width
            wh(2) = Rand()*(max_height-min_height)+min_height

            Call generate_obstacle%init(&
              (/c(1), c(2)/),&
              (/wh(1), wh(2)/),&
              0.0,&
              colour)

        End Function generate_obstacle

        Subroutine generate(w, startx, space, height,&
              min_width, max_width, min_height, max_height)
            Class(World), Intent(InOut) :: w
            Real,         Intent(In   ) :: startx, space, height,&
                                           min_width, max_width,&
                                           min_height, max_height

            Integer :: i
            Real, Dimension(1:2) :: c, wh

            w%minimum_space = space
            w%height = height
            w%min_width = min_width
            w%max_width = max_width
            w%min_height = min_height
            w%max_height = max_height

            c(1) = c(1) + startx
            c(2) = Rand()*w%height

            wh(1) = Rand()*(w%max_width-w%min_width)+w%min_width
            wh(2) = Rand()*(w%max_height-w%min_height)+w%min_height

            Call w%obstacles(1)%init(&
              (/c(1), c(2)/),&
              (/wh(1), wh(2)/),&
              0.0,&
              colour)

            Do i = 2, cache
                w%obstacles(i) = &
                      generate_obstacle(w%obstacles(i-1),&
                      w%minimum_space, w%height,&
                      w%min_width, w%max_width, w%min_height, w%max_height)
            End Do
        End Subroutine generate

        Logical Function collided(w, player)
            Class(World), Intent(InOut) :: w
            Type(Quad)                  :: player

            Real    :: dcrit
            Integer :: i

            collided = .false.

            Do i = 1, cache
                dcrit = 0.5*(player%get_w()+w%obstacles(i)%get_w())
                If (Norm2(player%get_c()-w%obstacles(i)%get_c()) < dcrit) Then
                    collided = .true.
                    Exit
                End If
            End Do
    
        End Function collided

        Subroutine update_world(w, v)
            Class(World), Intent(InOut) :: w
            Real,         Intent(In   ) :: v(1:2)

            Integer              :: i, j
            Real, Dimension(1:2) :: br, c

            Do i = 1,cache
                br = w%obstacles(i)%get_br()
                c = w%obstacles(i)%get_c()
                c = c - v
                Call w%obstacles(i)%set_position(c)
                If (br(1) < w%obstacles(i)%get_w()) Then
                    ! off left side of the screen
                    Do j = i, cache-1
                        w%obstacles(j) = w%obstacles(j+1)
                    End Do
                    w%obstacles(cache) = &
                      generate_obstacle(w%obstacles(cache-1),&
                      w%minimum_space, w%height,&
                      w%min_width, w%max_width, w%min_height, w%max_height)
                End If
            End Do
        End Subroutine update_world

End Module worlds