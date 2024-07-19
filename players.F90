Module Players
    ! A module for drawing untextured Quads
    Use opengl_gl
    Use opengl_glu
    Use opengl_glut
    Use Quads, only: Quad

    Implicit None

    Type, Public :: Player
        Real, Private                   :: cx, cy, w, h, theta
        Real, Private                   :: rot(1:2,1:2)
        Type(Quad), Private             :: body, bb
        Real, Private, Dimension(1:2,3) :: wing, beak
        Real, Private, Dimension(1:4)   :: wing_colour, body_colour, beak_colour
        
        Contains 
        Private
            Procedure, Public :: draw, set_rotation, set_position, init => init_player
            Procedure, Public :: get_w, get_c, get_bb
    End Type Player

    Contains

        Subroutine init_player(q, c, wh, theta)
            Class(Player), Intent(InOut)                 :: q
            Real,          Intent(In   ), Dimension(1:2) :: c, wh
            Real,          Intent(In   )                 :: theta

            q%cx = c(1)
            q%cy = c(2)
            q%w = wh(1)
            q%h = wh(2)
            q%theta = theta
            q%beak_colour = (/1.0, 102.0/255.0, 0.0, 1.0/)
            q%body_colour = (/102.0/255.0, 204.0/255.0, 1.0, 1.0/)
            q%wing_colour = (/51.0/255.0, 153.0/255.0, 1.0, 1.0/)
            
            Call q%bb%init(c, wh, theta, (/1.0, 0.0, 0.0, 0.25/))
            Call q%body%init(c, wh*0.75, theta, q%body_colour)
            
            q%beak(:, 1) = (/c(1)+wh(1)*0.3, c(2)+wh(2)*0.45/)
            q%beak(:, 2) = (/c(1)+wh(1)*0.75, c(2)/)
            q%beak(:, 3) = (/c(1)+wh(1)*0.3, c(2)-wh(2)*0.45/)

            q%wing(:, 1) = (/c(1)-wh(1)*0.1, c(2)/)
            q%wing(:, 2) = (/c(1)+wh(1)*0.1, c(2)+wh(2)*0.2/)
            q%wing(:, 3) = (/c(1)+wh(1)*0.1, c(2)-wh(2)*0.2/)
            Call q%set_rotation(theta)
            Call q%set_position(c)

        End Subroutine init_player

        Type(Quad) Function get_bb(q)
        Class(Player), Intent(In   ) :: q
            get_bb = q%bb
        End Function

        Real Function get_w(q)
            Class(Player), Intent(In   ) :: q
            get_w = q%w
        End Function

        Function get_c(q) Result(c)
            Class(Player), Intent(In   ) :: q

            Real, Dimension(1:2) :: c
            c = (/q%cx, q%cy/)
        End Function

        Subroutine set_rotation(q, theta)
            Class(Player), Intent(InOut) :: q
            Real,          Intent(In   ) :: theta

            q%theta = theta


        End Subroutine set_rotation

        Subroutine draw(q)
            Class(Player), Intent(InOut) :: q
            Call q%body%draw()
            Call glBegin(GL_TRIANGLES)
                Call glColor4f(q%beak_colour(1), q%beak_colour(2), &
                    q%beak_colour(3), q%beak_colour(4))
                Call glVertex2f(q%beak(1,1), q%beak(2,1))
                Call glVertex2f(q%beak(1,2), q%beak(2,2))
                Call glVertex2f(q%beak(1,3), q%beak(2,3))
            Call glEnd()
            Call glBegin(GL_TRIANGLES)
                Call glColor4f(q%wing_colour(1), q%wing_colour(2), &
                    q%wing_colour(3), q%wing_colour(4))
                Call glVertex2f(q%wing(1,1), q%wing(2,1))
                Call glVertex2f(q%wing(1,2), q%wing(2,2))
                Call glVertex2f(q%wing(1,3), q%wing(2,3))
            Call glEnd()
        End Subroutine draw

        Subroutine set_position(q, c)
            Class(Player),          Intent(InOut) :: q
            Real, Dimension(1:2),   Intent(In   ) :: c
            Integer :: i
            
            Do i = 1, 3
                q%beak(:, i) = q%beak(:, i) + c - (/q%cx, q%cy/)
                q%wing(:, i) = q%wing(:, i) + c - (/q%cx, q%cy/)
            End Do

            q%cx = c(1)
            q%cy = c(2)
            Call q%body%set_position(c)
            Call q%bb%set_position(c)

        End Subroutine set_position
End Module