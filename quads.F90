Module Quads
    ! A module for drawing untextured Quads
    Use opengl_gl
    Use opengl_glu
    Use opengl_glut

    Implicit None

    Type, Public :: Quad
        Real, Private                   :: cx, cy, w, h, theta, r, g, b, a
        Real, Private                   :: rot(1:2,1:2)
        Real, Private, Dimension(1:2,1) :: bl, tl, tr, br
        
        Contains 
        Private
            Procedure, Public :: draw, set_rotation, set_position, init => init_quad
            Procedure, Public :: get_br, get_w, get_c, get_wh
    End Type Quad

    Contains

        Real Function get_w(q)
            Class(Quad), Intent(In   ) :: q
            get_w = q%w
        End Function

        Function get_br(q) Result(br)
            Class(Quad), Intent(In   ) :: q
            Real, Dimension(1:2)       :: br
            br = q%br(:,1)
        End Function

        Function get_c(q) Result(c)
            Class(Quad), Intent(In   ) :: q
            Real, Dimension(1:2)       :: c
            c = (/q%cx, q%cy/)
        End Function

        Function get_wh(q) Result(wh)
            Class(Quad), Intent(In   ) :: q
            Real, Dimension(1:2)       :: wh
            wh = (/q%w, q%h/)
        End Function

        Subroutine init_quad(q, c, wh, theta, colour)
            Class(Quad), Intent(InOut)                 :: q
            Real,        Intent(In   ), Dimension(1:2) :: c, wh
            Real,        Intent(In   )                 :: theta
            Real,        Intent(In   ), Dimension(1:4) :: colour

            q%cx = c(1)
            q%cy = c(2)
            q%w = wh(1)
            q%h = wh(2)
            q%theta = theta
            q%r = colour(1)
            q%g = colour(2)
            q%b = colour(3)
            q%a = colour(4)
            Call q%set_rotation(theta)
            Call q%set_position(c)

        End Subroutine init_quad

        Subroutine draw(q)
            Class(Quad), Intent(InOut) :: q
            Call glBegin(GL_QUADS)
                Call glColor4f(q%r,q%g,q%b,q%a)
                Call glVertex2f(q%bl(1,1), q%bl(2,1))
                Call glVertex2f(q%tl(1,1), q%tl(2,1))
                Call glVertex2f(q%tr(1,1), q%tr(2,1))
                Call glVertex2f(q%br(1,1), q%br(2,1))
            Call glEnd()
        End Subroutine draw

        Subroutine set_rotation(q, theta)
            Class(Quad), Intent(InOut) :: q
            Real,        Intent(In   ) :: theta

            q%theta = theta

            q%rot(1,1) = Cos(theta)
            q%rot(1,2) = -Sin(theta)
            q%rot(2,1) = -q%rot(1,2)
            q%rot(2,2) = q%rot(1,1)
        End Subroutine set_rotation

        Subroutine set_position(q, c)
            Class(Quad),          Intent(InOut) :: q
            Real, Dimension(1:2), Intent(In   ) :: c

            q%cx = c(1)
            q%cy = c(2)

            q%bl(:,1) = (/-0.5*q%w, -0.5*q%h/)
            q%bl = MatMul(q%rot, q%bl)
            q%bl(:,1) = q%bl(:,1)+(/q%cx, q%cy/)

            q%tl(:,1) = (/-0.5*q%w,  0.5*q%h/)
            q%tl = MatMul(q%rot, q%tl)
            q%tl(:,1) = q%tl(:,1)+(/q%cx, q%cy/)

            q%tr(:,1) = (/ 0.5*q%w,  0.5*q%h/)
            q%tr = MatMul(q%rot, q%tr)
            q%tr(:,1) = q%tr(:,1)+(/q%cx, q%cy/)

            q%br(:,1) = (/ 0.5*q%w, -0.5*q%h/)
            q%br = MatMul(q%rot, q%br)
            q%br(:,1) = q%br(:,1)+(/q%cx, q%cy/)
        End Subroutine set_position
End Module Quads