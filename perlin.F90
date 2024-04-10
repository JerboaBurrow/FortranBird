Module Perlin

    Implicit None

    Type, Public :: PerlinGenerator
        Integer :: seed = 314159, repeat = 256, size = 1930
        Real    :: turbulence = 0.07, xPeriod = 50.0, yPeriod = 5.0
        Integer :: table(0:1,0:255)

        Contains 
        
            Private

            Procedure, Public :: init => init_perlin
            Procedure         :: getValue, getTurbulence
            Procedure, Public :: sample

    End Type PerlinGenerator

    Contains

        Subroutine init_perlin(gen)
            Class(PerlinGenerator), Intent(InOut) :: gen

            Real :: randoms(0:gen%repeat-1)
            
            Call random_number(randoms)
            gen%table(0, :) = Mod(Int(randoms*gen%repeat), gen%repeat)

            Call random_number(randoms)
            gen%table(1, :) = Mod(Int(randoms*gen%repeat), gen%repeat)

        End Subroutine init_perlin

        Real Function sample(gen, x, y)
          Class(PerlinGenerator), Intent(InOut) :: gen
          Integer,    Intent(In   ) :: x, y

          Real :: u, v

          u = Real(x)*gen%xPeriod / Real(gen%size)
          v = Real(y)*gen%yPeriod / Real(gen%size)
  
          sample = u+v+gen%turbulence*gen%getTurbulence(Real(x), Real(y), gen%size, 0);
        End Function

        Real Function getValue(gen, x, y, t)
            Class(PerlinGenerator), Intent(InOut) :: gen
            Real,    Intent(In   ) :: x, y
            Integer, Intent(In   ) :: t 

            Real    :: xf, yf, trX, trY, tlX, tlY, brX, brY, blX, blY, gx, gy, &
                       dtr, dtl, dbr, dbl, u, v
            Integer :: ix, iy, vtr, vtl, vbr, vbl

            xf = Floor(x)
            yf = Floor(y)

            ix = Mod(Int(xf), gen%repeat)
            iy = Mod(Int(yf), gen%repeat)
            
            If (X < 0) ix = ix + gen%repeat
            If (Y < 0) ix = iy + gen%repeat

            xf = x-xf
            yf = y-yf

            trX = xf-1.0
            trY = yf-1.0
    
            tlX = xf
            tlY = yf-1.0
    
            brX = xf-1.0
            brY = yf
    
            blX = xf
            blY = yf

            vtr = gen%table(t,gen%table(t,Mod(Mod(ix+1,gen%repeat)+iy+1,gen%repeat)))
            vtl = gen%table(t,(gen%table(t,Mod(Mod(ix,gen%repeat)+(ix+1),gen%repeat))))
            vbr = gen%table(t,(gen%table(t,Mod(Mod((ix+1),gen%repeat)+iy,gen%repeat))))
            vbl = gen%table(t,(gen%table(t,Mod(Mod(ix,gen%repeat)+iy,gen%repeat))))

            Call gradient(vtr,gx,gy)
            dtr = trX*gx+trY*gy
            Call gradient(vtl,gx,gy)
            dtl = tlX*gx+tlY*gy
            Call gradient(vbr,gx,gy)
            dbr = brX*gx+brY*gy
            Call gradient(vbl,gx,gy)
            dbl = blX*gx+blY*gy

            u = smooth(xf)
            v = smooth(yf)

            getValue = lerp(u, lerp(v,dbl,dtl), lerp(v,dbr,dtr))*0.5+0.5

        End Function getValue

        Real Function getTurbulence(gen, x, y, size, table)
            Class(PerlinGenerator), Intent(InOut) :: gen
            Real,                   Intent(In   ) :: x, y
            Integer,                Intent(In   ) :: size, table

            Real :: scale
            
            getTurbulence = 0.0
            scale = size
            Do While (scale > 1.0)
                getTurbulence = getTurbulence + Abs(scale * gen%getValue(x/scale, y/scale, table))
                scale = scale / 2.0
            End Do
        End Function getTurbulence

        Real Function smooth(x)
            Real, Intent(In   ) :: x
            smooth = ((6.0*x-15.0)*x+10.0)*x*x*x
        End Function smooth

        Real Function lerp(x, a1, a2)
            Real, Intent(In   ) :: x, a1, a2
            lerp = a1+x*(a2-a1)
        End Function

        Subroutine gradient(v, cx, cy)
            Integer, Intent(In   ) :: v
            Real,    Intent(  Out) :: cx, cy

            Select Case (Mod(v, 4))
                Case (0)
                    cx = 1.0
                    cy = 1.0
                Case (1)
                    cx = -1.0
                    cy = 1.0
                Case (2)
                    cx = -1.0
                    cy = -1.0
                Case (3)
                    cx = 1.0
                    cy = -1.0
            End Select
        End Subroutine gradient

End Module Perlin