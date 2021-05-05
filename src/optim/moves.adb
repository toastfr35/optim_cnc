with Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;
with stock;

package body moves is

   ---------------------------------------------------
   --
   ---------------------------------------------------
   function to_string (pos : t_position) return String is
   begin
      return "(" & pos.x'Img & "," & pos.y'Img & "," & pos.z'Img & ")";
   end to_string;

   ---------------------------------------------------
   --
   ---------------------------------------------------
   function distance (from, to : t_position) return Float is
      use Ada.Numerics.Elementary_Functions;
      dx : constant Float := to.x - from.x;
      dy : constant Float := to.y - from.y;
      dz : constant Float := to.z - from.z;
      function square (a : Float) return Float is (a*a);
   begin
      return sqrt (square(dx) + square(dy) + square(dz));
   end distance;

   function square (a : Float) return Float is (a*a);

   ---------------------------------------------------
   --
   ---------------------------------------------------
   function linear (from, to : t_position) return t_positions is
      use Ada.Numerics.Elementary_Functions;
      R : t_positions;
      dx : constant Float := to.x - from.x;
      dy : constant Float := to.y - from.y;
      dz : constant Float := to.z - from.z;
      distance : constant Float := sqrt (square(dx) + square(dy) + square(dz));
      steps : constant Natural := Natural (distance * stock.f_resolution);

      function sdx (step : Natural) return Float is (from.x + dx * Float(step) / Float(steps));
      function sdy (step : Natural) return Float is (from.y + dy * Float(step) / Float(steps));
      function sdz (step : Natural) return Float is (from.z + dz * Float(step) / Float(steps));

   begin
      R.Append ((from.x, from.y, from.z));
      for step in 1 .. steps-1 loop
         R.Append ((sdx(step), sdy(step), sdz(step)));
      end loop;
      R.Append ((to.x, to.y, to.z));
      return R;
   end linear;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function circular (from, to : t_position; i,j : Float; clockwise : Boolean) return t_positions is
      use stock;
      use Ada.Numerics.Elementary_Functions;
      R : t_positions;

      sq_radius : constant Float := square(i) + square(j);
      radius : constant Float := sqrt (sq_radius);
      angle_step : constant Float := (1.0 / (f_resolution*2.0)) / radius;
      Xc : constant Float := from.x + i;
      Yc : constant Float := from.y + j;
      a0, a1, da, x, y, angle : Float;

      procedure angles (xc,yc,x0,y0,x1,y1 : Float; a0,a1 : out Float) is

         function angle (xc,yc,x,y : Float) return Float is
            a : Float;
         begin
            a := Arctan (y-yc,x-xc);
            if a < 0.0 then
               a := a + 2.0 * Pi;
            end if;
            return a;
         end angle;

      begin
         a0 := angle (xc,yc,x0,y0);
         a1 := angle (xc,yc,x1,y1);
         if a0 <= a1 then
            a0 := a0 + 2.0 * Pi;
         end if;
      end angles;

   begin
      pragma Assert (from.z = to.z); -- only XY plane supported
      pragma Assert (clockwise);
      --Ada.Text_IO.Put_Line ("!!!G2" & to_string (from) & " -> " & to_string (to) & " : " & i'Img & " " & j'Img);
      angles (Xc,Yc, from.x,from.y, to.x,to.y, a0,a1);
      --Ada.Text_IO.Put_Line ("Angles:" & a0'Img & " ->" & a1'Img);
      da := a0-a1;
      --Ada.Text_IO.Put_Line ("Delta =" & da'Img & "  step =" & angle_step'Img);
      R.Append ((from.x, from.y, from.z));
      angle := a0;
      loop
         angle := angle - angle_step;
         exit when angle <= a1;
         x := Xc + radius * Cos(angle);
         y := Yc + radius * Sin(angle);
         --Ada.Text_IO.Put_Line ("angle:" & angle'Img & " = " & x'Img & "," & y'Img);
         R.Append ((x, y, from.z));
      end loop;

      R.Append ((to.x, to.y, to.z));

      return R;
   end circular;


end moves;
