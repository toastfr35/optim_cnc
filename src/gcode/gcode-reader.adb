with Ada.Text_IO; use Ada.Text_IO;
with Gcode.Context; use Gcode.Context;
with Gcode.Parser;


package body gcode.reader is

   type t_Ctx is new GContext with record
      null;
   end record;

   Ctx : t_Ctx;

   ---------------------------------------------------
   --
   ---------------------------------------------------
   function read_file (filename : String) return t_moves is
      R : t_moves;
      f : File_Type;
      line_num : Natural := 0;
   begin
      Open (f, In_File, filename);
      while not End_Of_File (f) loop
         declare
            line : constant String := Get_Line (f);
         begin
            line_num := line_num + 1;
            --Put_Line (">>" & line);
            if Gcode.Parser.Parse (line, Ctx) then
               if Ctx.B ('G').Is_Set then
                  declare
                     Gnum : Natural := Natural (Float_Value'Floor (Ctx.B ('G').Value));
                     move : t_move;
                  begin
                     if Gnum in 0 .. 3 then
                        move.line_num := line_num;
                        move.move_type := Gnum;
                        move.x := Ctx.B ('X').Value;
                        move.y := Ctx.B ('Y').Value;
                        move.z := Ctx.B ('Z').Value;
                        if R.min_x > move.x then
                           R.min_x := move.x;
                        end if;
                        if R.min_y > move.y then
                           R.min_y := move.y;
                        end if;
                        if R.max_x < move.x then
                           R.max_x := move.x;
                        end if;
                        if R.max_y < move.y then
                           R.max_y := move.y;
                        end if;

                        if Gnum in 2 .. 3 then
                           move.i := Ctx.B ('I').Value;
                           move.j := Ctx.B ('J').Value;
                        else
                           move.i := 0.0;
                           move.j := 0.0;
                        end if;
                        R.moves.Append (move);
                     end if;

                  end;
               end if;
            else
               raise Constraint_Error;
            end if;
         end;
      end loop;
      Close (f);
      return R;
   end read_file;

end gcode.reader;
