with Ada.Containers.Vectors;

package gcode.reader is

   type t_move is record
      line_num : Natural;
      move_type : Natural;
      x,y,z,i,j : Float;
      force_keep : Boolean := False;
   end record;

   package p_moves is new Ada.Containers.Vectors (Natural, t_move);

   type t_moves is record
      moves : p_moves.Vector;
      min_x, max_x, min_y, max_y : Float := 0.0;
   end record;

   function read_file (filename : String) return t_moves;

end gcode.reader;
