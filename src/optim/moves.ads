with Ada.Containers.Vectors;

package moves is

   type t_position is record
      x,y,z : Float;
   end record;

   package p_positions is new Ada.Containers.Vectors (Natural, t_position);

   subtype t_positions is p_positions.Vector;

   function distance (from, to : t_position) return Float;

   function linear (from, to : t_position) return t_positions;

   function circular (from, to : t_position; i,j : Float; clockwise : Boolean) return t_positions;

   function to_string (pos : t_position) return String;

end moves;
