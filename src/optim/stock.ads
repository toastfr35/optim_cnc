with moves;

package stock is

   -- 10th of millimeter resolution
   resolution : constant Natural := 10;
   f_resolution : constant Float := Float (resolution);

   type t_stock is tagged private;

   function new_stock (x,y : Float) return t_stock;
   function new_stock (S : t_stock) return t_stock;

   function get_max_X (S : t_stock'Class) return Float;
   function get_max_Y (S : t_stock'Class) return Float;

   procedure reset (S : t_stock'Class);

   procedure to_stl (S : t_stock'Class; filename : String);

   procedure cut (S : t_stock'Class; x,y,z,d : Float);

   function check_interference (S : t_stock'Class; x,y,z,d : Float) return Boolean;

   function get_interference_Z (S : t_stock'Class; x,y,z,d : Float) return Float;

   procedure cut_gcode (S : in out t_stock'Class; filename : String; tool_diameter : Float);

   procedure cut_linear (S : in out t_stock'Class; from, to : moves.t_position; tool_diameter : Float);

   procedure cut_circular (S : in out t_stock'Class; from, to : moves.t_position; i,j : Float; tool_diameter : Float);

private

   type t_arr is array (Natural range<>, Natural range<>) of Integer;

   type t_arr_p is access t_arr;

   type t_stock is tagged record
      max_x, max_y : Integer;
      d : t_arr_p;
   end record;

end stock;
