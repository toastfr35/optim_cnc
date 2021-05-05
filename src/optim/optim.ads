with stock;

package optim is

   procedure optimize_gcode (S1, S2 : in out stock.t_stock'Class; filename1, filename2 : String; tool_diameter : Float);

end optim;
