--with System.Traceback.Symbolic;
with Ada.Exceptions;
with Ada.Exceptions.Traceback;
with Ada.Text_IO; use Ada.Text_IO;
with gcode.reader; use gcode.reader;
with stl; use stl;
with stock; use stock;
with moves; use moves;
with optim; use optim;

procedure main is
   S1, S2 : t_stock;
begin

   Put_Line ("Cut");
   cut_gcode (S1, "cyprus_1.gco", 6.35);
   Put_Line ("Cut");
   cut_gcode (S1, "cyprus_2.gco", 3.175);
   Put_Line ("Cut");
   cut_gcode (S1, "cyprus_3.gco", 1.0);
   Put_Line ("Cut");
   cut_gcode (S1, "cyprus_4.gco", 1.0);
   Put_Line ("Cut");
   cut_gcode (S1, "cyprus_5.gco", 6.35);

   S1.to_stl ("test.stl");


   return;

   Put_Line ("Cut");
   cut_gcode (S1, "ice_1.gco", 6.37);
   S2 := new_stock (S1);
   cut_gcode (S2, "ice_2.gco", 3.175);
   Put_Line ("Cut done");

   S1.to_stl ("test.stl");
   S2.to_stl ("test2.stl");

   Put_Line ("Optimize");
   optimize_gcode (S1, S2, "ice_2.gco", "out.gco", 3.175);
   Put_Line ("Optimize done");


exception
   when e : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (e));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (e));
      --Ada.Text_IO.Put_Line (System.Traceback.Symbolic.Symbolic_Traceback (Ada.Exceptions.Traceback.Tracebacks (e)));
end main;
