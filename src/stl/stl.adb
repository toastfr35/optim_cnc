with Interfaces.C;
with Gnat.OS_Lib;
with Ada.Text_IO;
with System;

package body stl is

     
   ---------------------------------------------------
   --
   ---------------------------------------------------   
   procedure create_stl (filename : String) is
      procedure c_create_stl (filename : Interfaces.C.char_array);      
      pragma Import (C, c_create_stl, "create_stl");         
      success : Boolean;
   begin
      if Gnat.OS_Lib.Is_Regular_File (filename) then
         Gnat.OS_Lib.Delete_File (filename, success);
         pragma Assert (success);
      end if;
      c_create_stl (Interfaces.C.To_C (filename));
   end create_stl;
            

   ---------------------------------------------------
   --
   ---------------------------------------------------   
   procedure add_facet (ax,ay,az,bx,by,bz,cx,cy,cz : Float) is
      
      procedure c_write_float (v : Interfaces.C.C_float);      
      pragma Import (C, c_write_float, "write_float");         

      procedure c_write_0_byte;      
      pragma Import (C, c_write_0_byte, "write_0_byte");         
      
      procedure write_float (v : Float) is
      begin
         c_write_float (Interfaces.C.C_float (v));
      end;
      
   begin      
      -- normal vector 0.0, 0.0, 0.0
      for i in 1 .. 3 loop
         write_float (0.0);
      end loop;
      write_float (ax);
      write_float (ay);
      write_float (az);
      
      write_float (bx);
      write_float (by);
      write_float (bz);
      
      write_float (cx);
      write_float (cy);
      write_float (cz);
      
      c_write_0_byte;
      c_write_0_byte;
   end add_facet;
      
   
   ---------------------------------------------------
   --
   ---------------------------------------------------   
   procedure close_stl is
      procedure c_close_stl;
      pragma Import (C, c_close_stl, "close_stl");   
   begin
      c_close_stl;
   end close_stl;   

end stl;

