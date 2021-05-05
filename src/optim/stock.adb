with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with gcode.reader;
with moves;
with stl;

package body stock is

   procedure free is new Ada.Unchecked_Deallocation (t_arr, t_arr_p);


   type t_pos is record
      x,y  : Integer;
   end record;

   function "<" (a,b : t_pos) return Boolean is
   begin
      if a.x = b.x then
         return a.y < b.y;
      else
         return a.x < b.x;
      end if;
   end "<";

   package p_pos_set is new Ada.Containers.Ordered_Sets (t_pos, "<", "=");
   subtype t_pos_set is p_pos_set.Set;
   package p_tools is new Ada.Containers.Ordered_Maps (Float, t_pos_set, "<", p_pos_set."=");

   tools : p_tools.Map;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function to_float (v : Integer) return Float is
   begin
      return Float(v) / f_resolution;
   end to_float;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function to_index (v : Float) return Natural is
   begin
      return Natural (v * f_resolution);
   end to_index;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function to_depth (v : Float) return Integer is
   begin
      return Integer (v * f_resolution);
   end to_depth;


   function get_max_X (S : t_stock'Class) return Float is
   begin
      return to_float (S.max_x);
   end get_max_X;

   function get_max_Y (S : t_stock'Class) return Float is
   begin
      return to_float (S.max_y);
   end get_max_Y;

   ---------------------------------------------------
   --
   ---------------------------------------------------
   function new_tool (diameter : Float) return t_pos_set is
      use Ada.Numerics.Elementary_Functions;
      R : t_pos_set;
      radius : Float := diameter/2.0;
      x, y : Float;
      pos : t_pos;

      function to_index (v : Float) return Integer is
      begin
         return Integer (v * f_resolution);
      end to_index;

   begin
      while radius > 0.0 loop
         for angle in 0 .. 359 loop
            x := radius * Cos(Float(angle));
            y := radius * Sin(Float(angle));
            pos.x := to_index(x);
            pos.y := to_index(y);
            R.Include (pos);
         end loop;
         radius := radius - 0.05;
      end loop;
      return R;
   end new_tool;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function get_tool (diameter : Float) return t_pos_set is
   begin
      if not tools.Contains (diameter) then
         tools.Insert (diameter, new_tool (diameter));
      end if;
      return tools.Element (diameter);
   end get_tool;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function new_stock (x,y : Float) return t_stock is
      R : t_stock;
   begin
      R.max_x := Natural(x+10.0) * resolution;
      R.max_y := Natural(y+10.0) * resolution;
      R.d := new t_arr (0 .. R.max_x, 0 .. R.max_y);
      for i in 0 .. R.max_x loop
         for j in 0 .. R.max_y loop
            R.d(i,j) := 0;
         end loop;
      end loop;
      return R;
   end new_stock;

   function new_stock (S : t_stock) return t_stock is
      R : t_stock;
   begin
      R.max_x := S.max_x;
      R.max_y := S.max_y;
      R.d := new t_arr (0 .. R.max_x, 0 .. R.max_y);
      for i in 0 .. R.max_x loop
         for j in 0 .. R.max_y loop
            R.d(i,j) := S.d(i,j);
         end loop;
      end loop;
      return R;
   end new_stock;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   procedure reset (S : t_stock'Class) is
   begin
      for i in 0 .. S.max_x loop
         for j in 0 .. S.max_y loop
            S.d(i,j) := 0;
         end loop;
      end loop;
   end reset;


   pci, pcj : Natural := 0;
   pck : Integer := 10;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function add (n : Natural; i : Integer) return Natural is
      R : Integer := i + n;
   begin
      if R < 0 then
         return 0;
      else
         return R;
      end if;
   end add;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   procedure cut (S : t_stock'Class; x,y,z,d : Float) is
      ci, cj, i, j : Natural;
      ck : Integer;
      k : Integer;
   begin

      if z >= 0.0 then
         return;
      end if;

      ci := to_index (x);
      cj := to_index (y);
      ck := to_depth (z);

      if ci=pci and cj=pcj and ck=pck then
         -- same as last position
         return;
      end if;

      pci := ci;
      pcj := cj;
      pck := ck;

      for pos of get_tool (d) loop
         i := add (to_index(x) , pos.x);
         j := add (to_index(y) , pos.y);
         k := to_depth (z);
         if S.d (i,j) > k then
            S.d (i,j) := k;
         end if;
      end loop;
   end cut;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function check_interference (S : t_stock'Class; x,y,z,d : Float) return Boolean is
      ci, cj, i, j : Natural;
      ck : Integer;
      k : Integer;
   begin

      if z >= 0.0 then
         return True;
      end if;

      ci := to_index (x);
      cj := to_index (y);
      ck := to_depth (z);

      if ci=pci and cj=pcj and ck=pck then
         -- same as last position (already reported interference)
         return False;
      end if;

      pci := ci;
      pcj := cj;
      pck := ck;

      for pos of get_tool (d) loop
         i := add (to_index(x) , pos.x);
         j := add (to_index(y) , pos.y);
         k := to_depth (z);
         if S.d (i,j) > k then
            return True;
         end if;
      end loop;

      return False;
   end check_interference;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   function get_interference_Z (S : t_stock'Class; x,y,z,d : Float) return Float is
      ci, cj, i, j : Natural;
      ck : Integer;
      nz, zz : Float := Float'First;
   begin
      if z >= 0.0 then
         return Float'First; -- no interference
      end if;

      ci := to_index (x);
      cj := to_index (y);
      ck := to_depth (z);

      if ci=pci and cj=pcj and ck=pck then
         -- same as last position (already reported interference)
         return Float'First;
      end if;

      pci := ci;
      pcj := cj;
      pck := ck;

      for pos of get_tool (d) loop
         i := add (to_index(x) , pos.x);
         j := add (to_index(y) , pos.y);
         nz := to_float (S.d (i,j));
         zz := Float'Max (z, nz);
      end loop;

      return zz;
   end get_interference_Z;



   ---------------------------------------------------
   --
   ---------------------------------------------------
   procedure cut_linear (S : in out t_stock'Class; from, to : moves.t_position; tool_diameter : Float) is
      use moves;
      steps : t_positions := linear (from, to);
   begin
      for step of steps loop
         stock.cut (S, step.x, step.y, step.z, tool_diameter);
      end loop;
   end cut_linear;

   ---------------------------------------------------
   --
   ---------------------------------------------------
   procedure cut_circular (S : in out t_stock'Class; from, to : moves.t_position; i,j : Float; tool_diameter : Float) is
      use moves;
      steps : t_positions := circular (from, to, i, j, True);
   begin
      for step of steps loop
         stock.cut (S, step.x, step.y, step.z, tool_diameter);
      end loop;
   end cut_circular;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   procedure cut_gcode (S : in out t_stock'Class; filename : String; tool_diameter : Float) is
      use gcode.reader;
      use moves;
      gcode_moves : t_moves;
      cur_pos : t_position := (0.0,0.0,0.0);
      target_pos : t_position;
      steps : t_positions;
      count : Natural := 0;
      reduced_tool_diameter : constant Float := tool_diameter; -- - (1.0 / f_resolution);
   begin
      gcode_moves := gcode.reader.read_file (filename);
      if S.d = null then
         S := stock.new_stock (gcode_moves.max_x, gcode_moves.max_y);
      end if;

      for move of gcode_moves.moves loop
         target_pos := (move.x, move.y, move.z);
         case move.move_type is
            when 0 | 1 =>
               steps := linear (cur_pos, target_pos);
            when 2 =>
               steps := circular (cur_pos, target_pos, move.i, move.j, clockwise => True);
            when 3 =>
               steps := circular (cur_pos, target_pos, move.i, move.j, clockwise => False);
            when others => pragma Assert (False);
         end case;

         for step of steps loop
            stock.cut (S, step.x, step.y, step.z, reduced_tool_diameter);
         end loop;

         cur_pos := target_pos;
         count := count + 1;
      end loop;
   end cut_gcode;


   ---------------------------------------------------
   --
   ---------------------------------------------------
   procedure to_stl (S : t_stock'Class; filename : String) is
      use stl;
      res : float := 1.0 / Float (resolution);
      done : t_arr_p := new  t_arr (0 .. S.max_x, 0 .. S.max_y);

      procedure add_square (i,j : Natural; di,dj : Natural) is
         f_x1, f_y1,f_x2, f_y2 : Float;
      begin
         f_x1 := to_float (i);
         f_y1 := to_float (j);
         f_x2 := to_float (i+di);
         f_y2 := to_float (j+dj);
         add_facet (f_x1, f_y1, to_float (S.d(i,j)),
                    f_x1, f_y2, to_float (S.d(i,j+dj)),
                    f_x2, f_y1, to_float (S.d(i+di,j))
                   );
         add_facet (f_x1, f_y2, to_float (S.d(i,j+dj)),
                    f_x2, f_y2, to_float (S.d(i+di,j+dj)),
                    f_x2, f_y1, to_float (S.d(i+di,j))
                   );
         for ii in i .. i+di-1 loop
            for jj in j .. j+dj-1 loop
               done (ii,jj) := 1;
            end loop;
         end loop;
      end add_square;

      procedure find_square (i,j : Natural; di, dj : out Natural) is
         ref_z : Integer := S.d (i,j);
         extended : Boolean;

         function check (di,dj : Natural) return Boolean is
         begin
            for ii in i .. i+di loop
               for jj in j .. j+dj loop
                  if ii > S.max_x or else
                    jj > S.max_y or else
                    S.d (ii,jj) /= ref_z
                  then
                     return False;
                  end if;
               end loop;
            end loop;
            return True;
         end check;

      begin
         di := 1;
         dj := 1;
         loop
            extended := False;

            -- extend square on X
            if i+di+1 < S.max_x then
               if not check (di+1,dj) then
                  exit;
               end if;
               di := di + 1;
               extended := True;
            end if;

            -- extend square on Y
            if j+dj+1 < S.max_y then
               if not check (di,dj+1) then
                  exit;
               end if;
               dj := dj + 1;
               extended := True;
            end if;

            exit when di = 2 * resolution;
            exit when dj = 2 * resolution;
            exit when not extended;
         end loop;
      end find_square;


      di, dj : Natural;

   begin
      --Ada.Text_IO.Put_Line ("TO_STL" & S.max_x'Img & S.max_y'Img);

      -- init done array
      for i in 0 .. S.max_x loop
         for j in 0 .. S.max_y loop
            done(i,j) := 0;
         end loop;
      end loop;

      stl.create_stl (filename);

      for i in 0 .. S.max_x-1 loop
         for j in 0 .. S.max_y-1 loop
            if done (i,j) = 0 then
               find_square (i,j,di,dj);
               add_square (i,j,di,dj);
            end if;
         end loop;
      end loop;
      close_stl;
      free (done);
   end to_stl;




end stock;

