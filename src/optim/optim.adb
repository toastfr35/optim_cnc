with Ada.Text_IO;
with gcode.reader;
with moves;

package body optim is

   type t_distance is record
      required, skipped, orig : Float := 0.0;
   end record;

   type t_distances is array (0..3) of t_distance;

   ---------------------------------------------------
   --
   ---------------------------------------------------
   procedure optimize_gcode (S1, S2 : in out stock.t_stock'Class; filename1, filename2 : String; tool_diameter : Float) is
      use Ada.Text_IO;
      use gcode.reader;
      use moves;
      use stock;
      gcode_moves : t_moves;
      real_pos : t_position := (0.0,0.0,0.0);
      cur_pos : t_position := (0.0,0.0,0.0);
      first, last : t_position;
      target_pos : t_position;
      steps : t_positions;
      prev_step : t_position;
      count : Natural := 0;
      move_required : Boolean;
      move_unchanged : Boolean;
      distances : t_distances;
      distance : Float;
      int_distance : Natural;
      increased_tool_diameter : constant Float := tool_diameter; -- + (1.0 / stock.f_resolution);
      f_out : File_Type;
      active : Boolean := True;
      min_z : Float := 0.0;

      function to_gcode (axis : String; v : Float) return String is

         vv : Float := v;
         neg : Boolean := False;

         function to_int (v : Float) return Integer is
         begin
            if v < 0.0 then
               vv := -v;
               neg := True;
            end if;
            return Integer (Float'Floor(vv));
         end to_int;

         int : Integer := to_int(v);
         dec : Integer := Integer (Float'Floor ((vv-Float(int)) * 1000.0));
         dec_str : String (1..3) := "000";
      begin
         dec_str(3) := Character'Val (Character'Pos('0') +  (dec mod 10));
         dec := dec / 10;
         dec_str(2) := Character'Val (Character'Pos('0') +  (dec mod 10));
         dec := dec / 10;
         dec_str(1) := Character'Val (Character'Pos('0') +  (dec mod 10));

         declare
            int_str : constant String := int'Img;
            R : constant String := " " & axis & (if neg then "-" else "") & int_str(int_str'First+1..int_str'Last) & "." & dec_str;
         begin
            Put_Line ("#" & axis & " : " & v'Img & " -> " & R);
            return R;
         end;
      end to_gcode;

      procedure add_linear_move (command : String; target : t_position; comment : String ; line_num : Natural) is
         line : constant String := command & to_gcode ("X", target.x) & to_gcode ("Y", target.y) & to_gcode ("Z", target.z) & " ; " & comment & line_num'Img;
      begin
         Put_Line (">>" & line);
         Put_Line (f_out, line);
      end add_linear_move;

      procedure add_circular_move (command : String; target : t_position; i,j : Float; comment : String ; line_num : Natural) is
         line : constant String := command & to_gcode ("X", target.x) & to_gcode ("Y", target.y)
           & to_gcode ("I", i) & to_gcode ("J", j)
           & " ; " & comment & line_num'Img;
      begin
         Put_Line (">>" & line);
         Put_Line (f_out, line);
      end add_circular_move;


      procedure add_transfer (target : t_position; z_up : Float) is
      begin
         Put_Line (f_out, "G0" & to_gcode ("Z", z_up) & "; transfer"); -- up
         Put_Line (f_out, "G0" & to_gcode ("X", target.x) & to_gcode ("Y", target.y)& "; transfer"); -- across
         Put_Line (f_out, "G0" & to_gcode ("Z", target.z)& "; transfer"); -- down
      end add_transfer;

      function move_is_safe (S : t_stock'Class; from, to : t_position) return Boolean is
         steps : t_positions;
         R : Boolean := True;
      begin
         steps := linear (cur_pos, target_pos);
         for step of steps loop
            if S.check_interference (step.x, step.y, step.z, increased_tool_diameter) then
               --Ada.Text_IO.Put_Line ("!!!" & to_string (step));
               R := False;
               exit;
            end if;
         end loop;
         return R;
      end move_is_safe;

      function get_safe_Z (S : t_stock'Class; from, to : t_position) return Float is
         steps : t_positions;
         R : Boolean := True;
         Z : Float := Float'First;
         nZ : Float;
      begin
         steps := linear (cur_pos, target_pos);
         for step of steps loop
            nZ := S.get_interference_z (step.x, step.y, step.z, increased_tool_diameter);
            if nZ /= Float'First then
               Z := Float'Max (Z, nZ);
            end if;
         end loop;
         pragma Assert (Z /= Float'First);
         return Z + 1.0;
      end get_safe_Z;

   begin
      gcode_moves := gcode.reader.read_file (filename1);

      -- force keep 'n' move around G2
      for i in gcode_moves.moves.First_Index .. gcode_moves.moves.Last_Index loop
         if gcode_moves.moves.Element(i).move_type = 2 then
            declare
               a : Integer := Integer'Max (gcode_moves.moves.First_Index, i-2);
               b : Integer := Integer'Min (gcode_moves.moves.Last_Index, i+2);
               move : t_move;
            begin
               for j in a .. b loop
                  move := gcode_moves.moves.Element(j);
                  move.force_keep := True;
                  gcode_moves.moves.Replace_Element(j, move);
               end loop;
            end;
         end if;
      end loop;


      active := True;
      Create (f_out, Out_File, filename2);

      for move of gcode_moves.moves loop

         if move.z < min_z then
            Close (f_out);
            min_z := move.z;
            declare
               min_z_str : constant String := to_gcode ("", min_z);
            begin
               Create (f_out, Out_File, min_z_str & "_" & filename2);
            end;
         end if;

         if active then

            Put_Line ("---------------------------------");
            Put_Line ("-- LINE" & move.line_num'Img & "    " & move.move_type'Img & " "
                      & move.x'Img & " " & move.y'Img & " " & move.z'Img & " " & move.i'Img & " " & move.j'Img
                      & " " & move.force_keep'Img);
            target_pos := (move.x, move.y, move.z);
            steps.Clear;
            move_required := False;
            move_unchanged := False;

            case move.move_type is
            when 0 | 1 =>
               steps := linear (cur_pos, target_pos);
            when 2 =>
               steps := circular (cur_pos, target_pos, move.i, move.j, clockwise => True);
            when others =>
               pragma Assert (False);
            end case;

            distance := moves.distance (cur_pos, target_pos);
            int_distance := Natural (distance);
            distances(move.move_type).orig := distances(move.move_type).orig + distance;

            if cur_pos.z /= target_pos.z or cur_pos.z = 5.0 or target_pos.z = 5.0 or move.force_keep then
               -- vertical motion
               move_required := True;
               move_unchanged := True;
               first := cur_pos;
               last  := target_pos;
            else
               -- XY motion
               -- check each step of the move for interference with remaining material
               prev_step := steps.First_Element;
               for step of steps loop
                  if stock.check_interference (S1, step.x, step.y, step.z, increased_tool_diameter) then
                     -- this move is required

                     if distance < 10.0 or else move.move_type = 2 then
                        -- short or circular move (keep the entire move)
                        move_required := True;
                        first := cur_pos;
                        last  := target_pos;
                        move_unchanged := True;
                        exit;

                     else
                        -- long linear move, find the useful part
                        if not move_required then
                           move_required := True;
                           first.x := prev_step.x;
                           first.y := prev_step.y;
                           first.z := prev_step.z;
                        end if;
                        last.x := step.x;
                        last.y := step.y;
                        last.z := step.z;
                     end if;

                  end if;
                  prev_step := step;
               end loop;
            end if;

            if move_required then

               Ada.Text_IO.Put_Line ("");
               Ada.Text_IO.Put_Line ("O G" & move.move_type'Img & " " &  to_string (cur_pos) & " -> " & to_string (target_pos)
                                     & " O" & Natural'Image(Natural(moves.distance (cur_pos, target_pos))) & " /" & int_distance'Img);

               if real_pos /= first then
                  -- fast move to start of command
                  distances(0).required := distances(0).required + moves.distance (real_pos, first);
                  if move_is_safe (S1, real_pos, first) then
                     -- OK to move fast
                     add_linear_move ("G0", first, "FT", move.line_num);
                  else
                     Ada.Text_IO.Put_Line ("Warning: fast move error");
                     if move_is_safe (S2, real_pos, first) then
                        -- OK to move slow
                        add_linear_move ("G1", first, "ST", move.line_num);
                        S1.cut_linear (real_pos, first, tool_diameter);
                     else
                        Ada.Text_IO.Put_Line ("Warning: slow move error");
                        add_transfer (first, get_safe_Z (S2, real_pos, first));
                     end if;
                  end if;
                  Ada.Text_IO.Put_Line ("+ G 0 " & to_string (real_pos) & " -> " & to_string (first) & " +" & Natural'Image(Natural(moves.distance (real_pos, first))));
               end if;

               -- perform move
               distances(move.move_type).required := distances(move.move_type).required + moves.distance (first, last);
               if move_unchanged then
                  declare
                     cmd : String := move.move_type'Img;
                  begin
                     cmd(cmd'First) := 'G';
                     if move.move_type = 2 then
                        add_circular_move (cmd, target_pos, move.i, move.j, "orig" & (if move.force_keep then "FK" else ""), move.line_num);
                     else
                        add_linear_move (cmd, target_pos, "orig" & (if move.force_keep then "FK" else ""), move.line_num);
                     end if;
                  end;
               else
                  Ada.Text_IO.Put_Line ("K G" & move.move_type'Img & " " &  to_string (first) & " -> " & to_string (last)
                                        & " K" & Natural'Image(Natural(moves.distance (first, last))) & " /" & int_distance'Img);
                  add_linear_move ("G1", last, "shortened", move.line_num);
               end if;
               real_pos := last;
            else
               distances(move.move_type).skipped := distances(move.move_type).skipped + distance;
               Ada.Text_IO.Put_Line ("S G" & move.move_type'Img & " " & to_string (cur_pos) & " -> " & to_string (target_pos) & " S" & int_distance'Img);
            end if;

            for step of steps loop
               S1.cut (step.x, step.y, step.z, tool_diameter);
            end loop;

            cur_pos := target_pos;
            count := count + 1;

         end if;

      end loop;

      for i in distances'Range loop
         Ada.Text_IO.Put_Line ("Distance for G" & i'Img & " : " & distances(i).required'Img & " /" & distances(i).orig'Img);
      end loop;

      Close (f_out);

   end optimize_gcode;

end optim;
