-------------------------------------------------------------------------------
--                                                                           --
--                                   ACNC                                    --
--                                                                           --
--         Copyright (C) 2016 Fabien Chouteau (chouteau@adacore.com)         --
--                                                                           --
--                                                                           --
--    ACNC is free software: you can redistribute it and/or modify it        --
--    under the terms of the GNU General Public License as published by      --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    ACNC is distributed in the hope that it will be useful, but WITHOUT    --
--    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY     --
--    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public        --
--    License for more details.                                              --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with ACNC. If not, see <http://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Gcode.Error; use Gcode.Error;

package body Gcode.Context is

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Ctx : in out GContext; Msg : String) is
   begin
      Ctx.Error_Flag := True;
      raise Gcode_Exception with Msg;
   end Raise_Error;

   -----------------
   -- Clear_Error --
   -----------------

   procedure Clear_Error (Ctx : in out GContext) is
   begin
      Ctx.Error_Flag := False;
   end Clear_Error;

   ------------------
   -- Error_Raised --
   ------------------

   function Error_Raised (Ctx : in out GContext) return Boolean is
   begin
      return Ctx.Error_Flag;
   end Error_Raised;

   ----------
   -- Home --
   ----------

   function Home (Ctx : in out GContext; Axis : Axis_Name)
                  return Boolean is
      pragma Unreferenced (Axis, Ctx);
   begin
      return False;
   end Home;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart, EEnd : Natural) is
   begin
      Ctx.Error_Flag := True;
      Ctx.Raise_Error ("Error: " & Msg);
   end Report_Error;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart : Natural) is
   begin
      Report_Error (Ctx, Line, Msg, EStart, EStart);
   end Report_Error;

   ---------
   -- Log --
   ---------

   procedure Log (Ctx : in out GContext; Lvl : Log_Level; Str : String) is
   begin
      null;
   end Log;


end Gcode.Context;
