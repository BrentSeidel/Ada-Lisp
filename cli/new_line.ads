--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of Tiny-Lisp CLI.
--  Tiny-Lisp is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  Tiny-Lisp is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Tiny-Lisp. If not, see <https://www.gnu.org/licenses/>.--
--
--  The text_io version of newline contains an optional parameter indicating
--  the number of lines to skip.  The type of this parameter is defined in
--  Ada.Text_IO.  This makes it awkward to define a function prototype that can
--  be used both when Ada.Text_IO is available and when it isn't.  This is a
--  crude hack to define locally a new_line that has no parameters and uses the
--  Ada.Text_IO new_line with the default value.
--
package new_line is
   procedure new_line;
end new_line;
