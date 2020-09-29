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
