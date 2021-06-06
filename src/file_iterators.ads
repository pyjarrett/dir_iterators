with Ada.Directories;

package File_Iterators is

--  type Parent_Iterator is private;
--  function Start(Dir : String) return Parent_Iterator;
--  function Next (It : in out Parent_Iterator) return Ada.Directories.Dir_Entry;
--  function Done (It : in Parent_Iterator) return Boolean;

type Recursive_File_Iterator is private
function Start (Dir : String) Return Recursive_File_Iterator;
function Next (It : in out Recursive_File_Iterator);

end File_Iterators;
