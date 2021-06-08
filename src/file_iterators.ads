with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Strings.Unbounded;

package File_Iterators is
     type Reference_Type
      (Element : not null access constant Ada.Directories.Directory_Entry_Type)
     is null record with
       Implicit_Dereference => Element;

    type Cursor is private;
    function Has_Element (Position : Cursor) return Boolean;

    package Recursive_File_Iterator_Interfaces is new Ada.Iterator_Interfaces
        (Cursor => Cursor, Has_Element => Has_Element);

    type Recursive_File_Iterator is
        new Ada.Finalization.Limited_Controlled and
            Recursive_File_Iterator_Interfaces.Forward_Iterator with private;

    overriding function First (Object : Recursive_File_Iterator) return Cursor;
    overriding function Next
        (It : Recursive_File_Iterator; Position : Cursor) return Cursor;

    type Recursive_Dir_Tree is tagged limited private with
        Default_Iterator  => Iterate,
        Iterator_Element  => Reference_Type,
        Constant_Indexing => Element_Value;

    function Walk (Dir : String) return Recursive_Dir_Tree;

    function Iterate
        (Tree : Recursive_Dir_Tree)
         return Recursive_File_Iterator_Interfaces.Forward_Iterator'Class;

    function Element_Value
        (Tree : Recursive_Dir_Tree; Position : Cursor) return Reference_Type;

private

    -- The base type that lets us use an iterator in a nice `for` loop.
    type Recursive_Dir_Tree is tagged limited record
        Root : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    -- The list of unprocessed directories needs to be stored.
    package String_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Ada.Strings.Unbounded.Unbounded_String,
         "="          => Ada.Strings.Unbounded."=");

    type Recursive_File_Iterator is new Ada.Finalization.Limited_Controlled and
        Recursive_File_Iterator_Interfaces.Forward_Iterator with record
        -- There's some weirdness and complexity resulting from skipping the
        -- files of the current and parent directories (. and .. respectively).
        --
        -- The state of the next step current search is stored, so for last
        -- entry in a directory, there will be "no more entries", while Next
        -- Entry is still valid.
        --
        -- This causes the strange state of the current search being done, there
        -- being no more directories left to process, but still one single valid
        -- entry.
        Has_Valid_Entry : Boolean;
        Current_Search  : Ada.Directories.Search_Type;
        Next_Entry      : aliased Ada.Directories.Directory_Entry_Type;
        Left_To_Process : String_Vectors.Vector;
    end record;

    type Recursive_File_Iterator_Access is access all Recursive_File_Iterator;

    type Cursor is record
        It : Recursive_File_Iterator_Access;
    end record;

end File_Iterators;
