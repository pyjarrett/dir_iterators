with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Strings.Unbounded;

package Dir_Iterators.Recursive is

    -- A simple type to return each directory entry.
    type Reference_Type
       (Element : not null access constant Ada.Directories
           .Directory_Entry_Type)
    is null record with
        Implicit_Dereference => Element;

    type Cursor is private;
    function Has_Element (Position : Cursor) return Boolean;

    package Recursive_Dir_Iterator_Interfaces is new Ada.Iterator_Interfaces
       (Cursor => Cursor, Has_Element => Has_Element);

    type Filter_Function is access function
       (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean;
    -- A function used to prune directories or files from the search results.

    type Recursive_Dir_Iterator(Filter : access function
       (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean) is
       new Ada.Finalization.Limited_Controlled and
          Recursive_Dir_Iterator_Interfaces.Forward_Iterator with private;

    overriding function First (Object : Recursive_Dir_Iterator) return Cursor;
    overriding function Next
       (It : Recursive_Dir_Iterator; Position : Cursor) return Cursor;

    type Recursive_Dir_Walk (Filter : Filter_Function) is
       tagged limited private with
        Default_Iterator  => Iterate,
        Iterator_Element  => Reference_Type,
        Constant_Indexing => Element_Value;

    function Walk
       (Dir : String; Filter : access function
       (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean
       := null)
        return Recursive_Dir_Walk;

    function Iterate
       (Tree : Recursive_Dir_Walk)
        return Recursive_Dir_Iterator_Interfaces.Forward_Iterator'Class;

    function Element_Value
       (Tree : Recursive_Dir_Walk; Position : Cursor) return Reference_Type;

private

    -- The base type that lets us use an iterator in a nice `for` loop.
    type Recursive_Dir_Walk(Filter : Filter_Function) is tagged limited record
        Root : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    -- The list of unprocessed directories needs to be stored.
    package String_Vectors is new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Ada.Strings.Unbounded.Unbounded_String,
        "="          => Ada.Strings.Unbounded."=");

    type Recursive_Dir_Iterator (Filter : access function
       (Dir_Entry : Ada.Directories.Directory_Entry_Type) return Boolean)
    is new Ada.Finalization.Limited_Controlled and
       Recursive_Dir_Iterator_Interfaces.Forward_Iterator with record
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
        Current_Level   : String_Vectors.Vector;
    end record;

    overriding
    procedure Finalize (It : in out Recursive_Dir_Iterator);

    type Recursive_Dir_Iterator_Access is access all Recursive_Dir_Iterator;

    type Cursor is record
        It : Recursive_Dir_Iterator_Access;
    end record;

end Dir_Iterators.Recursive;
