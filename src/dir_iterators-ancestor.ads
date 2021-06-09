with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Strings.Unbounded;

package Dir_Iterators.Ancestor is

    type Cursor is private;
    function Has_Element (Position : Cursor) return Boolean;

    package Ancestor_Dir_Iterator_Interfaces is new Ada.Iterator_Interfaces
        (Cursor => Cursor, Has_Element => Has_Element);

    type Ancestor_Dir_Iterator is
        new Ada.Finalization.Limited_Controlled and
            Ancestor_Dir_Iterator_Interfaces.Forward_Iterator with private;

    overriding function First (Object : Ancestor_Dir_Iterator) return Cursor;
    overriding function Next
        (It : Ancestor_Dir_Iterator; Position : Cursor) return Cursor;

    type Ancestor_Dir_Walk is tagged limited private with
        Default_Iterator  => Iterate,
        Iterator_Element  => String,
        Constant_Indexing => Element_Value;

    function Walk (Dir : String) return Ancestor_Dir_Walk;

    function Iterate
        (Tree : Ancestor_Dir_Walk)
         return Ancestor_Dir_Iterator_Interfaces.Forward_Iterator'Class;

    function Element_Value
        (Tree : Ancestor_Dir_Walk; Position : Cursor) return String;

private

    -- The base type that lets us use an iterator in a nice `for` loop.
    type Ancestor_Dir_Walk is tagged limited record
        Start : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    type Ancestor_Dir_Iterator is new Ada.Finalization.Limited_Controlled and
        Ancestor_Dir_Iterator_Interfaces.Forward_Iterator with record
        Start : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    type Cursor is record
        Current : Ada.Strings.Unbounded.Unbounded_String;
    end record;

end Dir_Iterators.Ancestor;
